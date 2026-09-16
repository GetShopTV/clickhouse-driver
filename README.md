# clickhouse-driver

Streaming [ClickHouse](https://clickhouse.com) HTTP client for Haskell.

The transport is built on [hcurl](https://github.com/Reykudo/hcurl) (libcurl
multi interface) and the wire format is binary:

* INSERT payloads are encoded as `RowBinary`;
* SELECT responses use `RowBinaryWithNamesAndTypes` — the response header
  carries column names and types, so rows decode into dynamically typed
  `ClickhouseType` vectors without a client-side schema;
* result rows are decoded and yielded incrementally (a conduit), so large
  result sets can be consumed as a stream;
* the response body is read through hcurl's bounded streaming reader and
  decoded row by row, so consumers that do not retain rows (folds, filters,
  writers) keep memory bounded per decoded row instead of holding the whole
  result — `runQuery`/`runQueryWithExternals` collect by design; stopping a
  stream early releases the transfer and the agent stays usable for later
  queries;
* `JSON` columns decode into `Aeson.Value` only with explicitly enabled
  RowBinary string settings (see below). Ordinary requests send no settings.

## Usage

```haskell
import Database.ClickHouse

main :: IO ()
main = do
  conn <- connectHTTP defaultHTTPSettings
  runCommand conn "CREATE TABLE IF NOT EXISTS t (n UInt64, s String) ENGINE = Memory"
  runInsert conn "t" ["n", "s"]
    [ [ClickUInt64 1, ClickString "one"]
    , [ClickUInt64 2, ClickString "two"]
    ]
  rows <- runQuery conn "SELECT n, s FROM t ORDER BY n"
  print rows
```

Connection parameters come from `ClickhouseConnectionSettings`; HTTP details
(host, port, timeouts) from `ClickhouseHTTPSettings`. The library API is
polymorphic over a `ClickhouseClient` transport so alternative transports can
be added later.  `connectHTTP` spawns the default managed agent and returns
ready-to-use settings; override the default credentials with record update:

```haskell
let conn' = conn { username = "report", password = "secret", database = "analytics" }
```

Server settings use the existing value type: `settings :: [(String, ClickhouseType)]`,
defaulting to `[]`. For example:

```haskell
let jsonConn = conn { settings = [("output_format_binary_write_json_as_string", ClickBool True)] }
rows <- runQuery jsonConn "SELECT CAST('{\"n\":1}' AS JSON)"
```

Supported setting values are `ClickBool` (rendered as `0`/`1`), `ClickString`
(raw bytes, not SQL-quoted), all signed/unsigned integer constructors (exact
decimal), and finite `ClickFloat32`/`ClickFloat64`. Nonfinite floats and other
constructors fail with `ClickhouseSettingsException` before HTTP. HTTP query
encoding escapes names and values normally. Use connection fields for credentials
and database, and `requestParams` for SQL `param_*` bindings, not `settings`.

Request `requestParams` override connection settings; the first occurrence of a
key wins within either list. Only one value per key is sent. `sourceRequest`
streams and decodes a custom request with those same effective parameters;
`sourceQuery` and the other query helpers use it automatically.

JSON output requires `output_format_binary_write_json_as_string=1`. Without it,
decoding fails with `ClickhouseDecodeException` as soon as the schema contains
JSON, including nested or zero-row results. No schema query or retry is made.
JSON input separately requires `input_format_binary_read_json_as_string=1` for
`runInsert` and external tables. Neither setting implies the other. Enable only
the settings your server/user permits; the driver never changes readonly policy.
Boolean modes recognize `1` and case-insensitive `true`; `0`/`false` disable them.
SQL-embedded `SETTINGS` or server-profile defaults are not inferred: configure
the matching setting explicitly so the codec knows the wire representation.

Direct codec calls default to no settings too. Their `*WithSettings` variants
accept the effective HTTP parameter list returned by `effectiveRequestParams`.
Raw `insertRequest` payloads have no schema and remain the caller's responsibility;
when encoding JSON manually, use `encodeRowsWithSettings` with the same effective
parameters that will be sent.

Streaming query: `sourceQuery` returns a `ConduitT` that yields one decoded
row at a time, as soon as the server sends it. Plain helpers `runQuery`,
`runInsert`, `runCommand` wrap the conduit for simple use; `runQuery`
collects, so prefer `sourceQuery` for large results:

```haskell
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit (ConduitT, await, runConduit, (.|))
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Database.ClickHouse

streamedSum :: ClickhouseConnectionSettings ClientHTTP -> IO Integer
streamedSum conn =
  runResourceT $
    runConduit $
      sourceQuery conn "SELECT number FROM numbers(1000000)" .| foldNumberRows

foldNumberRows :: Monad m => ConduitT (Vector ClickhouseType) o m Integer
foldNumberRows = go 0
  where
    go !total = await >>= \case
      Nothing -> pure total
      Just row -> case Vector.toList row of
        [ClickUInt64 value] -> go (total + toInteger value)
        _ -> error "unexpected row shape"

main :: IO ()
main = do
  conn <- connectHTTP defaultHTTPSettings
  total <- streamedSum conn
  print total
```

Consuming only a prefix of the conduit is safe: closing the resource scope
aborts the underlying transfer and the connection (hcurl agent) can be used
for the next query.

## Binary parameters (external tables)

Instead of rendering `param_*` values as text, pass parameter data as
temporary external tables in `RowBinary`. The statement references the table
by name:

```haskell
let ids =
      externalTable
        "ids"
        [("value", "UInt64")]
        [[ClickUInt64 1], [ClickUInt64 2]]

rows <-
  runQueryWithExternals
    conn
    [ids]
    "SELECT count() FROM events WHERE user_id IN (SELECT value FROM ids)"
```

Tables are attached as `multipart/form-data` parts (data in `RowBinary`,
plus `<name>_format` and `<name>_structure` metadata). Use
`sourceQueryWithExternals` to stream the rows instead of collecting them.

External type declarations are inspected structurally for JSON before sending,
independently of the supported response-decoder types. Timezones, decimal aliases,
named tuple fields and enum labels remain server-validated; JSON nested in type
arguments still requires the explicit input setting, even for empty tables.

## hcurl agent

The transport never creates an agent implicitly.  The caller owns it and
passes it through `ClickhouseHTTPTransport`:

```haskell
transport <-
  newHTTPTransport -- spawns the default managed agent (hcurl default policy)
    defaultHTTPSettings
```

or, to pick the agent topology yourself:

```haskell
import HCurl.Agent (spawnAgent, spawnThreadedAgent, spawnManagedAgent)
import HCurl.Simple (initCurl)

agent <- spawnThreadedAgent 4 HCurl.Types.defaultConfig
let transport = ClickhouseHTTPTransport defaultHTTPSettings agent
```

`initCurl` is required once before any custom agent is used; agents created
by `newManagedAgent` do that automatically.

## Integration harness

`cabal run exe:clickhouse-driver-example` exercises the driver against a live
server; it reads `CH_URL`, `CH_PORT`, `CH_DATABASE`, `CH_USER`,
`CH_PASSWORD` from the environment. See
[`rollout/2026-09-03-clickhouse-local.md`](rollout/2026-09-03-clickhouse-local.md)
for the original recorded run and
[`rollout/2026-09-16-hcurl-b9b16d6-streaming.md`](rollout/2026-09-16-hcurl-b9b16d6-streaming.md)
for the streaming validation after the hcurl pin bump.

`cabal test` additionally runs `clickhouse-driver-integration`, which checks
the streaming behaviour against a live server: a large SELECT folded without
materialising the result, rows arriving before the response completes, early
termination cancelling the transfer while the agent stays usable, transport
truncation and server errors, plus INSERT and external-table round trips. It
reads the same `CH_*` variables and reports a skip when `CH_URL` is unset.

Those checks are read-only by default. The INSERT round trips require
`CH_INTEGRATION_ALLOW_WRITES=1`; when enabled they create and drop
uniquely-named throwaway tables, so point `CH_URL` at a disposable server or
database whose data may be modified.

## Development

`nix develop` (or `direnv allow`) provides GHC 9.10, cabal-install, `c2hs`,
pkg-config and the libcurl/libuv development files. The pinned `hcurl`
revision is declared in `cabal.project` (and mirrored as a flake input);
cabal fetches and builds it from source. The current pin is
`b9b16d6f1f676904ce5fd70ad5384f3d144681a3` (upstream `master`; hcurl publishes
no tags, so the revision is pinned explicitly).
