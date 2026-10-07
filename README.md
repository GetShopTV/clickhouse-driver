# clickhouse-driver

Streaming [ClickHouse](https://clickhouse.com) HTTP client for Haskell.

The default transport is built on [hcurl](https://github.com/Reykudo/hcurl) (libcurl
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

## Query parameters (escaped text)

`{name:Type}` placeholders bind typed values with `runQueryWithParams` (or
`sourceQueryWithParams` to stream the rows):

```haskell
rows <-
  runQueryWithParams
    conn
    [("p", ClickUInt64 41), ("ids", ClickArray (Vector.fromList [ClickUInt64 1, ClickUInt64 2]))]
    "SELECT {p:UInt64} + arraySum({ids:Array(UInt64)})"
```

The bindings travel as `param_<name>` values. When the request already is
`multipart/form-data` (external tables or typed parameters), they are plain text
fields of that body, in the pinned order: `query`, typed parameters,
external-table parts (`<name>_format`, `<name>_structure`, RowBinary data).
Without a multipart body the values are sent as URL parameters, together with
the ClickHouse settings from `requestParams`.

Values are escaped *text*, not RowBinary: the server parses `{name:Type}` with
`deserializeTextEscaped`, so raw bytes and file parts named `param_x` are
rejected (`BAD_QUERY_PARAMETER`). `Conversion.Text.Escaped` renders the same
type coverage as `RowBinary`, with the literal forms that escaped text needs:
strings are bare at the top level but single-quoted inside `Array`/`Tuple`/`Map`
(`['a','b']`, `(1,'x')`, `{'k':1}`), NULL is `\N` at the top level but `NULL`
inside a container, and `DateTime`/`DateTime64` are written as epoch seconds so
the value does not depend on the time zone of the placeholder. `Decimal*` and
`JSON` parameters are rejected with `ClickhouseSettingsException` because their
escaped-text form cannot be derived from the value alone (the decimal scale and
the JSON text shape live in the SQL placeholder type); use an external table for
those.

An INSERT cannot carry a multipart body (its body is the RowBinary payload), so
an INSERT with typed parameters falls back to `param_<name>` URL parameters
next to the `query` parameter. The parameter values themselves are still
escaped text:

```haskell
let statement =
      "INSERT INTO t (n, tag) SELECT {add:UInt64} + length(tag) - 1 AS n, tag"
        <> " FROM input('tag String') FORMAT RowBinary"
    payload = encodeRowsWithSettings [] (map (Vector.singleton . ClickString) ["x", "yy"])
    request = (insertRequest statement payload){requestQueryParams = [("add", ClickUInt64 40)]}
runResourceT (runConduit (sendSource conn request .| sinkNull))
```

For large collections prefer the external-table API above: a parameter value is
a single text field, while an external table is streamed as RowBinary.

## Execution policy and custom transports

`ClickhouseExecution` separates transport from whole-operation policy. Obtain
the stock execution with `defaultExecution conn`, update its fields, and attach
it with `withExecution execution conn`:

* `executeSource` supplies raw response chunks for a `CHRequest`. Replace it
  to use another HTTP client or an application-owned transport. Its contract
  uses shared connection settings, `CHRequest`, and a resource-managed conduit,
  not hcurl request/response types.
* `executeRequest` wraps an `IO` action for a fully consumed request, including
  row decoding and resource cleanup. It can add logging, deadlines or retry
  without replacing the stock transport. It receives the current connection
  and request, and is polymorphic in the action's result.

`runRequest`, `runQuery`, `runQueryWithParams`, `runQueryWithExternals`,
`runInsert` and `runCommand` work with any `ClickhouseClient`. Existing instances
need no changes: their new `runClientRequest` method defaults to running the
action once. `defaultExecution` preserves an existing client's wrapper,
including nested execution records. Updates to shared connection fields after
`withExecution` reach both the wrapper and the original transport. Native
transport settings are captured from the connection passed to `defaultExecution`;
configure HTTP hooks before adapting that connection.

For example, an application can use [retry](https://hackage.haskell.org/package/retry)
by updating just `executeRequest`. Add `retry` and `exceptions` to the
application's dependencies; neither is a new dependency of the driver library.
The application supplies both an explicit idempotency decision and a predicate
for retryable transport failures:

```haskell
{-# LANGUAGE RankNTypes #-}

import Control.Monad.Catch (Handler(..))
import Control.Retry (exponentialBackoff, limitRetries, recovering)
import Database.ClickHouse

retrying
  :: (CHRequest -> Bool)
  -> (ClickhouseTransportException -> IO Bool)
  -> ClickhouseConnectionSettings ClientHTTP
  -> ClickhouseConnectionSettings ClientExecution
retrying mayRetry shouldRetry base =
  let normal = defaultExecution base
      policy = exponentialBackoff 50_000 <> limitRetries 2
      execution = normal
        { executeRequest = \connection request action ->
            let once = executeRequest normal connection request action
            in if mayRetry request
                 then recovering policy [const (Handler shouldRetry)] (\_ -> once)
                 else once
        }
  in withExecution execution base
```

Capture `normal` outside the record update and call its field, not the updated
record's field, to avoid recursion. Each attempt consumes the whole response
and releases its `ResourceT` resources before the next attempt; a failed
attempt's partially collected rows are discarded. Native HTTP policy, hooks
and final transfer metrics still apply separately to every attempt.

Retry is opt-in. A lost response does not prove that ClickHouse did not execute
the statement: do not automatically retry INSERT, DDL or other non-idempotent
operations. Do not infer safety from HTTP POST or a SQL prefix; use application
knowledge about the complete operation. Limit attempts, classify failures,
propagate asynchronous cancellation and avoid catching `SomeException` as
retryable. The example retries only matching transport exceptions, not server
or decode errors. Backoff and attempt limits do not impose an overall deadline;
apply one outside the retry block if required.

Streaming helpers (`sourceRequest`, `sourceQuery`, `sourceQueryWithParams` and
`sourceQueryWithExternals`) use `executeSource`, but deliberately bypass
`executeRequest`: automatically replaying a stream could duplicate rows already
delivered to a consumer. Wrap a complete consumer operation explicitly only
when its effects can safely be repeated.

For a custom transport without a `ClickhouseClient` instance, start with
`executionFromSource applicationSource` and `defaultConnection execution`.
`applicationSource :: ClickhouseRequestSource` must yield the requested wire
format (normally `RowBinaryWithNamesAndTypes` for queries), honor effective
settings, credentials, headers and request payloads, report failures including
incomplete responses, and release resources on early close or cancellation.
Its default `executeRequest` runs once. The execution API does not depend on
hcurl types, although the package still includes and depends on the stock
hcurl backend.

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

## HTTP policy and hooks

`connectHTTP`, `newHTTPTransport`, and the two-field
`ClickhouseHTTPTransport` constructor retain their original defaults.
Use `connectHTTPWith` or `newHTTPTransportWith` to supply a separate
`ClickhouseHTTPConfig` without changing connection credentials or ClickHouse
settings:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Database.ClickHouse

main :: IO ()
main = do
  let config = defaultHTTPConfig
        { httpExtraOptions = [OptionAcceptEncoding "identity"]
        , httpStreamConfig = StreamConfig { bufferedChunks = 4 }
        , httpErrorBodyLimit = 4096
        , httpOnEvent = print
        }
  conn <- connectHTTPWith defaultHTTPSettings config
  rows <- runQuery conn "SELECT number FROM numbers(10)"
  print rows
```

`OptionAcceptEncoding "identity"` opts out of HTTP response compression for
this connection. Compression can delay the first bytes of small streaming
responses; it is not disabled globally by the driver. Other native hcurl
options can control HTTP version, timeouts, redirects, TLS and TCP policy.
Options are applied after native defaults, in order.

The integration harness can exercise this opt-in policy against a local server
without changing the original streaming assertions:
`cabal test clickhouse-driver-integration --test-options=--http-identity`.

The extension points are:

* `httpModifyRequest`: receives the effective `CHRequest` and the fully built
  `HCurl.Request.Request`, including authentication headers and the serialized
  multipart body. Return a modified native request to change its URL, headers,
  body, timeouts or options. This also applies to public `buildRequest` calls;
  higher-level send functions resolve effective ClickHouse settings first.
* `httpOnResponse`: receives the original `CHRequest` and
  `HCurl.Response.HttpParts` before the body is consumed. It can inspect response
  headers or reject a response by throwing an exception.
* `httpIsErrorStatus`: controls which statuses throw
  `ClickhouseServerException`; the default is `>= 400`.
* `httpStreamConfig`: bounds the response queue in chunks. The capacity must
  be positive; a smaller queue applies backpressure rather than dropping data.
* `httpErrorBodyLimit`: bounds the retained server-error text. The full body
  is still drained and counted in metrics. Zero discards all text; negative
  values are rejected before submission.
* `httpOnEvent`: a logger/metrics callback for `HTTPRequestStarted`,
  `HTTPResponseReceived` and `HTTPRequestFinished`. Every submitted transfer
  has a correlation UUID and one terminal event, even after early termination
  or asynchronous cancellation. Terminal events contain the completed transfer
  metrics and distinguish success, HTTP failure, curl failure and cancellation.

Callbacks run synchronously on the request or resource-cleanup thread, never
on the curl reactor. Slow callbacks delay that thread. Synchronous exceptions
from `httpOnEvent` are ignored so a broken logger cannot replace a query error;
asynchronous exceptions still propagate. Exceptions from the request/response
policy hooks propagate: the request hook fails before submission, and the
response hook cancels the transfer immediately, even if its exception is caught
inside a longer-lived resource scope. Hooks can run concurrently for different
requests, so shared callback state must be thread-safe; use the correlation UUID
to group each request's events.

Automatic events include no SQL, parameters, credentials, URLs, headers or
body text. The raw request/response hooks deliberately expose more information;
sanitize it before logging. A successful transfer event does not guarantee that
row decoding or downstream application processing succeeded.

For a caller-owned agent or a per-call override, configure the existing
transport instead of creating another agent:

```haskell
let configured = conn
      { connectionSettings = withHTTPConfig config (connectionSettings conn) }
```

`withHTTPConfig` does not acquire ownership of the agent or change its lifetime.
Record updates of `transportOptions` and `transportAgent` retain the configured
hooks. Use these selectors and `transportConfig` to inspect a configured
transport; the legacy constructor remains available for default transports.

## Error diagnostics

Transport and HTTP-status exceptions raised by the HTTP driver include a
completed transfer snapshot. Logging the exception with `show` or
`displayException` includes these metrics automatically; the driver does not
write to stderr or require a logger.

The typed snapshot is available through `transportMetrics` on
`ClickhouseTransportException` and `serverMetrics` on
`ClickhouseServerException`. Both return `Maybe ClickhouseTransferMetrics`;
the original exception constructors and patterns remain usable, and manually
constructed legacy exceptions have `Nothing` metrics.

`ClickhouseTransferMetrics` contains:

* `requestBodyBytes`: the complete planned request-body size in bytes, even
  if connecting or uploading failed.
* `responseStatus`: the last observed HTTP status, or `Nothing` when no status
  arrived. This can be an informational status such as `100` if the connection
  reset before a final response.
* `transferCode`: the final libcurl result, including errors such as
  `RecvError`, `PartialFile`, `CouldntConnect`, or `OperationTimedout`.
* `curlMetrics`: the hcurl snapshot, including `uploadProgress`, `uploadTotal`,
  `downloadProgress`, `downloadTotal`, upload/download speeds, and DNS,
  connection, TLS, first-response-byte and total timings.

Sizes are bytes, speeds are bytes per second, and timings are microseconds.
Streaming `downloadProgress` counts decoded response-body bytes; native download
totals and speeds can refer to compressed wire bytes, so they need not agree
when HTTP compression is enabled.
Timings are cumulative milestones from the beginning of the transfer, not
individual phase durations. Unknown or unmeasured native values are preserved;
for example, a refused connection can report `uploadTotal = 0` even though
`requestBodyBytes` is positive. Uploaded bytes do not prove that ClickHouse
received or processed the entire query.

The new snapshot does not include SQL, parameters, credentials, URLs, or request
headers. The existing `serverMessage` still contains the server's response text
and is not sanitized by this feature.

The unit suite uses loopback TCP servers to exercise resets, partial uploads,
truncated responses, connection refusal, timeouts and HTTP errors without a
ClickHouse server. Set `CH_TEST_LOG_METRICS=1` to print the captured exceptions
when running `cabal test clickhouse-driver-test --test-show-details=direct`.

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
also binds typed query parameters (scalars, an escaped string, arrays, and a
multipart body that carries parameters *and* an external table at the same
time). It reads the same `CH_*` variables and reports a skip when `CH_URL` is
unset.

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
