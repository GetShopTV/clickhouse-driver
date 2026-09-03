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
* `JSON` columns travel as RowBinary Strings (the driver sets
  `output_format_binary_write_json_as_string` /
  `input_format_binary_read_json_as_string`) and decode into `Aeson.Value`.

## Usage

```haskell
import Database.ClickHouse

main :: IO ()
main = do
  transport <- newHTTPTransport defaultHTTPSettings
  let conn = defaultConnection transport
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
be added later.

Streaming query: `sourceQuery` returns a `ConduitT` that yields one decoded
row at a time. Plain helpers `runQuery`, `runInsert`, `runCommand` wrap the
conduit for simple use.

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
for the recorded run.

## Development

`nix develop` (or `direnv allow`) provides GHC 9.10, cabal-install, `c2hs`,
pkg-config and the libcurl/libuv development files. The pinned `hcurl`
revision is declared in `cabal.project` (and mirrored as a flake input);
cabal fetches and builds it from source.
