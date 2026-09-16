# Rollout: hcurl pin bump to b9b16d6f and streaming validation

- date: 2026-09-16
- repository: clickhouse-driver (branch `hcurl-rowbinary`, no commits made)
- change: `hcurl` pin `04647ddd851d0567e93a3a473ca61d8e8220967f` →
  `b9b16d6f1f676904ce5fd70ad5384f3d144681a3` in `cabal.project`, `flake.nix`
  and `flake.lock`
- server under test: ClickHouse 25.3.14.14, isolated docker container
  `ch-driver-it2` (image `clickhouse/clickhouse-server:25.3`,
  `-p 18123:8123`, user `default`, password `itpass`, ephemeral data;
  removed again after the runs)

## Upstream evidence

```
$ git ls-remote https://github.com/Reykudo/hcurl
b9b16d6f1f676904ce5fd70ad5384f3d144681a3        HEAD
c944320a743b14171a8768eae4ea9ec3787d9ea0        refs/heads/libuv
a55a90a5d36012dee5383c49f7ade6c8e96bd2ad        refs/heads/libuv-resourcet
b9b16d6f1f676904ce5fd70ad5384f3d144681a3        refs/heads/master
baaeb93c66fb1cee0e16e24214bbcf8df27515eb        refs/heads/refactored
```

No tags exist, so the newest supported revision is `master` HEAD
`b9b16d6f1f676904ce5fd70ad5384f3d144681a3` (commit date 2026-09-04). The
remaining heads are older side branches (`libuv` is from 2023, `refactored`
is an ancestor of `master`). Two commits changed behaviour since the old pin:

- `77d5730 feat!: rebuild reactor for duplex streaming`
- `b9b16d6 perf(managed)!: unbox worker counters`

The only source change required in this repository is the renamed request
field (`Request.host` → `Request.url`); `Client.hs` now builds requests from
`HCurl.Request.defaultRequest` and overrides the fields it needs.

## Nix environment

`nix flake update hcurl` locked the new revision
(`narHash: sha256-Y/H47I2I1B4aWw2u6Oo+GAIuqni0pdRspfZifZAZqxs=`). A forced
fresh evaluation (`nix print-dev-env path:.`) rebuilt
`cabal2nix-hcurl.drv`/`hcurl-0.1.0.0.drv` and the dev shell, and `direnv
reload` reported `nix-direnv: cache invalidated ... Renewed cache` (no
“Falling back to previous environment”). The dev shell’s `hcurl` resolves to
the output of that freshly built derivation. `nix build --no-link
"path:<temp copy>#clickhouse-driver"` produced
`/nix/store/3s2fr461l0kihln3c51jc713w4wl1rl2-clickhouse-driver-0.2.0.0`
(the temp copy was used because flakes only see git-tracked files and the new
test suite is still untracked).

## Verification performed

All commands ran through `direnv exec .` with `cabal -j1`.

- `cabal test` — unit suite 9/9, integration suite skips without `CH_URL`.
- `CH_URL=http://localhost CH_PORT=18123 CH_PASSWORD=itpass cabal test
  clickhouse-driver-integration` — read-only default: 6/6, prints that
  DDL/DML checks are skipped.
- `... CH_INTEGRATION_ALLOW_WRITES=1 cabal test clickhouse-driver-integration`
  — 8/8:

```
[evidence] folded 1e6 rows in 136.177ms
[evidence] max live bytes grew by 100056 B during the fold
[evidence] first row after 52.182ms, stream complete after 2012.523ms
[evidence] aborted a ~1000s stream after 152.247ms
[evidence] inserted 400000 rows (~5 MiB RowBinary) in 150.794ms
[evidence] ClickhouseServerException 404: Code: 60. DB::Exception: Unknown table expression identifier '__ch_driver_missin
[evidence] truncated body raised PartialFile
```

  Covered: large SELECT strictly folded without materialising the result,
  first rows before response completion, early downstream termination with a
  follow-up query on the same agent, server errors, transport-level
  truncation (raw TCP server sending `Content-Length: 1000` and 10 bytes)
  with a follow-up query on the same agent, INSERT round trips (1k and 400k
  rows) and multipart external tables (UInt64, String, `scalarExternal`).
- Write-safety check: pre-created `it_roundtrip`/`it_bulk` tables holding
  marker rows, then ran the write-enabled suite; both tables and their rows
  survived (`user-data-1`, `user-data-2`), because the suite creates
  uniquely-named owned tables (`it_roundtrip_<unique>_<nanos>`) and drops
  only what it created. After the run `system.tables` for the database was
  empty again.
- `CH_URL=... cabal run -v0 exe:clickhouse-driver-example` — exit 0; full
  wide-type matrix round trip.

## How to reproduce

```console
docker run -d --name ch-driver-it2 -p 18123:8123 \
  -e CLICKHOUSE_USER=default -e CLICKHOUSE_PASSWORD=itpass \
  clickhouse/clickhouse-server:25.3
direnv exec . cabal -j1 build all
direnv exec . cabal -j1 test clickhouse-driver-test
CH_URL=http://localhost CH_PORT=18123 CH_PASSWORD=itpass \
  direnv exec . cabal -j1 run test:clickhouse-driver-integration
CH_URL=http://localhost CH_PORT=18123 CH_PASSWORD=itpass CH_INTEGRATION_ALLOW_WRITES=1 \
  direnv exec . cabal -j1 run test:clickhouse-driver-integration
```

## Limitations

- The live checks ran against an ephemeral local container; no production
  server was touched. The container was stopped and removed afterwards.
- The write checks require `CH_INTEGRATION_ALLOW_WRITES=1` and should only be
  pointed at a disposable server/database; they create and drop uniquely
  named temporary tables and never touch pre-existing ones.
- Streaming request bodies (`HCurl.Upload`, new in this revision) are not
  used by the driver: INSERT payloads are still sent as one buffered body.
  Adopting upload streaming is a functional change beyond this pin bump.
- The integration suite skips itself when `CH_URL` is unset, so the default
  `cabal test` run does not contact a server.
