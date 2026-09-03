# Rollout: integration test against local ClickHouse (RowBinary + hcurl transport)

- date: 2026-09-03
- branch: `hcurl-rowbinary`
- repository: clickhouse-driver (full rework: transport on `hcurl`, wire format `RowBinary` / `RowBinaryWithNamesAndTypes`)
- server under test: ClickHouse 25.3.14.14 (docker container `ch-driver-it`, user `default`, password `itpass`, HTTP `http://localhost:8123`)
- verification: `cabal test` (unit: type-name parser, golden RowBinary bytes, full round trip, streaming across chunk boundaries, truncation) — passed 5/5.
- verification: `clickhouse-driver-example` integration run against the live server — exit code 0.

## What was exercised

1. `SELECT version()` through the new HTTP transport (hcurl).
2. DDL via POST body (`CREATE TABLE`, `DROP TABLE`).
3. `runInsert` — rows serialised as `RowBinary`, statement sent as URL `?query=` parameter, payload as POST body.
4. `runQuery` — statement in POST body, response format `RowBinaryWithNamesAndTypes`, header (column count, names, types) decoded, rows decoded incrementally and compared field-by-field against the inserted values.
5. Streaming decode path across chunk boundaries (unit test feeding 7-byte chunks).

## Column types round-tripped live

UInt64, String, Float64, Bool, DateTime, Array(String) — basic table.
Full matrix table: Date, Date32 (including pre-epoch `1969-12-31`), DateTime, DateTime64(3) (ms precision), UUID, Decimal(18,4), Nullable(String) (present + NULL), Array(UInt16) (non-empty + empty), Map(String, UInt8), Tuple(Int8, String), FixedString(5), Bool, Float64 (negative).

Observed decoded rows:

```
| 1 | 2024-02-29 | 1969-12-31 | 2020-09-13 12:26:40 UTC | 2021-12-03 15:03:45.123 UTC | 550e8400-e29b-41d4-a716-446655440000 | 1234567890 | hi | [1, 2, 3] | {k: 7} | (-1, t) | fx(hello) | True | 1.5
| 2 | 1970-01-01 | 2024-03-01 | 1970-01-01 00:00:00 UTC | 1970-01-01 00:00:00 UTC | 00000000-0000-0000-0000-000000000000 | 0 | NULL | [] | {} | (5, ) | fx(ab\0\0c) | False | -2.5
```

All expected == actual.

## Findings fixed during the run

- hcurl/linkage: cabal build needs libcurl/libuv dev `.pc` (incl. transitive: libidn2, openssl, zlib under `share/pkgconfig`, brotli, nghttp2, ...) plus `LIBRARY_PATH`/`LD_LIBRARY_PATH` for `-lcurl -luv` during TH/compile; build tool `c2hs`.
- ClickHouse URL: `Network.HTTP.Types.renderQuery` already emits the leading `?`; appending `/?` produced `/?/?query=...` which the server rejected as `Setting ?query ... UNKNOWN_SETTING`. Fixed by concatenating the rendered query directly to the base URL.
- RowBinary header is `LEB128 count + N name strings + N type strings` (verified against server).
- UUID wire layout (first 8 bytes = most significant half, little-endian per half) matches the live server.
- `DateTime64(p)`: wire value is the scaled integer (`* 10^p`), decoder reconstructs `UTCTime` from the rational.

## How to reproduce

```console
docker run -d --name ch-driver-it -p 8123:8123 -e CLICKHOUSE_USER=default -e CLICKHOUSE_PASSWORD=itpass clickhouse/clickhouse-server:25.3
# build environment (nix): c2hs + curl.dev + libuv.dev + pkg-config; see rollout notes above for PKG_CONFIG_PATH/LIBRARY_PATH
cabal test
CH_PASSWORD=itpass cabal run -v0 exe:clickhouse-driver-example
```
