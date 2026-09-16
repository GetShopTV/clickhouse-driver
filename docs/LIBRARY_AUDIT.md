# Аудит библиотеки clickhouse-driver: возможности и корректность

- **Дата аудита:** 2026-09-16
- **Метод:** статическое чтение исходников (`src/`, `test/`, `example/`, `README.md`, `clickhouse-driver.cabal`, `cabal.project`, `flake.nix`), сверка протокольных фактов с первоисточниками (исходники ClickHouse `master`, официальная документация clickhouse.com). Сборки, тесты и живые серверы аудитором **не запускались** (эксклюзивная runtime-зона владельца реализации).
- **Состояние дерева:** `HEAD` = `dd3c97c` (pinned hcurl `04647ddd`), рабочее дерево содержит незакоммиченный апгрейд hcurl на `b9b16d6f1f676904ce5fd70ad5384f3d144681a3` (пины `cabal.project`/`flake.nix`/`flake.lock` согласованы; адаптация `Request.host` → `defaultRequest { url = ... }` в `Client.hs`), новый `test/integration/Spec.hs` и `rollout/2026-09-16-hcurl-b9b16d6-streaming.md`.
- **Runtime-доказательства владельца (приняты координатором, не дублировались):** `cabal build all` + свежая Nix-сборка; unit 9/9; live против изолированного ClickHouse 25.3.14.14: fold 1e6 строк без материализации (max-live +~100 КБ), первая строка ~52 мс при ~2013 мс полного потока, abort+reuse ~152 мс, bulk insert 400k строк, HTTP-ошибка и усечённое тело с повторным использованием агента, external tables; итог: read-only 6/6 по умолчанию, 8/8 с `CH_INTEGRATION_ALLOW_WRITES=1`.

Формат находок: **тип** (Ошибка / Неподдерживаемое / Дизайн / Документация), **сeverity**, доказательство по коду, влияние, идея репро, минимальный следующий шаг.

---

## 1. Определённые баги (подтверждены чтением кода + первоисточниками)

### B1. `DateTime` декодируется как Int32 вместо UInt32 — тихая порча дат после 2038 — ВЫСОКИЙ

- **Код:** `src/Database/Clickhouse/Conversion/Binary/Decode.hs:194` — `ChDateTime -> ClickDateTime . secondsToUTC . fromIntegral <$> pInt32le`.
- **Протокол:** на проводе `DateTime` — **UInt32** секунд, диапазон до `2106-02-07 06:28:15` (официальные доки: data-types/datetime, formats/RowBinary).
- **Влияние:** значения `>= 2^31` (после `2038-01-19 03:14:08`) знаково расширяются в отрицательные секунды → даты до 1970. Типичный продакшен-кейс «valid until 2100» читается как ~1903-1911 гг. Encode при этом пишет корректные байты в диапазоне 1970..2106 (`Encode.hs:75`, wrap mod 2^32 совпадает с UInt32), то есть **round-trip асимметричен**: вставил 2040 — прочитал 1903.
- **Репро:** `SELECT toDateTime('2040-01-01 00:00:00')` → ожидается `2040-01-01`, будет `1903-11-25 05:51:44 UTC`.
- **Минимальный шаг:** читать `pWord32le` и конвертировать как unsigned; unit-тест на границе `2147483647`/`2147483648` и `SELECT` с датой 2106.

### B2. Парсер типов отвергает `DateTime('TZ')` — падает весь запрос — ВЫСОКИЙ

- **Код:** `src/Database/Clickhouse/Conversion/Types.hs:144-164` — в `applyArgs` нет ветки `"DateTime"`, попадает в `Left "unsupported parametrised ClickHouse type"` (строка 164).
- **Протокол:** ClickHouse рендерит имя типа с таймзоной, когда она задана явно: `DataTypeDateTime::doGetName()` → `"DateTime('Europe/Berlin')"` при `has_explicit_time_zone` (src/DataTypes/DataTypeDateTime.cpp, master).
- **Влияние:** любая колонка/выражение с явной таймзоной (`toDateTime(x, 'UTC')` — включая даже `'UTC'`) делает весь SELECT недекодируемым. В схемах с явной TZ это полный отказ драйвера на таких запросах.
- **Репро:** `SELECT toDateTime('2024-01-01 00:00:00', 'Europe/Berlin')`.
- **Минимальный шаг:** принимать `"DateTime"` с аргументами и игнорировать TZ (как уже сделано для `DateTime64`, строка 161): тики на проводе epoch-based, декод в UTCTime остаётся корректным.

### B3. Именованные `Tuple` не парсятся — падает весь запрос — СРЕДНИЙ

- **Код:** `src/Database/Clickhouse/Conversion/Types.hs:107-142` — `readArgs` умеет только числа, кавычки и вложенные типы; `Tuple(\`name\` String, ...)` (ClickHouse квотит имена бэктиками) упирается в `readIdent`/`plainType "name"` → ошибка.
- **Влияние:** таблицы с именованными кортежами (частый паттерн для JSON-подобных схем) недекодируемы.
- **Репро:** таблица с колонкой `t Tuple(s String, n UInt8)`, `SELECT t FROM ...`.
- **Минимальный шаг:** в `readArgs` пропускать `ident` + (опционально бэктик-quoted) перед типом элемента; unit-тест на `Tuple(a Int8, b String)`.

### B4. `Enum8/Enum16` с отрицательными значениями не парсятся — НИЗКИЙ/СРЕДНИЙ

- **Код:** `src/Database/Clickhouse/Conversion/Types.hs:121-125` — после `'='` читаются только цифры (`C8.span isDigit`); `Enum8('a' = -1)` → `Left "expected a number after '='"`.
- **Протокол:** Enum8 допускает диапазон `[-128, 127]` (официальные доки data-types/enum).
- **Минимальный шаг:** допустить опциональный `-` перед числом; unit-тест.

### B5. Encode `Date`/`DateTime`/`Date32` молча заворачивает вне-диапазонные даты — СРЕДНИЙ/ВЫСОКИЙ

- **Код:** `src/Database/Clickhouse/Conversion/Binary/Encode.hs:73-75` — `int16LE (fromIntegral (epochDays day))` и `int32LE (round (utcTimeToPOSIXSeconds time))`.
- **Протокол:** `Date` — **UInt16** дней `[1970-01-01, 2149-06-06]`, `DateTime` — UInt32 `[1970-01-01, 2106-02-07]` (официальные доки date/datetime).
- **Влияние:** `ClickDate` на дату до 1970 (например 1960) заворачивается в валидный UInt16 (~2139 год): сервер молча сохраняет **другую дату**, round-trip возвращает её без ошибки. Аналогично `ClickDateTime` до 1970 → далёкое будущее. Это тихая порча данных на записи, без какого-либо сигнала.
- **Репро:** `runInsert conn t ["d"] [[ClickDate (fromGregorian 1960 1 1)]]` → чтение вернёт 2139-*.
- **Минимальный шаг:** range-check в encode (исключение при выходе за `[1970-01-01, 2149-06-06]` / `[epoch, 4294967295]`), unit-тест на 1960 и 2150.

### B6. Устаревший docstring про «неизвестные типы декодируются как ByteString» — Документация, НИЗКИЙ

- **Код:** `src/Database/Clickhouse/Client/Types.hs:194-195` — комментарий к `ClickhouseType` противоречит реальному поведению: неизвестные типы **падают явно** (`parseChType` → `Left`, см. `Conversion/Types.hs:207`, тест «unsupported column types fail loudly» в `test/Spec.hs`).
- **Минимальный шаг:** поправить комментарий (одна строка).

---

## 2. Неподдерживаемые типы и возможности протокола

Строки «не парсятся» падают **явно** (decode-ошибка при разборе заголовка), а не портят данные — это соответствует принятому решению «fail loudly». Остальные строки таблицы — потерянная метаинформация или отсутствующие возможности: они не «падают», а молча недоступны.

| Возможность | Статус | Доказательство |
|---|---|---|
| `SimpleAggregateFunction(f, T)` | не парсятся; на проводе = внутренний тип | `Conversion/Types.hs:164` |
| `AggregateFunction(...)`, geo (`Point`/...), `Nothing`, `Interval` | не парсятся (осмысленно: layout состояний версионируется) | `Conversion/Types.hs:164,207` |
| `Dynamic`, `Variant`, `Time`, `Time64` (24.x–25.x) | не парсятся | `Conversion/Types.hs:164,207` |
| Имена значений Enum | теряются (декод в `ClickInt8/16`) | `Decode.hs:220-222` |
| Имена/типы колонок результата | отбрасываются при разборе заголовка, в API недоступны | `Decode.hs:282,312` |
| Per-query settings (`wait_end_of_query`, `max_execution_time`, `max_result_rows`, `quota_key`, `session_id`, `query_id`, timezone) | нет высокоуровневого API в хелперах `runQuery`/`sourceQuery`/`runInsert`; низкоуровнево доступно уже сейчас: `CHRequest (..)` экспортируется вместе с полем `requestParams` и реэкспортируется из `Database.ClickHouse`, так что `sendSource conn ((selectRequest sql) { requestParams = ... })` работает | `Client/Types.hs:13-33,101-115`, `ClickHouse.hs:27,84-101` |
| HTTP-компрессия (ответ `Accept-Encoding`+`enable_http_compression`, запрос `Content-Encoding`/`compress=1`) | отсутствует | `Client.hs:207-228` (заголовки фиксированы) |
| Streaming insert (`HCurl.Upload` в новом hcurl) | не используется; payload полностью буферизуется | `ClickHouse.hs:133-137`, `Encode.hs:108-110`; rollout Ферта прямо откладывает это |
| Батчинг/чанкование больших insert | отсутствует | `ClickHouse.hs:127-137` |
| Повторные попытки / идемпотентность insert | отсутствуют; тело HTTP-запроса само по себе не устанавливает серверную транзакционность: при обрыве/таймауте исход неизвестен или частичен, ретрай может задублировать строки | дизайн, задокументировать |

**Проверено и НЕ является проблемой** (сверка с первоисточником/живым прогоном): `Decimal` всегда приходит каноническим именем `Decimal(P, S)` (`DataTypesDecimal.cpp: doGetName → fmt::format("Decimal({}, {})", ...)`) — алиасы `Decimal32(2)` современные серверы в заголовке не шлют; `LowCardinality(T)` в RowBinary сериализуется как plain-значение внутреннего типа (per-value `SerializationLowCardinality::serializeBinary → serializeImpl → nested`), поэтому `Decode.hs:228` корректен; UUID/IPv4/IPv6/Int128/256/Decimal256 layout подтверждён живым round-trip на 25.3.14.14 (rollout 2026-09-03 и 2026-09-16).

---

## 3. Жизненный цикл, отмена, backpressure, ресурсы

### R1. Буферизация insert и external tables — Неподдерживаемое, ВЫСОКИЙ (для «чего не хватает»)

- **Код:** `ClickHouse.hs:135` (`encodeRows` → один strict `ByteString` → `CurlTypes.Buffer`, `Client.hs:202-206`); multipart-тело external tables целиком собирается в память (`Client.hs:259-273`).
- **Влияние:** пиковая память = полный RowBinary payload; нет backpressure на записи. 400k строк (~5 МиБ) проверены живым прогоном; десятки миллионов строк упрётся в RAM.
- **Минимальный шаг:** streaming insert через `HCurl.Upload` (доступен в пине `b9b16d6`) либо явный batched API (`runInsertBatched` с размером блока); задокументировать потолок буферизации.

### R2. Отмена раннего чтения привязана к концу ResourceT-скоупа — Дизайн, СРЕДНИЙ

- **Код:** `sendSourceHTTP` (`Client.hs:109-126`) поднимает `httpStreaming` в монаде кондуита; освобождение transfer происходит через `register cancelOnce` hcurl при выходе из скоупа. `closeBody` (экспортируется новым hcurl) драйвером **не вызывается**.
- **Статус:** для хелперов `runQuery/runInsert/runCommand` и задокументированного паттерна `runResourceT $ runConduit ...` поведение корректное и подтверждено живым прогоном (abort ~1000-секундного потока за ~152 мс, агент остаётся рабочим).
- **Остаточный риск:** в долгоживущем скоупе (несколько запросов в одном `runResourceT`, либо `sendSourceAcquire` у Persistent-подобных call-site'ов) брошенные transfer'ы копятся до конца скоупа. `sendSourceAcquire` — no-op shim (`Client/Types.hs:94-99`), время жизни transfer'а к `Acquire` не привязано — docstring слегка переобещает.
- **Минимальный шаг:** conduit-level cleanup, вызывающий `CurlStream.closeBody` при раннем завершении, либо явная документация «скоуп = запрос»; поправить docstring `sendSourceAcquire`.

### R3. Mid-stream ошибки сервера в RowBinary неотличимы от данных — Дизайн, СРЕДНИЙ

- **Механика:** HTTP-статус 200 уже отправлен → текст исключения дописывается в тело. В RowBinary нет фрейминга: драйвер выдаст `ClickhouseDecodeException` («trailing garbage»/«middle of a row»), а для коротких фиксированных колонок (например один `UInt64`) хвост текста длиной ≥8 байт теоретически декодируется как **ложные строки**.
- **Код:** `Decode.hs:286-302` (цикл `rows`), отсутствие `wait_end_of_query` в параметрах (`Client/Types.hs:140-182`).
- **Минимальный шаг:** экспонировать `wait_end_of_query=1` как опцию: сервер буферизует весь ответ (в память до `http_response_buffer_size`, далее во временный файл) и откладывает заголовки до конца запроса, поэтому статус отражает ошибку; цена — потеря стриминга и серверная буферизация. Это компромисс «корректность ошибок против потоковости», а не универсальная гарантия; выбор остаётся за вызывающим. Плюс задокументировать семантику mid-stream ошибок.

### R4. Совместимость по версиям: JSON-настройки на каждом запросе — Ошибка совместимости, ВЫСОКИЙ для серверов без этих настроек

- **Код:** `jsonAsStringSettings` добавляются во все select/insert/external запросы (`Client/Types.hs:145,156,168,186-190`).
- **Протокол:** HTTP-хендлер превращает неизвестные URL-параметры в settings changes, и неизвестная настройка падает с `UNKNOWN_SETTING` (src/Server/HTTPHandler.cpp: «pass them through as settings (which will likely fail with "unknown setting")»).
- **Влияние:** любой сервер, не знающий `output_format_binary_write_json_as_string` / `input_format_binary_read_json_as_string`, отвергает **каждый** select/insert драйвера, даже без JSON-колонок. DDL (`commandRequest`, `Client/Types.hs:174-182`) работает. Точная версия введения этих настроек **не подтверждена** (официальные доки её не маркируют); живьём проверен только ClickHouse 25.3.14.14 (rollout владельца). README минимальную версию сервера не называет.
- **Минимальный шаг:** зафиксировать в README требование «сервер с поддержкой `*_binary_*_json_as_string`» (числовую версию указывать только после живой проверки); опционально — настройка транспорта, отключающая JSON-параметры для серверов без них.

### R5. Жизненный цикл агента — Дизайн, НИЗКИЙ/СРЕДНИЙ

- **Код:** `connectHTTP`/`newHTTPTransport` создают новый managed agent на вызов (`Client.hs:96-107`); в API драйвера нет shutdown (hcurl экспортирует `closeAgent`/`withManagedAgent`).
- **Влияние:** короткоживущие приложения, создающие много соединений, копят потоки/дескрипторы до конца процесса.
- **Минимальный шаг:** `withConnectionHTTP :: ClickhouseHTTPSettings -> (conn -> IO a) -> IO a`, внутри `withManagedAgent`.

### R6. Мелкие ресурсные/робастностные заметки — НИЗКИЙ

- `failWithServerError` вычитывает тело ошибки **целиком** перед обрезкой до 4096 байт (`Client.hs:128-139`): злонамеренный/сломанный прокси может выдать гигабайты. Шаг: читать с потолком.
- `pLEB128` (`Decode.hs:158-167`) принимает overlong varint без проверки переполнения (shift ≥ 64); вход конечен, зависания нет — только гигиеническая проверка.
- Пере-парс строки/значения при пересечении chunk-границ: каждый новый chunk переигрывает parse накопленного буфера (`Decode.hs:286-293`) — квадратично на очень больших значениях; на практике строки малы. Задокументировать.
- Заголовок с 0 колонок + непустое тело → `decodeRow []` не потребляет ввод → бесконечный yield пустых строк (`Decode.hs:240-241,286-302`); от реального сервера недостижимо, стоит guard.
- `error` вместо исключений: external+INSERT (`Client.hs:179`), `ClickIPv6` не 16 байт (`Encode.hs:86-88`); частичные функции в публичном пути.
- `ClickFixedString` пишется без проверки длины (`Encode.hs:56`): несовпадение с `n` колонки десинхронизирует поток → серверный parse error (драйвер схемы не знает — проверить нельзя; задокументировать требование «ровно n байт»).
- Молчаливый wrap мантисс `Decimal`/`Int128/256` при переполнении (`Encode.hs:81-84,115-123`): задокументировать или range-check по ширине конструктора.
- Таймауты: `responseTimeoutMS` — это **полный** таймаут transfer'а (`HTTP/Types.hs:25-26`), длинный стриминг-SELECT с ним оборвётся; дефолт `0` = бесконечность. Документация точная, но стоит рекомендации по сочетанию с серверными `max_execution_time`.
- HTTPS: `clickhouseUrl` поддерживает `https://`, верификация сертификатов — дефолт libcurl (включена); plaintext-auth по `http://` — задокументировать риск для удалённых серверов.

---

## 4. Тесты, пакет, документация

### Покрытие (текущее, после приёмки владельца)

- Unit (`test/Spec.hs`, 9/9): парсинг имён типов, golden-bytes скаляров, round-trip буфера, явный фейл на неподдерживаемых типах, матрица chunk-границ (1..257 байт), усечённый поток → ошибка, декодер отдаёт первую строку до конца потока (блокирующий источник).
- Integration (`test/integration/Spec.hs`): по умолчанию read-only 6/6; DDL/DML только с `CH_INTEGRATION_ALLOW_WRITES=1` (строка 70, skip-текст 114), уникальные имена таблиц (`uniqueTableName`, строки 262-266) с bracket-cleanup; фейковый TCP-сервер для усечения тела.

### Пробелы тестов (привязаны к багам)

- Нет unit-кейсов: DateTime ≥ 2038 (B1), `DateTime('tz')` (B2), именованный Tuple (B3), отрицательный Enum (B4), encode Date до 1970 / после 2149 (B5), wrap мантисс Decimal.
- Live не покрыты: `LowCardinality`-колонки (только parse), `FixedString` несовпадение длины, external tables с Nullable/Array-типами, старый сервер без JSON-настроек (R4).

### Пакет/репозиторий

- Нет CI (ни `.github`, ни иного); `cabal test` без сервера зелёный за счёт skip — хорошо, но CI с service-контейнером закрыл бы регрессии R4/интеграции.
- `clickhouse-driver.cabal`: нет bounds почти у всех зависимостей (кроме `base`), нет `category`/`maintainer`/`source-repository`/`tested-with`, ChangeLog удалён — до Hackage не готово.
- Косметика: модуль `Database.ClickHouse` против неймспейса `Database.Clickhouse.*` (разный регистр «H»).
- `hcurl` публикуется без тегов/на Hackage нет — пин задокументирован в README (после апгрейда согласован в cabal/flake).

### Документация

- README не называет минимальную версию сервера (R4), семантику таймзон (декод всегда в UTC, TZ колонки отбрасывается), потерю имён Enum, отсутствие компрессии/сессий/query_id, требование `-threaded` для потребителей (agent hcurl работает на своих потоках — стоит подтвердить и записать), семантику «один запрос = один ResourceT-скоуп» (R2) и неизвестный/частичный исход insert при обрыве с риском дублей при ретрае.
- Stale docstring `ClickhouseType` (B6).

---

## 5. Заявления против реализации (сверено)

| Заявление (README/докстринги) | Вердикт |
|---|---|
| Строки декодируются и отдаются инкрементально | Подтверждено (unit блокирующим источником + live sleepEachRow 52ms/2013ms) |
| Ограниченная память при fold; ранняя остановка освобождает transfer | Подтверждено live; оговорки «per-row» и «на выходе из скоупа» теперь в README (R2) |
| JSON ездит как RowBinary String через `*_binary_*_json_as_string` | Подтверждено live; цена — отказ на серверах, не знающих эти настройки (R4; числовой порог версии не подтверждён) |
| Неизвестные типы падают явно | Подтверждено кодом/тестом; docstring `ClickhouseType` устарел (B6) |
| «Streaming» для INSERT | Не подтверждено: payload буферизуется целиком (R1) — заявка к доработке |

---

## 6. Приоритизированный роадмап

**P0 — корректность (тихая порча/отказы):**
1. B1: unsigned-декод DateTime + тесты границы 2038.
2. B2/B3/B4: парсер — `DateTime('tz')`, именованные Tuple, отрицательные Enum + unit-тесты.
3. B5: range-check в encode Date/DateTime (+ Date32) с явным исключением.

**P1 — возможности протокола:**
4. R1: streaming/batched insert (HCurl.Upload), потолок буферизации external tables.
5. R3+таблица: высокоуровневый per-query settings API поверх экспортируемого `requestParams` (`wait_end_of_query` как опция с документированным компромиссом буферизации, `max_execution_time`, `query_id`, `session_id`, timezone).
6. R4: задокументировать требование «сервер с поддержкой `*_binary_*_json_as_string`» (мин-версия не подтверждена, живьём проверен 25.3.14.14); опция отключения JSON-параметров.
7. HTTP-компрессия ответа/запроса.

**P2 — API/ресурсы:**
8. Экспорт схемы результата (имена+типы колонок).
9. R2: `closeBody` на раннем завершении / документация скоупа; R5: `withConnectionHTTP`; R6-пачка (bounded drainBody, исключения вместо `error`, guard 0-колонок, LEB128).
10. `SimpleAggregateFunction` как passthrough внутреннего типа; опция сохранения имён Enum.

**P3 — пакет/процесс:**
11. Unit-тесты на P0-кейсы; live-матрица LC/FixedString/старого сервера; CI с контейнером ClickHouse.
12. cabal bounds/метаданные/ChangeLog; нейминг модулей.

**Явно вне скоупа (осознанный дизайн, не пробел):** нативный TCP-протокол, типизированный (схемный) API поверх `ClickhouseType`, пул соединений поверх hcurl-агента, `AggregateFunction`-состояния.

---

## 7. Статус верификации

- Этот аудит — **статический** (2026-09-16): все находки подкреплены строками кода и первоисточниками протокола; репро-идеи **не прогонялись** аудитором.
- Runtime-верификация апгрейда hcurl и стриминга выполнена владельцем реализации и принята координатором (unit 9/9; integration 6/6 read-only / 8/8 с opt-in записью; пример wide-types exit 0) — см. `rollout/2026-09-16-hcurl-b9b16d6-streaming.md`.
- Ожидающие runtime-подтверждения гипотезы (после P0-фиксов): B1–B5 репро против живого сервера; R4 против сервера без настроек `*_binary_*_json_as_string` (точная версия их введения не подтверждена).

---

## 8. Первоисточники протокольных фактов

- `DateTime` = UInt32 секунд с эпохи, диапазон до `2106-02-07 06:28:15`: <https://clickhouse.com/docs/reference/data-types/datetime> и <https://clickhouse.com/docs/reference/formats/RowBinary/RowBinary>
- `Date` = UInt16 дней с `1970-01-01`, диапазон до `2149-06-06`: <https://clickhouse.com/docs/reference/data-types/date>
- `Enum8`/`Enum16` допускают отрицательные значения (`[-128, 127]` / `[-32768, 32767]`): <https://clickhouse.com/docs/reference/data-types/enum>
- Имя типа `DateTime` с явной TZ рендерится как `DateTime('...')` (`DataTypeDateTime::doGetName`, ветка `has_explicit_time_zone`): <https://github.com/ClickHouse/ClickHouse/blob/master/src/DataTypes/DataTypeDateTime.cpp>
- Каноническое имя Decimal в заголовках — `Decimal(P, S)` (`DataTypeDecimal<T>::doGetName`): <https://github.com/ClickHouse/ClickHouse/blob/master/src/DataTypes/DataTypesDecimal.cpp>
- `LowCardinality` в RowBinary сериализуется per-value как внутренний тип (`serializeBinary` → `serializeImpl` → nested): <https://github.com/ClickHouse/ClickHouse/blob/master/src/DataTypes/Serializations/SerializationLowCardinality.cpp>
- Неизвестные URL-параметры превращаются в settings и падают с «unknown setting» (`deferred_unrecognized_params` → `extra_changes.setSetting`): <https://github.com/ClickHouse/ClickHouse/blob/master/src/Server/HTTPHandler.cpp>
- Семантика `wait_end_of_query` / `http_response_buffer_size` (буферизация ответа до конца запроса): тот же `src/Server/HTTPHandler.cpp`
- Настройки `output_format_binary_write_json_as_string` / `input_format_binary_read_json_as_string` (версия введения в документации не указана): <https://clickhouse.com/docs/reference/settings/formats/output-format#output_format_binary_write_json_as_string> и <https://clickhouse.com/docs/reference/settings/formats/input-format#input_format_binary_read_json_as_string>
