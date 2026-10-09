## [1.180.0](https://github.com/semgrep/semgrep/releases/tag/v1.180.0) - 2026-10-07

### ### Added

- Promoted Dart to GA maturity. Pattern matching for Dart now supports type inference for typed declarations (id_type propagation), boolean operator typing (`&&`/`||` as bool), alias-aware import equivalence (a pattern like `http.get(...)` matches code that imports `package:http/http.dart` under any local alias), method-chain ellipsis (`obj.foo(). ... .bar()`), multi-statement patterns (e.g. `$V = get(); ...; eval($V);`), and class-body ellipsis (`class $X { ... }`). (dart-ga-types-equivalences)

### ### Changed

- Removed support for the QL language (the CodeQL query language): `--lang ql` and the `ql` language key in rules, and parsing of `.ql` and `.qll` targets, are no longer available. (remove-ql)

### ### Fixed

- Traced scans using the default parallelism mode no longer hang when the telemetry endpoint is unresponsive or unreachable. (CODE-9946)
- Updated the bundled `anyio` (4.12.1 to 4.14.2) and `urllib3` (2.6.3 to 2.8.0)
  dependencies to versions with fixes for known security advisories. (deps-anyio-urllib3)
