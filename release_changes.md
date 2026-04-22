## [1.160.0](https://github.com/semgrep/semgrep/releases/tag/v1.160.0) - 2026-04-16

### ### Added

- Scala: Added tree-sitter parser for improved parsing accuracy with pfff fallback. (LANG-255)
- pro: taint: Improved support for variadic functions (LANG-375)

### ### Fixed

- Fixed performance issues during parsing Semgrep rules containing emoji or
  other non-BMP Unicode characters. (gh-6070)
- Emit a warning when semgrep-core rule validation fails and falls back to JSON
  schema validation, alongside details of the failure. (gh-6071)
