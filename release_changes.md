## [1.151.0](https://github.com/semgrep/semgrep/releases/tag/v1.151.0) - 2026-02-04


### Added


- Added progress indicators for symbol analysis calculation and upload during CI scans (sc-3103)


### Fixed


- bumped `glom` to at least version `23.3`, which includes a fix to a `SyntaxWarning`
  warning log. (gh-11460)
- Semgrep no longer prints info log lines from semgrep-core RPC calls when --trace is passed and --debug isn't (loglines)
- Fixed the README not appearing in built wheels. (wheelreadme)
