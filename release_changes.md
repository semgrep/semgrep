## [1.161.0](https://github.com/semgrep/semgrep/releases/tag/v1.161.0) - 2026-04-22

### ### Added

- Scala 3.4+ trait parameters are now parsed correctly. (lang-73)

### ### Fixed

- Semgrep's HTTP requests no longer log URLs above the debug level; full request
  details remain available when running with `SEMGREP_LOG_SRCS=cohttp.client`. (ENGINE-2712)
