## [1.144.0](https://github.com/semgrep/semgrep/releases/tag/v1.144.0) - 2025-11-19


### Fixed


- pro: interfile scans no longer default to -j 1; instead, the number of
  available CPUs on the system is polled as part of a heuristic to determine how
  many threads should be spawned. (gh-4952)
- Semgrep will no longer rarely crash when --trace is passed. (incid-280)
