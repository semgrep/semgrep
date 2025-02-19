## [1.109.0](https://github.com/semgrep/semgrep/releases/tag/v1.109.0) - 2025-02-19


### Changed


- Pyproject.toml files are now parsed using a toml parser (tomli). (sc-2054)


### Fixed


- pro: taint-mode: Fixed limitation in custom taint propagators.
  See https://semgrep.dev/playground/s/ReJQO (code-7967)
- taint-mode: Disable symbolic-propagation when matching taint propagators
  to prevent unintended interactions. See https://semgrep.dev/playground/s/7KE0k. (code-8054)
- Fixed pattern match deduplication to avoid an O(n^2) worst-case complexity, and
  optimized the matching of ordered `..., PAT, ...` patterns. (saf-682)
