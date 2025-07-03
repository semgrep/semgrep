## [1.128.0](https://github.com/semgrep/semgrep/releases/tag/v1.128.0) - 2025-07-03


### Added


- `HTTP{,S}_PROXY=...` now accepts URIs without a scheme (e.g `HTTP_PROXY=domain.com:port`) (saf-2082)


### Fixed


- Java: Deprecated `class $A` partial class pattern, in favor of
  ```
  class $A { ... }
  ``` (safe-2104)
