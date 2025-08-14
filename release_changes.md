## [1.132.0](https://github.com/semgrep/semgrep/releases/tag/v1.132.0) - 2025-08-14


### Added


- PHP: When enabling option `taint_assume_safe_booleans` the return values of
  `boolval`, `is_bool`, and `||` will be considered safe.
  When enabling `taint_assume_safe_numbers` the return values of `intval`,
  `floatval`, `+`, `-`, `*`, `/` and `%` will also be considered safe. (php)
- When performing secrets validation, the amount of time that the HTTP request
  took to complete will now be visible in the debug logs. (#2130)
- Introduces a timeout to internal HTTP requests, to prevent remote endpoints
  from indefinitely hanging the engine. (#4295)


### Changed


- Pro scans will no longer attempt to parse tsconfig files for non-typescript scans. (gh-4407)


### Fixed


- Language server: Made it so that errors which occur no longer pop up in while using the
  IDE. They still log, but will no longer be displayed via UX. (saf-2193)
- When validating the results of a secrets scan, do not have more than 256
  outstanding validators executing at a given time. (#2130)
