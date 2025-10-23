## [1.141.0](https://github.com/semgrep/semgrep/releases/tag/v1.141.0) - 2025-10-23


### Added


- pro: scala: http4s-specific support for `$M -> ... / $X / ...` patterns (code-9114)


### Fixed


- Improved detection of implicitly returned expressions.
  Functions in some languages, such as Ruby and Scala, can return a value without an explicit `return` statement.
  More expressions, such as string interpolation, are now correctly identified as implicitly returned. (code-9101)
- Scala: Parser now accepts an $MVAR as a pattern alias (`@`), so
  e.g. `case $X @ ... => ...` is now a valid pattern. (code-9130)
- fixed an issue where CamlinternalLazy.Undefined would occur while using eio multicore (saf-1877)
