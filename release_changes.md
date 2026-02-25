## [1.153.0](https://github.com/semgrep/semgrep/releases/tag/v1.153.0) - 2026-02-25

### ### Added

- Semgrep core is now optimized with flambda (flambda)
- Scala: Support for `for`-`yield` (LANG-193)

### ### Fixed

- Scala: Fixed a parsing bug where subsequent calls in an implicit block would not
  be considered at the same scope, e.g.
  ```
  def f (a: t) =
    foo()
    bar()
  ``` (lang-194)
