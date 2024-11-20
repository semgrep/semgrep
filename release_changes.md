## [1.97.0](https://github.com/returntocorp/semgrep/releases/tag/v1.97.0) - 2024-11-19


### Added


- Improved logic for interfile analysis in TypeScript projects using
  [project references](https://www.typescriptlang.org/docs/handbook/project-references.html). (code-7677)
- Semgrep Pro engine now resolves method invocations on abstract classes. In
  addition to the existing resolution for interface method invocations, this
  change further enhances dataflow tracking accuracy for dynamic method
  invocations. (code-7750)
- Added the ability to validate temporary AWS tokens in the secrets product. (gh-2554)
- Poetry.lock & Pyproject.toml parsers can now handle multiline strings. (ssc-1942)


### Fixed


- Improved error handling for some networking errors (e.g., premature server
  disconnection). In some cases this would previously cause a fatal error, but we
  should instead be able to recover in most instances (and now can). (code-7715)
- Target file selection in git projects: files containing special characters
  (according to git) are now scanned correctly instead of being ignored. (saf-1687)
- Swift: Ellipses and metavariable ellipses can now be used as function parameters in patterns. (saf-1721)
- Semgrep will no longer freeze when tracing is enabled and it has a low memory limit (saf-1722)
- osemgrep-pro: Autofix and nosemgrep now work properly (saf-1724)
