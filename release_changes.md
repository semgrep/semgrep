## [1.112.0](https://github.com/semgrep/semgrep/releases/tag/v1.112.0) - 2025-03-13


### Added


- TypeScript parser now allows ellipses in class bodies. For example, you can
  write the pattern like:
  ```
  class $C {
    ...
    $FUNC() { ... }
    ...
  }
  ``` (code-8242)
- Semgrep will now present more detailed info when a scan is complete, such as what percent of lines were scanned. It is also formatted in a new manner (saf-details)
- Verbose output will now print additional info about parsing and internal semgrep errors, such as what % of lines were skipped, and the lines they occured on (saf-verbose)


### Fixed


- pro: Fixed bug in (experimental) "at-exit" sinks feature that would prevent
  the identification of a statement like `return foo()` as one such sink. (code-8199)
- FIX: `--gitlab-secrets` output has been updated to conform to GitLab JSON schema (scrt-849)
- The behavior of `--semgrepignore-v2` changed to be closer to the legacy
  Semgrepignore v1. `.gitignore` files are no longer loaded automatically
  as part of the Semgrepignore v2 exclusion mechanism.
  Loading a `.gitignore` file must be done
  by placing `:include .gitignore` in the `.semgrepignore` file
  as was the case with Semgrepignore v1. (semgrepignore-v1-compat)
