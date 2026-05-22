## [1.164.0](https://github.com/semgrep/semgrep/releases/tag/v1.164.0) - 2026-05-22

### ### Added

- Dart: typed metavariables (`$X as T`) and `metavariable-type`,
  metavariable binding inside string interpolations, and function-definition
  patterns that match Dart function definitions. (gh-11678)

### ### Changed

- The default memory limit for Pro interfile scans on Linux now adapts to the container's cgroup memory limit (90% of it) instead of the previous fixed 5 GiB, with an 8 GiB fallback when no cgroup limit is detected. (ENGINE-2568)

### ### Fixed

- Baseline diff scans (``semgrep ci`` and ``--baseline-commit``) no longer treat every finding on a file as newly introduced when rule(s) failed during the baseline run.

  Per-rule failures (for example a timeout for a single rule) on baseline analysis now hide only that rule's matches on that file from the "new vs baseline" comparison.
  Other rules on the same file are still taken in comparison for the "new vs baseline" comparison.

  Per-file, rule-independent failures now hide all findings on that file from the "new vs baseline" comparison. (LANG-515)
- Fixed a yarn.lock parse error on Yarn Berry entries written
  in YAML explicit-key form. Affected lockfiles previously failed to parse. (SC-3479)
- Fix `--sarif-output` and `--sarif` causing nosemgrep-suppressed findings to be reported in CLI scan output and to block scans. Suppressed findings are now correctly excluded from terminal text output, the scan-summary count, and the CLI's exit code. (engine-1824)
- Fixed a bug that could cause unreliable target filtering in parallel scans. (gh-6313)
- Dart: improved parser fidelity for Dart 3 grammar features and routed
  pattern parsing for statements beginning with `await`, `rethrow`, and other
  statement keywords. Eliminates a large class of `PartialParsing` errors on
  real-world pub.dev packages. (gh-11678)

### ### Infra/Release Changes

- pro: macOS: Fixed dynamic library lookup for `semgrep-core-proprietary` so the binary works when `semgrep install-semgrep-pro` is invoked, and `semgrep` is installed via Homebrew. (pro-binary-homebrew)
- Pro: Added optional `<case>.named_ast.expect` golden files for `tests/intrafile/maturity/` fixtures, exercised by `Unit_maturity_named_asts`. (LANG-287)
