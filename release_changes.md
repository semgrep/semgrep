## [1.167.0](https://github.com/semgrep/semgrep/releases/tag/v1.167.0) - 2026-06-17

### ### Added

- Added support for more operators for folding for constant propagation, including subtraction, division, bit ops, bit shifts, comparisons, and more. (const-folding)
- Added a `nosemgrep_disabled` field to the scan configuration so the platform can disable `nosemgrep` inline ignore comments org-wide for a scan. (APPEX-1122)
- Semgrep now skips binary files (images, archives, compiled executables,
  etc.) during scanning by default, detected via matching file extensions
  to known file-format magic bytes Pass `--no-exclude-binary-files` to
  scan binary files as before. (ENGINE-2708)

### ### Fixed

- `semgrep ci` with `--sarif` now correctly populates the output's `ignores`
  field with nosemgrep-suppressed findings, in accordance with other output
  formatters. (gh-6651)

### ### Infra/Release Changes

- Updated the `ocaml-tree-sitter-core` submodule to the latest upstream `main`, providing

  * improved thread-safety
  * bumps the tree-sitter CLI option used from 0.20.6 to 0.20.8.

  (ocaml-tree-sitter-core-bump)
