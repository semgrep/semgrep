## [1.157.0](https://github.com/semgrep/semgrep/releases/tag/v1.157.0) - 2026-03-31

### ### Added

- pro: Improved taint tracking through lambda calls. (LANG-268)
- It is now possible to match a class name like in `$C.getInstance(...)`, and then
  use  `metavariable-type` on `$C` to check its type. (LANG-271)
- pro: Improve cross-file taint tracking for globals. (LANG-275)

### ### Changed

- Pro: Reduces redundant recomputation during inter-file taint analysis by serializing intermediate results to disk. (ENGINE-2582)
- pro: Improved golang module resolution. (code-9225)
- Supply Chain Analysis of npm package lock files now uses a proprietary OCaml-based parser, replacing the old Python version. The supply-chain functionality for these files is now available only to Semgrep Pro users. (gh-5658)

### ### Fixed

- Fix Rust parsing of "&raw" where "raw" is an identifier. (rust-parser-updated)
- Errors during target file discovery (e.g., permission errors, git failures) are now surfaced as warnings instead of being silently ignored. (ENGINE-2627)
- kotlin: Fixed bug parsing FQNs in `metavariable-type`. (LANG-271)
- Fixed requirements.txt parser silently dropping pinned dependencies that followed unpinned package names. (SC-3379)
- Prevented certain deeply nested aliengrep matches from segfaulting semgrep-core. (engine-2628)
- Fix Python parsing for files that contains empty strings (or quotes in docstrings) along with match statements. (gh-11287)
- Fix rule paths.include/paths.exclude filtering when a single file is passed as a scan target. Previously, path patterns like '**/src/test/**/*.java' would not match because only the filename was used for filtering instead of the full project-relative path. (gh-11560)
- Pro: Improved type resolution in Scala (lang-79)
- Pro: Improved call resolution in Scala for parameterless methods (lang-80)
