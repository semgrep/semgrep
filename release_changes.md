## [1.156.0](https://github.com/semgrep/semgrep/releases/tag/v1.156.0) - 2026-03-17

### ### Changed

- The Kotlin tree-sitter parser has been updated to the latest available grammar significantly improving Kotlin support in Semgrep. (kotlin-parser)

### ### Fixed

- Pro: Experimental interfile tainting for Ruby now disambiguates between variable accesses and zero-argument method calls. (engine-2556)
- Pro: Memoize tsconfig.json parsing to avoid redundant re-parsing across a project hierarchy. (engine-2596)
- Fixed a crash in `semgrep ci` when run in a git repo with no remote origin set (gh-11342)
