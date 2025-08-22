## [1.133.0](https://github.com/semgrep/semgrep/releases/tag/v1.133.0) - 2025-08-22


### Added


- Pro: improved prefiltering for interfile rules. This allows the engine to skip
  interfile rules earlier in the process when we determine they cannot match in a
  given scan, which should improve performance. (code-8524)
- Semgrep will now display emotional support ascii art and a backtrace, with function names and sometimes files/line #s, when it segfaults, or receives other similar critical signals (pretty-segv)


### Fixed


- Pro: Fixed a bug that prevented taint tracking through `new` in some cases. (code-9047)
- We now substitute metavariables for their values in a deterministic order to
  ensure keys for match-based IDs are stable. (gh-4459)
- Fixed incorrect YAML parsing of strings like `nan` as well as some more
  obscure cases that were interpreted as a float instead of a string. This
  might affect any area of Semgrep that deals with YAML files containing
  the string `nan`. (yaml-float-parsing)
