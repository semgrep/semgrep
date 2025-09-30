## [1.139.0](https://github.com/semgrep/semgrep/releases/tag/v1.139.0) - 2025-09-30


### Added


- --pro-intrafile scans will now add built-in taint propagators, like --pro does,
  hence producing extra findings. For example, in Java, `list.add(taint)` will now
  make `list` tainted even if the rule does not explicitly request that. Scan times
  should not be generally affected in a significant way. (code-9103)
- Scala: Enable pattern `{ ... }` to match partial functions like `{ case 1 => "1" }`. (code-9106)
- Associate Containerfiles with the `dockerfile` language (gh-11091)


### Changed


- Rule parsing now happens solely in OCaml. This should have no change in the behavior of whether a rule successfully parses or not, but will change the parse errors emitted (#4346, #4269, #4379) (gh-4379)
- MCP: Removed the `config` parameter from the `semgrep_scan` tools, to prevent
  agents from inserting unwanted config files to scan with. (saf-2258)


### Fixed


- scala: Fixed matching of `{ case ... => ... }` patterns. (code-9111)
- Fixed a bug preventing metavariable-comparisons with more than two subsequent "and" or "or" conditions from producing findings. For example, the condition `$X > 1 or $Y > 1 or $Z > 1` would previously always evaluate to `false`. Now, it will behave as expected. (gh-11209)
- MCP: Fixed an issue where the `semgrep_scan` tool, when invoking the RPC-based
  scanning approach, would return JSON output not consistent with the CLI tool. (saf-2250)
- MCP: The `semgrep_findings` tool now gives a suitable error message when erring due
  to insufficient permissions on standard `semgrep login` tokens. (saf-2254)
- MCP: Fixed a bug where if the user is already logged in when running the setup flow,
  the Semgrep Pro Engine installation step would be ignored. (saf-2259)
