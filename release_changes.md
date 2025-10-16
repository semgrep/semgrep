## [1.140.0](https://github.com/semgrep/semgrep/releases/tag/v1.140.0) - 2025-10-16


### Added


- scala: Allow partial case patterns such as `case 1 => ...` to easily match
  individual case clauses within a match-expression. (code-9118)
- Added python `3.14` support. (gh-11250)
- MCP: Slash command `setup_semgrep_mcp` now supports Claude Code. (saf-2261)


### Changed


- Semgrep's Docker image base has been bumped from Alpine Linux 3.21 to 3.22 (docker-version)


### Fixed


- Java and Rust: Fixed parsing of float and double literals with type suffixes so they can be used in metavariable-comparison and pattern matching. Previously, Java literals like `0.5f` or `1.0d`, and Rust literals like `0.5f32` or `1.0f64` would fail to parse and could not be compared. (gh-7968)
- Display an error instead of a malformed success message
  when the show subcommand fails due to an invalid CLI token. (grow-630)
- new `semgrep/semgrep` images should now contain golang `v1.24` instead of `v1.23` (saf-2240)
- Fixed an issue where temporary files, containing rules to be validated,
  persisted after a semgrep scan. (saf-2257)
- MCP: Fixed tool calls failing for some models (e.g., GPT-5). (saf-2262)
- MCP: Fixed a bug where resource closure errors would occur when trying to use
  the MCP with the `streamable-http` tranport method. (saf-2264)
