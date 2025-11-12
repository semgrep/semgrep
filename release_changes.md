## [1.143.0](https://github.com/semgrep/semgrep/releases/tag/v1.143.0) - 2025-11-12


### Added


- Dataflow will now understand empty block expressions as having unit value in
  more instances. (code-9141)
- Parallel scans will now use shared-memory parallelism using multicore OCaml
  domains, rather than the legacy fork-join approach.  Users can opt into the
  legacy method with the `--x-parmap` CLI flag, and this deprecates the `--x-eio`
  flag (since it is now the default behaviour). (saf-2271)
- Add `-k/ --hook` flag to enable Semgrep scans via Claude Code Agent post-tool hooks (saf-2279)


### Fixed


- When running `semgrep scan` or `semgrep ci`, the progress bar now always ends at 100%. (SAF-2079)
- Pro: fixed various bugs relating to Scala match expression handling in dataflow
  analysis (e.g., some branches being misordered, especially when matching
  multiple variables against non-integer literal patterns). (code-9144)
- Semgrep will now emit better error messages when exceptions are raised at the beginning or end of scan (exit-message)
- Enabled taint tracking into Goroutines, by treating them as regular Go function calls. (gh-11207)
- Fixed missing Rust type alias translation. We can now
  accurately match the () type in a `type` declaration. (gh-11283)
- fixed MCP semgrep_findings tool to accept single issue_type parameter and corrected identity string role parsing (saf-2282)
