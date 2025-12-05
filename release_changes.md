## [1.145.0](https://github.com/semgrep/semgrep/releases/tag/v1.145.0) - 2025-12-04


### Added


- Added optional user-prompting for classifying findings as true/false positives via MCP Elicitation in the MCP server (behind SEMGREP_FINDINGS_ELICITATION_ENABLED, off by default). (elicitation)
- Added hook to inject secure-by-default library recommendations into Claude Code Agent context. (secure-defaults-hook)


### Changed


- Symbol analysis upload now runs before scan completion to ensure it is available during initial scan postprocessing. (sc-2933)


### Fixed


- Fix issue that could lead to validation failures for certain well-formed rules, such as those with emoji in their messages. (incid-293)
- The correct range for `let ... in` expressions in OCaml is now reported. Previously, the location of the `let` was omitted. This is mainly relevant for autofix. (ocaml-let)
- Debug log lines concerning telemetry collection that are only relevant inside
  Semgrep's managed scanning environment are not emitted if a scan runs outside
  that environment. (saf-2321)
- pro: in 1.144.0 interfile scans no longer default to -j 1; instead, the number of available CPUs on the system was used to inform how many jobs should be spawned. This caused a change in timeouts due to how time is measured for certain parts of the pro engine. This change has now been reverted (saf-default-jobs)
