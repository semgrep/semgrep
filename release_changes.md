## [1.147.0](https://github.com/semgrep/semgrep/releases/tag/v1.147.0) - 2026-01-07


### Added


- Gradle lockfiles of the form `gradle*.lockfile` are now supported. Previously, only lockfiles named exactly `gradle.lockfile` were supported. (SC-2999)
- `semgrep login` now supports a `--force` flag, which ignores existing tokens and starts a new login session. The MCP setup workflow has been updated to use `--force` too. (saf-2392)


### Fixed


- Deduplication should now pick the exact same findings across scans. Previously,
  findings were always *equivalent*, but not guaranteed to be *exactly* the same
  (e.g. metavariable bindings could differ). Depending on the rule and target code,
  this could cause findings' fingerprints to change from one scan to another, thus
  leading to finding flakiness and "cycling" in Semgrep App. Note that when
  upgrading to this Semgrep version, you may see different (but equivalent) findings
  wrt your current Semgrep version in the first scan, one more time. However, in
  subsequent scans/upgrades, this problem should go away or at least be greatly
  reduced. (saf-2304)
