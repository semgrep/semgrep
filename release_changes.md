## [1.131.0](https://github.com/semgrep/semgrep/releases/tag/v1.131.0) - 2025-07-30


### Fixed


- Semgrep diff scans can now query the app for which merge base to use. This fixes the issue where some diff scans on shallow clones would use the wrong merge base, and do a diff scan on commits not in a PR. (better-merge-base)
- Fix a possibility that an empty file be created in place of a missing input file. This bug had been introduced with Semgrep 1.115.0. (dont-create-missing-input-files)
- When processing a target with debug logging enabled, we now only log the target
  path rather than the entire internal structure representation.  This allows for
  more succinct log files and no longer introduces mid-entry newlines, which can
  break log-parsing tooling. (gh-4315)
- Language server: Fixed a bug which broke the `Sign in` command (saf-2151)
- CiScanComplete.dependencies is now populated with parsed dependencies (sc-2468)
- Print error details when a `SemgrepError` exception is raised and causes `semgrep` to fail. (silent-semgrep-error)
