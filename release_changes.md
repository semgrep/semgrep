## [1.174.0](https://github.com/semgrep/semgrep/releases/tag/v1.174.0) - 2026-08-20

### ### Added

- Diff scans now report which dependency source files were added or modified relative to the merge base. (changed-dependency-sources)

### ### Changed

- Partial scans (`--x-partial-scan-rule-id`) now resolve dependencies only for
  subprojects whose ecosystem is evaluated by the rules being run. Subprojects in
  any other ecosystem are skipped and reported to semgrep-app as skipped
  subprojects. (SC-3830)
