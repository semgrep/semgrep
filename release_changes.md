## [1.149.0](https://github.com/semgrep/semgrep/releases/tag/v1.149.0) - 2026-01-21


### Added


- Added a warning in --debug mode when a user runs a parallel scan with a larger
  value for -j/--jobs than the number of CPUs we detect the host has made
  available to Semgrep.  Additionally, a suggested starting value for -j/--jobs
  is reported to give the user a place to start tuning their scan. (saf-2474)
- Upload symbol analysis on a per-subproject basis during supply chain scans. (sc-3038)


### Changed


- The MCP server no longer supports SSE transport. (saf-2462)


### Fixed


- pro: Improved virtual method resolution in Java (code-9210)
- pro: Improved virtual method resolution in Scala (code-9212)
- Improve performance of scan planning, a part of the Python CLI, by reducing
  the cost of re-hashing `Target` objects.  Performance should improve on
  large repo scans proportionally to the number of files in the repo. (gh-5407)
- `semgrep ci` no longer applies autofixes to disk, even when the "Suggest autofixes" toggle in the app is enabled. (saf-2446)
