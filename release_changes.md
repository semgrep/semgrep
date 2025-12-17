## [1.146.0](https://github.com/semgrep/semgrep/releases/tag/v1.146.0) - 2025-12-17


### Added


- Added support for Cursor post-code-generation hooks via new `record-file-edit` and `stop-cli-scan` semgrep mcp flags (cursor-hooks)
- Added `skipped_paths` field to CI scan results to report files that failed to scan due to errors (timeout, OOM, etc.), preventing the app from incorrectly marking findings in those files as fixed (gh-5122)
- Symbol analysis, if enabled, now runs for Supply Chain only scans when calling `semgrep ci`. (sc-2927)


### Changed


- Semgrep's Docker image base has been bumped from Alpine Linux 3.22 to 3.23 (docker-version)
- bumped the `mcp` python-sdk from `1.16.0` to `1.23.3` (mcp-version)
- pro: [experimental] enabling and disabling transitive reachability
  analysis in `semgrep ci` regardless of app settings is now possible with
  `--x-enable-transitive-reachability` (or `--x-tr`)
  and `--x-disable-transitive-reachability`. (tr-flags)


### Fixed


- The PHP AST now distinguishes between if statements with no else clause and those with an explicit but empty else {}. (gh-11330)
- git-lfs objects are now excluded from baseline scans, as they are usually binary files, or simply too large to scan. (saf-2020)
- Fix a OCaml stdlib bug that would cause nondeterministic UnixErrors on Windows under the multicore runtime due to a race condition in the socketpair implementation (saf-2316)
- Fixed an issue that in rare cases could lead timeouts to be mishandled. This typically manifested only through slightly different warning messages, but it is possible that more serious consequences could have occasionally resulted. (saf-2368)
- Fixed symbol analysis incorrectly analyzing all files instead of only the relevant language files per ecosystem. (sc-3020)
