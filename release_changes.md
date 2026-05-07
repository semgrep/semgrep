## [1.162.0](https://github.com/semgrep/semgrep/releases/tag/v1.162.0) - 2026-05-07

### ### Added

- pro: Improved support for tracking taint through nested functions. (LANG-95)
- Added indexes to file targeting to improve performance of semgrepignore matching. (gh-27830)

### ### Changed

- Faster JSON rule parsing: rule files in JSON format now parse roughly 5x faster end-to-end (measured ~134s → ~28s on a 382MB rule pack) by going through a new hand-written RFC 8259 parser instead of the previous JS-parser-based chain. (ENGINE-2725)
- Scala projects are now identified for Supply Chain only by their root build.sbt, rather than treating each build.sbt as a different subproject. (SC-3293)
- MCP `semgrep_findings` tool: added a `refs` parameter to filter findings by branch (defaults to the primary branch when not specified), and made `autotriage_verdict` optional so that findings without an AI verdict can also be returned. (engine-2723)

### ### Fixed

- jsonnet: `import` and `importstr` now reject paths that resolve outside the
  rule file's parent directory. (ENGINE-2727)
- semgrep ci: redact URL-embedded credentials and `Authorization` header
  values from git error messages and from the captured tracebacks sent to
  the fail-open telemetry endpoint, preventing leaks of secrets like
  `CI_JOB_TOKEN` from a failed `git fetch` in GitLab CI. Also closes
  ENGINE-2731 (raw, unsanitized tracebacks in fail-open telemetry). (ENGINE-2728)
- `semgrep ci` no longer transmits SCM tokens to the Semgrep Platform. (ENGINE-2729)
- semgrep CLI: the on-disk log file (`~/.semgrep/semgrep.log` or `$SEMGREP_LOG_FILE`) now respects the requested log level instead of always being written at DEBUG. This narrows the surface for credentials to land on disk via CI runner filesystems or job artifacts; pass `--debug` to restore the previous behavior. (ENGINE-2730)
- jsonnet rules: bound recursion in both rule loading and evaluation so a
  malicious rule can no longer hang semgrep via mutually-recursive `import`s
  or runtime function calls that recurse forever. (ENGINE-2727-dos)
- Scala: Merging consecutive top-level package declarations into a single package path. (LANG-374)
- Fixed PHP parse errors during highly-parallel parsing. (gh-6197)
- Fixed Scala parse errors during highly-parallel parsing. (gh-6198)
- Surface a clearer error from the MCP scan tool when metrics is off and auto config is specified (gh-11649)
- Fixed unknown option error when spawning the MCP daemon (gh-11660)
