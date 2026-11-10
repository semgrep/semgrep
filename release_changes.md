## [1.184.0](https://github.com/semgrep/semgrep/releases/tag/v1.184.0) - 2026-11-05

### ### Added

- PHP: Improved virtual call resolution for basic trait composition. (CODE-9176)
- Supply Chain: a `requirements.txt` dependency declared as a version range (`litellm>=1.10`) or with no version (`flask`) now produces a finding for every advisory whose affected versions overlap the declared range, in both the Python and native resolvers. Such a finding has no single installed version to confirm, so it needs review. A range line next to a pinned copy of the same package produces no finding of its own, since the pinned copy names the installed version. (SC-4243)
- Set `SEMGREP_LOG_JSON_FILE` to emit structured execution events with scan metadata alongside existing text output. (SMS-3670)

### ### Changed

- Scans are slightly faster. The dataflow analyses build the name of each variable with a plain string join, and no longer with `Printf` and `Format`. On 6 open-source repos, the scan time was a median of 3% lower, with identical findings. (dataflow-variable-keys)
- go grammar update (v0.25.0):

  - Go source parsing now supports generic type aliases, preserving type parameters, constraints, and alias semantics. Existing alias patterns can match these declarations; spelling generic parameter lists in rule patterns remains unsupported.
  - Go correctly parses unnamed map parameters and results in mixed signatures such as ([]string, int, map[int]int, error), allowing function patterns to match those signatures.
  - Go correctly represents chained indexing such as table[key][next] as nested index expressions, restoring matches for nested-index patterns.

  (go-v0.25.0)

### ### Fixed

- Semgrep no longer logs credentials embedded in a proxy URL (for example
  `HTTPS_PROXY=http://user:password@proxy:8080`) when it reports the configured
  proxy settings. (proxy-log-redact-userinfo)
- Logs and traces no longer include snippets of the scanned source code when a
  file fails to parse. Parsing exceptions now report the file, line, column, and
  the length of the unexpected text instead. Dependency resolution errors for
  malformed lockfiles are reported the same way. (redact-source-in-telemetry)
- pro: Reduced peak memory for large projects (CODE-10136)
- Supply Chain: the yarn v1 lockfile parser no longer reports a package named
  "dependencies" with an empty version when a blank line precedes the
  dependencies block of an entry, and it now keeps that entry's dependency
  edges. Entries without a version line are reported as parser errors instead
  of dependencies with an empty version. (SC-4214)
- Gradle module attribution now works for modules whose logical project name
  does not match their directory (a renamed `include` or a remapped
  `projectDir` in settings.gradle). The resolver's init script reports each
  project's actual build file, and dependencies are attributed to that file
  instead of falling back to the root manifest. (SC-4327)
- semgrep ci --no-suppress-errors with --allow-local-builds: when dependency resolution fails and the scan exits with code 2, the reason is now also in the `errors` of the JSON output (one entry per resolver error or lockfile parse error of each failed subproject, or a generic entry if none was reported; resolver errors include the failed command and the end of its output, with common credential shapes such as URL credentials and Authorization headers redacted), not only in the log. Previously the JSON had no error, so pipelines that read it could not tell why the scan failed. (SC-4348)
