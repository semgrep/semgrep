## [1.185.0](https://github.com/semgrep/semgrep/releases/tag/v1.185.0) - 2026-11-10

### ### Added

- PHP: Improved virtual call resolution for basic trait composition. (CODE-9176)
- Supply Chain: a `requirements.txt` dependency declared as a version range (`litellm>=1.10`) or with no version (`flask`) now produces a finding for every advisory whose affected versions overlap the declared range, in both the Python and native resolvers. Such a finding has no single installed version to confirm, so it needs review. A range line next to a pinned copy of the same package produces no finding of its own, since the pinned copy names the installed version. (SC-4243)
- Secrets: emit structured, privacy-safe telemetry for custom validator HTTP requests (hostname, port, status, latency, rule/scan correlation) (SMS-3122)
- Set `SEMGREP_LOG_JSON_FILE` to emit structured execution events with scan metadata alongside existing text output. (SMS-3670)

### ### Changed

- Scans are slightly faster. The dataflow analyses build the name of each variable with a plain string join, and no longer with `Printf` and `Format`. On 6 open-source repos, the scan time was a median of 3% lower, with identical findings. (dataflow-variable-keys)
- The `semgrep/semgrep` Docker image no longer includes the OpenSSH server (`sshd`) or `sftp-server`. It now installs only the OpenSSH client (`ssh`, `scp`, `ssh-keyscan`, `ssh-agent`, `ssh-add` and `ssh-keygen`), which is all git needs to fetch over SSH. Container scanners will no longer report sshd-only CVEs against the image. (docker-ssh-client-only)
- go grammar update (v0.25.0):

  - Go source parsing now supports generic type aliases, preserving type parameters, constraints, and alias semantics. Existing alias patterns can match these declarations; spelling generic parameter lists in rule patterns remains unsupported.
  - Go correctly parses unnamed map parameters and results in mixed signatures such as ([]string, int, map[int]int, error), allowing function patterns to match those signatures.
  - Go correctly represents chained indexing such as table[key][next] as nested index expressions, restoring matches for nested-index patterns.

  (go-v0.25.0)
- julia grammar update (v0.25.0):

  - Julia source parsing and rule patterns now support public API declarations while distinguishing them from export declarations and calls named public.
  - Julia source parsing and rule patterns now support outer-qualified loop bindings, preserving their distinction from ordinary bindings and a variable named outer.
  - Julia parsing and matching now support typed const fields without initializers in mutable structs.
  - Julia source parsing and rule patterns now support renaming imported macros, such as import A: @m as @n and import A: @m as @B.n, and keep the @ on macro names in imports, exports, and public declarations.
  - Julia rules can now match unary ± expressions and distinguish binary ± from ∓, reducing operator-related false positives.
  - Julia numeral juxtaposition such as 1__2 is now represented as multiplication by __2 and can be found by multiplication patterns.
  - Julia function-definition patterns now retain where constraints, avoiding matches against definitions with a different constraint.

  (julia-v0.25.0)

### ### Fixed

- OCaml: taint analysis now follows data through expression statements and
  sequence expressions. For example, in `let x = (r := 1; source ()) in sink x`,
  the value of the sequence is the result of `source ()`, so the finding on
  `sink x` is now reported. Taint also no longer leaks from one `match` case
  into the next one. (ocaml-expression-statements)
- OCaml: the last expression of a function body is now treated as the function's result, so taint flows through calls. (ocaml-implicit-returns)
- Semgrep no longer logs credentials embedded in a proxy URL (for example
  `HTTPS_PROXY=http://user:password@proxy:8080`) when it reports the configured
  proxy settings. (proxy-log-redact-userinfo)
- Logs and traces no longer include snippets of the scanned source code when a
  file fails to parse. Parsing exceptions now report the file, line, column, and
  the length of the unexpected text instead. Dependency resolution errors for
  malformed lockfiles are reported the same way. (redact-source-in-telemetry)
- On Windows, Semgrep now runs programs it calls by name, such as git and pysemgrep, only from fully qualified PATH directories. Previously Windows checked the current directory first, so an executable committed to a scanned repository could run during a scan. Initially reported by Aikido Security. (CODE-10122)
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
