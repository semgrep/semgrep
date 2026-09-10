## [1.177.0](https://github.com/semgrep/semgrep/releases/tag/v1.177.0) - 2026-09-10

### ### Added

- Added native Supply Chain support for Bazel workspaces using `rules_jvm_external`. Semgrep now recognizes a `maven_install.json` pinned lockfile (versions `0.1.0` and `3`, as emitted by rules_jvm_external 3.x through current) paired with a `MODULE.bazel` (or legacy `WORKSPACE` / `WORKSPACE.bazel`) marker as a Maven-ecosystem subproject, and attributes findings to the workspace root rather than the lockfile's directory. Workspace-declared root artifacts are identified via `__INPUT_ARTIFACTS_HASH` for accurate direct-vs-transitive classification; dependencies are emitted with `Unknown` transitivity when that field is not available. This is the first milestone of native Bazel coverage; broader ecosystem support (`rules_python`, `rules_go`, `rules_js`) and Bazel-aware reachability attribution follow. (SC-2008)
- Several performance improvements for regex-only rules where the underlying
  regex are inefficient to run on our default regex engine (currently PCRE2). For
  example, a rule matching `FOOBAR(a+)\1` will skip any file that does not
  contain `FOOBAR` without running the regex. (scrt-979)

### ### Changed

- Prefilter conditions now evaluate their cheap string predicates before their
  expensive regex predicates. Since evaluation short-circuits, a file that a
  string check already rules in or out no longer pays for regex predicates
  (which is what a pattern's prefilter falls back to when no literal substring
  can be extracted from it, and which can be slow on files with very long
  lines). (prefilter-rank-conjuncts)
- Supply Chain scans can report dependencies from their Gradle module build files instead of the root manifest. This behavior is disabled by default during rollout and can be tested with `--x-gradle-module-attribution`. Enabling it can change finding IDs because finding paths change; the ID calculation is unchanged. (SC-2560)

### ### Fixed

- Speed up `semgrep ci` filtering when a deployment has many triage-ignored findings. (triage-ignored-performance)
- Semgrep no longer crashes with an OCaml stack trace when a proxy environment
  variable holds an unusable value. `HTTP_PROXY`, `HTTPS_PROXY`, or `ALL_PROXY`
  set to an empty value is now ignored with a warning, and the scan
  proceeds without a proxy. A non-empty value that is not a usable proxy URL
  now exits with an error message, with any credentials in the URL
  redacted, instead of failing inside the HTTP client.

  Semgrep also now adds the missing scheme to a proxy URL supplied
  without one; `https` for `HTTPS_PROXY and `http` otherwise. (ENGINE-2208)
- Supply Chain: lockfileless Gradle scans now report a "Resource Inaccessible"
  resolution error when a repository refuses a request (for example a 401 from a
  private registry), instead of exiting successfully with a silently incomplete
  dependency list. (sc-3358)

### ### Infra/Release Changes

- Improves shutdown time during scans with --trace. (otel-shutdown-flush)
