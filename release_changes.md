## [1.183.0](https://github.com/semgrep/semgrep/releases/tag/v1.183.0) - 2026-10-29

### ### Added

- The `--help` output of `semgrep` subcommands now includes a "THIRD-PARTY
  LICENSES" section linking to the licenses and notices for Semgrep's open
  source dependencies at https://semgrep.dev/cli-credits. (cli-credits)
- Pro: Supply Chain now records which Go module requires which when dependency paths are enabled and local builds are allowed, so transitive Go findings can show the dependency path. (SC-4268)

### ### Changed

- Semgrep no longer ships binaries for Intel Macs; 1.182 is the last release that supports them. On Intel Macs, use the `semgrep/semgrep` Docker image; on Apple Silicon, use a native arm64 Python. (drop-intel-macos)
- Speed up scans with many rules that share file prefilter conditions by checking each condition once per file. (prefilter-predicate-memo)
- semgrep ci `--no-suppress-errors` now exits with code 2 (instead of 0) when dependency resolution fails for a subproject while local builds are allowed (--allow-local-builds): a build or package manager command fails, a lockfile cannot be parsed at all, or a build succeeds but reports an incomplete dependency list (for example, a registry refused a request). The affected dependency sources are listed in the scan output and the scan no longer reports that it completed successfully. Findings from the other subprojects are still reported. The default behavior, where errors are suppressed, is unchanged, and so is any scan without --allow-local-builds. Semgrep Managed Scanning scans are not affected. (SC-3613)
- bash grammar update (v0.25.1):

  - Bash arithmetic (`$(( ))`, `$[ ]`, `(( ))`, C-style `for` headers) is now translated into operator trees with bash precedence, so patterns such as `$(( $X * 2 ))` match structurally. `$((...))` is no longer confused with `$(...)`.
  - Bash patterns accept a lone `...` before `done`, `fi` or `}`, e.g. `while ...; do ... done`, and `until` loops now parse as loops.
  - Bash words containing `#` (`x=foo#bar`) are no longer truncated, and arguments after a redirect or heredoc delimiter stay with their command.
  - Many Bash constructs that failed to parse now parse, e.g. `$[ ]`, `time ( ... )`, extglob in `case` and `[[ ]]`, and `if` function bodies.

  (bash-v0.25.1)
- clojure grammar update (v0.0.13):

  - Clojure source parsing now accepts reader-evaluated metadata, such as ^#=(keyword "a") [1 2], including the legacy #^ prefix. Semgrep does not execute reader evaluation, and metadata remains ignored during matching.
  - Clojure source parsing now supports reader conditionals inside symbolic values, such as ## #?(:clj Inf :cljs NaN), retaining the conditional branches without evaluating them.

  (clojure-v0.0.13)
- swift grammar update (0.5.0):

  - Swift parsing now supports type and value parameter packs, including each/repeat expansions and pack constraints.
  - Swift parsing now supports if and switch expressions used as variable initializers.
  - Swift parsing now supports macro declarations with typed signatures and #externalMacro definitions. This does not implement macro expansion.
  - Swift property observers now retain their willSet/didSet bodies and parameters separately from property initializers.
  - Swift parsing now supports borrowing, consuming, package, and distributed modifiers while preserving their distinct spellings.
  - Swift parsing now supports #unavailable conditions and distinguishes them from #available conditions. Both are mapped as `#…` expression nodes (not boolean `Not`) so patterns like `if !$X` do not match availability checks.
  - Swift parsing now supports member access on parenthesized existential types, such as (any P).self, and preserves the underlying parenthesized existential or opaque type.
  - Swift parsing now supports references to the bitwise AND operator using (&).

  (swift-0.5.0)

### ### Fixed

- The Cursor hooks installed by `semgrep mcp` now track edited files separately for each Cursor conversation, so one window's stop scan no longer scans or discards another conversation's edits. (mcp-hook-cache-per-conversation)
- The `semgrep mcp` server's `semgrep_scan` tool now only scans files inside the client's workspace root, or the server's working directory when the client reports none. Set `SEMGREP_MCP_ALLOW_SCAN_OUTSIDE_WORKSPACE=true` to scan files outside it. (mcp-scan-workspace-roots)
- Interfile scans no longer fail with `Failure: Failed to unmarshal cached
  value: Exception during parallelization: Exception.Timeout(...)` when a
  taint configuration timeout fires at an unlucky moment. The timeout now only
  skips the affected rule and file, as intended. (CODE-9810)
- The Semgrep source distribution on PyPI is once again published with provenance. (CODE-10070)
- Update certificate trust in Semgrep and its container images to address the eMudhra certificate authority compromise. Disable curl-based telemetry on macOS and Windows; default telemetry and scanning remain available. (INCID-521)
- Supply Chain: multi-document pnpm-lock.yaml files (written by pnpm 11+) are
  now parsed instead of reporting zero dependencies. A lockfile that fails to
  parse now marks its subproject as unresolved rather than resolved with zero
  dependencies. (SC-3870)
- Supply Chain now parses go.mod files that contain a `godebug ( ... )` block. Such files used to report no dependencies and a parse error. (SC-4268)
- Supply Chain now finds the direct dependencies of Poetry projects that declare them in the `[project] dependencies` table (Poetry 2.0 and later), in dependency groups or as extras, instead of reporting every package with unknown transitivity. Packages from Poetry dependency groups are now reported as direct. PyPI rule matching now ignores differences in case, `-`, `_` and `.` in package names, so rules match however a lockfile spells the name. (SC-4278)
- Supply Chain now parses yarn.lock files generated by yarn 0.16 and earlier, whose header has no blank line after `# yarn lockfile v1`. Previously these failed with "Failed to parse yarn.lock at 1:1" and no dependencies were analyzed. (SC-4286)
- Recognize all valid Python f-string prefixes without accepting invalid prefix combinations. (gh-11913)

### ### Infra/Release Changes

- Apply external rule-test expectations consistently in CLI and OCaml CI tests. (rule-expectation-ci)
- Add --override-expectations to the OCaml rule test runner to replace expected finding lines for individual targets, rules, and engines from an external JSON file. (rule-test-expectations)
