## [1.172.0](https://github.com/semgrep/semgrep/releases/tag/v1.172.0) - 2026-07-28

### ### Added

- Added support for the OpenTofu `.tofu` file extension. Because OpenTofu uses the same HCL grammar as Terraform, `.tofu` files are now automatically detected and scanned as Terraform, so they are picked up by recursive scans and Terraform rulesets (e.g. `p/terraform`) with no extra configuration. (ENGINE-2884)

### ### Changed

- Updated the Ruby parser to tree-sitter-ruby v0.23.1, improving support for `!=`, case/when expressions with line breaks, and element references that take a block. (LANG-206)
- The window for collecting git contributor information during `semgrep ci` has been extended from the last 30 days to the last 90 days, to match the updated usage policy. (contributor-window-90-days)

### ### Fixed

- Semgrep will now print richer error messages on segfaults when `--debug` is NOT passed, matching the segfault error output when `--debug` is passed (engine-segv)
- Fixed a source of rare, nondeterministic crashes and incorrect results caused
  by an OCaml compiler bug. Semgrep now builds against a compiler fork that
  backports the upstream fix. (ocaml_codegen_fix)
- Fixed excessive heap growth after explicit major garbage collections. Semgrep
  now builds against an OCaml compiler that improves garbage collection duty
  cycle pacing. (ocaml_gc_pacing_fix)
- Improved the `Scan Status` output when no code rules will run (e.g. a
  Secrets-only or Supply-Chain-only scan). The summary line no longer reports a
  confusing "0 Code rules", and the "Code Rules" section now states explicitly
  either that code scanning is not enabled or that there are no code rules to run,
  instead of printing an empty table. (ENGINE-2878)
- Fixed lockfileless Gradle dependency resolution failing with "Parsing
  dependency output failed (Resolve_gradle.gradle_resolved_dependency)". The
  github-dependency-graph-gradle-plugin used during resolution was fetched
  unpinned, and its 1.4.2 release renamed keys in its JSON output. The plugin is
  now pinned to 1.4.1. (sc-3738)
