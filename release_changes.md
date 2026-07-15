## [1.170.0](https://github.com/semgrep/semgrep/releases/tag/v1.170.0) - 2026-07-15

### ### Added

- Pro C/C++ scans now skip code inside statically-dead preprocessor branches
  (for example, `#if 0 ... #else ... #endif`). Patterns that would otherwise
  match against intentionally-disabled code no longer report on it. (cpp-if-zero-filter)
- Restored obackward: semgrep-core and semgrep-core-proprietary once again print a backtrace when receiving a fatal signal (e.g. SIGSEGV) (obackward)
- `semgrep install-semgrep-pro` now sends usage metrics so that
  installation errors can be tracked. Metrics can be disabled with
  `--metrics off` or `SEMGREP_SEND_METRICS=off`. Metrics payloads also
  now include the method used to install the Semgrep CLI (pip, homebrew,
  docker, or unknown), detected heuristically. See metrics.md for
  more details of what exactly is sent. (engine-2858)

### ### Changed

- Increased the timeout for dynamic dependency resolution subprocesses from
  600 to 900 seconds, giving large projects more time to resolve dependencies
  before timing out. (SC-3699)
- Pro C/C++ `#if 0` filtering now also handles cases where the directive splits a
  syntactic unit.  For example, a function signature toggle like `#if 0 void
  foo(int i) { #else void foo(uint32_t i) { #endif`. (engine-994)

### ### Fixed

- Fixed a crash at startup (`Fatal error: Failed to allocate signal stack for
  domain 0`) when running Semgrep on systems with musl 1.2.6 (e.g. Alpine 3.24) on
  recent Intel CPUs whose kernel-reported minimum signal-stack size exceeds musl's
  build-time SIGSTKSZ (notably AMX-capable Xeons). (ENGINE-2863)
- Dockerfile: Fixed parse errors on `RUN` instructions that use heredoc syntax
  (`<<EOF`, `<<-EOF`, quoted delimiters). (LANG-263)
- `metavariable-type` now supports fully qualified type names in languages
  where a qualified name in type position parses as an expression (e.g.
  Python's `types: [a.b.C]`) when the metavariable's type is determined by
  type inference, such as Pro engine cross-file type resolution. (LANG-583)
- Updated the ocaml-tree-sitter-core dependency to the latest `main`.

    * Fails loudly on a parser/runtime ABI mismatch
    * Stamps every generated `parser.c` with the tree-sitter version that produced it.
    * Changed paths where tree-sitter versions are installed (lang-591)
