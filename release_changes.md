## [1.173.0](https://github.com/semgrep/semgrep/releases/tag/v1.173.0) - 2026-08-12

### ### Added

- `semgrep-core -version` now reports the git commit that the binary was built
  from. (core-version-git-sha)
- Pro: Added a `no_disk_cache` memory policy (`--x-mem-policy no_disk_cache`) to trade higher memory usage for not caching intermediary scan data structures on disk. (no-disk-cache-memory-policy)

### ### Changed

- File targeting now submits path-filtering work to worker domains in batches
  removing multithreaded synchronization overhead for very large repos. (ENGINE-2854)
- Parallel rule parsing for large rulesets now shards rules into files sized by
  bytes rather than by worker count, stabilizing memory allocations at parse-time
  and improving parsing throughput. (ENGINE-2920)
- Updated the Solidity parser for newer language features (transient storage, named mapping parameters, `layout at`, assembly flags, `global` using-directives, EVM Cancun builtins) and corrected ternary/`?:` vs member-access precedence. (LANG-207)
- Pro: Unsupported patterns in case expressions are now handled more
  robustly, with improved error recovery during taint analysis — branch
  bodies are no longer dropped from the dataflow intermediate language when
  a pattern can't be compiled. Pattern matching in statement position also
  now benefits from the same compilation as expression position. (LANG-598)
- Improved performance in highly parallel scans with many target files, running
  on systems using the musl libc implementation. (pcre2-match-data-reuse)

### ### Fixed

- When the internal semgrep-core RPC subprocess is terminated by a signal (for
  example an out-of-memory kill or a segfault), Semgrep now logs a clear error
  naming the signal, instead of the misleading "Expected a number, got ''"
  message. When debug logging is enabled, the tail of the subprocess's stderr is
  included as well. (rpc-subprocess-death-diagnostics)
- A target file whose path filtering failed was silently omitted from the scan,
  appearing in neither the results nor the list of skipped targets. Such paths
  are now retried, and any that still fail are reported as skipped targets so
  they show up in the scan report. (ENGINE-2854)
- Fixed a bug where a capture-group `metavariable-regex` or a binding-introducing
  `metavariable-pattern` would emit a duplicate finding on the same range whose
  message still contained the raw metavariable (e.g. `a hash $ALG was detected`)
  instead of the substituted value. Only the correctly-substituted finding is now
  reported. (ENGINE-2932)
- Fixed fully-qualified name resolution for Rust symbols imported via a braced
  grouped `use` with a nested path (e.g. `use a::b::C`). It was wrongly being
  resolved as b::a::C. (LANG-234)
- Fixed an issue where MCP mode could fail to scan UTF-8 files containing multibyte characters, including Japanese text, on systems using a non-UTF-8 locale such as cp932. Files are now read as UTF-8, with undecodable bytes replaced so a single unusual file does not fail the entire scan. (gdn-168)
