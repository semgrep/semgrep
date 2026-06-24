## [1.168.0](https://github.com/semgrep/semgrep/releases/tag/v1.168.0) - 2026-06-24

### ### Added

- Added an experimental `--x-dependency-paths` flag to `scan` and `ci` that includes the full dependency path(s) for transitive supply-chain findings in `--json` and `--sarif` output. (SC-3547)

### ### Changed

- Malicious supply chain rules are now labeled "Malicious" instead of "Basic" in the scan analysis summary table. (SC-3504)

### ### Infra/Release Changes

- semgrep-core no longer depends on libpcre 8.x; libpcre2 10.x is now the sole regex engine. (drop-libpcre)
- Aliengrep (generic mode) now uses the maintained libpcre2 10.x regular-expression library instead of the deprecated libpcre 8.x. Matching behavior is unchanged. (aliengrep-pcre2)
- The `metavariable-regex` and `metavariable-comparison` (`re.match()`) runtimes now use the maintained libpcre2 10.x library instead of the deprecated libpcre 8.x. Matching behavior is unchanged. (eval-generic-pcre2)
