## [1.165.0](https://github.com/semgrep/semgrep/releases/tag/v1.165.0) - 2026-06-03

### ### Added

- Added `--max-match-context-size` option to limit the number of characters of source code included as context for each match in the output. This prevents matches in minified files (e.g., minified JavaScript where the entire file is a single line) from producing enormous output Set to 0 for unlimited, which is the default value. (ENGINE-2117)

### ### Changed

- Replaced `--x-no-python-schema-validation` with a value-taking `--x-rule-validation=full|core-only|none` flag. The default (`full`) preserves existing Python rule validation behavior; `core-only` matches the old flag's semantics (disables Python rule validation and uses semgrep-core RPC validation only); `none` skips both pre-validation passes, surfacing rule errors at scan-time. `--x-no-python-schema-validation` is still accepted as a no-op with a deprecation warning, and will be removed in a future release. (x-rule-validation)
- Python: Updated Python grammar (LANG-201)

### ### Fixed

- Added bit shift operations to metavar comparison in addition to already present standard arithmetic operators and logical bit ops. (ENGINE-2448)
- Reduce intermittent `validation_error` results on HTTP secret validators (Facebook, Slack, Stripe, Google, Cloudflare, etc.) by retrying transient network failures, mirroring the retry behavior already present for AWS validators. (SCRT-965)
