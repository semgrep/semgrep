## [1.166.0](https://github.com/semgrep/semgrep/releases/tag/v1.166.0) - 2026-06-11

### ### Added

- Pro: Added experimental cross-file (interfile) analysis for Gosu, enabling taint tracking across multiple Gosu source files. (gosu-interfile)
- Added support for more operators for folding for constant propagation, including subtraction, division, bit ops, bit shifts, comparisons, and more (ENGINE-2789)

### ### Fixed

- Fixed parsing of integer literals with an underscore immediately after the radix prefix (e.g. `0x_dead_beef`, `0o_755`, `0b_1010_1010`). (LANG-533)
- Python parsing now preserves type parameters on `def` and `class` definitions. (LANG-536)
- Semgrep no longer stores the API token in  `~/.semgrep/settings.yml`'s stored
  token when the current scan's token is supplied via the `SEMGREP_APP_TOKEN`
  envvar. (SEC-2240)
- `semgrep ci` scans originating from a pre-commit hook will no longer fail with
  `Unable to create '<tmp>/.git/index.lock': Not a directory` in certain cases. (engine-2736)

### ### Infra/Release Changes

- Added parsing tests covering Python language features (Python 3.0–3.12). (LANG-531)
