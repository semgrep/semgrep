## [1.186.0](https://github.com/semgrep/semgrep/releases/tag/v1.186.0) - 2026-11-17

### ### Added

- Secrets findings can include a versioned `secret_hash` when requested (e.g., with `--compute-secret-hash`). This hash comprises the secret value and values used in the validators, to allow for identification of the same secret being present at multiple locations (cf. `match_based_id`, which depends on the location of the finding). (SCRT-987)

### ### Changed

- Semgrep's release binaries are now built with cross-module inlining enabled, so scans use about 5-11% less CPU time. (release-profile)

### ### Fixed

- pro: Rules with `interfile: true` now keep Pro intrafile interprocedural taint analysis when scanning languages without interfile support (e.g. OCaml, Rust), so taint through helper functions in the same file is reported. (interfile-rule-intrafile-fallback)
- `semgrep mcp` now respects `SEMGREP_SEND_METRICS=off`, which turns off its
  metrics and tracing like `SEMGREP_MCP_DISABLE_TRACING=true`. Its metrics and
  logs also no longer include scan error messages, semgrep's stderr, or
  supply chain scan results. (mcp-telemetry-redact)
- Pro: Python type filters now recognize calls to builtin classes and builtin functions when using interfile analysis or single-file type resolution. (CODE-10241)
- Dependency resolution errors for pip and setup.py projects now include the failing command's error output (for example `No module named 'setuptools'`) instead of an empty message. setup.py projects now resolve when `python3` is Python 3.12 or later, whose virtual environments no longer include setuptools. (SC-4349)
- Python CLI usage metrics will now be sent to the URL set by SEMGREP_METRICS_URL. The default destination remains unchanged. (SMS-3925)

### ### Infra/Release Changes

- Updated tree-sitter-requirements grammar to v0.5.0 (no behavior change). (requirements-v0.5.0)
