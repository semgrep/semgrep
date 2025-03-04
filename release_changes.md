## [1.111.0](https://github.com/semgrep/semgrep/releases/tag/v1.111.0) - 2025-03-04


### Changed


- Removed `.semgrepignore` file lookup using the `SEMGREP_R2C_INTERNAL_EXPLICIT_SEMGREPIGNORE` environment variable. This was used by semgrep-action which
  has been deprecated. (semgrep-action)


### Fixed


- pro: Fixed bug that could prevent taint tracking from following a virtual call
  in JS/TS. (code-8065)
- PRO: Restricted heuristic search of the Python module path to paths only under
  the project root to reduce inaccuracies in module resolution. This change
  mitigates the risk of resolving module specifiers to incorrect paths,
  particularly in cases involving common library names (e.g., `django`). (code-8146)
- Fix the incorrect schema and analysis type in the JSON output of the secret
  findings when using the --gitlab-secrets flag. (scrt-833)
