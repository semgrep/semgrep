## [1.159.0](https://github.com/semgrep/semgrep/releases/tag/v1.159.0) - 2026-04-10

### ### Fixed

- Semgrep now reports an error instead of silently returning zero findings when target file discovery fails (e.g., due to a git ls-files failure). (ENGINE-2626)
