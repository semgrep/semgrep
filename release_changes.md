## [1.150.0](https://github.com/semgrep/semgrep/releases/tag/v1.150.0) - 2026-01-29


### Added


- Connecting to the Semgrep MCP server via streamableHttp now requires OAuth. (saf-2453)


### Changed


- Migrated from `pipenv` to `uv` for `./cli` package management (uv)


### Fixed


- pro: Improved virtual method resolution in Scala (code-9213)
- Improved performance for supply chain scans by reducing pre-computation when printing the scan status. This results in slightly less information being displayed in the case that there are no rules to run. (gh-5436)
- Supply Chain Analysis: fixed version range matching for NPM packages with versions containing a prerelease identifier such as `-alpha` in `1.2.3-alpha`. (sc-3001)
