## [1.105.0](https://github.com/semgrep/semgrep/releases/tag/v1.105.0) - 2025-01-29


### Added


- Semgrep can dynamically resolve dependencies for C# Solutions denoted by *.csproj (sc-2015)


### Changed


- Added extra defensive try/catch around lockfile parsing (parsing)


### Fixed


- LSP shortlinks in diagnostics should no longer drop anchors or query parameters
  in URIs. (gh-10687)
- Some bug fixes to pnpm lockfile parsing. (gh-2955)
- Fix npm aliasing bug in yarn parser. (sc-2052)
- Fixed bug where supply chain diff scans of package-lock.json v2 projects incorrectly produced non-new findings (sc-2060)
