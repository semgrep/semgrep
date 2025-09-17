## [1.137.0](https://github.com/semgrep/semgrep/releases/tag/v1.137.0) - 2025-09-17


### Added


- pro: typescript: Improved name resolution for destructuring parameters. (code-9088)
- Added a new `semgrep mcp` subcommand, which runs the Semgrep MCP server, which previously
  used to live at https://github.com/semgrep/mcp. That repository will be *deprecated*
  as of this release, and future MCP contributions / issues should go into this repo. (saf-2239)


### Changed


- Update semgrep-interfaces to only accept valid lanugage keys for editor (PR-4600)


### Fixed


- Fix incorrect interpretation of `\#` and `\ ` in glob patterns found in
  Semgrepignore and included Gitignore files. (fix-glob-escape)
- Removed `pkg_resources is deprecated` warning by bumping opentelemetry-*
  packages (gh-11069)
- Fixes an issue in Dart language processing to return better results (gh-11173)
