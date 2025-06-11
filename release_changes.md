## [1.125.0](https://github.com/semgrep/semgrep/releases/tag/v1.125.0) - 2025-06-11


### Added


- Dependency resolution errors that result from local builds are now reported in the scan log by default. (SC-2442)
- Adds reporting of SSC subproject dependency resolution to the output when using `--json`. (SC-2458)
- Semgrep's JSON output now will always include some basic profiling data (WIP). (code-8529)
- C# Dependency Parsing can now handle dependencies with "Project" & "CentralTransitive" transitivities. (sc-2376)


### Fixed


- Fixed an issue present since v1.117.0 that led `.semgrepignore` excludes to be applied to Secrets product scans. Now, Semgrep will once again scan files that have been excluded from Code and SSC scans for possible leaked secrets. (SAF-2067)
- Added support for npm aliasing in package-lock.json, fixing a bug where packages would rarely be misidentified. (SC-2387)
- Fixed scenario where case statements with ellipsis did not match patterns correctly. (gh-10086)
- Nosemgrep ignore comments no longer require exactly one space before, allowing for more commenting styles. (gh-11041)
- Fixed bug where Javascript autofix breaks syntax for if statements by consuming parentheses. (gh-9522)
- Fix: the Semgrep findings returned by the Semgrep language server (LSP)
  are now sorted correctly based on their location within files.
  This benefits all the Semgrep IDE extensions (VSCode, IntelliJ). (ide-findings-order)
- fixed an issue where `semgrep ci` logs in GitLab would return an incorrect URL
  with the wrong `&ref=...` argument. (saf-959)
