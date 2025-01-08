## [1.102.0](https://github.com/semgrep/semgrep/releases/tag/v1.102.0) - 2025-01-08


### Added


- Added pro-only support for parsing a dependency graph from package-lock.json v1 files (SC-1858)
- Added pro-only support for parsing a dependency graph from package-lock.json v2 and v3 files (SC-1991)
- The poetry.lock parser can now parse dependency relationships (ssc-1970)
- The Yarn.lock V1 and V2 parsers can parse dependency relationships. (ssc-1988)


### Fixed


- The `semgrep test` and `semgrep validate` commands have been
  correctly documented as EXPERIMENTAL (in semgrep --help).
  Those commands are not GA yet and people should still
  use the `semgrep scan --test` and `semgrep scan --validate` (or
  the variants without the implicit "scan") commands (unless
  they want to experiment with getting results faster and are ok
  with incomplete coverage of the legacy `semgrep --test`
  and `semgrep --validate`). (experimental)
- Improve error handling for functionality ancillary to a scan (such as looking for nosemgrep comments and rendering autofixes) to reduce the likelihood of an unexpected error in such a component bringing down the entire scan. (saf-1737)
- Fix the behavior of semgrep when running into broken symlinks.
  If such a path is passed explicitly as a scanning root on the
  command line, it results in an error. Otherwise if it's a file discovered
  while scanning the file system, it's a warning. (saf-1776)
- Fixed another crash due to exception in lines_of_file. The code
  should now be more robust and not abort the whole scan when
  an out of bound line access happens during the nosemgrep analysis
  or when outputing the lines of a match. (saf-1778)
- Direct dev dependencies in yarn/npm lockfiles are now correctly marked as direct (sc-1996)
