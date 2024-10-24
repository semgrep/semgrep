## [1.93.0](https://github.com/returntocorp/semgrep/releases/tag/v1.93.0) - 2024-10-23


### Added


- Improved naming for Common JS module imports (`require`) in arbitrary
  expression contexts. Notably, in-line use of `require` should now be linked to
  the correct module. For instance, the pattern `foo.bar` should now match
  against `require('foo').bar` and taint is likewise similarily tracked. (code-7485)
- Secrets: `semgrep ci` output now includes a list of all secrets rules which
  generated at least one blocking finding (similar to Code) (code-7663)
- Added experimental support via `--allow-dynamic-dependency-resolution` for dynamic resolution of Maven and Gradle dependencies for projects that do not have lockfiles (in Semgrep Pro only). (gh-2389)
- Expanded support for pip requirement lockfiles is now available by default. Semgrep will now
  find any *requirement*.txt file and lockfiles in a requirements folder (**/requirements/*.txt).
  The existing experimental flag `--enable-experimental-requirements` is now deprecated and
  will be removed in a future release. (gh-2441)


### Changed


- Removed support for Vue. The tree-sitter grammar has not been updated in 3 years,
  there was no community rules added and semgrep-vue is causing linking conflicts
  when compiling semgrep under Windows so just simpler to remove support for Vue.
  In theory, extract mode could be a good substitute to parse Vue files. (vue)


### Fixed


- semgrep will now print exit codes if a segfault/OOM/other terminating signal happens in semgrep-core, or any of semgrep-core's child processes (saf-1646)
