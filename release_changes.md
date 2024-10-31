## [1.95.0](https://github.com/returntocorp/semgrep/releases/tag/v1.95.0) - 2024-10-31


### Changed


- Remove deprecated `--enable-experimental-requirements` flag. Functionality has
  been always enabled since Semgrep 1.93.0. (ssc-1903)


### Fixed


- osemgrep: Running `osemgrep` with the Pro Engine now correctly runs rules with proprietary languages (saf-1686)
- Fixed bug where semgrep would crash if --trace was passed (saf-tracing)
