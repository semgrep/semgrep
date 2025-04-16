## [1.119.0](https://github.com/semgrep/semgrep/releases/tag/v1.119.0) - 2025-04-16


### Added


- python: Semgrep will now perform dataflow analysis within and through comprehensions. (saf-1560)
- A new subcommand `semgrep show project-root` is now provided to display
  the project root path associated with a scan root. This is useful for
  troubleshooting Semgrepignore (v2) issues. (saf-1936)


### Fixed


- tainting: Apply `taint_assume_safe_numbers` and `taint_assume_safe_booleans`
  earlier when considering to track taint through class fields and function
  parameters. If the field/parameter has a number/Boolean type and the
  corresponding option is set, it will just not be tracked. In some cases this
  can help with performance.

  Also added `short`/`Short` to the list of integer types recognized by
  `taint_assume_safe_numbers`. (code-8345)
- IDE: The Semgrep VS Code Extension will no longer hang on `Getting code actions from Semgrep...`
  on saving a file, when updating rules. (saf-1954)
