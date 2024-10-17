## [1.92.0](https://github.com/returntocorp/semgrep/releases/tag/v1.92.0) - 2024-10-17


### Added


- Pro: taint-mode: Semgrep has now basic support to track taint through callbacks,
  when they lead to a sink, e.g.:

      function unsafe_callback(x) {
        sink(x); // finding here now !
      }

      function withCallback(val, callback) {
        callback(val);
      }

      withCallback(taint, unsafe_callback); (code-7476)
- New subcommand `dump-cst` for tree-sitter languages available via `semgrep
  show`. This shows the concrete syntax tree for a given file. (code-7653)
- Pro only: Updated C# parser supporting all versions of the language up to 13.0 (.NET 9) (saf-1610)
- Added support for the Move-on-sui language! (sui)
- Pro-only: semgrep test now supports the --pro flag to not only use pro languages
  but also run the tests with the --pro-intrafile engine flag. If a finding
  is detected only by the pro engine, please use `proruleid:` instead of `ruleid:`
  and if an OSS finding is actually a false positive for the pro engine, please
  add the `prook:` to your test annotation. (test_pro)


### Fixed


- pro: dataflow: Fixed a bug that could cause a class constructor to not be analyzed
  in the correct dependency order, potentially leading to FNs. (code-7649)
- Display an ✘ instead of a ✔ in the scan status print out when scanning with Semgrep OSS code
  is not enabled. (grow-422)
- semgrep will no longer randomly segfault when --trace is on with -j > 2 (saf-1590)
- Previously, semgrep fails when --trace-endpoint is specified, but --trace is not.

  Now, we relax this requirement a bit. In this case, we disable tracing, print out a warning, and continue to scan. (sms-550)
