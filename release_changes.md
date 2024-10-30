## [1.94.0](https://github.com/returntocorp/semgrep/releases/tag/v1.94.0) - 2024-10-30


### Fixed


- pro: taint-mode: Semgrep should no longer confuse a `return` in a lambda with
  a `return` in its enclosing function.

  E.g. In the example below the return value of `foo` is NOT tainted:

      function foo() {
          bar(() => taint);
          return ok;
      } (code-7657)
- OCaml: matching will now recognized "local open" so that a pattern like
  `Foo.bar ...` will now correctly match code such as `let open Foo in bar 1`
  or `Foo.(bar 1)` in addition to the classic `Foo.bar 1`. (local_open)
- Project files lacking sufficient read permissions are now skipped gracefully
  by semgrep. (saf-1598)
- Semgrep will now print stderr and additional debugging info when semgrep-core
  exits with a fatal error code but still returns a json repsonse (finishes
  scanning) (saf-1672)
- semgrep ci should parse correctly git logs to compute the set of contributors
  even if some authors have special characters in their names. (saf-1681)
