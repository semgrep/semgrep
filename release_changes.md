## [1.103.0](https://github.com/semgrep/semgrep/releases/tag/v1.103.0) - 2025-01-15


### Added


- pro: taint: Support for lambdas as callbacks.

      var tainted = source();

      function withCallback1(val, callback) {
          if (val) {
              callback(val);
          }
      }

      withCallback1(tainted, function (val) {
          sink(val); // finding !
      }); (code-7626)
- pro: python: Semgrep will now consider top-level lambdas like `x` below for
  inter-procedural analysis:

      x = lambda s: sink(s) # now we get a finding !

      x(taint) (gh-10731)


### Changed


- Removed `pip` from the Semgrep Docker image. If you need it, you may install it by running `apk add py3-pip`. (saf-1774)


### Fixed


- Python: Now correctly parsing files with parenthesized `with`s, like this:
  ```
  with (
    f() as a,
    g() as b,
  ):
    pass
  ``` (saf-1802)
- Semgrep will now truncate error messages that are produced when they are very long (saf-333)
