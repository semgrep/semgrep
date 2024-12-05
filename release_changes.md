## [1.98.0](https://github.com/returntocorp/semgrep/releases/tag/v1.98.0) - 2024-12-04


### Added


- taint-mode: Semgrep will now track invididual fields/keys in record/dict
  expressions.

  For example, in Semgrep Pro:

      def foo():
          return { 0: "safe", 1: taint }

      def test():
          t = foo()
          sink(t[0]) # safe thus NO finding
          sink(t[1]) # finding (code-7781)
- The TypeScript parser now supports ellipses in function parameters. For
  example, the following code is TypeScript (as opposed to pure JavaScript)
  because it uses decorators on function parameters:

  ```
  foo(x, @Bar() y, z): string { return ''; }
  ```

  You can match this method using the following pattern:

  ```
  function $FN(..., @Bar(...) $X, ...) { ... }
  ``` (code-7800)
- [Pro only] Patterns such as `new $T(...)` will now match C# [target-typed new expressions](https://devblogs.microsoft.com/dotnet/welcome-to-c-9-0/#target-typed-new-expressions) such as `new ()`. (csharp-new)
- Symbolic propagation will now propagate record expressions. (flow-86)
- Adds support for SwiftPM Package.resolved version 3 to Supply Chain (sc-1964)


### Changed


- Optimize matching performance of dot access ellipsis (`x. ... .foo`) on very long strings of dot accesses in source files. (match-perf)
- Renames the flag to enable experimental lockfileless scanning from `--allow-dynamic-dependency-resolution` to `--allow-local-builds` to reflect the security risk that enabling the feature introduces. Also removes the opt-out flag, since we do not plan to make the feature enabled by default. (sc-2006)


### Fixed


- taint-mode: Fixed what is considered a sink when a sink formula matches a
  lambda expression: it is the lambda itself that is the sink, not the
  individual statements in the lambda.

  Example:

      function test() {
        // no longer generates finding
        // note that `log(taint)` is *not* a sink
        sink(() => { log(taint); return "ok" });
      } (code-7758)
- taint-mode: Semgrep will no longer report an unexpected finding in cases like:

      var x = [taint];
      x = ["ok"];
      sink(x); // no finding, x is ok

  Variable `x` will be correctly cleaned after `x = ["ok"]`. (flow-87)
- Removed the experimental --use-osemgrep-sarif flag. (saf-1703)
- A single carriage return (CR) is not recognized anymore as a
  newline. This avoids out of bound error when reporting findings
  on old windows files using this old format. (saf-1743)
