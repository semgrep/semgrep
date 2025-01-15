<!-- Do not edit. This changelog is updated at release time by towncrier
     from individual files in the changelog.d/ directory.
-->

# Changelog

<!-- insertion point -->

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


## [1.101.0](https://github.com/semgrep/semgrep/releases/tag/v1.101.0) - 2024-12-18


### Added


- Improved pnpm-lock.yaml parsing. (gh-2663)


### Changed


- Re-ordered some terminal output of `semgrep ci` to allow semgrep-app to block scans based on specific findings (SECW-2740)
- A few fields in the JSON output (e.g., "fingerprint", "metavars") require now
  the user to be logged in to see them.
  See https://semgrep.dev/docs/semgrep-appsec-platform/json-and-sarif#json
  for more information. (json)
- We're renaming semgrep OSS to Semgrep Community Edition.
  See https://semgrep.dev/blog/2024/important-updates-to-semgrep-oss/
  for more information. (rename)
- A few fields in the SARIF output (e.g., "fingerprints") require now
  the user to be logged in to see them.
  See https://semgrep.dev/docs/semgrep-appsec-platform/json-and-sarif#sarif
  for more information. (sarif)


### Fixed


- pro: Improved inter-file tracking of tainted global variables. (code-7054)
- Python (pro-only): Taint now correctly tracks through calls to class methods
  within a class, via the `cls` parameter.

  So for instance, we would be able to determine a source-to-sink
  vulnerability in the following code snippet:
  ```
  class A:
    def foo(self, x):
      sink(x)

    @classmethod
    def bar(cls):
      cls.foo(source)
  ``` (saf-1765)
- pro: Fixed bug when generating inter-procedural taint traces, that it could
  cause a call-step to be missing in the trace. (saf-1783)
- Restored the "rules" field in the SARIF output, even when logged out. (saf-1794)


## [1.100.0](https://github.com/semgrep/semgrep/releases/tag/v1.100.0) - 2024-12-12


### Added


- Pro engine now correctly distinguishes overloaded Scala methods based on their
  arity and parameter types, e.g., `foo(x: Int, y: String)` vs. `foo(x: String,
  y: Int)`. (code-7870)


### Changed


- The minimum Python version for semgrep is now 3.9.
  We are dropping support for Python 3.8 (python)


### Fixed


- pro: Fixed a bug in interprocedural index-sensitive taint analysis that caused
  false negatives when a function updated an arbitrary index, e.g.:

      var x = {};

      function foo(k) {
          x[k] = source();
      }

      function test(k) {
          foo(k);
          sink(x); // finding here!
      } (CODE-7838)
- Fixed bug affecting taint tracking through static fields when mixing accesses
  using the class name and using an instance object, e.g.:

      class C {
          static String s;
      }

      ...

              C o = new C();
              C.s = taint;
              sink(o.s); // finding ! (CODE-7871)
- No more RPC error when using --sarif with some join-mode rules.
  Moreover, regular rules without the 'languages:' field will be skipped
  instead of aborting the whole scan. (gh-10723)


## [1.99.0](https://github.com/returntocorp/semgrep/releases/tag/v1.99.0) - 2024-12-05


### Fixed


- Fix the date format used in --gitlab-sast option to match
  the spec and not use the RFC 3339.
  Thanks to Elias Haeussler for the fix. (saf-1755)


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


## [1.97.0](https://github.com/returntocorp/semgrep/releases/tag/v1.97.0) - 2024-11-19


### Added


- Improved logic for interfile analysis in TypeScript projects using
  [project references](https://www.typescriptlang.org/docs/handbook/project-references.html). (code-7677)
- Semgrep Pro engine now resolves method invocations on abstract classes. In
  addition to the existing resolution for interface method invocations, this
  change further enhances dataflow tracking accuracy for dynamic method
  invocations. (code-7750)
- Added the ability to validate temporary AWS tokens in the secrets product. (gh-2554)
- Poetry.lock & Pyproject.toml parsers can now handle multiline strings. (ssc-1942)


### Fixed


- Improved error handling for some networking errors (e.g., premature server
  disconnection). In some cases this would previously cause a fatal error, but we
  should instead be able to recover in most instances (and now can). (code-7715)
- Target file selection in git projects: files containing special characters
  (according to git) are now scanned correctly instead of being ignored. (saf-1687)
- Swift: Ellipses and metavariable ellipses can now be used as function parameters in patterns. (saf-1721)
- Semgrep will no longer freeze when tracing is enabled and it has a low memory limit (saf-1722)
- osemgrep-pro: Autofix and nosemgrep now work properly (saf-1724)


## [1.96.0](https://github.com/returntocorp/semgrep/releases/tag/v1.96.0) - 2024-11-07


### Added


- The pro engine now handles duplicate function names in C. When duplicate
  functions are found, we assume that any of the duplicated functions could be
  called. For example, if the function `foo` is defined in two different files,
  taint errors will be reported for both instances:

  ```
  // "a/test.h"
  void foo(int x) {
      //deepruleid: dup-symbols
      sink(x);
  }

  // "b/test.h"
  void foo(int x) {
      //deepruleid: dup-symbols
      sink(x);
  }

  // "main.c"
  #ifdef HEADER_A
      #include "a/test.h"
  #else
      #include "b/test.h"
  #endif

  int main() {
      int x = source();
      foo(x);
  }
  ``` (code-7654)


### Changed


- Reduced memory allocations while processing nosemgrep comments, improving memory use and time for scans with a large number of findings. (nosem-mem)


### Fixed


- Optimized taint-mode (only in Pro) to scale better when there is a large number
  of matches of sources/propagators/sanitizers/sinks within a function. (flow-83)
- Fixed a bug in the supply chain scanner's gradle lockfile parser. Previously, semgrep would fail to parse
  any gradle lockfile which did not start with a specific block comment. Now, semgrep will parse gradle
  lockfiles correctly by ignoring the comment (allowing any or no comment at all to exist). (gh-10508)
- Exceptions thrown during the processing of a target should not fail
  the whole scan anymore (regression introduced in 1.94.0). The scan will
  have an exit code of 0 instead of 2 (unless the user passed --strict in which
  case it will exit with code 2). (incid-110)
- Fix exponential parsing time with generic mode on input containing many
  unclosed braces on the same line. (saf-1667)
- Fix regexp parsing error occurring during ReDoS analysis when encountering
  a character class starting with `[:` such as `[:a-z]`. (saf-1693)
- Fix in `semgrep scan`: anchored semgrepignore patterns for folders such
  as `/tests` are now honored properly. Such patterns had previously no
  effect of target file filtering. (semgrepignore-anchored-dirs)


## [1.95.0](https://github.com/returntocorp/semgrep/releases/tag/v1.95.0) - 2024-10-31


### Changed


- Remove deprecated `--enable-experimental-requirements` flag. Functionality has
  been always enabled since Semgrep 1.93.0. (ssc-1903)


### Fixed


- osemgrep: Running `osemgrep` with the Pro Engine now correctly runs rules with proprietary languages (saf-1686)
- Fixed bug where semgrep would crash if --trace was passed (saf-tracing)


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


## [1.91.0](https://github.com/returntocorp/semgrep/releases/tag/v1.91.0) - 2024-10-10


### Added


- Type inference in the Pro engine has been improved for class fields in
  TypeScript that are assigned a new instance but lack an explicit type
  definition. When no explicit type is provided for a class field, its type is
  inferred from the type of the expression assigned to it. For example, in the
  class definition `class Foo { private readonly bar = new Bar(); }`, the type of
  `bar` is inferred to be `Bar`. (code-7635)
- Cargo.lock parser can now associate dependencies with lockfile line numbers (sc-1140)


### Fixed


- Address python `rich.errors.LiveError` where attempting to display multiple progress bars
  raises an exception as flagged in #10562. (grow-414)
- C: Fix a regression causing pattern `-n` to sometimes not match code `-n`. (saf-1592)
- When a scan runs into an exception, the app is appropriately notified
  about the failure. Previously, in the app, it would seem to the user
  that the scan is still in progress. (sms-502)


## [1.90.0](https://github.com/returntocorp/semgrep/releases/tag/v1.90.0) - 2024-09-25


### Added


- Expanded support for requirement lockfiles. Semgrep will now find any `*requirement*.txt`
  file and lockfiles in a requirements folder (`**/requirements/*.txt`). This functionality
  will be gated behind the `--enable-experimental-requirements` CLI flag. (sc-1752)


### Changed


- Security update for code snippet storage & access methods. (gh-2038)


### Fixed


- Errors that occur in semgrep scans with jobs > 1 will now have more detail (SAF-1628)
- Dockerfile matching: `CMD $...ARGS` now behaves like `CMD ...` and matches
  any CMD instruction that uses the array syntax such as `CMD ["ls"]`. This
  fix also applies to the other command-like instructions RUN
  and ENTRYPOINT. (gh-9726)
- Pro Engine: There is now improved type inference in Kotlin and Scala. Constructor invocations like
  `Foo()` will now be inferred properly to be of type `Foo`. (saf-1537)


## [1.89.0](https://github.com/returntocorp/semgrep/releases/tag/v1.89.0) - 2024-09-19


### Fixed


- Fix crash on certain SCA parse errors caused by an access to an unbound variable. (gh-2259)


## [1.88.0](https://github.com/returntocorp/semgrep/releases/tag/v1.88.0) - 2024-09-18


### Added


- The dataflow analysis in the Pro engine can now track method invocations on
  variables of an interface type, safely assuming that any implementation of the
  method can be called. For example, tainted input vulnerabilities in both
  implementation classes can now be detected in the following code:

  ```java
  public interface MovieService {
    String vulnerableInjection(String input);
  }

  public class SimpleImpl implements MovieService {
    @Override
    public String vulnerableInjection(String input) {
      return sink(input);
    }
  }

  public class MoreImpl implements MovieService {
    @Override
    public String vulnerableInjection(String input) {
      return sink(input);
    }
  }

  public class AppController {
    private MovieService movieService;

    public String pwnTest(String taintedInput) {
      return movieService.vulnerableInjection(taintedInput);
    }
  }
  ``` (code-7435)
- Type inference for constructor parameter properties in TypeScript is now
  supported in the Pro engine. For example, the taint analysis can recognize that
  `sampleFunction` is defined in `AbstractedService` class in the following code:

  ```
  export class AppController {
      constructor(private readonly abstractedService: AbstractedService) {}

      async taintTest() {
          const src = source();
          await this.abstractedService.sampleFunction(src);
      }
  }
  ``` (code-7597)


### Changed


- include the exit code that semgrep will emit in the fail-open payload prior to exiting with a failure. (gh-2033)


## [1.87.0](https://github.com/returntocorp/semgrep/releases/tag/v1.87.0) - 2024-09-13


### Added


- Semgrep now infers more accurate type information for class fields in
  TypeScript. This improves taint tracking for dependency injection in
  TypeScript, such as in the following example:

  ```
  export class AppController {
      private readonly abstractedService: AbstractedService;

      constructor(abstractedService: AbstractedService) {
          this.abstractedService = abstractedService;
      }

      async taintTest() {
          const src = taintedSource();
          await this.abstractedService.sinkInHere(src);
      }
  }
  ``` (code-7591)
- Semgrep's interfile analysis (available with the Pro Engine) now ships with information about Python's standard library, improving its ability to resolve names and types in Python code and therefore its ability to produce findings. (py-libdefs)
- Added support for comparing Golang pre-release versions. With this, strict
  core versions, pseudo-versions and pre-release versions can all be
  compared to each other. (sc-1739)


### Changed


- If there is an OOM error during interfile dataflow analysis (`--pro`) Semgrep will
  now try to recover from it and continue the interfile analysis without falling back
  immediately to intrafile analysis. This allows using `--max-memory` with `--pro` in
  a more effective way. (flow-81)
- Consolidates lockfile parsing logic to happen once, at the beginning of the scan. This consolidated parsing now considers both changed and unchanged lockfiles during all steps of diff scans. (gh-2051)


### Fixed


- pro: taint-mode: Restore missing taint findings after having improved index-
  sensitivity:

      def foo(t):
          x = third_party_func(t)
          return x

      def test1():
          t = ("ok", taint)
          y = foo(t)
          sink(y) # now it's found! (code-7486)
- The Semgrep proprietary engine added a new entropy analyzer `entropy_v2` that supports strictness options. (gh-1641)


## [1.86.0](https://github.com/returntocorp/semgrep/releases/tag/v1.86.0) - 2024-09-04


### Added


- The taint analysis can now track method invocations on variables of an
  interface type, when there is a single implementation. For example, the tainted
  input vulnerability can now be detected in the following code:

  ```java
  public interface MovieService {
    String vulnerableInjection(String input);
  }

  @Service
  public class MovieServiceImpl implements MovieService {
    @Override
    public String vulnerableInjection(String input) {
      return sink(input);
    }
  }

  @RestController("/")
  public class SpringController {

    @Autowired
    private MovieService movieService;

    @GetMapping("/pwn")
    public String pwnTest(@RequestParam("input") String taintedInput) {
      return movieService.vulnerableInjection(taintedInput);
    }
  }
  ```

  When there are multiple implementations, the taint analysis will not follow any
  of them. We will add handling of cases with multiple implementations in
  upcoming updates. (code-7434)
- Uses of values imported via ECMAScript `default` imports (e.g., `import example
  from 'mod';`) can now be matched by qualified name patterns (e.g.,
  `mod.default`). (code-7463)
- Pro: taint-mode: Allow (experimental) control taint to propagate through `return`s.

  Now this taint rule:

      pattern-sources:
      - control: true
        pattern: taint()
      pattern-sinks:
      - pattern: sink()

  It is able to find this:

      def foo():
        taint()

      def test():
        foo()
        sink() # now it is found! (code-7490)
- A new flag --max-log-list-entries allows to control the
  maximum number of entries that will be shown in the log (e.g.,
  list of rule ids, list of skipped files).
  A zero or negative value disables this filter.
  The previous hardcoded limit was at 100 (and now becomes a default value). (max_log_list_entries)


### Changed


- Semgrep will now log memory-related warnings/errors when run in `--debug` mode,
  without the need to set `SEMGREP_LOG_SRCS=process_limits`. (logging)


### Fixed


- Fixed inter-file constant propagation to prevent some definitions from being
  incorrectly identified as constant, when they are modified in other parts of
  the codebase. (code-6793)
- pro: taint-mode: Fixed bug in taint signature instantiation that could cause an
  update to a field in a nested object to not be tracked.

  For example, in the code below, Semgrep knew that `Nested.update` updates the
  `fld` attribute of a `Nested` object`. But due to this bug, Semgrep would not
  know that `Wrapper.update` updated the `fld` attribute of the `nested` object
  attribute in a `Wrapper` object.

      public class Nested {

          private String fld;

          public void update(String str) {
              fld = str;
          }

          // ...
      }

      public class Wrapper {

          private Nested nested;

          public void update(String str) {
              this.nested.update(str);
          }

      // ...
      } (code-7499)
- Fixed incorrect range matching parametrized type expressions in Julia (gh-10467)
- Fixed an edge case that could lead to a failure to name or type imported Python symbols during interfile analysis. (py-imports)
- Fix overly-aggressive match deduplication that could, under certain circumstances, lead to findings being closed and reopened in the app. (saf-1465)
- Fixed regex-fix numbered capture groups, where it used to be the case that
  a `replacement:` regex with numbered capture groups like `\1\2\3` would effectivly
  be the same as `\1\1\1`.

  After the fix:
  ```python
  # src.py
  12345
  ```
  ```yaml
  pattern: $X
  fix-regex:
        regex: (1)(2)(3)(4)(5)
        replacement: \5\4\3\2\1
  ```
  actually results in the fix
  ```python
  54321
  ``` (saf-1497)


## [1.85.0](https://github.com/returntocorp/semgrep/releases/tag/v1.85.0) - 2024-08-15


### Added


- Semgrep now recognizes files ending with the extention `.tfvars` as terraform files (saf-1481)


### Changed


- The use of --debug will not generate anymore profiling information.
  Use --time instead. (debug)
- Updated link to the Supply Chain findings page on Semgrep AppSec Platform to filter to the specific repository and ref the findings are detected on. (secw-2395)


### Fixed


- Fixed an error with julia list comprehentions where the pattern:
  ```
  [$A for $B in $C]
  ```
  would match
  ```julia
  [x for y in z]
  ```
  However we would only get one binding [$A/x]

  Behavior after fix: we get three bindings [$A/x,$B/y,$C/z] (saf-1480)


## [1.84.1](https://github.com/returntocorp/semgrep/releases/tag/v1.84.1) - 2024-08-07


No significant changes.


## [1.84.0](https://github.com/returntocorp/semgrep/releases/tag/v1.84.0) - 2024-08-06


### Changed


- We switch from magenta to yellow when highlighting matches
  with the medium or warning severity. We now use magenta for
  cricical severity to be consistent with other tools such
  as npm. (color)


### Fixed


- Workaround deadlock when interfile is run with j>1 and tracing is enabled. (saf-1157)
- Fixed <multilang> file count to report the accurate number of files scanned by generic & regex
  so that no double counting occurs. (saf-507)


## [1.83.0](https://github.com/returntocorp/semgrep/releases/tag/v1.83.0) - 2024-08-02


### Added


- Dockerfile: Allow Semgrep Ellipsis (...) in patterns for HEALTHCHECK commands. (saf-1441)


### Fixed


- The use of --debug should generate now far less log entries.
  Moreover, when the number of ignored files, or rules, or
  other entities exceed a big number, we instead replace them
  with a <SKIPPED DATA> in the output to keep the output of semgrep
  small. (debuglogs)
- Fixed a bug introduced in 1.81.0 which caused files ignored for the Code
  product but not the Secrets product to fail to be scanned for secrets.
  Files that were not ignored for either product were not affected. (saf-1459)


## [1.82.0](https://github.com/returntocorp/semgrep/releases/tag/v1.82.0) - 2024-07-30


### Added


- Added `testsuite/` as a filepath to the default value for `.semgrepignore`. (gh-1876)


### Changed


- Update the library definitions for Java for the latest version of the JDK. (java-library-definitions)


### Fixed


- Fixed metavariable comparison in step mode.

  Used to be that the rule:
  ```yaml
      steps:
          - languages: [python]
            patterns:
              - pattern: x = f($VAR);
          - languages: [generic]
            patterns:
              - pattern-either:
                 - patterns:
                  - pattern: HI $VAR
  ```
  Wouldn't match, as one is an identifier, and the other an expression that has a
  string literal. The fix was chainging the equality used. (saf-1061)


## [1.81.0](https://github.com/returntocorp/semgrep/releases/tag/v1.81.0) - 2024-07-24


### Changed


- The --debug option will now display logging information from the semgrep-core
  binary directly, without waiting that the semgrep-core program finish. (incremental_debug)


### Fixed


- C++: Scanning a project with header files (.h) now no longer causes a
  spurious warnings that the file is being skipped, or not analyzed. (code-6899)
- Semgrep will now be more strict (as it should be) when unifying identifiers.

  Patterns like the one below may not longer work, particularly in Semgrep Pro:

      patterns:
        - pattern-inside: |
            class A:
              ...
              def $F(...):
                ...
              ...
            ...
        - pattern-inside: |
            class B:
              ...
              def $F(...):
                ...
              ...
            ...

  Even if two classes `A` and `B` may both have a method named `foo`, these methods
  are not the same, and their ids are not unifiable via `$F`. The right way of doing
  this in Semgrep is the following:

      patterns:
        - pattern-inside: |
            class A:
              ...
              def $F1(...):
                ...
              ...
            ...
        - pattern-inside: |
            class B:
              ...
              def $F2(...):
                ...
              ...
            ...
        - metavariable-comparison:
            comparison: str($F1) == str($F2)

  We use a different metavariable to match each method, then we check whether they
  have the same name (i.e., same string). (code-7336)
- In the app, you can configure Secrets ignores separately from Code/SSC ignores. However, the
  files that were ignored by Code/SSC and not Secrets were still being scanned during the
  preprocessing stage for interfile analysis. This caused significantly longer scan times than
  expected for some users, since those ignored files can ignore library code. This PR fixes that
  behavior and makes Code/SSC ignores apply as expected. (saf-1087)
- Fixed typo that prevented users from using "--junit-xml-output" flag and added a tests that invokes the flag. (saf-1437)


## [1.80.0](https://github.com/returntocorp/semgrep/releases/tag/v1.80.0) - 2024-07-18


### Added


- OSemgrep now can take `--exclude-minified-files` to skip  minified files. Additionally `--no-exclude-minified-files` will disable this option. It is off by default. (cdx-460)
- Users are now required to login before using semgrep scan --pro.

  Previously, semgrep will tell the users to log in, but the scan will still continue.

  With this change, semgrep will tell the users to log in and stop the scan. (saf-1137)


### Fixed


- The language server no longer scans large or minified files (cdx-460)
- Pro: Improved module resolution for Python. Imports like `from a.b import c` where
  `c` is a module will now be resolved by Semgrep. And, if a module cannot be found
  in the search path, Semgrep will try to heuristically resolve the module by matching
  the module specifier against the files that are being scanned. (code-7069)
- A scan can occasionally freeze when using tracing with multiprocesses.

  This change disables tracing when scanning each target file unless the scan runs in a single process. (saf-1143)
- Improved error handling for rules with invalid patterns. Now, scans will still complete and findings from other rules will be reported. (saf-789)
- The "package-lock.json" parser incorrectly assumed that all paths in the "packages" component of "package-lock.json" started with "node_modules/".

  In reality, a dependency can be installed anywhere, so the parser was made more flexible to recognize alternative locations ("node_modules", "lib", etc). (sc-1576)


## [1.79.0](https://github.com/returntocorp/semgrep/releases/tag/v1.79.0) - 2024-07-10


### Added


- Preliminary support for the Move on Aptos language
  (see https://aptos.dev/move/move-on-aptos for more info on this language).
  Thanks a lot to Zhiping Liao (ArArgon) and Andrea Cappa for their contributions! (move_on_aptos)
- The language server now reports number of autofixes and ignores triggered throught IDE integrations when metrics are enabled (pdx-autofix-ignore)
- Added support for comparing Golang Pseudo-versions. After replacing calls to the
  packaging module with some custom logic, Pseudo-versions can now be compared against
  strict core versions and other pseudo versions accurately. (sc-1601)
- We now perform a git gc as a side-effect of historical scans. (scrt-630)


### Fixed


- tainting: Fixed bug in `--pro-intrafile` that caused Semgrep to confuse a parameter
  with a top-level function with no arguments that happened to have the same name:

      def foo
        taint
      end

      def bar(foo)
        sink(foo) # no more FP here
      end (code-6923)
- Fixed fatal errors on files containing nosemgrep annotation without
  any rule ID after. (nosemgrep_exn)
- Matching explanations: Focus nodes now appear after filter nodes, which is
  the correct order of execution of pattern nodes. Filter nodes are now
  unreversed. (saf-1127)
- Autofix: Previews in the textual CLI output will now join differing lines
  with a space, rather than joining with no whitespace whatsoever. (saf-1135)
- Secrets: resolved some rare instances where historical scans would skip blobs
  depending on the structure of the local copy of the repository (i.e., blobs
  were only skipped if the specific copy of the git store had a certain
  structure). (scrt-630)


## [1.78.0](https://github.com/returntocorp/semgrep/releases/tag/v1.78.0) - 2024-06-27


### Added


- Matching of fully qualified type names in the metavariable-type operator has
  been improved. For example:

  ```
  from a.b import C

  x = C()
  ```

  The type of `x` will match both `a.b.C` and `C`.

  ```
    - pattern: $X = $Y()
    - metavariable-type:
        metavariable: $X
        types:
          - a.b.C  # or C
  ``` (code-7269)


### Fixed


- Symbolic propagation now works on decorator functions, for example:

      x = foo
      @x() # this is now matched by pattern `@foo()`
      def test():
        pass (code-6634)
- Fixed an issue where Python functions with annotations ending in `endpoint`,
  `route`, `get`, `patch`, `post`, `put`, `delete`, `before_request` or
  `after_request` (i.e., ones we associate with Flask) were incorrectly analyzed
  with the Code product in addition to the Secrets product when present in a file
  being ignored for Code analysis but included for Secrets. (scrt-609)


## [1.77.0](https://github.com/returntocorp/semgrep/releases/tag/v1.77.0) - 2024-06-24


### Added


- Semgrep will now report the id of the organization associated with logged in users when reporting metrics in the language server (cdx-508)
- Pro: taint-mode: Improved index-sensitive taint tracking for tuple/list (un)packing.

  Example 1:

       def foo():
           return ("ok", taint)

       def test():
            x, y = foo()
            sink(x)  # nothing, no FP
            sink(y)  # finding

  Example 2:

       def foo(t):
            (x, y) = t
            sink(x)  # nothing, no FP
            sink(y)  # finding

       def test():
            foo(("ok", taint)) (code-6935)
- Adds traces to help debug the performance of tainting. To send the traces added in the PR, pass
  `--trace` and also set the environment variable `SEMGREP_TRACE_LEVEL=trace`. To send them to a
  local endpoint instead of our default endpoint, use `--trace-endpoint`. (saf-1100)


### Fixed


- Fixed a bug in the generation of the control-flow graph for `try` statements that
  could e.g. cause taint to report false positives:

      def test():
          data = taint
          try:
              # Semgrep assumes that `clean` could raise an exception, but
              # even if it does, the tainted `data` will never reach the sink !
              data = clean(data)
          except Exception:
              raise Exception()

          # `data` must be clean here
          sink(data) # no more FP (flow-78)
- The language server (and semgrep --experimental) should not report anymore errors from
  the metrics.semgrep.dev server such as "cannot read property 'map' of undefined". (metrics_error)
- Fixed a bug in the gemfile.lock parser which causes Semgrep to miss direct
  dependencies whose package name does not end in a version constraint. (sc-1568)


## [1.76.0](https://github.com/returntocorp/semgrep/releases/tag/v1.76.0) - 2024-06-17


### Added


- Added type inference support for basic operators in the Pro engine, including
  `+`, `-`, `*`, `/`, `>`, `>=`, `<=`, `<`, `==`, `!=`, and `not`. For numeric
  computation operators such as `+` and `-`, if the left-hand side and right-hand
  side types are equal, the return type is assumed to be the same. Additionally,
  comparison operators like `>` and `==`, as well as the negation operator `not`,
  are assumed to return a boolean type. (code-6940)
- Added guidance for resolving token issues for `install-semgrep-pro` in non-interactive environments. (gh-1668)
- Adds support for a new flag, `--subdir <path>`, for `semgrep ci`, which allows users to pass a
  subdirectory to scan instead of the entire directory. The path should be a relative path, and
  the directory where `semgrep ci` is run should be the root of the repository being scanned.
  Unless `SEMGREP_REPO_DISPLAY_NAME` is explicitly set, passing the subdirectory
  will cause the results to go to a project specific to that subdirectory.

  The intended use case for `semgrep ci --subdir path/to/dir` is to help users with very large
  repos scan the repo in parts. (saf-1056)


### Fixed

- The min-version/max-version rule filtering is now done in pysemgrep too,
  avoiding previous crash when using new fields (or new enums) in a rule.
- Language Server will now send error messages properly, and error handling is greatly improved (cdx-502)
- Pro: Calling a safe method on a tainted object should no longer propagate taint.

  Example:

      class A {
          String foo(String str) {
              return "ok";
          }
      }

      class Test {
          public static void test() {
              A a;
              String s;
              a = taint();
              // Despite `a` is tainted, `a.foo()` is entirely safe !!!
              s = a.foo("bar");
              sink(s); // No more FP here
          }
      } (code-6935)
- Fixing errors in matching identifiers from wildcard imports. For example, this
  update addresses the issue where the following top-level assignment:
  ```
  from pony.orm import *
  db = Database()
  ```
  is not matched with the following pattern:
  ```
  $DB = pony.orm.Database(...)
  ``` (code-7045)
- [Pro Interfile JS/TS] Improve taint propagation through callbacks passed to `$X.map` functions and similar. Previously, such callbacks needed to have a return value for taint to be properly tracked. After this fix, they do not. (js-taint)
- Rust: Constructors will now properly match to only other constructors with
  the same names, in patterns. (saf-1099)


## [1.75.0](https://github.com/returntocorp/semgrep/releases/tag/v1.75.0) - 2024-06-03


### Added


- Pro: Semgrep can now track taint through tuple/list (un)packing intra-procedurally
  (i.e., within a single function). For example:

  ```python
  t = ["ok", "taint"]
  x, y = t
  sink(x) # OK, no finding
  sink(y) # tainted, finding
  ``` (code-6935)
- Optional type matching is supported in the Pro engine for Python. For example,
  in Python, `Optional[str]`, `str | None`, and `Union[str, None]` represent the
  same type but in different type expressions. The optional type match support
  enables matching between these expressions, allowing any optional type
  expression to match any other optional type expression when used with
  metavariable-type filtering. It's important to note that syntactic pattern
  matching still distinguishes between these types. (code-6939)
- Add support for pnpm v9 (pnpm)
- Added a new rule option decorators_order_matters, which allows users to make decorators/ non-keyword attributes matching stricter. The default matching for attributes is order-agnostic, but if this rule option is set to true, non-keyword attributes (e.g. decorators in Python) will be matched in order, while keyword attributes (e.g. static, inline, etc) are not affected.

  An example usage will be a rule to detect any decorator that is outside of the route() decorator in Flask, since any decorator outside of the route() decorator takes no effect.

  # bad: another.func() takes no effect
  @another.func("func")
  @app.route("route")
  def f():
      pass

  # ok: route() is the outermost decorator
  @app.route("route")
  @another.func("func")
  def f():
      pass (saf-435)


### Fixed


- Pro: taint-mode: Fixed issue causing findings to be missed (false negatives)
  when a global or class field was tainted, and then used in a sink after two
  or more function calls.

  For example:

      class Test {
          string bad;

          void test() {
              bad = "taint";
              foo();
          }

          void foo() {
              bar();
          }

          void bar() {
              sink(bad); // finding no longer missed
          }
      } (saf-1059)
- [Mostly applicable to Pro Engine] Typed metavariables will now match against the inferred type of a binding even if a constant is propagated for that binding, if we are unable to infer a type from the constant. Previously, we would simply fail to match in this case. (saf-1060)
- Removed the URLs at the end of the log when semgrep ci --dryrun is ran because dry run doesn't interact with the app so the URLs don't make sense. (saf-924)


## [1.74.0](https://github.com/returntocorp/semgrep/releases/tag/v1.74.0) - 2024-05-23


### Fixed


- One part of interfile tainting was missing a constant propagation phase, which causes semgrep to miss some true positives in some cases during interfile analysis.

  This fix adds the missing constant propagation. (saf-1032)
- Semgrep now matches YAML tags (e.g. `!number` in `!number 42`) correctly rather
  than ignoring them. (saf-1046)
- Upgraded Semgrep's Dockerfile parser. This brings in various
  [fixes from
  tree-sitter-dockerfile](https://github.com/camdencheek/tree-sitter-dockerfile/releases/tag/v0.2.0)
  including minimal support for heredoc templates, support for variables in keys
  of LABEL instructions, support for multiple parameters for ADD and COPY
  instructions, tolerance for blanks after the backslash of a line continuation.
  As a result of supporting variables in LABEL keys, the multiple key/value
  pairs found in LABEL instructions are now treated as if they each had they own
  LABEL instruction. It allows a pattern `LABEL a=b` to match `LABEL a=b c=d`
  without the need for an ellipsis (`LABEL a=b ...`). Another consequence is
  that the pattern `LABEL a=b c=d` can no longer match `LABEL c=d a=b` but it
  will match a `LABEL a=b` instruction immediately followed by a separate
  `LABEL c=d`. (upgrade-dockerfile-parser)


## [1.73.0](https://github.com/returntocorp/semgrep/releases/tag/v1.73.0) - 2024-05-16


### Added


- Added new AWS validator syntax for Secrets (scrt-278)


### Fixed


- Fix `couldn't find metavar $MT in the match results` error, which may occur
  when we capture FQN with the metavariable and use metavariable-type filter on
  it. (code-7042)
- Fixes the crash (during scan) caused by improper handling of unicode characters present in the source code. (gh-8421)
- [Pro Engine Only] Tainted values are now tracked through instantiation of React functional components via JSX. (jsx-taint)


## [1.72.0](https://github.com/returntocorp/semgrep/releases/tag/v1.72.0) - 2024-05-08


### Fixed


- Dockerfile support: Avoid a silent parsing error that was possibly accompanied
  with a segfault when parsing Dockerfiles that lack a trailing newline
  character. (gh-10084)
- Fixed bug that was preventing the use of `metavariable-pattern` with
  the aliengrep engine of the generic mode. (gh-10222)
- Added support for function declarations on object literals in the dataflow analysis.

  For example, previously taint rules would not have matched the
  following javascript code but now would.
  ```
  let tainted = source()
  let o = {
      someFuncDecl(x) {
          sink(tainted)
      }
  }
  ``` (saf-1001)
- Osemgrep only:

  When rules have metavariable-type, they don't show up in the SARIF output. This change fixes that.

  Also right now dataflow traces are always shown in SARIF even when --dataflow-traces is not passed. This change also fixes that. (saf-1020)
- Fixed bug in rule parsing preventing patternless SCA rules from being validated. (saf-1030)


## [1.71.0](https://github.com/returntocorp/semgrep/releases/tag/v1.71.0) - 2024-05-03


### Added


- Pro: const-prop: Previously inter-procedural const-prop could only infer whether
  a function returned an arbitrary string constant. Now it will be able to infer
  whether a function returns a concrete constant value, e.g.:

  ```python
  def bar():
    return "bar"

  def test():
    x = bar()
    foo(x) # now also matches pattern `foo("bar")`, previously only `foo("...")`
  ``` (flow-61)
- Python: const-prop: Semgrep will now recognize "..." * N expression as arbitrary
  constant string literals (thus matching the pattern "..."). (flow-75)


### Changed


- The `--beta-testing-secrets-enabled` option, deprecated for several months, is now removed. Use `--secrets` as its replacement. (gh-9987)


### Fixed


- When using semgrep --test --json, we now report in the
  config_missing_fixtests field in the JSON output not just rule files
  containing a `fix:` without a corresponding ".fixed" test file; we now also
  report rule files using a `fix-regex:` but without a corresponding a
  .fixed test file, and the `fix:` or `fix-regex:` can be in
  any rule in the file (not just the first rule). (fixtest)
- Fixes matching for go struct field tags metadata.

  For example given the program:
  ```
  type Rectangle struct {
      Top    int `json:"top"`
      Left   int `json:"left"`
      Width  int `json:"width"`
      Height int `json:"height"`
  }
  ```
  The pattern,
  ```
  type Rectangle struct {
      ...
      $NAME $TYPE $TAGS
      ...
  }
  ```
  will now match each field and the `$TAGS` metavariable will be
  bound when used in susequent patterns. (saf-949)
- Matching: Patterns of statements ending in ellipsis metavariables, such as
  ```
  x = 1
  $...STMTS
  ```
  will now properly extend the match range to accommodate whatever is captured by
  the ellipsis metavariable ($...STMTS). (saf-961)
- The SARIF output format should have the tag "security" when the "cwe"
  section is present in the rule. Moreover, duplicate tags should be
  de-duped.

  Osemgrep wasn't doing this before, but with this fix, now it does. (saf-991)
- Fixed bug in mix.lock parser where it was possible to fail on a python None error. Added handler for arbitrary exceptions during lockfile parsing. (sc-1466)
- Moved `--historical-secrets` to the "Pro Engine" option group, instead of
  "Output formats", where it was previously (in error). (scrt-570)


## [1.70.0](https://github.com/returntocorp/semgrep/releases/tag/v1.70.0) - 2024-04-24


### Added


- Added guidance for resolving API token issues in CI environments. (gh-10133)
- The osemgrep show command supports 2 new options: `dump-ast` `dump-pattern`.
  See `osemgrep show --help` for more information. (osemgrep_show)
- Added additional output flags which allow you to write output to multiple files in multiple formats.

  For example, the comand `semgrep ci --text --json-output=result.json --sarif-output=result.sarif.json`
  Displays text output on stdout, writes the output that would be generated by passing the `--json` flag
  to `result.json`, and writes the output that would be generated by passing the `--sarif` to `result.sarif.json`. (saf-341)
- Added an experimental feature for users to use osemgrep to format
  SARIF output.

  When both the flags --sarif and --use-osemgrep-sarif are specified,
  semgrep will use the ocaml implementation to format SARIF.

  This flag is experimental and can be removed any time. Users must not
  rely on it being available. (saf-978)


### Changed


- The main regex engine is now PCRE2 (was PCRE). While the syntax is mostly
  compatible, there are some minor instances where updates to rules may be
  needed, since PCRE2 is slightly more strict in some cases. For example, while
  we previously accepted `[\w-.]`, such a pattern would now need to be written
  `[\w.-]` or `[\w\-.]` since PCRE2 rejects the first as having an invalid range. (scrt-467)


### Fixed


- Semgrep LS now waits longer for users to login (gh-10109)
- When semgrep ci finishes scanning and uploads findings, it tells the
  app to mark the scan as completed.

  For large findings, this may take a while and marking the scan as
  completed may timeout. When a scan is not marked as completed, the app
  may show that the repo is still processing, and confuses the user.

  This change increases the timeout (previously 20 minutes) to 30
  minutes. (saf-980)
- Fix `semgrep ci --oss-only` when secrets product is enabled. (scrt-223)


## [1.69.0](https://github.com/returntocorp/semgrep/releases/tag/v1.69.0) - 2024-04-16


### Added


- Tracing: remove support for SEMGREP_OTEL_ENDPOINT and replace with
  `--trace-endpoint <url>`.
  This change is for an internal feature for debugging performance. (saf-885)


### Changed


- Passing --debug to Semgrep should now print less logs. We do
  not want --debug's output to be enormous, as it tends not to be useful and yet
  cause some problems. Note that --debug is mainly intended for Semgrep developers,
  please ask for help if needed. (gh-10044)


### Fixed


- In generic mode (default, spacegrep engine), matching a pattern that
  ends with an ellipsis now favors the longest match rather than the shortest
  match when multiple matches are possible. For example, for a given target
  program `a a b`, the pattern `a ... b` will match `a b` as before but
  the pattern `a ...` will now match the longer `a a b` rather than `a b`. (gh-10039)
- Fixed the inter-file diff scan issue where the removal of pre-existing findings
  didn't work properly when adding a new file or renaming an existing file. (saf-897)


## [1.68.0](https://github.com/returntocorp/semgrep/releases/tag/v1.68.0) - 2024-04-08


### Added


- Scan un-changed lockfiles in diff-aware scans (gh-9899)
- Languages: Added the QL language (used by CodeQL) to Semgrep (saf-947)
- SwiftPM parser will now report package url and reference. (sc-1218)
- Add support for Elixir (Mix) SCA parsing for pro engine users. (sc-1303)


### Fixed


- Output for sarif format includes dataflow traces. (gh-10004)
- The environment variable `LOG_LEVEL` (as well as `PYTEST_LOG_LEVEL`) is
  no longer consulted by Semgrep to determine the log level. Only
  `SEMGREP_LOG_LEVEL` is consulted. `PYTEST_SEMGREP_LOG_LEVEL` is also
  consulted in the current implementation but should not be used outside of
  Semgrep's Pytest tests. This is to avoid accidentally affecting Semgrep
  when inheriting the `LOG_LEVEL` destined to another application. (gh-10044)
- Fixed swiftpm parser to no longer limit the amount of found packages in manifest file. (sc-1364)
- Fixed incorrect ecosystem being used for Elixir. Hex should be used instead of Mix. (sc-elixir)
- Fixed the match_based_ids of lockfile-only findings to differentiate between findings in cases where one rule produces multiple findings in one lockfile (sca-mid)
- Secrets historical scans: fixed a bug where historical scans could run on differential scans. (scrt-545)


## [1.67.0](https://github.com/returntocorp/semgrep/releases/tag/v1.67.0) - 2024-03-28


### Added


- `--historical-secrets` flag for running Semgrep Secrets regex rules on git
  history (requires Semgrep Secrets). This flag is not yet implemented for
  `--experimental`. (scrt-531)


### Changed


- Files with the `.phtml` extension are now treated as PHP files. (gh-10009)
- [IMPORTANT] Logged in users running `semgrep ci` will now run the pro engine by default! All `semgrep ci` scans will run with our proprietary languages (Apex and Elixir), as well as cross-function taint within a single file, and other single file pro optimizations we have developed. This is equivalent to `semgrep ci --pro-intrafile`. Users will likely see improved results if they are running `semgrep ci` and did not already have additional configuration to enable pro analysis.

  The current default engine does not include cross-file analysis. To scan with cross-file analysis, turn on the app toggle or pass in the flag `--pro`. We recommend this unless you have very large repos (talk to our support to get help enabling cross-file analysis on monorepos!)

  To revert back to our OSS analysis, pass the flag `--oss-only` (or use `--pro-languages` to continue to receive our proprietary languages).

  Reminder: because we release first to our canary image, this change will only immediately affect you if you are using `semgrep/semgrep:canary`. If you are using `semgrep/semgrep:latest`, it will affect you when we bump canary to latest. (saf-845)


### Fixed


- Fixed a parsing error in Kotlin when there's a newline between the class name and the primary constructor.

  This could not parse before

  ```
  class C
  constructor(arg:Int){}
  ```

  because of the newline between the class name and the constructor.

  Now it's fixed. (saf-899)


## [1.66.2](https://github.com/returntocorp/semgrep/releases/tag/v1.66.2) - 2024-03-26


### Added


- osemgrep now respects HTTP_PROXY and HTTPS_PROXY when making network requests (cdx-253)


### Changed


- [IMPORTANT] The public rollout of inter-file differential scanning has been
  temporarily reverted for further polishing of the feature. We will reintroduce
  it in a later version. (saf-268)


### Fixed


- Autofix on variable definitions should now handle the semicolon
  in Java, C++, and C#. (saf-928)


## [1.66.1](https://github.com/returntocorp/semgrep/releases/tag/v1.66.1) - 2024-03-25


### Fixed


- Autofix on variable definitions should now handle the semicolon
  in Rust, Cairo, Solidity, Dart. (autofix_vardef)
- [IMPORTANT] we restored bash, jq, and curl in our semgrep docker image as some
  users were relying on it. We might remove them in the futur but in the
  mean time we restored the packages and if we remove them we will announce
  it more loudly. We also created a new page giving more information
  about our policy for our docker images:
  https://semgrep.dev/docs/semgrep-ci/packages-in-semgrep-docker/ (docker_bash)
- Fixed autofix application on lines containing multi-byte characters. (multibyte)


## [1.66.0](https://github.com/returntocorp/semgrep/releases/tag/v1.66.0) - 2024-03-19


### Added


- Added information about interfile pre-processing to --max-memory help. (gh-9932)
- We've implemented basic support for the `yield` keyword in Python. The Pro
  engine now detects taint findings from taint sources returned by the yield
  keyword. (saf-281)


### Changed


- osemgrep --remote will no longer clone into a tmp folder, but instead the CWD (cdx-remote)
- [IMPORTANT] Inter-file differential scanning is now enabled for all Pro users.

  Inter-file differential scanning is now enabled for all Pro users. While it may
  take longer than intra-file differential scanning, which is the current default
  for pro users, it offers deeper analysis of dataflow paths compared to
  intra-file differential scanning. Additionally, it is significantly faster
  than non-differential inter-file scanning, with scan times reduced to
  approximately 1/10 of the non-differential inter-file scan. Users who
  enable the pro engine and engage in differential PR scans on GitHub or
  GitLab may experience the impact of this update. If needed, users can
  revert to the previous intra-file differential scan behavior by configuring
  the `--no-interfile-diff-scan` command-line option. (saf-268)


### Fixed


- The official semgrep docker image does not contain anymore the
  bash, jq, and curl utilities, to reduce its attack surface. (saf-861)


## [1.65.0](https://github.com/returntocorp/semgrep/releases/tag/v1.65.0) - 2024-03-11


### Changed


- Removed the extract-mode rules experimental feature. (extract_mode)


## [1.64.0](https://github.com/returntocorp/semgrep/releases/tag/v1.64.0) - 2024-03-07


### Changed


- Removed the AST caching experimental feature (--experimental --ast-caching
  in osemgrep and -parsing_cache_dir in semgrep-core). (ast_caching)
- Removed the Registry caching experimental feature (--experimental --registry-caching)
  in osemgrep. (registry_caching)


### Fixed


- Clean any credentials from project URL before using it, to prevent leakage. (saf-876)
- `ci`: Updated logic for informational message printed when no rules are sent to
  correctly display when secrets is enabled (in additional to
  when code is). (scrt-455)


## [1.63.0](https://github.com/returntocorp/semgrep/releases/tag/v1.63.0) - 2024-02-27


### Added


- Dataflow: Added support for nested record patterns such as `{ body: { param } }`
  in the LHS of an assignment. Now given `{ body: { param } } = tainted` Semgrep
  will correctly mark `param` as tainted. (flow-68)
- Matching: `metavariable-regex` can now match on metavariables of interpolated
  strings which use variables that have known values. (saf-865)
- Add support for parsing Swift Package Manager manifest and lockfiles (sc-1217)


### Fixed


- fix: taint signatures do not capture changes to parameters' fields (flow-70)
- Scan summary links printed after `semgrep ci` scans now reflect a custom SEMGREP_APP_URL, if one is set. (saf-353)


## [1.62.0](https://github.com/returntocorp/semgrep/releases/tag/v1.62.0) - 2024-02-22


### Added


- Pro: Adds support for python constructors to taint analysis.

  If interfile naming resolves that a python constructor is called taint
  will now track these objects with less heuristics. Without interfile
  analysis these changes have no effect on the behavior of tainting.
  The overall result is that in the following program the oss analysis
  would match both calls to sink while the interfile analysis would only
  match the second call to sink.

  ```
  class A:
      untainted = "not"
      tainted = "not"
      def __init__(self, x):
      	self.tainted = x

  a = A("tainted")
  # OK:
  sink(a.untainted)
  # MATCH:
  sink(a.tainted)
  ``` (ea-272)
- Pro: taint-mode: Added basic support for "index sensitivity", that is,
  Semgrep will track taint on individual indexes of a data structure when
  these are constant values (integers or strings), and the code uses the
  built-in syntax for array indexing in the corresponding language
  (typically `E[i]`). For example, in the Python code below Semgrep Pro
  will _not_ report a finding on `sink(x)` or `sink(x[1])` because it will
  know that only `x[42]` is tainted:

  ```python
  x[1] = safe
  x[42] = source()
  sink(x) // no more finding
  sink(x[1]) // no more finding
  sink(x[42]) // finding
  sink(x[i]) // finding
  ```

  There is still a finding for `sink(x[i])` when `i` is not constant. (flow-7)


### Changed


- taint-mode: Added `exact: false` sinks so that one can specify that _anything_
  inside a code region is a sink, e.g. `if (...) { ... }`. This used to be the
  semantics of sink specifications until Semgrep 1.1.0, when we made sink matching
  more precise by default. Now we allow reverting to the old semantics.

  In addition, when `exact: true` (the default), we simplified the heuristic used
  to support traditional `sink(...)`-like specs together with the option
  `taint_assume_safe_functions: true`, now we will consider that if the spec
  formula is not a `patterns` with a `focus-metavarible`, then we must look for
  taint in the arguments of a function call. (flow-1)
- The project name for repos scanned locally will now be `local_scan/<repo_name>` instead
  of simply `<repo_name>`. This will clarify the origin of those findings. Also, the
  "View Results" URL displayed for findings now includes the repository and branch names. (saf-856)


### Fixed


- taint-mode: experimental: For now Semgrep CLI taint traces are not adapted to
  support multiple labels, so Semgrep picks one arbitrary label to report, which
  sometimes it's not the desired one. As a temporary workaround, Semgrep will
  look at the `requires` of the sink, and if it has the shape `A and ...`, then
  it will pick `A` as the preferred label and report its trace. (flow-65)
- Fixed trailing newline parsing in pyproject.toml and poetry.lock files. (gh-9777)
- Fixed an issue that led to incorrect autofix application in certain cases where multiple fixes were applied to the same line. (saf-863)
- The tokens for type parameters brackets are now stored in the generic AST allowing
  to correctly autofix those constructs. (tparams)


## [1.61.1](https://github.com/returntocorp/semgrep/releases/tag/v1.61.1) - 2024-02-14


### Added


- Added performance metrics using OpenTelemetry for better visualization.
  Users wishing to understand the performance of their Semgrep scans or
  to help optimize Semgrep can configure the backend collector created in
  `libs/tracing/unix/Tracing.ml`.

  This is experimental and both the implementation and flags are likely to
  change. (ea-320)
- Created a new environment variable SEMGREP_REPO_DISPLAY_NAME for use in semgrep CI.
  Currently, this does nothing. The goal is to provide a way to override the display
  name of a repo in the Semgrep App. (gh-8953)
- The OCaml/C executable (`semgrep-core` or `osemgrep`) is now passed through
  the `strip` utility, which reduces its size by 10-25% depending on the
  platform. Contribution by Filipe Pina (@fopina). (gh-9471)


### Changed


- "Missing plugin" errors (i.e., rules that cannot be run without `--pro`) will now
  be grouped and reported as a single warning. (ea-842)


## [1.60.1](https://github.com/returntocorp/semgrep/releases/tag/v1.60.1) - 2024-02-09


### Added


- Rule syntax: Metavariables by the name of `$_` are now _anonymous_, meaning that
  they do not unify within a single pattern or across patterns, and essentially
  just unconditionally specify some expression.

  For instance, the pattern `foo($_, $_)` may match the code `foo(1, 2)`.

  This will change the behavior of existing rules that use the metavariable
  `$_`, if they rely on unification still happening. This can be fixed by simply
  giving the metavariable a real name like `$A`. (ea-837)
- Added infrastructure for semgrep supply chain in semgrep-core. Not fully functional yet. (ssc-port)


### Changed


- Dataflow: Simplified the IL translation for Python `with` statements to let
  symbolic propagation assume that `with foo() as x: ...` entails `x = foo()`,
  so that e.g. `Session().execute("...")` matches:

      with Session() as s:
          s.execute("SELECT * from T") (CODE-6633)


### Fixed


- Output: Semgrep CLI now no longer sometimes interpolated metavariables twice, if
  the message that was substituted for a metavariable itself contained a valid
  metavariable to be interpolated (ea-838)


## [1.59.1](https://github.com/returntocorp/semgrep/releases/tag/v1.59.1) - 2024-02-02


### Added


- taint-mode: Pro: Semgrep can now track taint via static class fields and global
  variables, such as in the following example:

  ```c
  static char* x;

  void foo() {
      x = "tainted";
  }

  void bar() {
      sink(x);
  }

  void main() {
      foo();
      bar();
  }
  ``` (pa-3378)


### Fixed


- Pro: Make inter-file analysis more tolerant to small bugs, resorting to graceful
  degradation and continuing with the scan, rather than crashing. (pa-3387)


## [1.59.0](https://github.com/returntocorp/semgrep/releases/tag/v1.59.0) - 2024-01-30


### Added


- Swift: Now supports typed metavariables, such as `($X : ty)`. (pa-3370)


### Changed


- Add Elixir to Pro languages list in help information. (gh-9609)
- Removed `sg` alias to avoid naming conflicts
  with the shadow-utils `sg` command for Linux systems. (gh-9642)
- Prevent unnecessary computation when running scans without verbose logging enabled (gh-9661)
- Deprecated option `taint_match_on` introduced in 1.51.0, it is being renamed
  to `taint_focus_on`. Note that `taint_match_on` was experimental, and
  `taint_focus_on` is experimental too. Option `taint_match_on` will continue
  to work but it will be completely removed at some point after 1.63.0. (pa-3272)
- Added information on product-related flags to help output, especially for Semgrep Secrets. (pa-3383)
- taint-mode: Improve inference of best matches for exact-sources, exact-sanitizers,
  and sinks. Now we also avoid FPs in cases such as:

      dangerouslySetInnerHTML = {
        // ok:
        {__html: props ? DOMPurify.sanitize(props.text) : ''} // no more FPs!
      }

  where `props` is tainted and the sink specification is:

      patterns:
        - pattern: |
           dangerouslySetInnerHTML={{__html: $X}}
        - focus-metavariable: $X

  Previously Semgrep wrongly considered the individual subexpressions of the
  conditional as sinks, including the `props` in `props ? ...`, thus producing a
  false positive. Now it will only consider the conditional expression as a whole
  as the sink. (rules-6457)
- Removed an internal legacy syntax for secrets rules (`mode: semgrep_internal_postprocessor`). (scrt-320)


### Fixed


- Autofix: Fixes that span multiple lines will now try to align
  inserted fixed lines with each other. (gh-3070)
- Matching: Try blocks with catch clauses can now match try blocks that have
  extraneous catch clauses, as long as it matches a subset. For instance,
  the pattern
  ```
  try:
    ...
  catch A:
    ...
  ```
  can now match
  ```
  try:
    ...
  catch A:
    ...
  catch B:
    ...
  ``` (gh-3362)
- Previously, some people got the error:

  ```
  Encountered error when running rules: Other syntax error at line NO FILE INFO YET:-1:
  Invalid_argument: String.sub / Bytes.sub
  ```

  Semgrep should now report this error properly with a file name and line number and
  handle it gracefully. (gh-9628)
- Fixed Dockerfile parsing bug where multiline comments were parsed incorrectly. (gh-9628-2)
- The language server will now properly respect findings that have been ignored via the app (lsp-fingerprints)
- taint-mode: Pro: Semgrep will now propagate taint via instance variables when
  calling methods within the same class, making this example work:

  ```java
  class Test {

    private String str;

    public setStr() {
      this.str = "tainted";
    }

    public useStr() {
      //ruleid: test
      sink(this.str);
    }

    public test() {
      setStr();
      useStr();
    }

  }
  ``` (pa-3372)
- taint-mode: Pro: Taint traces will now reflect when taint is propagated via
  class fields, such as in this example:

  ```java
  class Test {

    private String str;

    public setStr() {
      this.str = "tainted";
    }

    public useStr() {
      //ruleid: test
      sink(this.str);
    }

    public test() {
      setStr();
      useStr();
    }

  }
  ```

  Previously Semgrep will report that taint originated at `this.str = "tainted"`,
  but it would not tell you how the control flow got there. Now the taint trace
  will indicate that we get there by calling `setStr()` inside `test()`. (pa-3373)
- Addressed an issue related to matching top-level identifiers with meta-variable
  qualified patterns in C++, such as matching ::foo with ::$A::$B.  This problem
  was specific to Pro Engine-enabled scans. (pa-3375)


## [1.58.0](https://github.com/returntocorp/semgrep/releases/tag/v1.58.0) - 2024-01-23


### Added


- Added a severity icon (e.g. "❯❯❱") and corresponding color to our CLI text output
  for findings of known severity. (grow-97)
- Naming has better support for if statements. In particular, for
  languages with block scope, shadowed variables inside if-else blocks
  that are tainted won't "leak" outside of those blocks.

  This helps with features related to naming, such as tainting.

  For example, previously in Go, the x in sink(x) will report
  that x is tainted, even though the x that is tainted is the
  one inside the scope of the if block.

  ```go
  func f() {
    x := "safe";
    if (c) {
      x := "tainted";
    }
    // x should not be tainted
    sink(x);
  }
  ```

  This is now fixed. (pa-3185)
- OSemgrep can now scan remote git repositories. Pass `--experimental --pro --remote http[s]://<website>/.../<repo>.git` to use this feature (pa-remote)


### Changed


- Rules stored under an "hidden" directory (e.g., dir/.hidden/myrule.yml)
  are now processed when using --config <dir>.
  We used to skip dot files under dir, but keeping rules/.semgrep.yml,
  but not path/.github/foo.yml, but keeping src/.semgrep/bad_pattern.yml
  but not ./.pre-commit-config.yaml, ... This was mainly because
  we used to fetch rules from ~/.semgrep/ implicitely when --config
  was not given, but this feature was removed, so now we can keep it simple. (hidden_rules)
- Removed support for writing rules using jsonnet. This feature
  will be restored once we finish the port to OCaml of the semgrep CLI. (jsonnet)
- The primitive object construct expression will no longer match the new
  expression pattern. For example, the pattern `new $TYPE` will now only match
  `new int`, not `int()`. (pa-3336)
- The placement new expression will no longer match the new expression without
  placement. For instance, the pattern `new ($STORAGE) $TYPE` will now only match
  `new (storage) int` and not `new int`. (pa-3338)


### Fixed


- Java: You can now use metavariable ellipses properly in
  function arguments, as statements, and as expressions.

  For instance, you may write the pattern
  ```
  public $F($...ARGS) { ... }
  ``` (gh-9260)
- Nosemgrep: Fixed a bug where Semgrep would err upon reading a `nosemgrep`
  comment with multiple rule IDs. (gh-9463)
- Fixed bugs in gitignore/semgrepignore globbing implementation affecting `--experimental`. (gh-9544)
- Fixed rule IDs, descriptions, findings, and autofix text not wrapping as expected.
  Use newline instead of horiziontal separator for findings with a shared file
  but for different rules per design spec. (grow-97)
- Keep track of the origin of `return;` statements in the dataflow IL so that
  recently added (Pro-only) `at-exit: true` sinks work properly on them. (pa-3337)
- C++: Improve translation of `delete` expressions to the dataflow IL so that
  recently added (Pro-only) `at-exit: true` sinks work on them. Previously
  `delete` expression at "exit" positions were not being properly recognized
  as such. (pa-3339)
- cli: fix python runtime error with 0 width wrapped printing (pa-3366)
- Fixed a bug where Gemfile.lock files with multiple GEM sections
  would not be parsed correctly. (sc-1230)


## [1.56.0](https://github.com/returntocorp/semgrep/releases/tag/v1.56.0) - 2024-01-10


### Added


- Added a new field that breaks down the number of findings per product
  in the metrics that are sent out by the CLI. This will help Semgrep
  understand users better. (pa-3312)


## [1.55.2](https://github.com/returntocorp/semgrep/releases/tag/v1.55.2) - 2024-01-05


### Fixed


- taint-mode: Semgrep was missing some sources occurring inside type expressions,
  for example:

  ```cpp
  char *p = new char[source(x)];
  sink(x);
  ```

  Now, if `x` is tainted by side-effect, Semgrep will check `x` inside the type
  expression `char[...]` and record it as tainting, and generate a finding for
  `sink(x)`. (pa-3313)
- taint-mode: C/C++: Sanitization by side-effect was not working correctly for
  `ptr->fld` l-values. In particular, if `ptr` is tainted, and then `ptr->fld` is
  sanitized, Semgrep will now correctly consider `ptr->fld` as clean. (pa-3328)


## [1.55.1](https://github.com/returntocorp/semgrep/releases/tag/v1.55.1) - 2024-01-04


### Fixed


- Honor temporary folder specified via the TMPDIR environment variable (or
  equivalent on Windows) in some instances where it used to be hardcoded as
  `/tmp`. (gh-9534)
- Fix pipfile manifest parser error (sc-1084)


## [1.55.0](https://github.com/returntocorp/semgrep/releases/tag/v1.55.0) - 2024-01-02


### Changed


- The rule option `commutative_compop` has been renamed to `symmetric_eq`. It is
  deprecated and will be removed after the 1.60.0 release. (gh-9496)


## [1.54.3](https://github.com/returntocorp/semgrep/releases/tag/v1.54.3) - 2023-12-22


### Added


- Pro only: taint-mode: Added experimental `at-exit: true` option for sinks, that
  makes a sink spec only apply on the "exit" instructions/statements of a function.
  That is, the instructions after which the control-flow exits the function. This is
  useful for writing rules to find "leaks", such as checking that file descriptors
  are being closed within the same function where they were opened.

  For example, given this taint rule:

  ```yaml
  pattern-sources:
    - by-side-effect: true
      patterns:
        - pattern: $FILE = open(...)
        - focus-metavariable: $FILE
  pattern-sanitizers:
    - by-side-effect: true
      patterns:
        - pattern: $FILE.close(...)
        - focus-metavariable: $FILE
  pattern-sinks:
    - at-exit: true
      pattern: |
        def $FUN(...):
          ...
  ```

  Semgrep will report a finding in the code below since at `print(content)`, after
  which the control flow reaches the exit of the function, the `file` has not yet
  been closed:

  ```python
  def test():
      file = open("test.txt")
      content = file.read()
      print(content) # FINDING
  ``` (pa-3266)


## [1.54.2](https://github.com/returntocorp/semgrep/releases/tag/v1.54.2) - 2023-12-21


### Added


- metrics: added more granular information about pro engine configurations to
  help differentiate scans using different engine capabilities. For instance,
  maintainers are now able to distinguish intraprocedural scans without secrets
  validation from intraprocedural scans with secrets validation. This allows us
  to have a better understanding of usage and more accurately identify
  product-specific issues (e.g., to see if something only affects secrets scans). (ea-297)


### Fixed


- Revise error message when running `semgrep ci` without being logged in to clarify that `--config` is used with `semgrep scan`. (gh-9485)


## [1.54.1](https://github.com/returntocorp/semgrep/releases/tag/v1.54.1) - 2023-12-20


No significant changes.


## [1.54.0](https://github.com/returntocorp/semgrep/releases/tag/v1.54.0) - 2023-12-19


### Added


- Pro only: taint-mode: In a function/method call, it is now possible to arbitrarily
  propagate taint between arguments and the callee. For example in C, one can
  propagate taint from the second argument of `strcat` to the first, that is,
  `strcat($TO, $FROM)`. Another example, in C++ one can propagate taint from the
  left operand of `>>` to the right one, that is, `$FROM >> $TO`. (pa-3131)
- Semgrep IDE integrations will now cache workspace targets, so a full traversal of a workspace is no longer needed on every scan (pdx-148)


### Changed


- OCaml: switch to using the tree-sitter based parser instead of
  the menhir parser, which has a more complete AST, especially
  for objects and classes. (ocaml)


### Fixed


- solidity: support ellipsis in for loops header in the init part. (gh-9431)
- taint-mode: Fixed recently added `by-side-effect: only` option for taint sources,
  so that it does not incorrectly taint expressions that are not l-values, e.g.
  given this taint source:

  ```yaml
  pattern-sources:
    - by-side-effect: only
      patterns:
        - pattern: delete $VAR;
        - focus-metavariable: $VAR
  ```

  The `get(*from)` expression should not become tainted since it's not an l-value:

  ```cpp
  delete get(*from);
  ``` (pa-2980)
- In C++, the string literal now has a type of `char *`. It won't match with the
  `string` type. For instance,

  ```yaml
  - metavariable-type:
      metavariable: $EXPR
      type: string
  ```

  will only match

  ```cpp
  string f;
  // MATCH
  int x = f.length();
  ```

  but not

  ```cpp
  const char *s;
  // OK
  s = "foo";
  ``` (pa-3236)
- taint-mode: Semgrep will now treat lambdas' parameters as fresh, so a taint rule
  that finds double-delete's should not be triggered on the code below:

  ```cpp
  for (ListNode *node : list) {
  	list.erase(node, [](ListNode *p) {
  		delete p;
  	});
  }
  ``` (pa-3298)
- Fixed bug where empty tables in pyproject.toml files would fail to parse (sc-1196)


## [1.53.0](https://github.com/returntocorp/semgrep/releases/tag/v1.53.0) - 2023-12-12


### Added


- Users can now ignore findings locally in Semgrep IDE Extensions, per workspace, and this will persist between restarts (pdx-154)
- A new subcommand 'semgrep test', which is an alias for 'semgrep scan
  --test'. This means that if you were running semgrep on a test
  directory, you will now have to use 'semgrep scan test' otherwise it
  will be interpreted as the new 'semgrep test' subcommand. (subcommand_test)


### Changed


- Handling qualified identifiers in constant propagation

  We've added support for qualified identifiers in constant propagation. Notably,
  this enables the following matches (with the pro engine):

  ```yaml
  rules:
    - id: cpp-const-field
      languages:
        - cpp
      message: testing
      severity: INFO
      pattern: std::cout<<1
  ```

  ```cpp
  #include<iostream>
  #include "a.h"

  namespace B {
  class Bar {
      public:
          static const int one = 1;
  };
  }

  int main() {
      // ruleid: cpp-const-field
      std::cout<<1<<std::endl;

      // ruleid: cpp-const-field
      std::cout<<A::Foo::one<<std::endl;

      // ruleid: cpp-const-field
      std::cout<<B::Bar::one<<std::endl;
  }
  ``` (gh-9354)


### Fixed


- Updated the parser used for Rust. The largest change relates to how macros are
  parsed. (rust)


## [1.52.0](https://github.com/returntocorp/semgrep/releases/tag/v1.52.0) - 2023-12-05

### Added

- Java: Semgrep will now recognize `String.format(...)` expressions as constant
  strings when all their arguments are constant, but it will still not know
  what exact string it is. For example, code `String.format("Abc %s", "123")`
  will match pattern `"..."` but it will _not_ match pattern `"Abc 123"`. (pa-3284)

### Changed

- Inter-file diff scan will be gradually introduced to a small percentage of
  users through a slow rollout process. Users who enable the pro engine and
  engage in differential PR scans on Github or Gitlab may experience the impact
  of this update. (ea-268)
- secrets: now performs more aggressive deduplication for instances where an
  invalid and valid match are reported at the same range. Instead of reporting
  both, we now report only the valid match when they are otherwise visually
  identical. (scrt-271)

### Fixed

- In expression-based languages, definitions are also expressions.

  This change allows dataflow to properly handle definition expressions.

  For example, the pattern `0 == 0` will match `x == 0` in

  ```elixir
  def f(c) do
    x = (y = 0)
    x == 0
  end
  ```

  because now dataflow is able to handle the expression `y = 0`. (pa-3262)

- In version 1.14.0 (pa-2477) we made sink-matching more precise when the sink
  specification was like:

  ```yaml
  pattern-sinks:
    - patterns:
        - pattern: sink($X, ...)
        - focus-metavariable: $X
  ```

  Where the sink specification most likely has the intent to specify the first
  argument of `sink` as a sink, and `sink(ok1 if tainted else ok2)` should _NOT_
  produce a finding, because `tainted` is not really what is being passed to
  the `sink` function.

  But we only intercepted the most simple pattern above, and more complex sink
  specifications that had the same intent were not properly recognized.

  Now we have generalized that pattern to cover more complex cases like:

  ````yaml
  patterns:
   - pattern-either:
     - patterns:
       - pattern-inside: |
           def foo(...):
             ...
       - pattern: sink1($X)
     - patterns:
       - pattern: sink2($X)
       - pattern-not: bar(...)
   - focus-metavariable: $X
  ``` (pa-3284)
  ````

- Updated the parser used for Rust (rust)

## [1.51.0](https://github.com/returntocorp/semgrep/releases/tag/v1.51.0) - 2023-11-29

### Added

- taint-mode: Added experimental rule option `taint_match_on: source` that makes
  Semgrep report taint findings on the taint source rather than on the sink. (pa-3272)

### Changed

- Elixir got moved to Pro. (elixir_pro)
- The 'fix_regex' field has been removed from the semgrep JSON output. Instead,
  the 'fix' field contains the content the result of the fix_regex. (fix_regex)
- taint-mode: Tweaked experimental option `taint_only_propagate_through_assignments`
  so that when it is enabled, `tainted.field` and `tainted(args)` will no longer
  propagate taint. (pa-2193)

### Fixed

- Fixed Kotlin parse error.

  Previously, code like this would throw a parse error

  ```
  fun f1(context : Context) {
      Foo(context).elem = var1
  }
  ```

  due to not recognizing `Foo(context).elem = ...` as valid.
  Now calls are recognized as valid in the left hand of
  assignments. (ea-104)

- Python: `async` statements are now translated into the Dataflow IL so Semgrep
  will be able to report findings e.g. inside `async with ...` statements. (gh-9182)
- In gitlab output, use correct url attached to rule instead of generating it.
  This fixes url for supply chain findings. (gitlab)
- - The language server will no longer crash on startup for intellij (language-server)
- - The language server no longer crashes when installed through pip on Mac platforms (language-server-macos)
- taint-mode: When we encountered an assignment `lval := expr` where `expr` returned
  no taints, we automatically cleaned `lval`. This was correct in the early days of
  taint-mode, before we introduced taint by side-effect, but it is wrong now. The LHS
  `lval` may be tainted by side-effect, in which case we cannot clean it just because
  `expr` returns no taint. Now that we introduced `by-side-effect: only` it is also
  possible for `expr` to taint `lval` by side-effect and return no immediate taint.

  This kind of source should now work as expected:

  ````yaml
  - by-side-effect: true
    patterns:
      - pattern: |
          $X = source()
      - focus-metavariable: $X
  ``` (pa-3164)
  ````

- taint-mode: Fixed a bug in the recently added `by-side-effect: only` option
  causing that when matching l-values of the form `l.x` and `l[i]`, the `l`
  occurence would unexpectedly become tainted too. This led to FPs in some
  typestate rules like those checking for double-lock or double-free.

  Now a source such as:

  ```yaml
  - by-side-effect: only
    patterns:
      - pattern: lock($L)
      - focus-metavariable: $L
  ```

  will not produce FPs on code such as:

  ````python
  lock(obj.l)
  unlock(obj.l)
  lock(obj.l)
  ``` (pa-3282)
  ````

- taint-mode: Removed a hack that made `lval = new ...` assignments to not clean
  the `lval` despite the RHS was not tainted. This caused FPs in double-free rules.
  For example, given this source:

  ```yaml
  pattern-sources:
    - by-side-effect: only
      patterns:
        - pattern: delete $VAR;
        - focus-metavariable: $VAR
  ```

  And the code below:

  ```cpp
  while (nondet) {
    int *v = new int;
    delete v; // FP
  }
  ```

  The `delete v` statement was reported as a double-free, because Semgrep did not
  consider that `v = new int` would clean the taint in `v`. (pa-3283)

## [1.50.0](https://github.com/returntocorp/semgrep/releases/tag/v1.50.0) - 2023-11-17

No significant changes.

## [1.49.0](https://github.com/returntocorp/semgrep/releases/tag/v1.49.0) - 2023-11-15

### Added

- Added support in Ruby, Julia, and Rust to match implicit return statement inside functions.

  For example:

  ```julia
  return 0
  ```

  can now match 0 in

  ```julia
  function f()
    0
  end
  ```

  This matching is enabled by default and can be disabled with the rule option `implicit_return`. (gh-8408)

- Pro engine supports constant propagation of numbers defined via macro in C++ (gh-9221)
- taint-mode: The `by-side-effect` option for taint sources (only) now accepts a
  third value `only` (besides `true` and `false`). Setting `by-side-effect: only`
  will define a taint source that _only_ propagates by side effect. This option
  should allow (ab)using taint-mode for writing some typestate rules.

  For example, this taint rule:

  ```yaml
  pattern-sources:
    - by-side-effect: only
      patterns:
        - pattern: lock($L)
        - focus-metavariable: $L
  pattern-sanitizers:
    - by-side-effect: true
      patterns:
        - pattern: unlock($L)
        - focus-metavariable: $L
  pattern-sinks:
    - pattern: lock($L)
  ```

  will match the second `lock(x)` in this code:

  ```python
  lock(x) # no finding
  lock(x) # finding
  ```

  The first `lock(x)` will not result in any finding, because the occurrence of `x` in
  itself will not be tainted. Only after the function call we will record that `x` is
  tainted (as a side-effect of `lock`). The second `lock(x)` will result in a finding
  because the `x` has been tainted by the previous `lock(x)`. (pa-2980)

### Changed

- In the metrics sent we now record the languages for which we invoked the interfile engine.
  This will enable us to measure the performance impact and error rates of new interfile
  languages. (For scans which don't send metrics, there is no change.) See the PRIAVCY.md
  for more information. (ea-251)
- Removed support for named snippets (`org_name:rule_id`) from `semgrep scan` which were removed from semgrep.dev a few months ago. (gh-9203)
- Added support for `--config <code|secrets>` to semgrep scan. When using
  code or secrets, the environment variable `SEMGREP_REPO_NAME` must be set.

  For example,

  ```
  $ SEMGREP_REPO_NAME=test_repo semgrep --config secrets
  ```

  Internally, `semgrep scan --config <product>` now uses the same endpoint as the
  `semgrep ci` to fetch the scan configuration. (gh-9205)

- Improved handling of unused lambdas to reduce false positives

  Previously, we used to insert the CFGs of unused lambdas at the declaration
  site. However, this approach triggered some false positives. For example,
  consider the following code:

  ```
  void incorrect(int *p) {
    auto f1 = [&p]() {
      source(p);
    };
    auto f2 = [&p]() {
      sink(p);
    };
  }
  ```

  In this code, there's no actual control flow between the source and sink, and
  the lambdas are never even called. But when we inserted their CFGs at the
  declaration site, it incorrectly indicated a taint finding. To prevent these
  types of false positives while still scanning the body of unused lambdas, we
  now insert their CFGs in parallel at the end of their parent function, right
  after all other statements and just before the end node. (pa-3089)

- Bumped timeout (per-rule and per-file) from 2s to 5s. Recently we lowered it
  from 30s down to 2s, but based on what we have observed so far, we believe 5s
  is a better timeout for the time being. (timeout)

### Fixed

- Fixed a bug where enabling the secret beta causes the default scan mode to be
  set to OSS, even when the Pro flag is turned on in the web UI. (ea-248)
- Semgrep does not stop a scan anymore for parsing errors due to
  unconventional exceptions (e.g., Failure "not a program") in some
  parsers. Instead, such errors are reported as "Other syntax error". (lang-13)
- Fix regression for the unused lambda change in react-nextjs-router-push test

  A lambda expression defined in a return expression is also treated as used at
  the location of the return expression. (pa-3089)

- Updated the Rust parser with miscellaneous improvements. In particular, Semgrep can now parse `yield` expressions in Rust. (rust)
- taint-mode: If an expressions is tainted by multiple labels A and B, with B
  requiring A, the expression will now get boths labels A and B. (taint-labels)

## [1.48.0](https://github.com/returntocorp/semgrep/releases/tag/v1.48.0) - 2023-11-06

### Added

- Matching: Matches with the same range but bindings in different locations
  will now no longer deduplicate.

  For instance, the pattern `$FUNC(..., $A, ...)` would produce only
  one match on the target file:

  ```
  foo(true, true)
  ```

  because you would have two matches to the range of the call, and both
  bindings of `$A` would be to `true`.

  Now, the deduplication logic sees that the bindings of `$A` are in
  different places, and thus should not be considered the same, and
  produce two matches. (pa-3230)

### Fixed

- Fixed out of bounds list access error in Cargo.lock parser (sc-1072)
- Secrets: metadata overrides specified in validators were incorrectly applied on
  top of one another (on a per-rule basis), so that only the last was applied.
  Each update is now correctly applied independently to each finding based on the
  rule's validators. (scrt-231)

## [1.47.0](https://github.com/returntocorp/semgrep/releases/tag/v1.47.0) - 2023-11-01

### Added

- taint-mode: Added a Boolean `exact` option to sources and sanitizers to make
  matching stricter (default is `false`).

  If you specify a source such as `foo(...)`, and Semgrep encounters `foo(x)`,
  by default `foo(x)`, `foo`, and `x`, will all be considered tainted. If you add
  `exact: true` to the source specification, then only `foo(x)` will be regarded
  as tainted, that is the "exact" match for the specification. The same applies
  to "exact" sanitizers. (gh-5897)

- Added `sg` alias for semgrep binary which is functionally equivalent to
  ```sh
  alias sg="/opt/homebrew/bin/semgrep"
  ```
  with one fewer step. (gh-9117)
- secrets: Added independent targeting from other semgrep products.

  This change allows Secrets to scan all tracked files. In particular, those ignored
  by semgrepignore will now get scanned. There will be additional changes
  in the future to allow configuring the files that are scanned secrets. (gh-9125)

- Adds an optional `--no-secrets-validation` flag to skip secrets validation. (no-secrets-validation)
- Secrets rules (i.e., with metadata product: secrets) now mask, by replacing
  with \*s the ending component of the matched content. (pa-2333)
- Commutativity Support for Comparison Operators EQ and NOT_EQ

  We've introduced the `commutative_compop` rule option, enabling commutativity
  for comparison operators EQ and NOT_EQ. With this option, `a == b` will also
  match `b == a`, and `a != b` will also match `b != a`. (pa-3140)

- Validation errors are separated from unvalided findings in the terminal output. (validation-error)

### Changed

- For taint rules using labels (experimental) Semgrep now preferably picks a
  source without `requires` for the taint trace

  Semgrep now prioritizes taint sources without `requires` condition when
  choosing a representative taint trace from multiple source traces. This helps
  users to more clearly identify the initial taint source when multiple traces
  are involved. (pa-3122)

- Unreachable supply chain findings report only on line dependency was found in (no longer incorrectly including the next line)
  this change could affect syntactic_id generated by said findings (sc-727)
- When running `semgrep ci --supply-chain`, defaults to using OSS engine even if
  PRO engine would otherwise be used (turned on in semgrep.dev, or with `--pro` flag) (supply-chain-oss)

### Fixed

- - Semgrep no longer supports python 3.7 (gh-8698)
- Semgrep will now refuse to run incompatible versions of the Pro Engine, rather than crashing with a confusing error message. (gh-8873)
- Fixed an issue that prevented the use of `semgrep install-semgrep-pro --custom-binary ...` when logged out. (gh-9051)
- The --severity=XXX scan flag is working again. (gh-9062)
- The --sarif does not crash when semgrep itself encountered errors
  while processing targets. (gh-9091)
- Fixed how the end positions assigned to metavariable bindings are computed, in
  order to handle trailing newlines. This affected Semgrep's JSON output. If a
  metavariable `$X` was bound to a piece of text containing a trailing newline,
  such as "a\n", where the starting position was e.g. at line 1, Semgrep reported
  that the end position was at line 2, when in fact the text is entirely within
  line 1. If the text happened to be at the end of a file, Semgrep could report
  an end position that was outside the bounds of the file. (lang-18)
- - Semgrep Language Server now only scans open files on startup
  - Semgrep Language Server no longer scans with pro engine rules (ls)
- Rust: `unsafe` blocks are now translated into the Dataflow IL so e.g. it becomes
  possible for taint analysis to track taint from/to an `unsafe` block. (pa-3218)
- Correctly handle parsing toolchain directive in go.mod files (parsegomode)

## [1.46.0](https://github.com/returntocorp/semgrep/releases/tag/v1.46.0) - 2023-10-24

### Added

- `semgrep install-semgrep-pro` now takes an optional `--custom-binary` flag to install the specified `semgrep-core-proprietary` binary rather than downloading it. (custom-pro-binary)

### Fixed

- pyproject.toml parser now handles optional newlines right after section headers. (gh-10879)
- Updated the parsers for poetry.lock, pipfile.lock, and requirements.txt to ignore case sensitivity from package names.
  This matches their respective specifications. Test cases were added to account for this change. (gh-8984)
- Reduced the limits for the prefilter optimization so that rules that cause
  computing the prefilter to blow up will abort more quickly. This improves
  performance by 2-3 seconds for each of the slowest rules. May cause a
  slowdown if a rule that previously could be filtered out no longer will be,
  but based on testing this is unlikely. (gh-9040)
- Fixed issue where conditional expressions aren't handled properly in expression based language.

  Rust example:

  Before:

  ````rust
  fn expr_stmt_if(c) {
    y = 0;
    x = if c { y = 1 };

    // Before: this matches when it shouldn't because y is not always 1.
    // After: this does not match, which is the correct behavior.
    y == 1;
  }
  ``` (pa-3205)
  ````

- Fixed type error in creation of DependencyParserError object in the pnpm-lock.yaml parser (sc-1115)

## [1.45.0](https://github.com/returntocorp/semgrep/releases/tag/v1.45.0) - 2023-10-18

### Changed

- Previously, to ignore a finding from rule 'foo.bar.my-rule' we only accepted
  `nosemgrep: foo.bar.my-rule`, now we also accept `nosemgrep: my-rule`. (pa-3094)
- [Breaking Change] Improved Matching of C++ Constructors

  In this update, we've enhanced Semgrep's ability to match C++ constructors more
  accurately. C++ introduces a syntactic ambiguity between function and variable
  definitions, particularly with constructors. The C++ compiler determines how to
  interpret an expression based on contextual information, such as whether the
  immediate parent scope is a function or a class, and whether the identifiers
  within the parentheses represent variables or types. Due to this complexity,
  static analyzers face challenges in precisely parsing these expressions without
  additional information.

  This commit introduces several workarounds to provide a better solution for
  handling this ambiguity:

  - By default, when parsing a target file, Semgrep will consider an expression
    like `foo bar(x, y, z);` defined within the body of a function as a variable
    definition with a constructor. This is because variable initialization is a
    more common use case within the body of a function.

  - Users can specify rule options that annotate, in patterns where the
    expression can be interpreted in both ways, which interpretation should take
    precedence. For instance, `foo bar(x, y, z);` will be parsed as a function
    definition when the `as_fundef` option is used and as a variable definition
    with a constructor when the `as_vardef_with_ctor` option is applied. It's
    worth noting that an expression like `foo bar(1, y, z);` will be parsed as a
    variable definition without any additional annotation since `1` cannot be a
    type.

  Here's an example rule and its corresponding target file to illustrate these
  changes,

  ```
  rules:
    - id: cpp-match-func-def
      message: Semgrep found a match
      options:
        cpp_parsing_pref: as_fundef
      languages:
        - cpp
      severity: WARNING
      pattern-either:
        - pattern: foo $X($Y);
        - pattern: foo $X($Y, $Z);

    - id: cpp-match-ctor
      message: Semgrep found a match
      options:
        cpp_parsing_pref: as_vardef_with_ctor
      languages:
        - cpp
      severity: WARNING
      patterns:
        - pattern: foo $X(...);
        - pattern-not: foo $X(3, ...);

    - id: cpp-match-ctor-3
      message: Semgrep found a match
      languages:
        - cpp
      severity: WARNING
      pattern: foo $X(3, ...);
  ```

  ````
  class Test {

    // ruleid: cpp-match-func-def
    foo bar(x);
    // ruleid: cpp-match-func-def
    foo bar(x, y);

    void test() {
      // ruleid: cpp-match-ctor
      foo bar(1);
      // ruleid: cpp-match-ctor
      foo bar(1, 2);

      // ruleid: cpp-match-ctor
      foo bar(x);
      // ruleid: cpp-match-ctor
      foo bar(x, y);

      // ruleid: cpp-match-ctor
      foo bar(x, 2);
      // ruleid: cpp-match-ctor
      foo bar(1, y);

      // ruleid: cpp-match-ctor-3
      foo bar(3);
      // ruleid: cpp-match-ctor-3
      foo bar(3, 4);
      // ruleid: cpp-match-ctor-3
      foo bar(3, y);
    }
  };
  ``` (pa-3114)
  ````

### Fixed

- Reduction of the docker image size by using --no-cache when apk upgrading.
  Thanks to Peter Dave Hello for the contribution. (docker)
- Fixed a bug with pre-filtering introduced in 1.42.0 that caused significant slowdowns,
  particularly for Kotlin repos. Kotlin repos running default pro rules may see a 30 minute
  speedup. (ea-208)
- Taint analysis: track `ptr->field` l-values in C++

  In C++, we now track tainted field access via pointer dereference. For
  instance, consider the following code snippet:

  ```
  void test_intra_001() {
    TestObject *obj = new TestObject();

    obj->a = taint_source();
    obj->b = SAFE_STR;

    // ok: cpp-tainted-field-ptr
    sink(obj->b, __LINE__);
    // ruleid: cpp-tainted-field-ptr
    sink(obj->a, __LINE__);
  }
  ```

  This can be matched by the rule:

  ````
  rules:
    - id: cpp-tainted-field-ptr
      languages:
        - cpp
      message: testing flows though C++ ptrs
      severity: INFO
      mode: taint
      pattern-sources:
        - pattern: taint_source()
      pattern-sinks:
        - patterns:
            - pattern: sink($X, ...)
            - focus-metavariable:
                - $X
  ``` (gh-1058)
  ````

- Do not crash anymore with an Invalid_arg exception when the terminal
  has very few columns (e.g., in some precommit context). (gh-8792)
- add `--supply-chain` flag to `semgrep ci --help` documentation (gh-8975)
- Avoid catastrophic `Invalid_argument: index out of bounds` errors
  when reporting the location of findings (#9011) (gh-9011)
- - The Semgrep Language server will no longer freeze while scanning long files (ls-perf)
- Pre-filtering is now less aggressive and tries not to skip files that could be
  matched by a rule due to constant-propagation. Previously, a rule searching for
  the string `"foobar"` would skip a file that did not contain exactly `"foobar"`,
  but that contained e.g. `"foo" + "bar"`. (pa-3110)
- semgrep ci does not crash anymore when ran from git repositories coming from
  Azure projects with whitespaces in the name. (pa-3145)
- The `--test` will now process test target files even if they do not match
  the `paths:` directive of a rule. This is especially useful for rules
  using the `include:` which is now disabled in a test context. (pa-8192)

## [1.44.0](https://github.com/returntocorp/semgrep/releases/tag/v1.44.0) - 2023-10-11

### Added

- A new --matching-explanations CLI flag has been added, to get matching
  explanations. This was internally used by the Semgrep Playground to
  help debug rules, but is now available also directly from the CLI. (explanations)
- Using C++ tree-sitter as a failsafe pattern parser for C (gh-8905)
- Allowing multiple type fields in metavariable-type rule syntax

  Users have the flexibility to utilize multiple type fields to match the type of
  metavariables. For instance:

  metavariable-type:
  metavariable: $X
  types: - typeA - typeB

  This approach is also supported in rule 2.0. (gh-8913)

- Support for parsing pubspec (Dart/Flutter) lockfiles (gh-8925)
- Added support for matching template type arguments using metavariables in C++.
  Users can now successfully match code snippets like:

  ```
  #include <memory>
  using namespace std;

  void foo() {
      int *i = 0;

      // ruleid: match-with-template
      shared_ptr<int> p;
  }
  ```

  with the pattern:

  ````
  shared_ptr<$TY> $LOCAL_VAR;
  ``` (pa-3102)
  ````

### Fixed

- Avoid fatal "missing plugin" exceptions when scanning some Apex rules
  for which no Apex pattern is used by the rule such as a `pattern-regex:`
  and nothing else. (gh-8945)
- Semgrep can now parse optional assignments in Swift (e.g. `a.b? = 1`). (lang-1)
- Sequential tainting is now supported in Elixir.

  ````elixir
  def f() do
    x = "tainted"
    y = x

    # This now matches.
    sink(y)
  end
  ``` (pa-3130)
  ````

- Target files that disappeared before the scan or that have special byte
  characters in their filename do not cause the whole scan to crash anymore.
  The file is skipped instead. (pa-3144)
- go.mod parsing now correctly allows arbitrary newlines and whitespace between dependencies (sc-1076)
- fix: Improve typed metavariable matching against expressions consisting of names only. (type-inference)

## [1.43.0](https://github.com/returntocorp/semgrep/releases/tag/v1.43.0) - 2023-10-03

### Added

- Dart: Full Semgrep support for Dart has been added, whereas previously
  most Semgrep constructs (and Semgrep itself) would not work correctly. (pa-2968)

### Changed

- We have reduced the default timeout (per-rule and per-file) to 2s (down from 30s).
  Typically, running a rule on a file should take a fraction of a second. When a rule
  takes more than a couple of seconds is often because the rule is not optimally
  written, or because the file is unusually large (a minified file or machine-
  generated code), so waiting 30s for it does not tend to bring any value. Plus, by
  cutting it earlier, we may prevent a potential OOM crash when running the rule is
  very memory intensive. (pa-3155)

### Fixed

- The language server will no longer surface committed findings when a user types but does not save (pdx-ls-git)

## [1.42.0](https://github.com/returntocorp/semgrep/releases/tag/v1.42.0) - 2023-09-29

### Added

- Rule-writing: Capture group metavariables used in regexes in a
  `metavariable-regex` can now introduce their bindings into the
  scope of the pattern, similarly to `metavariable-pattern`.

  For instance, in the pattern:
  patterns:

  - pattern: |
    foo($BAR)
  - metavariable-regex:
    metavariable: $BAR
    regex: "(?<X>.\*)end"
  - focus-metavariable: $X

  the rule will match the contents of what is inside of the
  `foo` to the regex that binds anything before an "end" to
  the metavariable `$X`. This metavariable can then be focused
  at a later time, or processed somewhere above this pattern. (pa-3011)

- Try-catch-else-finally is now supported in taint analysis.

  This change also includes some updates to our analysis. Previously we assumed that
  any statement inside the try clause may throw an exception, but now only
  function calls are assumed to possibly throw exceptions.

  Throw statements always throw an exception as it was before.

  This kind of statement is supported in languages including Python, Ruby, and Julia.

  Python example:

  ````py
  def f(tainted_input):
    try:
      a = 0
      b = 0
      c = tainted_input
      d = tainted_input
    except RuntimeError:
      a = tainted_input
      c = sanitize(c)
    else:
      b = tainted_input
    finally:
      d = sanitize(d)

    # a is not tainted because exception wasn't assumed to be thrown
    sink(a)
    # b is tainted through the else clause
    sink(b)
    # c is tainted at the beginning, but it was not sanitized
    # because an exception was not thrown
    sink(c)
    # d is tainted at the beginning, but it was sanitized
    # because the finally clause is always executed
    sink(d)
  ``` (pa-3054)
  ````

- Pro: Semgrep can now derive facts about constants from equality tests.

  For example, pattern `foobar(&nullptr)` will not match here:

  ```cpp
  int* ptr = nullptr;

  do_something(ptr);

  if (ptr == nullptr) {
      return;
  }

  foobar(&ptr); // OK
  ```

  But it will match here:

  ````cpp
  if (ptr != nullptr) {
      return;
  }

  foobar(&ptr); // finding
  ``` (pa-3091)
  ````

- Metavariable-type rule support for C, C++

  Users now can use metavariable-type rules in both C and C++. For instance, the
  provided code snippet:

  ```
  #include <fstream>

  using namespace std;

  void test_001() {
      ifstream in;
      // ruleid: match-simple-metavar-type
      in.get(str, 2);

      mystream my;
      // ok: type mismatch
      my.get(str, 2);
  }
  ```

  can be matched by the following rule:

  ````
  rules:
    - id: match-simple-metavar-type
      patterns:
        - pattern: $X.get($SRC, ...)
        - metavariable-type:
            metavariable: $X
            type: ifstream
      message: Semgrep found a match
      languages:
        - cpp
      severity: WARNING
  ``` (pa-3106)
  ````

- C/C++: If conditions such as `if (int x = f())` are now correctly translated
  into the Dataflow IL, so Semgrep can report a finding in the example below:

  ````c
  if (const char *tainted_or_null = source("PATH"))
  {
      // ruleid:
      sink(tainted_or_null);
  }
  ``` (pa-3107)
  ````

### Changed

- The \_comment field in the JSON output of semgrep scan has been removed. (\_comment)
- Use config=auto by default for the scan command when other options are not specified (grow-50)
- Use subprocess.run to get contributions instead of StreamingSemgrepCore so crashes don't affect the actual scan. (os-967)

### Fixed

- The CLI autocompletion code has been removed. It was not currently working
  and nobody reported it, which probably means nobody was using it. (autocomplete)
- The --core-opts flag has been removed. (core_opts)
- fix: metavariable-type now correctly matches non-primitive types in php (gh-8781)
- fixed the regression in --registry-caching and add better error message
  to tell the user he needs also --experimental. (gh-8828)
- Support labeled let bindings within Swift case statements

  Correctly parsing labeled let bindings within Swift case statements.
  For instance, the code snippet:

  ```
  switch self {
    case .bar(_, _, x: let y):
      return y
  }
  ```

  now successfully matches the pattern:

  ````
  switch self {case .$X(..., $Y: $Z): ...}
  ``` (pa-3120)
  ````

- Add parsing support for various rare Swift constructs (swift-parsing)

## [1.41.0](https://github.com/returntocorp/semgrep/releases/tag/v1.41.0) - 2023-09-19

### Changed

- Rule validation no longer fails if a rule contains additional unknown fields. This makes it so older versions of semgrep do not fail rules that contain extra functionality. min-version field should be used to identify rules that should not be run (i.e. the additional functionality is necessary in running the rule) (gh-8712)
- Limit collection of the contributions from git log to the last 30 days of commits. (scp-965)

### Fixed

- Ruby: Fixed a bug where patterns like `<id> ... do ... end` would not
  match properly. (gh-8714)
- Show more specific error message if scan cannot complete because user has disabled all rules on semgrep.dev (gh-8716)
- For the `nonroot` Docker build stage, moved `semgrep-core` to
  `/home/semgrep/bin` and updated `$PATH` env variable with the
  new location. This avoids permissions issues when running and
  installing Pro Engine while using the `nonroot` Docker image. (pa-3026)
- Implemented key path expression parsing in Swift. The following example should
  now be correctly matched by the \$X.isActive pattern:

  ```
  employee.filter(\.isActive)
  ```

  Note that when the implicit type is used, the metavariable $X will bind to the
  backslash character instead of the type name. (pa-3070)

- C++: Translate `for (T var : E)` loops into the Dataflow IL as for-each loops,
  so that Semgrep reports no finding in the following code:

  ```
    for (int *p : set) {
      sink(p); // no finding
      source(p);
    }
  ```

  Since each `p` is (in principle) a different object, even if `source(p)` taints
  the current `p`, that should not affect the next one. (pa-3090)

- Ruby: Fixed patterns which involve command calls with blocks and Semgrep ellipses,
  when there are newlines around.

  For instance, the pattern

  ```
  $METHOD ... do
    ...
  end
  ```

  will now parse properly. (pa-3100)

- Fixes how semgrep identifies the transitivity of dependencies in node v9 (lockfile version 3) and above.
  Specifically, dependencies that should have been identified as "direct" were being miscategorized as "transitive",
  which should no longer be the case. (sc-1057)

## [1.40.0](https://github.com/returntocorp/semgrep/releases/tag/v1.40.0) - 2023-09-14

### Added

- Dot files (e.g., .vscode) are now displayed in the skip report when
  using --verbose and --develop. (dotfiles)
- Add textual output for secrets findings and scan summary on command line interface. (gh-8666)
- Skip rules with an informational message if they can't run due to an
  unavailable plugin such as those provided by the Pro version of Semgrep.
  The intended use is for a public rule registry to provide all kinds of rules
  including some that require particular plugins. (gh-8668)
- Allow Semgrep CI users to specify Code product using `--code` command-line option. This works the same as `--supply-chain` now and fleshes out the product suite. (gh-8679)
- Semgrep Language Server will now not show findings that have been ignored in Semgrep Code (lang-server)
- taint-mode: Semgrep will now track taint via globals or class attributes that are
  _effectively_ `final` (as in Java), e.g.:

  ```java
  class Test {
    private String x = source();

    void test() {
      sink(x); // finding here !
    }
  }
  ```

  Semgrep will recognize that `x` must be tainted because it is a private class
  attribute that is initialized to `source()`, and it is not re-defined anywhere
  else. This will also work if `x` is initialized in _the_ constructor (if there
  is only one constructor), or in a `static` block. (pa-1636)

- const-prop: Semgrep can now identify as constants private class attributes
  that are assigned just once in a class constructor, e.g.:
  https://semgrep.dev/playground/s/R1re. (pa-3006)
- Added `-dump_contributions` flag to semgrep-core and include contributions when posting findings to Scan API. (scp-313)
- There is a new 'semgrep show' command to display information about
  semgrep, for example 'semgrep show supported-languages'. The goal is to
  cleanup 'semgrep scan' which is currently abused to not scan but
  also display semgrep information (e.g., 'semgrep scan --show-supported-languages).
  See 'semgrep show --help' for more information. (show)

### Changed

- Further improvements to timeouts and logging for `semgrep ci` (gh-8656)

### Fixed

- Semgrep LS will no longer duplicate some findings (lang-server)
- Output: GitLab SAST output has now been updated to accommodate the new SAST schema
  as of GitLab 16.x, which means that findings in GitLab will now properly display
  descriptions of the findings. (pa-3014)
- Julia: Ellipses can now properly match when used in conjunction
  with single statements, when matching 0 statements.

  For instance, the pattern

  ...
  foo()

  can now properly match a target of

  foo() (pa-3049)

- Matching: Numeric capture group metavariables of the form $1, $2, etc that are
  introduced by unnamed capture groups, now no longer will cause matches to fail
  if they do not unify. They are still referenceable, however.

  This is so that capture group metavariables (which are introduced rather implicitly)
  do not cause rules to "invisibly" fail to match. (pa-3050)

- The CFG now supports case statements in Ruby, which does not fall through. (pa-3055)
- Constant propagation now handles implicit number-to-string conversions in Java
  and JS/TS. A Java expression such as `"foo" + 123` will now match the string
  pattern "foo123". (pro-169)
- Add exception handling for dump_contributions core command in pysemgrep (scp-313)

## [1.39.0](https://github.com/returntocorp/semgrep/releases/tag/v1.39.0) - 2023-09-07

### Added

- Matching: Qualified names written as patterns can now match valid instances of
  identifiers which lie underneath a wildcard import. For instance, in Python,
  we could write the pattern `A.B.C.x`, and match the usage in the program

  ````
  from A.B import *

  foo(C.x)
  ``` (pa-1006)
  ````

- Ruby: Replaced old Ruby parser with the latest tree-sitter ruby parser,
  meaning that there could be small edge cases of differences in how
  Semgrep matches Ruby programs. (pa-3017)

### Fixed

- Request retry logic now includes 504's (gh-8629)
- The error message for skipped rules due to incompatible `min-version` or
  `max-version` constraints now makes sense. (gh-8634)
- When `metavariable-type` cannot be evaluated then it defauls to "false", that is,
  it filters out the range. Therefore e.g. this rule:

  ```yaml
  patterns:
    - pattern: private int $X;
    - metavariable-type:
        metavariable: $Y
        type: int
  ```

  now will produce no matches because `$Y` is not bound to anything. (pa-3027)

- Julia: `using` and `import` now match separately, instead of before, where
  if you wrote `using $X`, you would also match to `import`s. (pa-3028)
- Diagnostics from a full scan through Semgrep LS no longer disappear when file is opened (pa-3046)

## [1.38.3](https://github.com/returntocorp/semgrep/releases/tag/v1.38.3) - 2023-09-02

No significant changes.

## [1.38.2](https://github.com/returntocorp/semgrep/releases/tag/v1.38.2) - 2023-09-01

### Fixed

- restore access to the --text option (gh-8610)

## [1.38.1](https://github.com/returntocorp/semgrep/releases/tag/v1.38.1) - 2023-09-01

### Fixed

- restored access to the --output flag (gh-8602)
- Adds an error message clarifying the removal of `python -m semgrep`. This change originated in https://github.com/returntocorp/semgrep/pull/8504. (gh-8605)

## [1.38.0](https://github.com/returntocorp/semgrep/releases/tag/v1.38.0) - 2023-08-31

### Added

- The CLI now returns the commit timestamp when using `semgrep ci` (cli-timestamp)
- Add support for `min-version` and `max-version` fields for each rule,
  specifying a range of compatible Semgrep versions. If a rule is incompatible
  with the version of Semgrep being used, it is reported in the JSON output at
  the "info" level which doesn't cause an exit failure. (gh-8496)
- Dependency data is now also sent to the /results endpoint of semgrep app. It is still sent to the /complete endpoint. (sc-async)

### Changed

- Adjust the count printed at the conclusion summary to match the top summary
  (only printing the count of rules actually run by semgrep and not just the number of rules received from the server). (counts)
- The option to omit --config and to look for the presence of a .semgrep.yml
  or .semgrep/.semgrep.yml in the current directory has been removed. You now
  have to explicitly use --config. (dotsemgrep)
- The deprecated --enable-metrics and --disable-metrics flags have finally been
  removed. Use --metrics=on or --metrics=off instead (or --metrics=auto). (enable_metrics)
- The semgrep_main.py module has been renamed to run_scan.py and its
  invoke_semgrep() function renamed to run_scan_and_return_json().
  External tools (e.g., semgrep wrappers) using directly those functions
  should be updated. Note that this function will soon disappear as
  part of a migration effort converting Python code to OCaml. Thus,
  those tools should instead wrap the semgrep CLI and rely on
  semgrep_output_v1.atd for a more stable official API. (internals)

### Fixed

- Running just `semgrep` now displays the help message. Semgrep does not
  try anymore to look for a .semgrep.yml config file or .semgrep/ in the
  current directory, which used to cause issues when running from your
  home directory which can contain the .semgrep/settings.yml file (which
  is actually not a semgrep rule). (gh-4457)
- Fixed CLI output to display matches from different rules with the same message. (gh-8557)
- Semgrep PyPI package can now be pip install-ed on aarch64 libmusl platforms (e.g. Alpine) (gh-8565)
- Updated `--max-memory` help description to make it more clear/concise. To say "Defaults to 0 for all CLI scans." implies a different default for non-CLI scans, where in practicality the default is 0 for all scans except when using Pro Engine, where the default is 5000. (max_memory_help)
- Julia: Fixed a bug where `let end` blocks were not being parsed
  correctly, causing their contents to not strictly match while inside of
  a block.

  For instance, `let ... end` would not count as being inside of the `let`,
  and would match everything. (pa-3029)

- Fixed bug where dependencies in (pnpm-lock.yaml at version 6.0 or above) files were not parsed. (sc-1033)

## [1.37.0](https://github.com/returntocorp/semgrep/releases/tag/v1.37.0) - 2023-08-25

### Added

- `semgrep scan` is now more resilient to failures when fetching config from semgrep.dev. If it can't fetch a config from semgrep.dev it will use backup infrastructure to fetch the most recent successful config for that customers environment. (gh-8459)
- C#: Added experimental NuGet ecosystem parser (gh-8484)
- metavariable-comparison: You can now use "in" and "not in" for strings
  in the same sense as in Python, for substring checking. (pa-2979)
- Julia: Added the deep expression operator, so now you can write patterns like
  foo(<... 42 ...>) to find instances of calls to `foo` that contain `42` somewhere
  inside of it. (pa-3018)
- `semgrep ci` displays enabled products when scans are created and/or when the scan
  config is generated from Semgrep Cloud Platform. Additionally, if no products are
  enabled then a friendly error is raised. (scp-432)

### Changed

- The --dump-ast flag now requires the additional --experimental flag
  and does not require to pass a --config flag anymore.
  Example of use: `semgrep --experimental --lang python --dump-ast foo.py` (dumpast)
- The 'semgrep shouldafound' command has been removed. It was not really used
  and it might be better to offer such a functionality in the IDE instead of
  in the CLI. (shouldafound)

### Fixed

- Parsing: Some parsing errors involving tree-sitter inserting fake "missing"
  nodes were previously unreported. They are now reported as errors although the
  parse tree is preserved, including the phony node inserted by tree-sitter.
  This should not result in different Semgrep findings. It results only in more
  reports of partial parsing. See the original issue at
  https://github.com/returntocorp/ocaml-tree-sitter-core/issues/8 for technical
  details. (gh-8190)
- fix(extract): correctly map metavariable locations into source file (gh-8416)
- fix(julia): correctly parse BitOr and BitAnd (gh-8449)
- Implement missing pcre-ocaml stub (pcre_get_stringnumber_stub_bc) in JavaScript (gh-8520)
- Julia: Fixed a bug where parenthesized expressions would sometimes
  not match in constructs like `metavariable-comparison`. (pa-2991)
- Fixed a regression introduced three years ago in 0.9.0, when optimizing
  the evaluation of `...` (ellipsis) to be faster. We made `...` only match
  deeply (inside an `if` for example) if nothing matched non-deeply, thus
  causing that this pattern:

  ```python
  foo()
  ...
  bar($A)
  ```

  would only produce a match rather than two on this code:

  ```python
  foo()
  if cond:
      bar(x)
  bar(y)
  ```

  Semgrep matched from `foo()` to `bar(y)` and because of that it did not
  try to match inside the `if`, thus there was no match from `foo()` to `bar(x)`.
  However, if we commented out `bar(y)`, then Semgrep did match `bar(x)`.

  Semgrep now produces the two expected matches. (pa-2992)

- Julia: Type information from declarations can now be used in
  `metavariable-type`. For instance, the program:
  ```
  x :: Int64 = 2
  ```
  will now allow uses of `x` to match to the type `Int64`. (pa-3001)
- Julia: Metavariables should now be able to appear anywhere that
  identifiers can.

  For instance, they were not able to appear as the argument to a
  do block. Now, we can write patterns like:

  ````
  map($Y) do $X
    ...
  end
  ``` (pa-3007)
  ````

- Java: Fixed naming bug affecting Java and other OO languages that allowed a
  method parameter to shadow a class attribute, e.g. in:

  ```java
  class Test {

      private int x;

      public void test2(int x) {
          foo(this.x);
      }

  }
  ```

  Semgrep was considering that `this.x` referred to the parameter `x` of `test2`
  rather than to the class attribute `x`. (pa-3010)

- Fixed bug where packages in build.gradle files had their names incorrectly parsed without their group ID (sc-1012)

## [1.36.0](https://github.com/returntocorp/semgrep/releases/tag/v1.36.0) - 2023-08-14

### Added

- Added general machinery to support languages with case insensitive identifiers and generalized php to use these case insensitive identifiers.

  For example, in php the pattern `MyClass()` will now match calls with different capitalization such as `myclass()` and `Myclass()`. (gh-8356)

### Fixed

- Convert all '@r2c.dev' email addresses to '@semgrep.com'. (gh-8437)
- Semgrep LSP now compiled with tls, should no longer crash with not compiled with tls error (ls-conduit)
- Fixed multiprocess testing crash due to new osemgrep entrypoint (pa-2963)
- Pro: JS/TS: taint-mode: Fix bug introduced in 1.33.1 that had the side-effect of
  hurting performance of taint rules on JS/TS repos that used destructuring in
  functions formal parameters. (pro-119)

## [1.35.0](https://github.com/returntocorp/semgrep/releases/tag/v1.35.0) - 2023-08-09

### Added

- Maven Dep Tree parsing now surfaces children dependencies per package (sc-996)

### Fixed

- fix(promql): make aggregation labels not depend on order

  "sum by (..., b, a, c, ...) (X)" should match "sum by (a,b,c) (X)" (gh-8399)

## [1.34.1](https://github.com/returntocorp/semgrep/releases/tag/v1.34.1) - 2023-07-28

### Added

- feat(eval): add "parse_promql_duration" function to convert a `promql` duration into milliseconds. This makes it possible to write comparisons like this:

  ````
  - metavariable-comparison:
      metavariable: $RANGE
      comparison: parse_promql_duration(str($RANGE)) > parse_promql_duration("1d")
  ``` (gh-8381)
  ````

### Fixed

- fix(yaml): fix captures for sequences that contain mappings (gh-8388)

## [1.34.0](https://github.com/returntocorp/semgrep/releases/tag/v1.34.0) - 2023-07-27

### Added

- Added support for naming propagation when the left-hand side (lhs) of a variable definition is an identifier pattern

  In certain languages like Rust, the variable definition is parsed as a pattern assignment, for example:

  ```
  let x: SomeType = SomeFunction();
  ```

  This commit ensures that the annotated type is propagated to the identifier pattern on the left-hand side (lhs) of the assignment, thus ensuring proper naming behavior. (gh-8365)

- feat(metavar type): Metavariable type support for Julia

  Metavariable type is supported for Julia. (gh-8367)

- New --legacy flag to force the use of the old Python implementation of
  Semgrep (also known as 'pysemgrep'). Note that by default most semgrep
  commands are still using the Python implementation (except 'semgrep
  interactive'), so in practice you don't need to add this flag, but as
  we port more commands to OCaml, the new --legacy flag might be useful
  if you find some regressions. (legacy)
- Matching: Added the ability to use metavariables in parameters to match more
  sophisticated kinds of parameters.

  In particular, metavariables should now be able to match `self` parameters,
  such as in Rust.

  So `fn $F($X, ...) { ... }` should match `fn $F(self) { }`. (pa-2937)

- taint-mode: Added **experimental** `control: true` option to `pattern-sources`,
  e.g.:

  ```yaml
  pattern-sources:
    - control: true
      pattern: source(...)
  ```

  Such sources taint the "control flow" (or the program counter) so that it is
  possible to implement reachability queries that do not require the flow of any
  data. Thus, Semgrep reports a finding in the code below, because after `source()`
  the flow of control will reach `sink()`, even if no data is flowing between both:

  ````python
  def test():
    source()
    foo()
    bar()
    #ruleid: test
    sink()
  ``` (pa-2958)
  ````

- taint-mode: Taint sanitizers will be included in matching explanations. (pa-2975)

### Changed

- Started using ATD to define the schema for data sent to the /complete endpoint of semgrep app (app-4255)
- Targets in a `.yarn/` directory are now ignored by the default .semgrepignore patterns. (dotyarn)

### Fixed

- Aliengrep mode: Fix whitespace bug preventing correct matching of parentheses. (gh-7990)
- yaml: exclude style markers from matched token in block scalars (gh-8348)
- Fixed stack overflow caused by symbolic propagation. (pa-2933)
- Rust: Macro calls which involve dereferencing and reference operators
  (such as `foo!(&x)` and `foo!(*x)`) now properly transmit taint (pa-2951)
- Semgrep no longer crashes when running --test (pa-2963)
- Exceptions raised during parsing of manifest files no longer interrupt general parser execution, which previously prevented lockfile parsing if a manifest failed to parse. (sc-exceptions)

## [1.33.2](https://github.com/returntocorp/semgrep/releases/tag/v1.33.2) - 2023-07-21

No significant changes.

## [1.33.1](https://github.com/returntocorp/semgrep/releases/tag/v1.33.1) - 2023-07-21

### Added

- Rust: Added support for ellipsis patterns in attribute argument position. (e.g. `#[get(...)]`) (gh-8234)
- Promql: Initial language support (gh-8281)
- `.h` files will now run when C or C++ are selected as the language. (pa-123)
- `.cjs` and `.mjs` files will now run when javascript is selected as the language. (pa-124)
- Tainting: Parameters to functions in languages with pattern matching in function
  arguments, such as Rust and OCaml, now transmit taint when they are sources.
  This works with nested patterns too. For instance, in Rust:
  ```
  fn f ((x, (y, z)): t) {
    let x = 2;
  }
  ```
  tainting the sole argument to this function will result in all of the identifiers
  `x`, `y`, and `z` now being tainted. (pa-2919)
- Added rule option `interfile: true`, so this can be set under `options:` as it
  is the norm for rule options. This rule option shall replace setting `interfile`
  under `metadata`. Metadata is not mean to have any effect on how a rule is run. (pro-94)

### Changed

- Updated semgrep-interfaces, changed `api_scans_findings` to `ci_scan_results`, removed `gitlab_token` field and added `ignores` and `renamed_paths` field to `ci_scan_results`. (app-4252)

### Fixed

- Dockerfile language support: String matching is now done by contents, treating
  the strings `foo`, `'foo'`, or `"foo"` as equal. (gh-8229)
- Fixed error where we were not filtering the logging of a new third party library. (gh-8310)
- Julia: Fixed a bug where try-catch patterns would not match properly.
  Now, you can use an empty try-catch pattern, such as:

  ```
  try
    ...
  catch
    ...
  end
  ```

  to catch _only_ Julia code which does not specify an identifier for the `catch`.

  Otherwise, if you want to match any kind of try-catch, you can specify an ellipsis
  for the catch identifier instead:

  ```
  try
    ...
  catch ...
    ...
  end
  ```

  and this will match any try-catch, including those that do not specify an
  identifier for the `catch`. It is strictly more general than the previous. (pa-2918)

- Rust: Fixed an issue where implicit returns did not allow taint to flow,
  and various other small translation issues that would affect taint. (pa-2936)
- Fixed bug in gradle.lockfile parser where we would error on `empty=` with nothing after it (sc-987)

## [1.32.0](https://github.com/returntocorp/semgrep/releases/tag/v1.32.0) - 2023-07-13

### Added

- feat(docker): Create a semgrep user for our docker container so that people can run it as a non-root user (gh-8116)
- feat(typed metavar): Typed metavariable support for Rust

  Users can create `TypedMetavar` using Rust's type annotation syntax `:`.
  For example, the following rule works for matching `HttpResponseBuilder`
  type of variables:

  ````
  rules:
  - id: no-direct-response-write
    patterns:
    - pattern: '($BUILDER : HttpResponseBuilder).body(...)'
    - pattern-not: '($BUILDER : HttpResponseBuilder).body("...".to_string())'
    message: find dangerous codes
    severity: WARNING
    languages: [rust]
  ``` (gh-8200)
  ````

### Fixed

- baseline scans reporting on existing findings (baseline-supply-chain)
- Fixed an issue leading to incorrect autofix results involving JS/TS async arrow functions (e.g. `async () => {}`, etc.). (gh-7353)
- Workaround for rootless containers as git operations may fail due to dubious ownership of /src (gh-8267)

## [1.31.2](https://github.com/returntocorp/semgrep/releases/tag/v1.31.2) - 2023-07-07

No significant changes.

## [1.31.1](https://github.com/returntocorp/semgrep/releases/tag/v1.31.1) - 2023-07-07

No significant changes.

## [1.31.0](https://github.com/returntocorp/semgrep/releases/tag/v1.31.0) - 2023-07-07

### Added

- Make CLI hit the new endpoint for the reliable fixed status on the Semgrep app. (cod-16)
- feat(rule syntax): Metavariable Type Extension for Semgrep Rule Syntax 2.0

  This PR introduces the changes made in Semgrep rule syntax 1.0 to version 2.0 as well.

  # rule syntax 2.0

  rules:

  - id: no-string-eqeq
    message: find errors
    severity: WARNING
    languages:
    - java
      match:
      all: - not: null == (String $Y) - $X == (String $Y)

  # rule syntax 2.0 after proposed change

  rules:

  - id: no-string-eqeq
    message: find errors
    severity: WARNING
    languages:
    - java
      match:
      all: - not: null == $Y - $X == $Y
      where: - metavariable: $Y
      type: String (gh-8183)

- Rust: Added the ability to taint macro calls through its arguments, in macro calls
  with multiple arguments. (pa-2902)
- Add severity and suggested upgrade versions to Supply Chain findings (sc-772)
- Added support for pnpm lockfile versions >= 6.0 (sc-824)
- (sc-866)

### Fixed

- Fixed an issue leading to incorrect autofix results involving JS/TS arrow functions (e.g. `() => {}`). (gh-7353)
- Dockerfile support: single-quoted strings are now parsed without an error. (gh-7780)
- Fixes Go issue with patterns like make(...); make(...,$X); make($A,$B). (gh-8171)
- Fixed rust attribute patterns to allow matching on simple attribute syntax. (pa-2903)
- Rust: Fixed a bug where standalone metavariable patterns
  were not matching as expected (pa-2915)
- Fixed python semgrep pattern parsing to also parse match statements, by chaining in the python tree-sitter parser, and adding metavariable support to the python tree-sitter parser. (pa-6442)
- poetry.lock parsing: handle empty toml tables, quoted table keys, and arbitrarily placed comments (sc-834)

## [1.30.0](https://github.com/returntocorp/semgrep/releases/tag/v1.30.0) - 2023-06-28

### Added

- feat(rule syntax): Support metavariable-type field for Kotlin, Go, Scala

  `metavariable-type` field is now supported for Kotlin, Go and Scala. (gh-8147)

- feat(rule syntax): Support metavariable-type field for csharp, typescript, php, rust

  `metavariable-type` field is now supported for csharp, typescript, php, rust. (gh-8164)

- Pattern syntax: You may now introduce metavariables from parts of regular
  expressions using `pattern-regex`, by using regular expression with
  named capturing groups (see https://www.regular-expressions.info/named.html)

  Now, such capture group metavariables must be explicitly named.
  So for instance, the pattern:

  ```
  pattern-regex: "foo-(?P<X>.*)"
  ```

  binds what is matched by the capture group to the metavariable `$X`,
  which can be used as normal.

  `pattern-regex` patterns with capture groups, such
  as

  ```
  pattern-regex: "(.*)"
  ```

  will still introduce metavariables of the form `$1`, `$2`, etc, but this
  should be considered deprecated behavior, and that functionality will be
  taken away in a future release. Named capturing groups should be primarily
  used, instead. (pa-2765)

- Rule syntax: Errors during rule parsing are now better. For instance,
  parsing will now complain if you miss a hyphen in a list of patterns,
  or if you try to give a string to `patterns` or `pattern-either`. (pa-2877)
- JS/TS: Now, patterns of records with ellipses, like:
  ```
  { $X: ... }
  ```
  properly match to records of anonymous functions, like:
  ````
  {
    func: () => { return 1; }
  }
  ``` (pa-2878)
  ````

### Changed

- engine: Removed matching cache optimization which had been previously disabled by
  default in 1.22.0 (we got no reports of any performance regression during this time). (cleanup-1)

### Fixed

- Language server no longer crashes when a user is logged in and opens a non git repo folder (pa-2886)
- It is not required anymore to have semgrep (and pysemgrep) in the PATH. (pa-2895)

## [1.29.0](https://github.com/returntocorp/semgrep/releases/tag/v1.29.0) - 2023-06-26

### Added

- feat(rule syntax): Metavariable Type Extension for Semgrep Rule Syntax

  We've added a dedicated field for annotating the type information of
  metavariables. By adopting this approach, instead of relying solely on
  language-specific casting syntax, we provide an additional way to enhance
  the overall usability by eliminating the need to write redundant type cast
  expressions for a single metavariable.

  Moreover, the new syntax brings other benefits, including improved support for
  target languages that lack built-in casting syntax. It also promotes a unified
  approach to expressing type, pattern, and regex constraints for metavariables,
  resulting in improved consistency across rule definitions.

  Current syntax:

  ```
  rules:
    - id: no-string-eqeq
      severity: WARNING
      message: find errors
      languages:
        - java
      patterns:
        - pattern-not: null == (String $Y)
        - pattern: $X == (String $Y)
  ```

  Added syntax:

  ````
  rules:
    - id: no-string-eqeq
      severity: WARNING
      message: find errors
      languages:
        - java
      patterns:
        - pattern-not: null == $Y
        - pattern: $X == $Y
        - metavariable-type:
            metavariable: $Y
            type: String
  ``` (gh-8119)
  ````

- feat(rule syntax): Support metavariable-type field for Python

  `metavariable-type` field is now supported for Python too. (gh-8126)

- New --experimental flag to switch to a new implementation of Semgrep entirely
  written in OCaml with faster startup time, incremental display of matches,
  AST and registry caching, a new interactive mode and more. Not all
  features of the legacy Python Semgrep have been ported though. (osemgrep)
- Matching: Writing a pattern which is a sequence of statements, such as
  ```
  foo();
  ...
  bar();
  ```
  now allows matching to sequences of statements within objects, classes,
  and related language constructs, in all languages. (pa-2754)

### Changed

- taint-mode: Several improvements to `taint_assume_safe_{booleans,numbers}` options.
  Most notably, we will now use type info provided by explicit type casts, and we will
  also use const-prop info to infer types. (pa-2777)

### Fixed

- Added support for post-pip0614 decorators; now semgrep accepts decorators of
  the form `@ named_expr_test NEWLINE`, so for example with the pattern
  `lambda $X:$X($X)`:

  ````python
  #match 1
  @omega := lambda ha:ha(ha)
  def func():
    return None

  #match 2
  @omega[lambda a:a(a)].a.b.c.f("wahoo")
  def fun():
    return None
  ``` (gh-4946)
  ````

- Fixed a typing issue with go; where semgrep with the pattern
  '($VAR : *tau.rho).$F()` wouldn't produce a match in the
  following:

  ```go
  func f() {
    i_1 := &tau.rho{}
    i_2 := new(tau.rho)

    i_1.shift() //miss one
    i_2.left()  //miss two

    return 101
  }
  ```

  but now we don't miss those two findings! (gh-6733)

- Constant propagation is now applied to stack array declarations in C; so
  a pattern `$TYPE $NAME[101];` will now produce two matches in the following snippet:

  ````c
  int main() {

    int bad_len = 101;
    /* match 1 */
    int arr1[101];
    /* match 2 */
    int arr2[bad_len];
    return 0;
  }
  ``` (gh-8037)
  ````

- Solidity: allow metavariables for version, as in `pragma solidity >= $VER;` (gh-8104)
- Added support for parsing patterns of the form
  ```
  #[Attr1]
  #[Attr2]
  ```
  In code such as
  ```
  #[Attr1]
  #[Attr2]
  function test ()
  {
      echo "Test";
  }
  ```
  Previously, to match against multiple attributes it was required to write
  ````
  #[Attr1, Attr2]
  ``` (pa-7398)
  ````

## [1.28.0](https://github.com/returntocorp/semgrep/releases/tag/v1.28.0) - 2023-06-21

### Added

- Added lone decorators as a valid Python semgrep pattern, so for example `$NAME($X)` will
  generate two seperate findings here:
  ````
  @hello("world")
  @hi("semgrep!")
  def shift():
    return "left!"
  ``` (gh-4722)
  ````
- Add tags to the python wheel for 3.10 and 3.11 (gh-8040)
- JS/TS: Patterns for class properties can now have the `static`
  and `async` modifiers.

  For instance:

  ```
  @Foo(...)
  async bar(...) {
    ...
  }
  ```

  or

  ````
  @Foo(...)
  static bar(...) {
    ...
  }
  ``` (pa-2675)
  ````

- Semgrep Language Server now supports multi-folder workspaces (pa-2772)
- New pre-commit hook `semgrep-ci` to use CI rules in pre-commit, which will pull from the rule board + block those in the block column (pa-2795)
- Added support for date comparison and functionality to get current date.
  Currently this requires date strings to be in the format "yyyy-mm-dd" next step is to support other formats. (pa-7992)

### Changed

- The output of `--debug` will be much less verbose by default, it will only show
  internal warning and error messages. (debug-1)
- Updated the maximum number of cores autodetected to 16 to prevent overloading on large machines when users do not specify number of jobs themselves (pa-2807)

### Fixed

- taint analysis: Improve handling of dataflow for tainted value propagation in class field definitions

  This change resolves an issue where dataflow was not correctly accounted for
  when tainted values flowed through field definitions in class/object
  definitions. For instance, in Kotlin or Scala, singleton objects are commonly
  used to encapsulate executable logic, where each field definition behaves like
  a statement during object initialization. In order to handle this scenario, we
  have introduced an additional step to analyze a sequence of field definitions
  as a sequence of statements for taint analysis. This enhancement allows us to
  accurately track tainted values during object initialization. (gh-7742)

- Allow any characters in file paths used to create dotted rule IDs. File path
  characters that aren't allowed in rule IDs are simply removed. For example, a
  rule whose ID is `my-rule` found in the file `hello/@world/rules.yaml`
  becomes `hello.world.my-rule`. (gh-8057)
- Diff aware scans now work when git state isn't clean (pa-2795)

## [1.27.0](https://github.com/returntocorp/semgrep/releases/tag/v1.27.0) - 2023-06-13

### Added

- PHP: Added composer ecosystem parser (gh-7734)
- Pro: taint-mode: Java: Semgrep can now relate Java properties and their corresponding
  getters/setters even when these are autogenerated (so the actual getters/setters are
  not declared in the sources). (pa-2833)

### Fixed

- semgrep-core now validates rule IDs. This should not affect users since rule
  ID validation is done by the Python wrapper. (gh-8026)

## [1.26.0](https://github.com/returntocorp/semgrep/releases/tag/v1.26.0) - 2023-06-09

### Added

- In Java, Semgrep can now track taint through more getters and setters. It could already relate setters to getters (e.g. `o.setX(taint); o.getX()` but now it can relate setters and getters to properties (e.g. `o.setX(taint); o.x`). (getters)
- taint-mode: Added experimental options `taint_assume_safe_booleans` and
  `taint_assume_safe_numbers` to avoid propagating taint coming from expressions
  with Boolean or number (integer, float) types. (pa-2777)

### Fixed

- swift: Support if let shorthand for shadowing an existing optional variable. (gh-7583)
- Elixir: fix the string extraction used for -filter_irrelevant_rules (gh-7855)
- Fixed comparison of taint information that was causing duplicate taints to be tracked.
  Interfile analysis on large repos will see a small speedup. (misc-1)
- taint-mode: Fixed performance regression in 1.24.0 that affected taint rules. (pa-2777-1)
- Fix a recent regression that caused failures to match in certain cases that combined metavariable-regex and typed metavariables which themselves contain metavariables (e.g. in Go `($X: $T)` with a `metavariable-regex` operating on `$T`). (pa-2822)
- Gomod comments: fix parsing comments that end in ')' (sc-716)

## [1.25.0](https://github.com/returntocorp/semgrep/releases/tag/v1.25.0) - 2023-06-06

### Added

- aliengrep: new option 'generic_caseless' to achieve case-insensitive matching (gh-7883)
- Semgrep now includes heuristics based on the Java standard library and common naming patterns. These allow Semgrep to determine the types of more expressions in Java, for use with typed metavariables (https://semgrep.dev/docs/writing-rules/pattern-syntax/#typed-metavariables). (heuristics)
- Language server now supports search (and replace) with semgrep patterns through semgrep/search (ls-search)
- Language Server will now notify users of errors, and reason for crash (pa-2791)

### Fixed

- Pro (taint analysis): Check function calls without parameters or parenthesis in Ruby (gh-7787)
- Aliengrep: ellipsis patterns that would be useless because of being placed
  at the extremity of a pattern (always) or a line (in single-mode) are now
  anchored to the beginning/end of input/line. For example, `...` in multiline
  mode matches the whole input rather than matching nothing many times. (gh-7881)
- Fixed bug in constant propagation that made Semgrep fail to compute the value of
  an integer constant when this was obtained via the multiplication of two other
  constants. (gh-7893)
- Fix regexps potentially vulnerable to ReDoS attacks in Python code for parsing
  git URLs. Sets maximum length of git URLs to 1024 characters since parsing is
  still perceptibly slow on 5000-byte input. Reported by Sebastian Chnelik,
  PyUp.io. (gh-7943)

## [1.24.1](https://github.com/returntocorp/semgrep/releases/tag/v1.24.1) - 2023-06-01

### Fixed

- Yarn v1: fix parsing for package headers without version constraints (sc-749)

## [1.24.0](https://github.com/returntocorp/semgrep/releases/tag/v1.24.0) - 2023-05-31

### Added

- New experimental aliengrep engine that can be used as an alternative to the
  default spacegrep engine with `options.generic_engine: aliengrep`. (aliengrep)
- Pro: Taint labels now mostly work interprocedurally, except for labeled propagators.
  Note that taint labels are experimental! (pa-2507)
- Pro: Taint-mode now supports inter-procedural field-sensitivity for JS/TS.

  For example, given this class:

  ```javascript
  class Obj {
    constructor(x, y) {
      this.x = x;
      this.y = y;
    }
  }
  ```

  Semgrep knows that an object constructed by `new Obj("tainted", "safe")` has its
  `x` attribute tainted, whereas its `y` attribute is safe. (pa-2570)

### Changed

- Set limits to the amount of taint that is tracked by Semgrep to prevent perf
  issues. (pa-2570)

### Fixed

- Allow symbolic propagation for rvals in lhs of assignments. (gh-6780)
- XML: you can now use metavariable-comparison on XML attributes or XML text body (gh-7709)
- Java: support for record patterns (gh-7911)
- C#: support ellipsis in enum declarations (gh-7914)
- Fixed a recent regression which caused typed metavariables to fail to match when
  the type itself also contained a metavariable, and the target was a builtin
  type. For example, the pattern `(List<$T> $X)` would fail to match a value of
  type `List<String>`. (typed-mvar)

## [1.23.0](https://github.com/returntocorp/semgrep/releases/tag/v1.23.0) - 2023-05-24

### Added

- On scan complete during logged in `semgrep ci` scans, check returned exit code to
  see if should block scans. This is to support incoming features that requires
  information from semgrep.dev (complete)
- Extract mode: users can now choose to include or exclude rules to run on, similar to `paths:`. For example,
  to only run on the rules `example-1` and `example-2`, you would write

  ```
  rules:
     - id: test-rule
       mode: extract
       rules:
          include:
          - example-1
          - example-2
  ```

  To run on everything except `example-1` and `example-2`, you would write

  ````
  rules:
     - id: test-rule
       mode: extract
       rules:
          exclude:
          - example-1
          - example-2
  ``` (gh-7858)
  ````

- Kotlin: Added literal metavariables, from patterns like `"$FOO"`.
  You can still match strings that only contain a single interpolated
  ident by using the brace notation, e.g. `"${FOO}"`. (pa-2755)
- Increase timeout of `semgrep ci` upload findings network calls
  and make said timeout configurable with env var SEMGREP_UPLOAD_FINDINGS_TIMEOUT (timeout)

### Changed

- Relaxed restrictions on symbolic propagation so that symbolic values survive
  branching statements. Now (with symbolic-propagation enabled) `foo(bar())` will
  match match the following code:

  ```python
  def test():
    x = bar()
    if cond:
      exit()
    foo(x)
  ```

  Previously any symbolically propagated value was lost after any kind of branching
  statement. (pa-2739)

### Fixed

- swift: support ellipsis metavariable (gh-7666)
- Scala: You can now put an ellipsis inside of a `catch`, to
  write a pattern like:
  try {
  ...
  } catch {
  ...
  }
  which will match every kind of try-catch. (gh-7807)
- When scanning with `-l dockerfile`, files named `dockerfile` as well as `Dockerfile` will be scanned. (gh-7824)
- Fix for very long runtimes that could happen due to one of our optimizations. We now detect when that might
  happen and skip the optimization. (gh-7839)
- Improve type inference for some simple arithmetic expressions (inference)
- Fixed bug introduced in 1.19.0 that was causing some stack overflows. (pa-2740)

## [1.22.0](https://github.com/returntocorp/semgrep/releases/tag/v1.22.0) - 2023-05-15

### Added

- Add support for language Cairo 1.0 (develop). Thanks to Frostweeds (Romain Jufer) for his contribution! (gh-7757)
- On logged in `semgrep ci` scans, report lockfile parse errors to display in webUI (lockfileparse)
- Pro: Java: Taint-mode can now do field-sensitive analysis of class constructors.
  For example, if the default constructor of a class `C` sets its field `x` to a
  tainted value, given `o = new C()`, Semgrep will know that `o.getX()` is tainted. (pa-2570)
- Kotlin: Added named ellipses, like $...X (pa-2710)
- Kotlin: Interpolated identifiers in strings, such as "$foo", are now properly
  able to match explicitly interpolated expressions, like "${...}". (pa-2711)

### Changed

- Cleanup: Removed Bloom filter optimization. This optimization had been turned off by
  default since September 2022 (release 0.116.0) without any noticeable effect. It had
  its role in the past when it was first introduced, but now it's time for it to go! (cleanup-1)
- engine: The use of a matching cache for statements is now disabled by default,
  please let us know if you notice any performance degradation. We plan to remove
  this optimization in a few weeks. (cleanup-2)

### Fixed

- Enable automatic removal of matched codes by allowing an empty string in the fix field. (gh-6318)
- Updated SARIF to use nested levels, added confidence to tags and included references with markdown links. (gh-7317)
- taint-mode: Fixed bug in taint labels that was causing some fatal errors:
  > Failure "Call AST_utils.with_xxx_equal to avoid this error." (gh-7694)

## [1.21.0](https://github.com/returntocorp/semgrep/releases/tag/v1.21.0) - 2023-05-04

### Added

- Scala: Most Scala 3 features can now be parsed (pa-2748)

### Fixed

- Fixed an issue where parentheses were incorrectly removed when autofixing JS/TS code. (gh-6233)
- Fixed issue where the semgrep language server would crash with "No such file or directory: 'semgrep-core'" (pa-2745)

## [1.20.0](https://github.com/returntocorp/semgrep/releases/tag/v1.20.0) - 2023-04-28

### Added

- Pro: Taint: Added support for simple cases of interprocedural taint labels (pa-2708)
- Language Server has been moved to OCaml core, with major speed improvements (pa-lsp)

### Changed

- Pro: `semgrep --pro` still requires a single target, but this target no longer
  needs to be a directory, it can be an individual file too. (misc-1)
- Partially analyzed files are no longer reported as skipped by --verbose. And if we
  lack info about what lines have been skipped we no longer report that all lines have
  been skipped. That was not accurate. For example, an error while evaluating a
  `metavariable-pattern` operator in one rule may cause a finding to be missed, and
  the file being reported as partially analyzed. However, that error did not affect
  any other rules, and even the affected rule may be able to produce some findings. (pa-2683)

### Fixed

- CLI: Fixed a bug where Git projects with URLs with subgroups would not parse correctly,
  and produce non-clickable links in Semgrep App. These are such as:
  https://gitlab.com/example/group2/group3/test-case.git (pa-2669)
- Taint: Fixed a bug where the new labeled propagators would sometimes not behave properly (pa-2682)
- Swift: Made it so that taint correctly propagates into
  the bodies of lambdas (pa-2718)
- Pro Engine: Fixed a bug where dataflow analysis would sometimes
  loop when analyzing interprocedural `get<name>` methods in a
  loop. (pro-56)
- The scan progress bar no longer gets stuck displaying 0% (zero-progress)

## [1.19.0](https://github.com/returntocorp/semgrep/releases/tag/v1.19.0) - 2023-04-21

### Added

- Java: Private static variables that are defined just once in a static block,
  even if they are not declared `final`, will be considered as `final` by
  constant-propagation. (pa-2228)
- Scala: Can now parse indented matches, like:

  e match
  case foo => "foo"
  case bar => "bar" (pa-2687)

- Scala: Can now parse arguments with `using`, as well as splatted arguments.

  E.g. foo(using bar) and foo(1, 2, bar\*) (pa-2688)

- Scala: Added parsing of `enum` constructs. (pa-2691)
- Scala: Can now parse `given` definitions (pa-2692)
- Scala: Can now parse `export`s (pa-2693)
- Scala: Can now parse top-level definitions (as added in Scala 3) (pa-2694)
- Scala: Can now parse indented `for` expression, such as

  for
  \_ <- 5
  yield
  ... (pa-2695)

- The title of Supply Chain findings will now consist of the package name and CVE,
  instead of just the rule's UUID. (sc-580)

### Changed

- The different lists of skipped files output by Semgrep when given --verbose will
  now be sorted, to make it easier to `diff` the outputs of two runs. (pa-2700)

### Fixed

- CLI: Setting Semgrep-specific environment variables for metadata (like
  SEMGREP_REPO_NAME, SEMGREP_REPO_URL, SEMGREP_PR_ID, and friends) now
  properly works on GitHub and GitLab CI scans.

  If not set, functionality is same as before. (pa-2644)

- CLI: Fixed a bug where repositories with a dot in the name would cause
  semgrep ci scans to crash (pa-2655)

## [1.18.0](https://github.com/returntocorp/semgrep/releases/tag/v1.18.0) - 2023-04-14

### Added

- Metavariable comparison: Added support for \*\*, the exponentiation operator. (gh-7474)
- Pro: Java: Semgrep is now able to track the propagation of taint from the
  arguments of a method, to the object being called. So e.g. given a method

      public void foo(int x) {
          this.x = x;
      }

  and a call `o.foo(tainted)`, Semgrep will be able to track that the field
  `x` of `o` has been tainted. (pa-2570)

- Kotlin: Class fields will now receive the correct types, and be
  found by typed metavariables correctly

  This applies to examples such as:
  class Foo {
  var x : int
  }
  for the variable `x` (pa-2684)

- Supply Chain support for package-lock.json version 3 (sc-586)

### Fixed

- metavariable-pattern: When used with the nested `language` key, if there was an
  error parsing the `metavariable`'s content, that error could abort the analysis
  of the current file. If there were other rules that were going to produce findings
  on that file, those findings were not being reported. (gh-7271)
- Matching: Fixed a bug where explicit casts of expressions would produce two matches to
  other explicit casts.

  So for instance, a pattern `(int $X)` in Java would match twice to `(int) 5`. (gh-7403)

- taint-mode: Given `x = tainted`, then `x.a = safe`, then `x.a.b = tainted`, Semgrep
  did not report `sink(x.a.b)`. Because `x.a` was clean, that made Semgrep disregard
  the tainting of any field of `x.a` such as `x.a.b`. This now works as expected. (pa-2486)
- When using `metavariable-pattern` to match embedded PHP code, Semgrep was
  unconditionally adding the `<?php` opening to the embedded code. When
  `<?php` was already present, this caused parsing errors. (pa-2696)
- Lockfile-only supply chain findings correctly include line numbers in their match data, improving the appearence of CLI output (sc-658)
- Increase timeout for `semgrep install-semgrep-pro` to avoid failures when the download is slow. (timeout)
- Fixed the range reported by findings for YAML files that include an anchor, so that the match does not include the original location of the snippet bound to the anchor. (yaml-alias)

## [1.17.1](https://github.com/returntocorp/semgrep/releases/tag/v1.17.1) - 2023-04-05

### Fixed

- Fix an issue that could lead to a crash when printing findings that contain snippets that look like markup to the Rich Python library (rich-markup-crash)

## [1.17.0](https://github.com/returntocorp/semgrep/releases/tag/v1.17.0) - 2023-04-04

### Added

- Scala: Added proper parsing for Scala 3 style imports (pa-2678)

### Changed

- taint-mode: Added option `taint_assume_safe_comparisons`, disabled by default, that
  prevents comparison operators to propagate taint, so e.g. `tainted != "something"`
  will not be considered tainted. Note that this a syntactic check, if the operator
  is overloaded to perform a different operation this will not be detected. (pa-2645)

### Fixed

- Fixed an issue where incorrect ranges for expressions containing parentheses could lead Semgrep to generate invalid autofixes in Python. (gh-2902)
- In rare cases, Semgrep could generate invalid autofixes where Python keyword arguments were placed before positional arguments. When using AST-based autofix, it no longer makes that error. (keywordarg-autofix)

## [1.16.0](https://github.com/returntocorp/semgrep/releases/tag/v1.16.0) - 2023-03-30

### Added

- Kotlin: Added support for typed metavariables. You can write a pattern like:
  ($X : String)
  to find all instances of expressions with type String. (pa-2648)
- Scala: Semgrep can now parse programs that contain quoted expressions, context
  parameter clauses using `using`, and soft modifiers like `inline` and `open`. (pa-2672)
- Scala: Can now parse programs containing matches on types, such as:
  type t = K match {
  case Int => String
  } (pa-2673)
- Parsing rules can take multiple seconds at the start of a scan.
  When running in an interactive terminal with more than 500 rules,
  Semgrep will show a progress bar for this step. (rule-progress)

### Changed

- Supply Chain scans will now understand `maven_dep_tree.txt` files
  that are made of multiple smaller `maven_dep_tree.txt` files concatenated with `cat`. (maven-dep-forest)
- Findings of a scan are now printed with section headers per group for the following categories:
  Code Blocking, Code Non-blocking, Supply Chain Reachable, Supply Chain Unreachable findings. (results-headings)
- Switched to using go.mod files to read go dependencies for Semgrep Supply Chain, instead of go.sum files (sc-gomod)

### Fixed

- Clojure: parse 'foo/bar' as two separate tokens, so one can use
  metavariable in it and get '$X/bar' to match 'foo/bar' (gh-7311)
- HTML/XML: support attribute as pattern (e.g., `foo="true"`) (gh-7344)
- Improved significantly the time to parse big rulesets such as p/default
  from 20s to a few seconds by parsing patterns lazily and by
  not using /tmp to parse those patterns. (pa-2597)
- Pipfiles with a line comment or inline comment will now parse correctly. (sc-664)

## [1.15.0](https://github.com/returntocorp/semgrep/releases/tag/v1.15.0) - 2023-03-15

### Added

- On full sca scans with dep search feature on, send dependency data for dep search (depsearch)
- metavariable-comparison: Added support for bitwise operators `~`, `&`, `|` and `^`. (gh-7284)
- Taint: `pattern-propagators` now have optional fields `requires` and `label`,
  which are used identically to their counterparts in `pattern-sources` and ` pattern-sinks`, for the experimental taint labels feature.

  For instance, we can define:

  ```
  pattern-propagators:
    - pattern: |
        $TO.foo($FROM)
      from: $FROM
      to: $TO
      requires: A
      replace-labels: [A, C]
      label: B
  ```

  to denote a propagator which only propagates from $FROM to $TO if $FROM has
  taint label A. In addition, it converts any taints from $TO with labels
  A or C to have label B.

  If `label` is not specified, the `to` is tainted with the same label of taint
  that $FROM has. If `requires` is not specified, it does not require $FROM to
  have a particular label of taint.

  Additionally, `replace-labels` only restricts the label being propagated if
  the output `label` is specified. (pa-1633)

- taint-mode: Java: Support for basic field sensitivity via getters and setters.
  Given `obj.setX(tainted)`, Semgrep will identify that a subsequent `obj.getX()`
  carries the same taint as `tainted`. It will also differentiate between
  `obj.getX()` and `obj.getY()`. Note that Semgrep does not examine the definitions
  for the getter or setter methods, and it does not know whether e.g. some other
  method `obj.clearX()` clears the taint that `obj.setX(tainted)` adds. (pa-2585)
- Pro Engine: Semgrep CLI will now download a version of Semgrep Pro Engine
  compatible with the current version of Semgrep CLI, as opposed to the most
  recently released version.

  This behavior is only supported for Semgrep 1.12.1 and later. Previous
  versions will still download the most recently released version, as before. (pa-2595)

### Changed

- Pro: `semgrep ci` will run intrafile interprocedural taint analysis by default
  in differential scans (aka PR scans). (Note that interfile analysis is not run
  in differential scans for performance reasons.) (pa-2565)
- Remove custom _entrypoint_ for returntocorp/semgrep Docker images, now you must
  explicitly call semgrep.

  This won't work now: `docker run -v $(pwd):/src returntocorp/semgrep scan ...`
  Must do this instead: `docker run -v $(pwd):/src returntocorp/semgrep semgrep scan ...` (pa-2642)

- Changed Maven version comparison to more closely reflect usage, so versions with more than 3 increments will not be treated as plain strings (sc-656)

### Fixed

- The AST dump produced by semgrep-core is now usable from Python
  with the provided ATD interface and the Python code derived from it with
  atdpy. (gh-7296)
- Terraform: Nested blocks can now be used as sources and sinks for taint.
  For instance, the block `x` in

  resource $A $B {
  x {
  ...
  }
  } (pa-2475)

- CLI: The scan progress bar now shows progress with higher granularity, and has fewer big jumps when using the Pro Engine.

  The abstract unit of 'tasks' has been removed, and now only a percentage number will be displayed. (pa-2526)

- Fix an error with rule targeting for extract mode. Previously, if a ruleset had
  two rules, the first being the extract rule, the second being the rule to run,
  no rules would run on the extracted targets. Additionally, with multiple rules
  the wrong rule might be run on the extracted target, causing errors. Now, in
  extract mode all the rules for the destination language will be run. (pa-2591)
- Metrics: logged in `semgrep ci` scans now send metrics, as our Privacy.md indicates
  (previously they incorrectly did not, which made it harder for us to track failure events) (pa-2592)
- Rust: Basic let-statement bindings (such as `let x = tainted`) now properly
  carry taint. (pa-2605)
- Improved error reporting for rule parsing by correctly reporting parse errors
  instead of engine errors in certain cases. (pa-2610)
- Taint: Fixed an issue where an error could be thrown if semgrep-core's output
  contained a dataflow trace without a sink. (pa-2625)
- Julia: Properly allow string literal metavariables like "$A" to be patterns. (pa-2630)

## [1.14.0](https://github.com/returntocorp/semgrep/releases/tag/v1.14.0) - 2023-03-01

### Added

- Add new hashes of a match (finding) to send to the app:
  - code_hash
  - pattern_hash
  - start_line_hash
  - end-line_hash (gh-7218)

### Changed

- taint-mode: Historically, the matching of taint sinks has been somewhat imprecise.
  For example, `sink(ok if tainted else ok)` was flagged. Recently, we made sink-
  matching more precise for sinks like `sink(...)` declaring that any argument of
  a given function is a sink. Now we make it more precise when specific arguments of
  a function are sinks, like:

  ```yaml
  pattern-sinks:
    - patterns:
        - pattern: sink($X, ...)
        - focus-metavariable: $X
  ```

  So `sink(ok1 if tainted else ok2)`, `sink(not_a_propagator(tainted))`, and
  `sink(some_array[tainted])`, will not be reported as findings. (pa-2477)

- The `--gitlab-sast` and `--gitlab-secrets` output formats have been upgraded.
  The output is now valid with the GitLab v15 schema,
  while staying valid with the GitLab v14 schema as well.
  Code findings now include the confidence of the rule.
  Supply Chain findings now include the exposure type. (sc-635)

### Fixed

- Fix: Semgrep Pro previously reported a crash for user errors such as
  invalid patterns. It will now give a good error message. (gh-7028)
- Dataflow: Fixed incorrect translation of side-effects inside Boolean expressions,
  this was (for example) causing `if (cond && x = 42) S1; S2` to be interpreted as
  `x = 42; if (cond && x) S1; S2`, thus incorrectly flagging `x` as a constant
  inside S2. (gh-7199)
- Solidity: support enum and event patterns (gh-7230)
- Kotlin: allow to match extended class in any order
  (e.g., the pattern `class $X : Foo` will also match `class Stuff : Bar, Foo`). (gh-7248)
- taint-mode: Code such as `sink(sanitizer(source) if source else ok)` will not be
  incorrectly reported as a tainted sink. This follows a previous attempt at fixing
  these issues in version 1.1.0. (pa-2509)
- metavariable-pattern: Fixed regression introduced in version 1.9.0 that broke
  the use of `pattern-not` within `metavariable-pattern` in some cases. (pa-2510)
- Make Semgrep parse HTML-style comments in JavaScript code. (pa-2560)
- Reduced peak memory usage during inter-file analysis (pa-2563)
- Supply Chain scans on a project using Pipenv
  will now detect transitivity from the committed Pipfile,
  instead of printing an error while trying to parse it. (pa-2577)
- `--oss-only` previously required `--oss-only true` to be passed. This PR fixes
  it so that `--oss-only` will invoke the oss engine. Note that `--oss-only true`
  will no longer be supported (pa-2587)

## [1.13.0](https://github.com/returntocorp/semgrep/releases/tag/v1.13.0) - 2023-02-24

### Added

- Detect `BITBUCKET_TOKEN` from environment to authenticate with the Bitbucket API. (app-3691)
- taint-mode: Taint propagators can now specify `by-side-effect`, just like sources and
  sanitizers. However, the default value of `by-side-effect` for propagators is `true`
  (unlike for sources or sanitizers). When using rule option
  `taint_assume_safe_functions: true`, this allows to specify functions that must
  propagate taint, for example:
  ```yaml
  pattern-propagators:
    - by-side-effect: false
      patterns:
        - pattern-inside: $F(..., $X, ...)
        - focus-metavariable: $F
        - pattern-either:
            - pattern: unsafe_function
      from: $X
      to: $F
  ```
  Without `by-side-effect: true`, `unsafe_function` itself would be tainted by side-
  effect, and subsequent invokations of this function, even if the arguments were safe,
  would be tainted. (pa-2400)
- CLI: SARIF output now includes a tag pertaining to which card of the Rule Board a rule originated from.
  This can be "rule-board-block", "rule-board-audit", or "rule-board-pr-comments". (pa-2519)

### Fixed

- Fixed a couple of typos in help/usage. (gh-7184)
- Matching: Fixed a bug where expressions would not match to explicit type casts of matching expressions (pa-1133)
- Golang: Fixed a bug where metavariable ellipses as parameters to functions were not working properly (pa-2545)
- Apex: Fix name resolution of class attributes. Among other things, this will allow
  Semgrep to perform constant propagation of `final` class attributes.
  See https://semgrep.dev/s/DG6v. (pa-2551)
- Go: Fixed a bug where function arguments in a group that share the same type,
  such as "func foo(x, y, z int)", would parse all arguments after the first as
  having the name ",". For instance, "y" and "z" here would not have the correct
  names. (pa-2558)
- Fixed bug in comparison of Maven versions where multi digit versions would cause a default to raw string comparison (sc-maven-cmp-bug)

## [1.12.1](https://github.com/returntocorp/semgrep/releases/tag/v1.12.1) - 2023-02-17

### Fixed

- Fix local scan hyperlinks by asking git for remote.origin.url if repo_url not provided (gh-7144)
- Improve error messages displayed with `--verbose` when the contents of a metavariable fails to parse. (pa-2537)
- Fixed parsing bug maven_dep_tree.txt files where dependency specs with 6 fields, or suffixed with `(optional)` would fail to parse (sc-622)
- Supply Chain rules now correctly understand Maven version strings, as described in https://docs.oracle.com/middleware/1212/core/MAVEN/maven_version.htm#MAVEN8855 (sc-maven-versions)
- package-lock.json files which contain `"resolved": false` as a result of a bug in NPM will now parse (sc-npm-bug)

## [1.12.0](https://github.com/returntocorp/semgrep/releases/tag/v1.12.0) - 2023-02-13

### Fixed

- CLI: No longer reports rules as being run with a lack of `interfile: true` when interfile
  analysis was not requested. (pa-2528)
- The 1.11.0 release started printing log lines to stderr even when --quiet was
  on, making it impossible to get well-formed JSON output when mixing stdout and
  stderr. These lines are now gone, and output is again restricted to just scan
  results. (quiet-please)
- Output lines in GitHub Actions logs could appear scrambled, due to GitHub
  Actions mixing together the stdout and stderr streams in non-deterministic
  order. Semgrep will now log everything only to one stream in GitHub Actions
  when using text output mode, which ensures lines no longer appear scrambled.
  (sc-607)

## [1.11.0](https://github.com/returntocorp/semgrep/releases/tag/v1.11.0) - 2023-02-10

### Changed

- Pro: Removed already deprecated flags `--deep` (now `--pro`), `--interfile` (now `--pro`),
  and `--interproc` (now `--pro-intrafile`). Also removed already deprecated command
  `install-deep-semgrep` (now `install-semgrep-pro`). (pa-2518)

### Fixed

- Go: Fixed a bug where the location of Go raw string literals were not being reported correctly.
  This could cause issues with Playground range highlighting and autofix (pa-2206)
- CLI: Progress bar for Semgrep Pro Engine interfile scans now reflects actual progress more faithfully (pa-2313)
- Pro: We now check the downloaded binary before installing it, this should prevent
  installation of corrupted binaries due to errors during the download. (pa-2492)
- Fix the location associated with metavariables bound inside of
  `metavariable-pattern` clauses, which could lead to matches being incorrectly
  displayed. (extracted-metavar-loc)
- CLI: Fixed bug with `semgrep --validate` for metavariables like $1, $2.
  Previously, it blocked rules that it shouldn't. (validate-regex-mvar)

## [1.10.0](https://github.com/returntocorp/semgrep/releases/tag/v1.10.0) - 2023-02-08

### Added

- Experimental support for XML (gh-5939)
- Rust: Beta support for Rust. (gh-6545)
- Rule syntax: Metavariable bindings bound within `metavariable-pattern` now persist to outside of the `metavariable-pattern` (pa-2490)
- Updated all lockfile parsers (except Cargo.lock) to produce better error messages, at the cost of a couple seconds of lowdown on large (>10k lines) lockfiles (sc-better-parsers)
- Allow metavariable-pattern clauses that use `language: generic` to run (and
  potentially match) on any metavariable binding kind, not just strings. For
  example, with the pattern `foo($...ARGS)`, it is now possible to use a
  `metavariable-pattern` on `$...ARGS` with `language: generic`, and match using
  generic mode against whatever text `$...ARGS` is bound to.
  (metavar-pattern-generic)
- Print handy links to review results on Semgrep App when a CI scan uploads
  findings. (sc-564)
- Allow metavariable ellipsis in template literals, e.g. `$...X${1 + 2}`
  (template-metavar-ellipsis)

### Changed

- Pro: `--pro` will now enable _all_ Pro features, including Apex, inter-procedural taint
  analysis, and also inter-file analysis for supported languages. For Apex support only
  (and more languages in the future) now use `--pro-languages`. For intra-file analysis
  only now use `--pro-intrafile`. Flags `--interproc` and `--interfile` are now
  deprecated. (pa-2488)
- The output formatting of semgrep ci is getting revamped in the coming weeks.
  This release includes the first couple changes to the output. (sc-590)
- Packages from the maven ecosystem are now parsed to include their org slug. This means a log4j rule must now use `org.apache.logging.log4j:log4j-core` instead of just `log4j-core`. This change is backwards incompatible, in that any Java Supply Chain rules not taking into account will stop producing any findings, since the packages parsed from lockfiles will include the org, but the old rules will not. (sc-maven-org)
- Add the engine requested by the user to the metrics (pa-2427)

### Fixed

- Rust: correctly parse the last expression in blocks (gh-7071)
- Dataflow traces: Findings now always display the separating line with `--dataflow-traces` in the CLI, to reduce
  confusion over where the findings fall between the dataflow traces. (pa-2471)
- CLI: Added `install-semgrep-pro` to the list of commands in the `semgrep --help` help text. (pa-2505)
- Fixed bug where gradle.lockfile files would fail to parse if they contained a trailing newline, and bug where an error on a trailing newline would cause our lockfile parse error pretty printing to fail (sc-trailing-newline)

## [1.9.0](https://github.com/returntocorp/semgrep/releases/tag/v1.9.0) - 2023-02-01

### Added

- Pro: If the "Semgrep Pro Engine" toggle is enabled in App, `semgrep ci` will add
  support for Apex in all scans (including diff scans). (pa-2462)

### Fixed

- Fix incorrectly reformatted Bitbucket repository URLs (app-3279)

## [1.8.0](https://github.com/returntocorp/semgrep/releases/tag/v1.8.0) - 2023-02-01

### Added

- Experimental support for Clojure, Lisp, and Scheme. (gh-3328)
- Experimental support for Jsonnet (writing semgrep rules to find
  bugs/security-isses/... in jsonnet files). (pa-1849)

### Fixed

- Regexps within string patterns such as `"=~/hello/"` now support the
  full PCRE syntax, including backreferences and other advanced features
  not supported by ocaml-re. (gh-6913)
- Removed new lockfile parsers for all lockfiles except requirements.txt and maven_dep_tree.txt, for performance reasons (sc-557)

## [1.6.0](https://github.com/returntocorp/semgrep/releases/tag/v1.6.0) - 2023-01-27

### Added

- Added exposure property to sarif output for sca findings (sarif)

### Changed

- Tests: Allow `--test` to process entire file trees rather than single files (gh-5487)
- metavariable-pattern: For performance reasons Generic mode ignores target files
  that look like machine generated. Unfortunately, this also prevented using the
  `metavariable-pattern` operator on text that may look like (or in fact be)
  machine generated, such as an RSA key contained in a legit file. Now, when the
  analysis is requested within a `metavariable-pattern` operator, Generic mode
  will always match any text even if it looks like machine generated. (pa-2386)
- Pro: Add **experimental** flags `--pro` and `--interproc`. Using `--pro` you can
  enable Apex support, and with `--fast-deep` you can enable intra-file inter-procedural
  taint analysis. Also, `--deep` has been renamed to `--interfile`. Note that to use
  any of the Pro features you must first run `semgrep install-semgrep-pro` while being
  logged in. (pa-2440)
- New lockfile parsers with better error messages for all supported ecosystems except Rust (sc-495)

### Fixed

- Solidity: parse correctly 'unchecked' blocks (gh-6055)
- Terraform: Implicit coercions between strings, bools, ints, and floats are now allowed to match. (gh-6898)
- Python: Taint now propagates via the splat operators `*` and `**`, thus both
  `sink(*tainted)` and `sink(**tainted)` will result in findings. (gh-6920)
- Solidity: support ellipsis in contract inheritance
  Thanks to Raz0r for his contribution! (gh-6935)
- CLI: No longer fails when running tests against a config file with no rules in it (gh-6954)
- Fixed a crash that could occur when using the GitHub API to fetch the merge base rather than computing it locally. (merge-base)
- JS/TS: Object types now support metavariables properly (pa-2414)
- CLI: No longer reports the wrong command if you are using the `semgrep-core-proprietary` executable. (pa-2417)
- Pro: Findings that occur due to rules run with the Semgrep Pro Engine are now displayed separately in the CLI. (pa-2432)
- Fixed bug where Semgrep would generate reachable findings for transitive dependencies, even if a direct dependency at a safe version exists (sc-547)
- Fixed bug in poetry.lock parser where quoted square brackets could cause failures (sc-548)
- Fixed bug where Semgrep would fail to generate unreachable findings for a particular library if a reachable finding for that library had already been found in the same lockfile, even if there are multiple copies of this library at different vulnerable versions. (sc-549)
- Fixed bug where npm lockfiles with packages that listed multiples hashes would fail to parse (sc-550)

## [1.5.1](https://github.com/returntocorp/semgrep/releases/tag/v1.5.1) - 2023-01-20

### Added

- Python: Constant propagation will now recognize the idiom `cond and X or Y`,
  as well as `True and X` and `False or X`. So e.g. `cond and "a" or "b"` will
  be identified as a constant string. (gh-6079)
- Julia: Julia is now experimental (pa-2366)

### Changed

- DeepSemgrep is now Semgrep PRO! To install the Semgrep PRO engine run:
  `semgrep install-semgrep-pro`. This engine is still invoked using the
  `--deep` flag, but please expect changes to the CLI in the near future.
  The new Semgrep PRO engine adds support for Apex! (pa-2389)

### Fixed

- New 'transform:' field in extract mode rules, with 'concat_json_string_array'
  option useful to extract python code from jupyter notebooks.
  Thanks to Jose Selvi for his contribution! (gh-4477)
- Java: Fixed regression introduced in 0.123.0 that could cause a private class
  attribute to be incorrectly regarded as a constant. (gh-6793)
- Make `$F(x)` match `eval(x)`. Previously, `eval` was special-cased and metavariable function call patterns would not match it. (gh-6877)
- DeepSemgrep: Enabled `--dataflow-traces` by default when `--deep` is specified (pa-2274)
- In rare situations, mainly in DeepSemgrep and related to naming bugs, the use of
  symbolic propagation could make Semgrep fall into an infinite loop during matching.
  This has been fixed by bounding the number of times that Semgrep can follow
  symbolically-propagated values. (pa-2324)
- CLI: Made an error message for when two autofix matches overlap have a more helpful message, as well as be displayed as a debug message. (pa-2393)
- CLI: Made the warning message when using Semgrep Pro more friendly when logged in (pa-2396)

## [1.3.0](https://github.com/returntocorp/semgrep/releases/tag/v1.3.0) - 2023-01-04

### Changed

- Removed the poor support for reading dependencies from pom.xml files. Instead semgrep will try to read dependencies from a maven_dep_tree.txt file,
  which can be generated using the command `mvn dependency:tree -DoutputFile=maven_dep_tree.txt` (sc-pom)

### Fixed

- Use the GitHub REST API when possible to compute the merge base for `semgrep ci`, improving performance on shallow clones of large repositories. (gha-mergebase)
- YAML: Fixed a bug where metavariables matching YAML double-quoted strings would not capture the entire range of the string, and would
  not contain the double-quotes. Also added the ability to properly use patterns like `"$FOO"`, which will unpack the contents of the matched string. (pa-2332)
- Fixed a race condition related to the parsing cache that could lead to internal errors (pa-2335)
- YAML: Fixed a bug where literal or folded blocks would not be parsed properly.

  So for instance, in:

  ```
  key: |
    string goes here
  ```

  A metavariable matching the contents of the string value might not be correct. (pa-2347)

- Julia: Greatly improved parsing support (pa-2362)

## [1.2.1](https://github.com/returntocorp/semgrep/releases/tag/v1.2.1) - 2022-12-16

### Fixed

- Go: fix NoTokenLocation for metavariables matching function type without
  an argument (e.g. `func()`) (gh-6715)
- typed-metavariables: handle ternary expressions so we can type correctly
  expressions like `foo(cond ? new A() : this.a)` (pa-2328)
- Reverted a change which caused findings with different sources (but the same sink) to be deduplicated. This would cause findings which
  looked identical in range and data, but had different taint traces. (pa-2336)

## [1.2.0](https://github.com/returntocorp/semgrep/releases/tag/v1.2.0) - 2022-12-15

### Fixed

- Fixed rare crash that could occur due to stale file caches when temp file names overlap (cache-invalidation)
- PHP: support metavariables in string (as in `foo("$VAR")`) (gh-6311)
- Java: support static block patterns (gh-6366)
- Rust: parse correctly scoped identifier in constructor (gh-6594)
- Java: support `super(...)` patterns (gh-6638)
- C#: bugfix on bad interaction between -fast and the internal code generated
  for LINQ queries (gh-6666)
- Java: support the Java 10 'var' keyword by not using 'var' as a valid type when
  using typed metavariables. (gh-6672)
- When matching module specifiers in imports, prevent metavariables from capturing quotes. (gh-6674)
- Swift: support complex expression in switch case (gh-6704)
- Constant propagation: Fixed a bug where constant propagation would only run within functions. Now,
  it runs on the top-level of the program as well. (pa-1656)
- DeepSemgrep: Fixed a bug where imports which reached type names (among other things) would not resolve properly (pa-2260)
- DeepSemgrep: Fixed a bug which caused Java functions with interfaces to wipe taint (instead of propagating taint by default) (pa-2265)
- Fix matching of qualified names. For example, `new $X.Foo()` will now match
  `new a.b.Foo()`. (pa-2296)
- DeepSemgrep: Fix regression in taint-mode introduced by Semgrep v1.1 that caused
  some findings to be missed. Also, DeepSemgrep will assume, for now, that a method
  call on a tainted object is always tainted. (pa-2304)
- Improved matching behavior on JS `require` calls (require-match)

## [1.1.0](https://github.com/returntocorp/semgrep/releases/tag/v1.1.0) - 2022-12-05

### Added

- JSON output: Added a `max_memory_bytes` field to the `semgrep --time` output which corresponds to the amount of memory allocated during the OCaml phase of Semgrep. This is useful for telemetry purposes. (pa-2075)

### Changed

- Don't print out summary of blocking rules unless running with semgrep ci subcommand (gh-6651)

### Fixed

- taint-mode: In 0.94.0 we made that when a `pattern-source` (or `pattern-sanitizer`)
  matched a variable exactly, this was understood as that variable being tainted
  (sanitized, resp.) by side-effect. For example, given `tainted(x)` we would taint `x`
  by side-effect, and subsequent occurrences of `x` were also considered tainted.
  This allowed to write rules like `c.lang.security.use-after-free.use-after-free`
  in a very succint way, and it also addressed some limitations of the workarounds that
  were being used to simulate this until then.

  This worked well initially, or so we thought, until in 0.113.0 we added
  field-sensitivity to taint-mode, and in subsequent versions we made sources and
  sanitizers apply by side-effect to more kinds of l-values than just simple variables.
  It was then that we started to see regressions that were fairly unintuitive for users.
  For example, if `$_GET['foo']` was a taint source, this would make `$_GET` itself to
  be tainted by side-effect, and a subsequent expression like `$_GET['bar']` was also
  considered tainted.

  We now correct the situation by adding the `by-side-effect` option to sources and
  sanitizers, and requiring this option to be explicitly enabled
  (that is, `by-side-effect: true`) in order to apply the source or the sanitizer by
  side-effect. Otherwise, the default is that sources and sanitizers matching l-values
  apply only to the precise occurrences that they match. (pa-1629)

- taint-mode: Fixed matching of `pattern-sinks` to be more precise, so that e.g.
  it will no longer report `sink(ok1 if tainted else ok2)` as a tainted sink, as
  the expression passed to the `sink` is actually not tainted. (pa-2142)
- CLI: Separated experimental rules from normal rules in `semgrep --debug` output. (pa-2159)
- Taint: Fixed an issue where findings with the same sink would be identified as the same, and cause
  only one of them to be reported, even if they had different sources. (pa-2208)
- DeepSemgrep: When the "DeepSemgrep" setting is enabled in Semgrep App, `semgrep ci`
  will try to run the analysis using the DeepSemgrep engine. But if this engine was
  not installed, `semgrep ci` failed. Now `semgrep ci` will automatically try to
  install DeepSemgrep if it is not already present. Note that, if DeepSemgrep is
  already installed, `semgrep ci` does not attempt to upgrade it to a newer version. (pa-2226)
- CLI: Made the number of jobs when using `semgrep --deep` default to 1. (pa-2231)
- Autofix: If multiple autofixes are targeting an overlapping range, then one of them is picked arbitrarily to occur, to prevent autofixes which may produce incorrect code. (pa-2276)
- DeepSemgrep: Time data now outputs properly when running `semgrep --deep --time` (pa-2280)
- DeepSemgrep: Added a message which suggests that users update their version of DeepSemgrep, if the DeepSemgrep binary crashes (pa-2283)
- Yarn 2 parse failure on versions like @storybook/react-docgen-typescript-plugin@canary. This is only present as some kind special version range specifier and never appears as a concrete version. It would only be used to check if the dependency was in the manifest file, so we just parse the version as "canary"
  Yarn 2 parse failure on versions like @types/ol-ext@npm:@siedlerchr/types-ol-ext@3.0.6
  Yarn 2 parse failure on versions like resolve@patch:resolve@^1.1.7#~builtin<compat/resolve>. These are now just ignored, as they appear to always come with a non-patch version as well. (sc-406)

## [1.0.0](https://github.com/returntocorp/semgrep/releases/tag/v1.0.0) - 2022-12-01

### Added

- DeepSemgrep: If you have a Team tier account in Semgrep App, and you enable the
  _DeepSemgrep_ setting, then `semgrep ci` will automatically run the DeepSemgrep
  engine instead of the OSS engine on full scans (but not in PR scans). (pa-2226)

## [0.123.0](https://github.com/returntocorp/semgrep/releases/tag/v0.123.0) - 2022-11-29

### Added

- CLI: Added deep traces to `--dataflow-traces` (pa-2116)

### Changed

- Reachable Supply Chain findings will no longer block pull requests when using `semgrep ci`.
  Note that unreachable findings have been non-blocking already. (sca-nonblocking)

### Fixed

- Fix matching issue related to JS imports with multiple imported values (gh-5305)
- DeepSemgrep: Keep only the shortest trace originating from a taint source. This
  also prevents falling into infinite loops when inferring taint signatures for
  mutually recursive procedures. (pa-2224)
- taint-mode: Improved taint tracking for array-like accesses. Previously, if
  `x.a.b[i].c` got tainted, Semgrep would track `x.a.b` as tainted, and thus
  `x.a.b[i].d` would be incorrectly considered as tainted too. Now Semgrep will
  do the right thing and track `x.a.b[*].c` as tainted, and `x.a.b[i].d` will
  not be considered tainted. (pa-2225)
- Java: `private`, singly-assigned class variables now permit constant propagation (pa-2230)
- JS/TS: Allow dependencies to @Injectable and @Component classes in Angular JS to be visible outside the scope of the constructor. (pa-2233)
- Fix matching issue related to Python imports with multiple imported values (python-imports)
- Supply Chain findings from a yarn.lock lockfile were marked as 'transitive'
  when we couldn't find the matching package.json file.
  These findings will now be marked as having 'unknown' transitivity. (sc-425)
- Make `$X(...)` match `this()` and `super()`. (this-match)

## [0.122.0](https://github.com/returntocorp/semgrep/releases/tag/v0.122.0) - 2022-11-16

### Fixed

- DeepSemgrep: Added installation path for DeepSemgrep on M1 machines (pa-2153)
- Correctly handle optional name field in yaml1 lockfiles (parse_yaml)

## [0.121.2](https://github.com/returntocorp/semgrep/releases/tag/v0.121.2) - 2022-11-10

### Fixed

- Fix yaml excessive mapping captures (gh-5698)
- Rule syntax: Allow `pattern-not`, `pattern-inside`, and `pattern-not-inside` to take in arbitrary patterns (such as `patterns`, `pattern-either`, and friends) (pa-1723)
- Kotlin: Fixed bug where constructor invocation with a trailing comma would not parse (pa-1988)
- Constant propagation: Type casts and parenthesized expressions (in Go) can now be symbolically propagated. (pa-2054)
- CLI: Added a fix preventing findings from experimental rules being displayed.
  Experimental rules also now no longer run silently. (pa-2121)

## [0.121.1](https://github.com/returntocorp/semgrep/releases/tag/v0.121.1) - 2022-11-08

No significant changes.

## [0.121.0](https://github.com/returntocorp/semgrep/releases/tag/v0.121.0) - 2022-11-07

### Changed

- taint-mode: Semgrep can now track taint through l-values of the form `this.x`.
  It will also be more precise when tracking taint on l-values involving an
  array access, previously if `x.a[i]` was tainted, then `x` itself was tainted;
  now only `x.a` will be considered tainted. (pa-2086)

### Fixed

- JS: correctly parse exported function pattern (gh-6218)
- Terraform: Can now analyze constant-propagated values of variables with `metavariable-comparison` and friends (pa-2088)

## [0.120.0](https://github.com/returntocorp/semgrep/releases/tag/v0.120.0) - 2022-11-02

### Added

- Fail gracefully and print error message when running in unsupported Linux aarch64/arm64 environment (arm-fail)
- Added 'fingerprints' field to results in sarif output (gh-5729)
- Add dataflow traces as 'codeFlows' object in SARIF output (gh-6367)
- Fail immediately if semgrep tries to run a supply chain rule with an invalid version range specifier (ssc-fail-early)
- Added Supply Chain support for requirements.txt lockfiles (with requirement.in manifest files) and Yarn 2/3 lockfiles (ssc-yarn-req)

### Changed

- Change default behavior of CircleCI configurations. If a user manually sets their environment variables (i.e. SEMGREP_REPO_NAME, SEMGREP_REPO_URL, SEMGREP_BRANCH, SEMGREP_JOB_URL, SEMGREP_COMMIT), use it before falling back on autodetection. (app-2434)
- Change default behavior of Buildkite CI configurations. If a user manually sets their environment variables (i.e. SEMGREP_REPO_NAME, SEMGREP_REPO_URL, SEMGREP_BRANCH, SEMGREP_JOB_URL, SEMGREP_COMMIT), use it before falling back on autodetection. (app-2435)
- Change default behavior of bitbucket CI configurations. If a user manually sets their environment variables (i.e. SEMGREP_REPO_NAME, SEMGREP_REPO_URL, SEMGREP_BRANCH, SEMGREP_JOB_URL, SEMGREP_COMMIT), use it before falling back on autodetection. (app-2436)

### Fixed

- Handle unexpected exceptions when performing AST-based autofix. (autofix-exception)
- Fix an issue that could cause AST-based autofix to fail on autofixes containing statements. (autofix-statement-exception)
- Fix an issue preventing AST-based autofix from running in the presence of `focus-metavariable`. (focus-metavariable-autofix)
- Implement string literal metavariables in Python AST-based autofix (gh-3648)
- Go: parse correctly generic types from other packages (gh-5799)
- Fix parsing of dot access ellipsis in PHP patterns (e.g. `$x-> ... ->bar()`). (gh-6183)
- JS/TS: Allowed parsing of patterns using the optional chaining "?." syntax. (gh-6201)
- Dockerfile language: Add support for RUN options such as
  `RUN --mount=type=$TYPE,target=$TARGET ...`. (gh-6353)
- taint-mode: Fixed a bug in the experimental taint-labels feature that caused labels to be incorrectly applied to dot accesses. For instance, if a pattern-source that requires label A and adds label B matches a dot-access expression like x.a, the field a will get the label B even if it does not carry label A as required. (gh-6355)
- Use AST-based autofix when possible for JS autofixes. This is more likely to lead to correct output, especially for complicated fixes. (js-autofix)
- taint-mode: Fixed regression in 0.113.0, after field sensitivity support was added,
  that broke existing behavior when a prefix in a chain of dot-accesses such as
  `x.a.b` was specified as a source/sanitizer/sink. For example, if `x` had been
  previously tainted, then we encountered `sink(x.a.b)` where `x.a` matched a
  sanitizer, there was a finding reported because `x.a.b` was incorrectly considered
  tainted. (pa-1928)
- JS/TS: Fixed a parsing bug where special identifiers were parsed differently in patterns (pa-2030)
- Language server now appropriately applies regex fixes (vscode-regex)

## 0.119.0 - Skipped

Version 0.119.0 of Semgrep was intentionally skipped. Version 0.120.0 immediately follows version 0.118.0.

## [0.118.0](https://github.com/returntocorp/semgrep/releases/tag/v0.118.0) - 2022-10-19

### Added

- Taint mode will now track taint coming from the default values of function
  parameters. For example, given `def test(url = "http://example.com"):`,
  if `"http://example.com"` is a taint source (due to not using TLS), then
  `url` will be marked as tainted during the analysis of `test`. (gh-6298)
- taint-mode: Added two new rule `options` that help minimizing false positives.

  First one is `taint_assume_safe_indexes`, which makes Semgrep assume that an
  array-access expression is safe even if the index expression is tainted. Otherwise
  Semgrep assumes that e.g. `a[i]` is tainted if `i` is tainted, even if `a` is not.
  Enabling this option is recommended for high-signal rules, whereas disabling it
  may be preferred for audit rules. Currently, it is disabled by default for pure
  backwards compatibility reasons, but this may change in the near future after some
  evaluation.

  The other one is `taint_assume_safe_functions`, which makes Semgrep assume that
  function calls do _NOT_ propagate taint from their arguments to their output.
  Otherwise, Semgrep always assumes that functions may propagate taint. This is
  intended to replace _not conflicting_ sanitizers (added in v0.69.0) in the future.
  This option is still experimental and needs to be complemented by other changes
  to be made in future releases. (pa-1541)

### Changed

- Ignore the .npm/ directory by default in Semgrep scans (gh-6315)
- The `--scan-unknown-extensions` option is now set to false by default.
  This means that from now on `--skip-unknown-extensions` is the default.
  This is an important change that prevents many errors when using
  Semgrep in a pre-commit context or in CI. (pa-1932)

### Fixed

- Add autodetection for pull request numbers for Azure Pipelines. If SEMGREP_PR_ID is set, override the autodetection. (app-2083)
- Fixed an autofix regression that caused Semgrep to fail to replace metavariables in string literals, e.g. `foo("xyz $X")`. (autofix-string-metavar)

## [0.117.0](https://github.com/returntocorp/semgrep/releases/tag/v0.117.0) - 2022-10-12

### Added

- taint-mode: It is now possible to use `pattern-propagators` to propagate taint
  through higher-order iterators such as `forEach` in Java. For example:
  ````yaml
    pattern-propagators:
      - pattern: $X.forEach(($Y) -> ...)
        from: $X
        to: $Y
  ``` (gh-5971)
  ````

### Changed

- Change default behavior of Travis CI configurations. If a user manually sets their environment variables (i.e. SEMGREP_REPO_NAME, SEMGREP_REPO_URL, SEMGREP_BRANCH, SEMGREP_JOB_URL, SEMGREP_COMMIT, SEMGREP_PR_ID), use it before falling back on autodetection.

### Fixed

- Scala: Fixed a bug where generators would not parse if newlines were present, in certain cases (pa-1902)
- Fixed bug where nested dependencies in package-lock.json files were not detected (sc-247)
- Removed Gradle as a separate supply chain ecosystem. Maven rules now work on Gradle projects (sc-256)
- Lockfiles are no longer subject to size filtering during file targetting, so very large lockfiles can now generate unreachable findings (sc-293)

## [0.116.0](https://github.com/returntocorp/semgrep/releases/tag/v0.116.0) - 2022-10-06

### Added

- Added support for named arguments in taint tracking. This is only relevant for DeepSemgrep users. (pa-1886)

### Changed

- Change default behavior of Jenkins CI configurations. If a user manually sets their environment variables (i.e. SEMGREP_BRANCH, SEMGREP_JOB_URL, SEMGREP_COMMIT), use it before falling back on autodetection. (app-2432)
- Change default behavior of Azure Pipelines configurations. If a user manually sets their environment variables (i.e. SEMGREP_REPO_NAME, SEMGREP_REPO_URL, SEMGREP_BRANCH, SEMGREP_JOB_URL, SEMGREP_COMMIT), use it before falling back on autodetection. (app-2433)
- taint-mode: Removed experimental poor-man's support for wrapper functions around
  taint sources. This was an early experiment to make Semgrep inter-procedural, but
  it was later abandoned in favor of DeepSemgrep. (pa-1838)
- Disabled Bloom filter optimization by default, due to undesired interactions with
  constant and symbolic propagation, while it appears to not provide a net major
  performance benefit (nowadays). If you do notice a significant drop in performance
  after this change, please let us know. (pa-1927)
- Semgrep-core will no longer accept a rule file containing only one rule object,
  rules must be given in an array unde the `rules:` key. This change does not
  affect Semgrep CLI which never accepted that relaxed format. (pa-1931)
- Changed command line flag for supply chain scans from `--sca` to `--supply-chain`.
  Correspondinly changed `--config sca` to `--config supply-chain` (sca-ssc)

### Fixed

- Change default behavior of Jenkins CI configurations. If the SEMGREP_REPO_URL is set, use it. Otherwise, default to autodetection. (app-2406)
- fix: Ensure the docker image uses the latest base packages (docker)
- Fixed symbolic propagation of the `new` operator, that had been broken since
  version 0.98.0. You can again e.g. use the pattern `new A().foo()` to match
  `a.foo()`, with `a = new A()`. (gh-6161)
- Some crypto code like hashing algorithms can lead to a very large amount of
  symbolically propageted values, which previously caused Semgrep's Bloom filter
  to hang. (pa-1920)
- taint-mode: It is now possible for `this` or `this.x` to be a source of taint. (pa-1929)
- taint-mode: Fixed a bug that made Semgrep miss taint findings when the sink was
  located inside an `if` condition or a `throw` (aka `raise`) expression/statement. (pa-1933)
- Start to use the AST to render autofixes in some specific cases. As this is extended, autofix will become more correct and more powerful. (ast-autofix)
- Fixed a parser error in some package-lock.json files (sca-parse-error)

## [0.115.0](https://github.com/returntocorp/semgrep/releases/tag/v0.115.0) - 2022-09-27

### Added

- Adds support for a .semgrepconfig file. Users can add metadata (such as a list of tags) to the .semgrepconfig YAML file which will automatically be assigned to the project. (app-2112)
- Modify the CLI output to separate non-blocking and blocking findings and show a list of the blocking rules that fired. (app-2306)

### Changed

- generic mode: allow text input without human-readable indentation up to 500
  bytes. This value is subject to change. This relaxing is intended to
  facilitate testing where someone might copy-paste a long line without a
  trailing newline. Semgrep users should not expect files that are not
  human-readable to be processed in semgrep's generic mode, or in any mode for
  the matter. (gh-6071)
- Changed behavior for renamed files on diff scans (scans in which a baseline ref is provided).
  Semgrep will not show old issues to developers when they rename a file now. (gh-6157)

### Fixed

- Fixed nondeterministic failure of test_api test due to invalid settings file by
  configuring home directory to temporary directory. (app-2166)
- Change default behavior of Jenkins CI configurations. If the SEMGREP_REPO_NAME environment variable is set, use it. Otherwise, default autodetection. (app-2331)
- Dockerfile mode: Fix failure to match where image name and image alias should
  be the same. The problem was due to some names and identifiers being
  fragmented due to parsing rules and not pieced back together. (gh-5229)
- Scala: add support for ellipsis in match body (e.g., `$X match { ... }`) (gh-6131)
- Added a fix for a bug involving parsing of TS imports, where they were not allowed to appear as patterns to a rule. (pa-1910)

## [0.114.0](https://github.com/returntocorp/semgrep/releases/tag/v0.114.0) - 2022-09-19

### Added

- Add functionality to exclude rules by id passing it by cli flag --exclude-rule (cli-2530)
- Fixes https://github.com/returntocorp/semgrep/issues/5686.
  You can now have multiple metavariables under `focus-metavariable`, which allows Semgrep to highlight the
  values matched by multiple metavariables more easily in certain circumstances.
  See the gist in the description of the original issue for an example. (gh-5686)

### Fixed

- C++: support ellipsis in right-hand-side of an assignment (gh-1923)
- Rust: support ellipsis in struct declarations (gh-3759)
- Fixed incorrect stripping of '\$' (literal dollar sign) in regexps used in the
  context of `metavariable-regex`. (gh-5987)
- Solidity: support constructor and modifier patterns (gh-6053)
- C#: support for metavariable ellipsis (e.g., `$...ARGS`) in arguments (gh-6065)
- Rust: support ellipsis inside module mody (gh-6066)
- Hold references to `NamedTemporaryFile` objects while their corresponding
  temporary files are still in use by the core runner. Failure to explicitly hold
  references to these objects on some Python implementations, such as PyPy,
  results in them sometimes being garbage-collected during processing. This,
  in turn, triggers removal of the temp files while they are still in use by
  the core runner or the worker subprocesses, resulting in various crashes and
  processing failures. (gh-6100)
- Swift: Fix parsing of statement ellipsis without a preceding semicolon (pa-1809)

## [0.113.0](https://github.com/returntocorp/semgrep/releases/tag/v0.113.0) - 2022-09-15

### Added

- Adds backwards-compatibility with older versions of semgrep-app. Only relevant for customers with on-prem versions of the app. (gh-6098)
- taint-mode: Experimental support for basic field-sensitive taint tracking.
  Semgrep can now track `x.a` and `x.b` separately, so that e.g. `x.a` can be
  tainted at the same time as `x.b` is clean, hence `sink(x.a)` would produce
  a finding but `sink(x.b)` would not. It is also possible for `x` to be tainted
  while `x.a` is clean. We expect this to have an net positive effect by reducing
  false positives. (pa-1278)
- Update the supply chain API to include information about the transitivity of matched dependencies (sca-199)

### Fixed

- Resolve imported classes when there are multiple chained classes.
  For example, if you `import world.Hello`, and create a
  `new Hello.internal_class()`, you can match that with
  `new world.Hello.internal_class()`. (gh-6001)
- Java: Correctly parse ellipsis in the body of top-level constructor patterns with privacy modifiers (e.g. public Foo() { ... }) (gh-6051)
- `semgrep --test` now fails when encountering a parsing error in target code. (gh-6068)
- Allowed metavariable-comparison to make use of the `not in` operator. (gh-6072)
- Fixed parsing of Rust impl definitions (gh-6078)
- Fixed a `TypeError: unbound method set.intersection() needs an argument` crash
  that occurred when all of a scan's rules were multilang (`regex` or `generic`). (gh-6093)

## [0.112.1](https://github.com/returntocorp/semgrep/releases/tag/v0.112.1) - 2022-09-08

### Fixed

- Fixed a regression introduced with the previous release, involving a bug with `pattern-inside`. (gh-6059)

## [0.112.0](https://github.com/returntocorp/semgrep/releases/tag/v0.112.0) - 2022-09-07

### Added

- JS/TS: Allow standalone switch cases as patterns (e.g. `case 5: ...`) (pa-1788)
- Symbolic propagation: Added propagation of all variables which are assigned to a single time. This will affect global variables and class attributes, in particular. (pa-1821)

### Changed

- Use new semgrep-app endpoint that combines the two POST requests to upload findings and ignored findings into one POST request. (app-1446)

### Fixed

- Update git url parser to support optional "org" after hostname. Example: https://some.enterprise.scm/myorg/owner/repo. (app-2202)
- Fix an incorrect autofix application when the fix includes Python f strings (gh-2995)
- Fix matching and autofix with bare tuples in Python array accesses (e.g. x[1, 2]) (gh-3387)
- Ruby: A pattern of the form `/.../` can now match any regexp, including regexp templates such as `/hello #{name}/`. (gh-5147)
- Fix parsing of Java constructors with privacy modifiers as patterns, e.g. the pattern `public Foo() { }` (gh-5558)
- Java: correctly parse class literal patterns (gh-6002)
- Solidity: correctly parse try statements (gh-6031)
- Python: Now support `match` statements (pa-1739)
- Fixed bug in constant propagation that caused incorrect constants to be inferred
  in the presence of subtraction. (pa-1846)

## [0.111.1](https://github.com/returntocorp/semgrep/releases/tag/v0.111.1) - 2022-08-23

### Changed

- Previously, the following error message appears when metrics are not uploaded within the set timeout timeframe:

  Error in send: HTTPSConnectionPool(host='metrics.semgrep.dev', port=443): Read timed out. (read timeout=3)

  As this causes users confusion when running the CLI, the log level of the message is reduced to appear for development and debugging purposes only. Note that metrics are still successfully uploaded, but the success status is not sent in time for the curent timeout set. (app-1398)

### Fixed

- taint-mode: Fixed the translation from Generic to IL for expressions like
  `"some string".concat(x)`. Previously, when `x` was tainted, the `concat`
  expression was not recognized as tainted and this caused false negatives. (pa-1787)

## [0.111.0](https://github.com/returntocorp/semgrep/releases/tag/v0.111.0) - 2022-08-22

### Added

- Introduced experimental support for Swift (gh-2232)
- Add configuration options for using a tree-sitter library installed anywhere
  on the system. (gh-5944)
- Updated the supply chain finding API:
  - The API is now typed and defined entirely in semgrep_output_v0.atd
  - Supply chain findings now have only one dependency match, not a list, and only one resolved url
  - Supply chain findings now have a field called reachable and reachability_rule,
    which indicate if the finding is reachable, and whether or not it was generated
    by a reachability rule (rule that had a semgrep pattern)
  - Supply chain findings now include a schema version
  - The complete finding information sent to semgrep app now includes a mapping from lockfile
    paths to the number of dependencies that were present in that lockfile (sca-197)

### Fixed

- When a YAML rule file had a string that contained an ISO timestamp, that would be parsed as a datetime object, which would then be rejected by Semgrep's rule schema validator. This is now fixed by keeping strings that contain an ISO timestamp as strings. (app-2157)
- When parsing PHP with tree-sitter, parse `$this` similar to pfff, as an IdSpecial. This makes it possible to match `$this` when the pattern is parsed with pfff and the program with tree-sitter. (gh-5594)
- Parse die() as exit() in tree-sitter PHP. This makes pfff and tree-sitter parse die() in the same way. (gh-5880)
- All: Applied a fix so that qualified identifiers can unify with metavariables. Notably, this
  affected Python decorators, among others. (pa-1700)
- Fixed a regression in DeepSemgrep after the experimental taint labels feature
  was introduced in 0.106.0. This prevented DeepSemgrep from reporting taint
  findings when e.g. the sink was wrapped by another function. (pa-1750)
- Fixed metavariable unification in JSON when one of the patterns is a single field. (pa-1763)
- Changed symbolic propagation such that "redundant" matches are no
  longer reported as findings. For instance:

  ```py
  def foo():
    x = g(5)
    f(x)
  ```

  If we are looking for the pattern `g(5)`, we should not match on line 3,
  since we will match on line 2 anyways, and this is just repeating information that
  we already know.

  This patch changes it so that we do not match on line 3 anymore. (pa-1772)

- Semgrep now passes -j to DeepSemgrep engine so --deep became noticeably faster. (pa-1776)
- taint-mode: Due to a mistake in the instantiation of a visitor, named function
  definitions were being analyzed twice! This is now fixed and you may observe
  significant speed ups in some cases. (pa-1778)
- Extract mode: fixed a possible exception in normal usage introduced due to
  changes in handling of search/taint rules. (pa-1786)
- Changed the fail-open message body (pm-194)

### Infra/Release Changes

- GHA Runner `macos-12` is unreliable and has begun failing without
  a clear explanation as to why: this downgrades to `macos-11`,
  since 10.15 is to be depracted ~10 from now. (devop-609)
- Keep the tree-sitter library inside a local folder rather than requiring
  a global installation. (gh-2956)

## [0.110.0](https://github.com/returntocorp/semgrep/releases/tag/v0.110.0) - 2022-08-15

### Changed

- Parse several built-in PHP functions in the same way in pfff and tree-sitter. This makes it possible to match exit, eval, empty and isset, even if the pattern is parsed with pfff and the PHP file with tree-sitter. (gh-5382)
- Use new semgrep-app endpoints that are pure GET with no side-effects and pure POST (app-2001)

### Fixed

- Skip fail-open for exit code 1 (app-2073)

## [0.109.0](https://github.com/returntocorp/semgrep/releases/tag/v0.109.0) - 2022-08-11

### Changed

- `semgrep ci` now defaults to fail open and will always exit with exit code 0, which is equivalent to passing `--suppress-errors`.
  To disable this behavior, you can pass `--no-suppress-errors` and semgrep will behave as it did previously, surfacing any exit codes that may result. (app-1951)

### Fixed

- taint-mode: Taint traces (`--dataflow-traces`) should no longer report "strange"
  intermediate variables when there are record accesses involved. This happened e.g.
  if `foo` was a tainted record and the code accessed some of its fields as in
  `foo.bar.baz`. This was related to the use of auxiliary variables in the Dataflow IL.
  These variables got tainted, but they had real tokens attached corresponding to the
  dot `.` operator. Now we do not include these variables in the taint trace. (pa-1672)

### Infra/Release Changes

- GHA runner-image `macos-10.15` is deprecated and will be unsupported by 30AUG2022. We've tested and can upgrade to `macos-12` to avoid issues with brownouts or end of support. (devop-586)

## [0.108.0](https://github.com/returntocorp/semgrep/releases/tag/v0.108.0) - 2022-08-03

### Added

- Metrics now include language-aggregated parse rates (files, bytes). The purpose
  of this is to help drive parsing improvements more intelligently. See
  [PRIVACY.md](PRIVACY.md) for more details. (pa-1678)

### Changed

- Updated SCA finding generation so that the following hold:
  - One SCA finding per vulnerable dependency. If one rule matches multiple dependencies in one lockfile,
    that will produce multiple findings. This still needs to be codified in the typed interface
  - No findings in files that were not targeted. If foo.py depends on Pipfile.lock,
    and foo.py is targeted but Pipfile.lock is not, then we can produce reachable findings
    in foo.py but not non-reachable findings in Pipfile.lock. If Pipfile.lock is included in
    our targets then we can produce non-reachable findings inside of it
  - No massive single scan for lockfiles. (sca-127)

### Fixed

- Fixed issue when scan fails due to pending changes in submodule. (cli-272)
- Semgrep CI now accepts more formats of git url for metadata provided to semgrep.dev and lets the user provide a fallback for repo name (SEMGREP_REPO_NAME) and repo url (SEMGREP_REPO_URL) if they are undefined by CI. (cli-280)
- Fixed a crash that occurred when reporting results when join mode and taint mode were used together (gh-5839)
- JS: Allowed decorators to appear in Semgrep patterns for class methods and fields. (pa-1677)
- Quick fix for a regression introduced in 0.107.0 (presumably by taint labels)
  that could cause some taint rules to crash Semgrep with:

      Invalid_argument "output_value: abstract value (Custom)" (pa-1724)

- Increase timeout for network calls to semgrep.dev from 30s to 60s (timeout-1)

## [0.107.0](https://github.com/returntocorp/semgrep/releases/tag/v0.107.0) - 2022-07-29

### Added

- Added metadata in App-connected scans to report extensions of files that do not match the language of any enabled rules in order to enable more effective language prioritization while developing new rules. (app-1354)
- Support fail-open in CI: adds --suppress-errors/--no-suppress-errors (defaults to --no-suppress-errors) (cli-254)
- New language Elixir with experimental support. (gh-3698)
- Kotlin: support for ellipsis in field access (e.g., `obj. ... .bar()`) (gh-5819)
- Changed `semgrep-core` so that it can now be run with `-rules` on `.yaml` files which do not have a top-level `rules: ...` key. This means you can now copy paste from the playground editor directly into a `.yaml` file for use with `semgrep-core`. (implicit-rules-sc-core)
- Add experimental support for _taint labels_, that is the ability to attach labels to
  different kinds of taint. Both sources and sinks can retrict what labels are present
  in the data that passes through them in order to apply. This allows to write more
  complex taint rules that previously required ugly workarounds. Taint labels are also
  useful for writing certain classes of typestate analyses (e.g., check that a file
  descriptor is not used after being closed). (pa-1362)
- Introduced the `--dataflow-traces` flag, which directs the Semgrep CLI to explain how non-local values lead to a finding. Currently, this only applies to taint mode findings and it will trace the path from the taint source to the taint sink. (pa-1599)

### Changed

- Made breaking changes to the dataflow_trace JSON output to make it more easily consumable by the App. Added content for taint_source and intermediate_vars, and collapsed the multile taint_source locations into one. (dataflow-trace-json)
- Removed the unique_id field from the semgrep (and semgrep-core) JSON output
  for metavariables. (unique-id-json)

### Fixed

- Fixed format of repository urls so links to findings can be properly displayed on semgrep.dev (cli-278)
- Scala: Allow metavariables in `import` patterns (gh-5219)
- Rules reported for LSP metrics now are hashed before sending (lsp-metrixs)
- `-filter_irrelevant_rules` was incorrectly skipping files when the PCRE engine threw
  an error, while trying to match a regex that determines whether a rule is relevant
  for a file. This has been fixed so that, in case of a PCRE error, we assume that the
  rule could be relevant and we do run it on the file. (pa-1635)

## [0.106.0](https://github.com/returntocorp/semgrep/releases/tag/v0.106.0) - 2022-07-21

### Changed

- `metavariable-comparison`: The `metavariable` field is now optional, except
  if `strip: true`. When `strip: false` (the default) the `metavaraible` field
  has no use so it was pointless to require it. (metavariable-comparison-metavariable)
- `metavariable-comparison` now also works on metavariables that cannot be evaluated
  to simple literals. In such cases, we take the string representation of the code
  bound by the metavariable. The way to access this string representation is via
  `str($MVAR)`. For example:

  ```
  - metavariable-comparison:
      metavariable: $X
      comparison: str($X) == str($Y)
  ```

  Here `$X` and `$Y` may bind to two different code variables, and we check whether
  these two code variables have the same name (e.g. two different variables but both
  named `x`). (pa-1659)

- When running an SCA scan with `semgrep ci --sca`,
  SCA findings will no longer be considered blocking if they are unreachable. (sca-128)

### Fixed

- Fixed a regression in name resolution that occurred with metavariable patterns (gh-5690)
- Rust: Fixed a bug with matching for scoped identifiers

  Basically, scoped identifiers were only looking at the last identifier. So something like `A::B::C` would result in something like `C`. (gh-5717)

## [0.105.0](https://github.com/returntocorp/semgrep/releases/tag/v0.105.0) - 2022-07-20

### Added

- _Experimental_ LSP support for: metavariable inlay hints, hot reloading, App integation, scan commands, and much much more (cli-235)
- Added a concatenation reduction for extract mode, so that disjoint snippets
  within a file can be treated as one unified file. (extract-reductions)
- Extract mode: taint trace positions are now updated to correspond to the source
  file. (extract-taint-traces)
- Extract mode: generic is now permitted as a `languages` value (pa-1648)
- Added handling for EXPERIMENT severity to allow the ability to test rules as experiments. (rules-680)

### Changed

- Do not store time or skipped targets output unless requested. This reduces memory consumption significantly in large repos (pa-1618)
- Reduce memory consumption of semgrep by passing the targets in a more condensed
  structure. Previously, we told semgrep which rules to run on which target by
  listing out all the rule_ids each target should run. Now, we have a separate
  rule_id list and for each target we only ilst the rule_id indices. On large
  repos, particularly when run with multiple processes, this has a significant
  impact. (pa-1618-2)

### Fixed

- C#: Improved error message when function parameters are declared with `var` (gh-5068)
- Scala/others: Added a fix allowing percolation of name information from class parameters

  For example, classes which take in arguments like the following in Scala:

  ```scala
  class ExampleClass(val x: TypeName) {
  }
  ```

  do not properly enter the context. So in our analysis, we would not know that the identifier
  `x` has type `TypeName`, within the body of `ExampleClass`. (gh-5506)

- Fixed the logged message describing the endpoint where rules are fetched from when SEMGREP_URL is set (gh-5753)
- Fixed what data was used for indexing match results to used match based id data (index)

## [0.104.0](https://github.com/returntocorp/semgrep/releases/tag/v0.104.0) - 2022-07-13

### Added

- `semgrep ci` will now not block builds on triage ignored issues (cli-162)
- Add support to cli/scripts/compare.py for podman environments (compare-script-podman)
- A new experimental 'extract' mode. This mode runs a Semgrep rule on a codebase
  and "extracts" code from matches, treating it as a different language. This
  allows users to supplement an existing set of rules, e.g., for JavaScript, by
  writing additional rules to find JavaScript in non-JavaScript files, e.g.,
  JavaScript contained in HTML or template files. While this is somewhat possible
  with metavariable-pattern, this reduces the work from an M \* N problem to an M \+ N one. (gh-4478)
- Added taint traces as part of Semgrep's JSON output. This helps explain how the sink became tainted. (pa-1271)

### Changed

- Metavariable-pattern now uses the same metavariable context as its parent. This will potentially
  cause breaking changes for rules that reuse metavariables in the pattern. For example, consider
  the following formula:

  ```
  - patterns:
     - pattern-either:
         - pattern-inside: $OBJ.output($RESP)
     - pattern: $RESP
     - metavariable-pattern:
         metavariable: $RESP
         pattern: `...{ $OBJ }...`
  ```

  Previously, the `$OBJ` in the metavariable-pattern would be a new metavariable. The formula would
  behave the same if that `$OBJ` was `$A` instead. Now, `$OBJ` will try to unify with the value bound
  by `$OBJ` in the pattern-inside. (gh-5060)

- The semgrep test output used to produce expected lines and reported lines which is difficult to read and interpret. This change introduces missed lines and incorrect lines to make it easier for the users to pinpoint the differences in output. (gh-5600)
- Separator lines are no longer drawn between findings that have no source code snippet. (sca-ui)
- Using ellipses in XML/HTML elements is now more permissive of whitespace.
  Previously, in order to have a element with an ellipsis no leading/trailing
  whitespace was permitted in the element contents, i.e., `<tag>...</tag>` was
  the only permitted form. Now, leading or trailing whitespace is ignored when
  the substantive content of the element is only an ellipsis. (xml-permissive-ellipsis)

### Fixed

- Semgrep App's links to repositories, code snippets, and pull requests are no longer broken for projects that run Semgrep in Circle CI, Bitbucket, and Buildkite. (cli-267)
- When running Semgrep via Docker for Mac,
  all volume-mounted files were considered executable and thus a candidate for scanning Bash code,
  making target collection take longer, and sometimes targeting more files than intended.
  This was fixed by using `os.stat` instead of `os.access` to determine if a file is executable. (gh-5560)
- Semgrep used to crash when trying to print findings that match only whitespace, such as when a rule disallows two newlines at the end of a file. This crash is now fixed. (gh-5633)
- Memory usage improvement: don't save skipped targets when `--debug` isn't passed
  since it isn't read unless `--debug` is used (pa-1618)

### Infra/Release Changes

- Updated the GH Release creation to publish the release instead of drafting one - this avoids a failure of validation jobs on release. (cli-243)
- Using some building blocks from release, set up a workflow to test `semgrep ci` e2e. (cli-253)
- Updated our changelog management system to `towncrier` to avoid merge conflicts in changelog on release (cli-77)
- The opening of PRs in other repos is automated with semgrep release - there is still a manual review required to build confidence. (cli-84)

## Unreleased

## [0.103.0](https://github.com/returntocorp/semgrep/releases/tag/v0.103.0) - 2022-07-05

### Added

- Expression statement patterns (e.g. `foo();`) used to also match when
  they were a bit deeper in the expression (e.g., `x = foo();`).
  This can now be disabled via rule `options:`
  with `implicit_deep_exprstmt: false` (#5472)
- The timeout for git commands Semgrep runs is now configurable by setting the
  `SEMGREP_GIT_COMMAND_TIMEOUT` environment variable.
  The unit used is seconds. The default value is 300.

### Fixed

- Error messages in JSON output used to contain ANSI color codes,
  this was fixed so they are now uncolored (and readable!) plain text.
- Fixed config parsing bug to cut CI scan speeds down significantly

## [0.102.0](https://github.com/returntocorp/semgrep/releases/tag/v0.102.0) - 2022-06-30

### Added

- Scala: ellipsis are now allowed in for loop headers, so you can write patterns
  like `for (...; $X <- $Y if $COND; ...) { ... }` to match nested for loops. (#5650)
- The `SEMGREP_GHA_MIN_FETCH_DEPTH` environment variable which lets you set how many
  commits `semgrep ci` fetches from the remote at the minimum when calculating the merge-base in GitHub Actions.
  Having more commits available helps Semgrep determine what changes came from the current pull request,
  fixing issues where Semgrep would report findings that weren't touched in a given pull request.
  This value is set to 0 by default (#5664)

### Fixed

- taint-mode: In some scenarios some statements were not being included in the
  CFG used by taint tracking, and as a result some expected findings were not being
  reported (i.e. false negatives). This affected mainly languages like Scala where
  traditional control-flow constructs are expressions rather than statements (or,
  seen in a different way, every statement returns a value). (#5652)
- Yaml: location information is fixed for unicode characters (#5660)

### Changed

- `--verbose` no longer toggles the display of timing information, use
  `--verbose --time` to display this information.
- Change timeout for git operations from 100s to 500s

## [0.101.1](https://github.com/returntocorp/semgrep/releases/tag/v0.101.1) - 2022-06-28

### Fixed

- `semgrep ci`: CI runs in GitHub Actions failed to checkout the commit assoociated with the head branch, and is fixed here.

## [0.101.0](https://github.com/returntocorp/semgrep/releases/tag/v0.101.0) - 2022-06-27

### Added

- Bash: Support for subshell syntax i.e. commands in parentheses (#5629)

### Changed

### Fixed

- `semgrep ci`: CI runs were failing to checkout the PR head in GitHub Actions, which is
  corrected here.
- TS: fixed the parsing of type predicates and typeof queries
- Deep expression matching now works on HTML in JavaScript
- taint-mode: Taint propagation via `pattern-propagators` now works correclty when the
  `from` or `to` metavariables match a function call. For example, given
  `sqlBuilder.append(page.getOrderBy())`, we can now propagate taint from
  `page.getOrderBy()` to `sqlBuilder`.
- Will no longer print "files were not tracked by git" if not in a git repo
- Will no longer print "Some files were skipped" if no files were skipped
- Fixed bug where semgrep would crash in nonexistent directory (#4785)
- taint-mode: Correctly propagate taint in for-each loops with typed iteration
  variables (as in Java or C#). If the iterator object is tainted, that taint will
  now be propagated to the iteration variable. This should fix some false negatives
  (i.e., findings not being reported) in the presence of for-each loops. (#5590)

## [0.100.0](https://github.com/returntocorp/semgrep/releases/tag/v0.100.0) - 2022-06-22

## Added

- taint-mode: New experimental `pattern-propagators` feature that allows to specify
  arbitrary patterns for the propagation of taint by side-effect. In particular,
  this allows to specify how taint propagates through side-effectful function calls.
  For example, you can specify that when tainted data is added to an array then the
  array itself becomes tainted. (#4509)

### Changed

- `--config auto` no longer sends the name of the repository being scanned to the Semgrep Registry.
  As of June 21st, this data is not recorded by the Semgrep Registry backend, even if an old Semgrep version sends it.
  Also as of June 21st, none of the previously collected repository names are retained by the Semgrep team;
  any historical data has been wiped.
- Gitlab SAST output is now v14.1.2 compliant
- Removed the following deprecated `semgrep scan` options:
  `--json-stats`, `--json-time`, `--debugging-json`, `--save-test-output-tar`, `--synthesize-patterns`,
  `--generate-config/-g`, `--dangerously-allow-arbitrary-code-execution-from-rules`,
  and `--apply` (which was an easter egg for job applications, not the same as `--autofix`)
- PHP: switch to GA maturity! Thanks a lot to Sjoerd Langkemper for most of the
  heavy work

### Fixed

- Inline join mode rules can now run taint-mode rules
- Python: correctly handle `with` context expressions where the value is not
  bound (#5513)
- Solidity: update to a more recent tree-sitter-solidity to fix certain parsing
  errors (#4957)

## 0.99.0 - Skipped

Version 0.99.0 of Semgrep was intentionally skipped. Version 0.100.0 immediately follows version 0.98.0.

## [0.98.0](https://github.com/returntocorp/semgrep/releases/tag/v0.98.0) - 2022-06-15

### Added

- New language R with experimental support (#2360)
  Thanks to Zythosec for some contributions.
- Autodetection of CI env now supports Azure Pipelines, Bitbucket, Buildkite, Circle CI, Jenkins,
  and Travis CI in addition to GitHub and GitLab
- You can now disable version checks with an environment variable by setting
  `SEMGREP_ENABLE_VERSION_CHECK=0`
- Dataflow: spread operators in record expressions (e.g. `{...foo}`) are now translated into the Dataflow IL
- An experimental LSP daemon mode for semgrep. Try it with `semgrep lsp --config auto`!

### Changed

- Rules are now downloaded from the Semgrep Registry in JSON format instead of YAML.
  This speeds up rule parsing in the Semgrep CLI,
  making a `semgrep --config auto` run on the semgrep Python package in 14s instead of 16s.

### Fixed

- Fixed a bug where `--disable-version-check` would still send a request
  when a scan resulted in zero findings.
- Fixed a regression in 0.97 where the Docker image's working directory changed from `/src` without notice.
  This also could cause permission issues when running the image.
- Go: single pattern field can now match toplevel fields in a composite
  literal (#5452)
- PHP: metavariable-pattern: works again when used with language: php (#5443)
- PHP: booleans are propagated by constant propagation (#5509)
- PHP: named arguments work in patterns (#5508)
- Fixed a non-deterministic crash when matching a large number of regexes (#5277)
- Fixed issue when running in GithubActions that caused semgrep to report on
  files not changed in the PR (#5453)
- JS/TS: `$X()` no longer matches `new Foo()`, for consistency with other languages (#5510)
- JS/TS: Typed metavariables now match constructor calls (e.g. `($X: C)` matches `new C()`. (#5540)

## [0.97.0](https://github.com/returntocorp/semgrep/releases/tag/v0.97.0) - 2022-06-08

### Added

- Dataflow: XML elements (e.g. JSX elements) have now a basic translation to the
  Dataflow IL, meaning that dataflow analysis (constant propagation, taint tracking)
  can now operate inside these elements (#5115)
- Java: you can now use a metavariable in a package directive (#5420),
  for example, `package $X`, which is useful to bind the package
  name and use it in the error message.

### Fixed

- The output of `semgrep ci` should be clear it is exiting with error code 0
  when there are findings but none of them being blockers
- Java: support for Sealed classes and Text Blocks via tree-sitter-java
  (#3787, #4644)
- The JUnit XML output should serialize the failure messages as a single
  string instead of a python list of strings.
- Typescript: update to latest tree-sitter-typescript, with support
  for 'abstract' modifier in more places
- Scala: stop parsing parenthesized expressions as unary tuples
- `yarn.lock` files with no depenencies, and with dependencies that lack URLs, now parse
- Scala: fixed bug where typed patterns inside classes caused an exception during name resolution
- metavariable-regex: patterns are now unanchored as specified by the
  documentation (#4807)
- When a logged in CI scan encounters a Git failure,
  we now print a helpful error message instead of a traceback.

## [0.96.0](https://github.com/returntocorp/semgrep/releases/tag/v0.96.0) - 2022-06-03

### Added

- Generic mode: new option `generic_ellipsis_max_span` for controlling
  how many lines an ellipsis can match (#5211)
- Generic mode: new option `generic_comment_style` for ignoring
  comments that follow the specified syntax (C style, C++ style, or
  Shell style) (#3428)
- Metrics now include a list of features used during an execution.
  Examples of such features are: languages scanned, CLI options passed, keys used in rules,
  or certain code paths reached, such as using an `:include` instruction in
  a `.semgrepignore` file.
  These strings will NOT include user data or specific settings. As an example,
  with `semgrep scan --output=secret.txt` we might send `"option/output"` but
  will NOT send `"option/output=secret.txt"`.

### Changed

- The output summarizing a scan's results has been simplified.

## [0.95.0](https://github.com/returntocorp/semgrep/releases/tag/v0.95.0) - 2022-06-02

### Added

- Sarif output format now includes `fixes` section
- `--test` flag will now seach for code files with `.fixed` suffix and use
  these to test the behaviour of autofixes of the rules.
- Rust: added support for method chaining patterns.
- `r2c-internal-project-depends-on`: support for poetry and gradle lockfiles
- M1 Mac support added to PyPi
- Accept `SEMGREP_BASELINE_REF` as alias for `SEMGREP_BASELINE_COMMIT`
- `r2c-internal-project-depends-on`:
  - pretty printing for SCA results
  - support for poetry and gradle lockfiles
- taint-mode: Taint tracking will now analyze lambdas in their surrounding context.
  Previously, if a variable became tainted outside a lambda, and this variable was
  used inside the lambda causing the taint to reach a sink, this was not being
  detected because any nested lambdas were "opaque" to the analysis. (Taint tracking
  looked at lambdas but as isolated functions.) Now lambas are simply analyzed as if
  they were statement blocks. However, taint tracking still does not follow the flow
  of taint through the lambda's arguments!
- Metrics now include an anonymous Event ID. This is an ID generated at send-time
  and will be used to de-duplicate events that potentially get duplicated during transmission.
- Metrics now include an anonymous User ID. This ID is stored in the ~/.semgrep/settings.yml file. If the ID disappears, the next run will generate a new one randomly. See the [Anonymous User ID in PRIVACY.md](PRIVACY.md#anonymous-user-id) for more details.

### Fixed

- M1 Mac installed via pip now links tree-sitter properly
- Restore `--sca`

### Changed

- The `ci` CLI command will now include ignored matches in output formats
  that dictate they should always be included
- Previously, you could use `$X` in a message to interpolate the variable captured
  by a metavariable named `$X`, but there was no way to access the underlying value.
  However, sometimes that value is more important than the captured variable.
  Now you can use the syntax `value($X)` to interpolate the underlying
  propagated value if it exists (if not, it will just use the variable name).

  Example:

  Take a target file that looks like

  ```py
  x = 42
  log(x)
  ```

  Now take a rule to find that log command:

  ```yaml
  - id: example_log
    message: Logged $SECRET: value($SECRET)
    pattern: log(42)
    languages: [python]
  ```

  Before, this would have given you the message `Logged x: value(x)`. Now, it
  will give the message `Logged x: 42`.

- A parameter pattern without a default value can now match a parameter
  with a default value (#5021)

### Fixed

- Numerous improvements to PHP parsing by switching to tree-sitter-php
  to parse PHP target code. Huge shoutout to Sjoerd Langkemper for most
  of the heavy lifting work
  (#3941, #2648, #2650, #3590, #3588, #3587, #3576, #3848, #3978, #4589)
- TS: support number and boolean typed metavariables (#5350)
- When a rule from the registry fails to parse, suggest user upgrade to
  latest version of semgrep
- Scala: correctly handle `return` for taint analysis (#4975)
- PHP: correctly handle namespace use declarations when they don't rename
  the imported name (#3964)
- Constant propagation is now faster and memory efficient when analyzing
  large functions with lots of variables.

## [0.94.0](https://github.com/returntocorp/semgrep/releases/tag/v0.94.0) - 2022-05-25

### Added

- `metavariable-regex` now supports an optional `constant-propagation` key.
  When this is set to `true`, information learned from constant propagation
  will be used when matching the metavariable against the regex. By default
  it is set to `false`
- Dockerfile: constant propagation now works on variables declared with `ENV`
- `shouldafound` - False Negative reporting via the CLI

### Changed

- taint-mode: Let's say that e.g. `taint(x)` makes `x` tainted by side-effect.
  Previously, we had to rely on a trick that declared that _any_ occurrence of
  `x` inside `taint(x); ...` was as taint source. If `x` was overwritten with
  safe data, this was not recognized by the taint engine. Also, if `taint(x)`
  occurred inside e.g. an `if` block, any occurrence of `x` outside that block
  was not considered tainted. Now, if you specify that the code variable itself
  is a taint source (using `focus-metavariable`), the taint engine will handle
  this as expected, and it will not suffer from the aforementioned limitations.
  We believe that this change should not break existing taint rules, but please
  report any regressions that you may find.
- taint-mode: Let's say that e.g. `sanitize(x)` sanitizes `x` by side-effect.
  Previously, we had to rely on a trick that declared that _any_ occurrence of
  `x` inside `sanitize(x); ...` was sanitized. If `x` later overwritten with
  tainted data, the taint engine would still regard `x` as safe. Now, if you
  specify that the code variable itself is sanitized (using `focus-metavariable`),
  the taint engine will handle this as expected and it will not suffer from such
  limitation. We believe that this change should not break existing taint rules,
  but please report any regressions that you may find.
- The dot access ellipsis now matches field accesses in addition to method
  calls.
- pattern-regex, pattern-not-regex, metavariable-regex: `^` and `$`
  now match at the beginning and end of each line, respectively,
  rather than previously just at the beginning and end of the input
  file. This corresponds to PCRE's multiline mode. To get the old
  behavior back, use `\A` instead of '^' and `\Z` instead of `$`. See
  the [PCRE
  manual](https://www.pcre.org/original/doc/html/pcrepattern.html#smallassertions)
  for details.
- Made error message for resource exhausion (exit code -11/-9) more actionable
- Made error message for rules with patterns missing positive terms
  more actionable (#5234)
- In this version, we have made several performance improvements
  to the code that surrounds our source parsing and matching core.
  This includes file targeting, rule fetching, and similar parts of the codebase.
  Running `semgrep scan --config auto` on the semgrep repo itself
  went from 50-54 seconds to 28-30 seconds.
  - As part of these changes, we removed `:include .gitignore` and `.git/`
    from the default `.semgrepignore` patterns.
    This should not cause any difference in which files are targeted
    as other parts of Semgrep ignore these files already.
  - A full breakdown of our performance updates,
    including some upcoming ones,
    can be found here https://github.com/returntocorp/semgrep/issues/5257#issuecomment-1133395694
- If a metrics event request times out, we no longer retry the request.
  This avoids Semgrep waiting 10-20 seconds before exiting if these requests are slow.
- The metrics collection timeout has been raised from 2 seconds to 3 seconds.

### Fixed

- TS: support for template literal types after upgrading to a more recent
  tree-sitter-typescript (Oct 2021)
- TS: support for `override` keyword (#4220, #4798)
- TS: better ASI (#4459) and accept code like `(null)(foo)` (#4468)
- TS: parse correctly private properties (#5162)
- Go: Support for ellipsis in multiple return values
  (e.g., `func foo() (..., error, ...) {}`) (#4896)
- semgrep-core: you can use again rules stored in JSON instead of YAML (#5268)
- Python: adds support for parentheses around `with` context expressions
  (e.g., `with (open(x) as a, open(y) as b): pass`) (#5092)
- C++: we now parse correctly const declarations (#5300)

## [0.93.0](https://github.com/returntocorp/semgrep/releases/tag/v0.93.0) - 2022-05-17

### Changed

- Files where only some part of the code had to be skipped due to a parse failure
  will now be listed as "partially scanned" in the end-of-scan skip report.
- Licensing: The ocaml-tree-sitter-core component is now distributed
  under the terms of the LGPL 2.1, rather than previously GPL 3.
- A new field was added to metrics collection: isAuthenticated.
  This is a boolean flag which is true if you ran semgrep login.

### Fixed

- `semgrep ci` used to incorrectly report the base branch as a CI job's branch
  when running on a `pull_request_target` event in GitHub Actions.
  By fixing this, Semgrep App can now track issue status history with `on: pull_request_target` jobs.
- Metrics events were missing timestamps even though `PRIVACY.md` had already documented a timestamp field.

## [0.92.1](https://github.com/returntocorp/semgrep/releases/tag/v0.92.1) - 2022-05-13

### Added

- Datafow: The dataflow engine now handles if-then-else expressions as in OCaml,
  Ruby, etc. Previously it only handled if-then-else statements. (#4965)

### Fixed

- Kotlin: support for ellispis in class parameters, e.g.. `class Foo(...) {}` (#5180)
- JS/TS: allow ellipsis in binding_pattern (e.g., in arrow parameters) (#5230)
- JS/TS: allow ellipsis in imports (e.g., `import {..., Foo, ...} from 'Bar'`) (#5012)
- `fixed_lines` is once again included in JSON output when running with `--autofix --dryrun`

## [0.92.0](https://github.com/returntocorp/semgrep/releases/tag/v0.92.0) - 2022-05-11

### Added

- The JSON output of `semgrep scan` is now fully specified using
  ATD (https://atd.readthedocs.io/) and jsonschema (https://json-schema.org/).
  See the semgrep-interfaces submodule under interfaces/
  (e.g., interfaces/semgrep-interfaces/Semgrep_output_v0.atd for the ATD spec)
- The JSON output of `semgrep scan` now contains a "version": field with the
  version of Semgrep used to generate the match results.
- taint-mode: Previously, to declare a function parameteter as a taint source,
  we had to rely on a trick that declared that _any_ occurence of the parameter
  was a taint source. If the parameter was overwritten with safe data, this was
  not recognized by the taint engine. Now, `focus-metavariable` can be used to
  precisely specify that a function parameter is a source of taint, and the taint
  engine will handle this as expected.
- taint-mode: Add basic support for object destructuring in languages such as
  Javascript. For example, given `let {x} = E`, Semgrep will now infer that `x`
  is tainted if `E` is tainted.

### Fixed

- OCaml: Parenthesis in autofixed code will no longer leave dangling closing-paren.
  Thanks to Elliott Cable for his contribution (#5087)
- When running the Semgrep Docker image, we now mark all directories as safe for use by Git,
  which prevents a crash when the current user does not own the source code directory.
- C++: Ellipsis are now allowed in for loop header (#5164)
- Java: typed metavariables now leverages the type of foreach variables (#5181)

## [0.91.0](https://github.com/returntocorp/semgrep/releases/tag/v0.91.0) - 2022-05-03

### Added

- `--core-opts` flag to send options to semgrep-core. For internal use:
  no guarantees made for semgrep-core options (#5111)

### Changed

- `semgrep ci` prints out all findings instead of hiding nonblocking findings (#5116)

## [0.90.0](https://github.com/returntocorp/semgrep/releases/tag/v0.90.0) - 2022-04-26

### Added

- Users can access the propagated value of a metavariable in the JSON output
  in the extra field
- Join mode now supports inline rules via the `rules:` key underneath the `join:` key.
- Added vendor.name field in gitlab sast output (#5077)

### Changed

- YAML parsing is more tolerant of `{}` appearing when it expects a scalar,
  allowing extensions of YAML that use `{}` to be parsed (#4849)
- Turn off optimization that trades off memory for performance because
  the effect is minor (with current parameters)

### Fixed

- Keep only latest run logs in last.log file (#5070)
- r2c-internal-project-depends-on:
  - Lockfiles that fail to parse will not crash semgrep
  - cargo.lock and Pipfile.lock dependencies that don't specify hashes now parse
  - go.sum files with a trailing newline now parse

## [0.89.0](https://github.com/returntocorp/semgrep/releases/tag/v0.89.0) - 2022-04-20

### Added

- Bash/Dockerfile: Add support for named ellipses such as in
  `echo $...ARGS` (#4887)
- PHP: Constant propagation for static constants (#5022)

### Changed

- When running a baseline scan on a shallow-cloned git repository,
  Semgrep still needs enough git history available
  to reach the branch-off point between the baseline and current branch.
  Previously, Semgrep would try to gradually fetch more and more commits
  up to a thousand commits of history,
  before giving up and just fetching all commits from the remote git server.
  Now, Semgrep will keep trying smaller batches until up to a million commits.
  This change should reduce runtimes on large baseline scans on very large repositories.
- Semgrep-core now logs the rule and file affected by a memory warning.
- Improved error messages from semgrep-core (#5013)
- Small changes to text output (#5008)
- Various exit codes changed so that exit code 1 is only for blocking findings (#5039)
- Subcommand is sent as part of user agent (#5051)

### Fixed

- Lockfiles scanning now respects .semgrepignore
- Workaround for git safe.directory change in github action (#5044)
- When a baseline scan diff showed that a path changed a symlink a proper file,
  Semgrep used incorrectly skip that path. This is now fixed.
- Dockerfile support: handle image aliases correctly (#4881)
- TS: Fixed matching of parameters with type annotations. E.g., it is now possible
  to match `({ params }: Request) => { }` with `({$VAR} : $REQ) => {...}`. (#5004)

## [0.88.0](https://github.com/returntocorp/semgrep/releases/tag/v0.88.0) - 2022-04-13

### Added

- Scala support is now officially GA
  - Ellipsis method chaining is now supported
  - Type metavariables are now supported
- Ruby: Add basic support for lambdas in patterns. You can now write patterns
  of the form `-> (P) {Q}` where `P` and `Q` are sub-patterns. (#4950)
- Experimental `semgrep install-deep-semgrep` command for DeepSemgrep beta (#4993)

### Changed

- Moved description of parse/internal errors to the "skipped" section of output
- Since 0.77.0 semgrep-core logs a warning when a worker process is consuming above
  400 MiB of memory. Now, it will also log an extra warning every time memory usage
  doubles. Again, this is meant to help diagnosing OOM-related crashes.

### Fixed

- Dockerfile: `lang.json` file not found error while building the docker image
- Dockerfile: `EXPOSE 12345` will now parse `12345` as an int instead of a string,
  allowing `metavariable-comparison` with integers (#4875)
- Scala: unicode character literals now parse
- Scala: multiple annotated type parameters now parse (`def f[@an A, @an B](x : A, y : B) = ...`)
- Ruby: Allow 'unless' used as keyword argument or hash key (#4948)
- Ruby: Fix regexp matching in the presence of escape characters (#4999)
- `r2c-internal-project-depends-on`:
  - Generic mode rules work again
  - Semgrep will not fail on targets that contain no relevant lockfiles
  - `package-lock.json` parsing now defaults to `dependencies` instead of `packages`,
    and will not completely fail on dependencies with no version
  - `yarn.lock` parsing has been rewritten to fix a bug where sometimes
    large numbers of dependencies would be ignored
- Go: parse multiline string literals
- Handle utf-8 decoding errors without crashing (#5023)

## [0.87.0](https://github.com/returntocorp/semgrep/releases/tag/v0.87.0) - 2022-04-07

### Added

- New `focus-metavariable` operator that lets you focus (or "zoom in") the match
  on the code region delimited by a metavariable. This operator is useful for
  narrowing down the code matched by a rule, to focus on what really matters. (#4453)
- `semgrep ci` uses "GITHUB_SERVER_URL" to generate urls if it is available
- You can now set `NO_COLOR=1` to force-disable colored output

### Changed

- taint-mode: We no longer force the unification of metavariables between
  sources and sinks by default. It is not clear that this is the most natural
  behavior; and we realized that, in fact, it was confusing even for experienced
  Semgrep users. Instead, each set of metavariables is now considered independent.
  The metavariables available to the rule message are all metavariables bound by
  `pattern-sinks`, plus the subset of metavariables bound by `pattern-sources`
  that do not collide with the ones bound by `pattern-sinks`. We do not expect
  this change to break many taint rules because source-sink metavariable
  unification had a bug (see #4464) that prevented metavariables bound by a
  `pattern-inside` to be unified, thus limiting the usefulness of the feature.
  Nonetheless, it is still possible to force metavariable unification by setting
  `taint_unify_mvars: true` in the rule's `options`.
- `r2c-internal-project-depends-on`: this is now a rule key, and not part of the pattern language.
  The `depends-on-either` key can be used analgously to `pattern-either`
- `r2c-internal-project-depends-on`: each rule with this key will now distinguish between
  _reachable_ and _unreachable_ findings. A _reachable_ finding is one with both a dependency match
  and a pattern match: a vulnerable dependency was found and the vulnerable part of the dependency
  (according to the patterns in the rule) is used somewhere in code. An _unreachable_ finding
  is one with only a dependency match. Reachable findings are reported as coming from the
  code that was pattern matched. Unreachable findings are reported as coming from the lockfile
  that was dependency matched. Both kinds of findings specify their kind, along with all matched
  dependencies, in the `extra` field of semgrep's JSON output, using the `dependency_match_only`
  and `dependency_matches` fields, respectively.
- `r2c-internal-project-depends-on`: a finding will only be considered reachable if the file
  containing the pattern match actually depends on the dependencies in the lockfile containing the
  dependency match. A file depends on a lockfile if it is the nearest lockfile going up the
  directory tree.
- The returntocorp/semgrep Docker image no longer sets `semgrep` as the entrypoint.
  This means that `semgrep` is no longer prepended automatically to any command you run in the image.
  This makes it possible to use the image in CI executors that run provisioning commands within the image.

### Fixed

- `-` is now parsed as a valid identifier in Scala
- `new $OBJECT(...)` will now work properly as a taint sink (#4858)
- JS/TS: `...{$X}...` will no longer match `str`
- taint-mode: Metavariables bound by a `pattern-inside` are now available to the
  rule message. (#4464)
- parsing: fail fast on in semgrep-core if rules fail to validate (broken since 0.86.5)
- Setting either `SEMGREP_URL` or `SEMGREP_APP_URL`
  now updates the URL used both for Semgrep App communication,
  and for fetching Semgrep Registry rules.
- The pre-commit hook exposed from semgrep's repository no longer fails
  when trying to install with recent setuptools versions.

## [0.86.5](https://github.com/returntocorp/semgrep/releases/tag/v0.86.5) - 2022-03-28

## Changed

- Set minimum urllib3 version

## [0.86.4](https://github.com/returntocorp/semgrep/releases/tag/v0.86.4) - 2022-03-25

### Changed

- Increase rule fetch timeout from 20s to 30s

## [0.86.3](https://github.com/returntocorp/semgrep/releases/tag/v0.86.3) - 2022-03-25

### Fixed

- Network timeouts during rule download are now less likely.

## [0.86.2](https://github.com/returntocorp/semgrep/releases/tag/v0.86.2) - 2022-03-24

### Fixed

- Some finding fingerprints were not matching what semgrep-agent would return.

## [0.86.1](https://github.com/returntocorp/semgrep/releases/tag/v0.86.1) - 2022-03-24

### Fixed

- The fingerprint of findings ignored with `# nosemgrep` is supposed to be the same
  as if the ignore comment wasn't there.
  This has previously only worked for single-line findings, including in `semgrep-agent`.
  Now the fingerprint is consistent as expected for multiline findings as well.

### Changed

- `--timeout-threshold` default set to 3 instead of 0

## [0.86.0](https://github.com/returntocorp/semgrep/releases/tag/v0.86.0) - 2022-03-24

### Added

- Semgrep can now output findings in GitLab's SAST report and secret scanning
  report formats with `--gitlab-sast` and `--gitlab-secrets`.
- JSON output now includes a fingerprint of each finding.
  This fingerprint remains consistent when matching code is just moved around
  or reindented.
- Go: use latest tree-sitter-go with support for Go 1.18 generics (#4823)
- Terraform: basic support for constant propagation of locals (#1147)
  and variables (#4816)
- HTML: you can now use metavariable ellipsis inside <script> (#4841)
  (e.g., `<script>$...JS</script>`)
- A `semgrep ci` subcommand that auto-detects settings from your CI environment
  and can upload findings to Semgrep App when logged in.

### Changed

- SARIF output will include matching code snippet (#4812)
- semgrep-core should now be more tolerant to rules using futur extensions by
  skipping those rules instead of just crashing (#4835)
- Removed `tests` from published python wheel
- Findings are now considered identical between baseline and current scans
  based on the same logic as Semgrep CI uses, which means:
  - Two findings are now identical after whitespace changes such as re-indentation
  - Two findings are now identical after a nosemgrep comment is added
  - Findings are now different if the same code triggered them on different lines
- Docker image now runs as root to allow the docker image to be used in CI/CD pipelines
- Support XDG Base directory specification (#4818)

### Fixed

- Entropy analysis: strings made of repeated characters such as
  `'xxxxxxxxxxxxxx'` are no longer reported has having high entropy (#4833)
- Symlinks found in directories are skipped from being scanned again.
  This is a fix for a regression introduced in 0.85.0.
- HTML: multiline raw text tokens now contain the newline characters (#4855)
- Go: fix unicode parsing bugs (#4725) by switching to latest tree-sitter-go
- Constant propagation: A conditional expression where both alternatives are
  constant will also be considered constant (#4301)
- Constant propagation now recognizes operators `++` and `--` as side-effectful
  (#4667)

## [0.85.0](https://github.com/returntocorp/semgrep/releases/tag/v0.85.0) - 2022-03-16

### Added

- C#: use latest tree-sitter-c-sharp with support for most C# 10.0 features
- HTML: support for metavariables on tags (e.g., `<$TAG>...</$TAG>`) (#4078)
- Scala: The data-flow engine can now handle expression blocks.
  This used to cause some false negatives during taint analysis,
  which will now be reported.
- Dockerfile: allow e.g. `CMD ...` to match both `CMD ls` and `CMD ["ls"]`
  (#4770).
- When scanning multiple languages, Semgrep will now print a table of how
  many rules and files are used for each language.

### Fixed

- Fixed Deep expression matching and metavariables interaction. Semgrep will
  not stop anymore at the first match and will enumarate all possible matchings
  if a metavariable is used in a deep expression pattern
  (e.g., `<... $X ...>`). This can introduce some performance regressions.
- JSX: ellipsis in JSX body (e.g., `<div>...</div>`) now matches any
  children (#4678 and #4717)
- > ℹ️ During a `--baseline-commit` scan,
  > Semgrep temporarily deletes files that were created since the baseline commit,
  > and restores them at the end of the scan.

  Previously, when scanning a subdirectory of a git repo with `--baseline-commit`,
  Semgrep would delete all newly created files under the repo root,
  but restore only the ones in the subdirectory.
  Now, Semgrep only ever deletes files in the scanned subdirectory.

- Previous releases allowed incompatible versions (21.1.0 & 21.2.0)
  of the `attrs` dependency to be installed.
  `semgrep` now correctly requires attrs 21.3.0 at the minimum.
- `package-lock.json` parsing defaults to `packages` instead of `dependencies` as the source of dependencies
- `package-lock.json` parsing will ignore dependencies with non-standard versions, and will succesfully parse
  dependencies with no `integrity` field

### Changed

- File targeting logic has been mostly rewritten. (#4776)
  These inconsistencies were fixed in the process:

  - > ℹ️ "Explicitly targeted file" refers to a file
    > that's directly passed on the command line.

    Previously, explicitly targeted files would be unaffected by most global filtering:
    global include/exclude patterns and the file size limit.
    Now `.semgrepignore` patterns don't affect them either,
    so they are unaffected by all global filtering,

  - > ℹ️ With `--skip-unknown-extensions`,
    > Semgrep scans only the explicitly targeted files that are applicable to the language you're scanning.

    Previously, `--skip-unknown-extensions` would skip based only on file extension,
    even though extensionless shell scripts expose their language via the shebang of the first line.
    As a result, explicitly targeted shell files were always skipped when `--skip-unknown-extensions` was set.
    Now, this flag decides if a file is the correct language with the same logic as other parts of Semgrep:
    taking into account both extensions and shebangs.

- Semgrep scans with `--baseline-commit` are now much faster.
  These optimizations were added:

  - > ℹ️ When `--baseline-commit` is set,
    > Semgrep first runs the _current scan_,
    > then switches to the baseline commit,
    > and runs the _baseline scan_.

    The _current scan_ now excludes files
    that are unchanged between the baseline and the current commit
    according to `git status` output.

  - The _baseline scan_ now excludes rules and files that had no matches in the _current scan_.

  - When `git ls-files` is unavailable or `--disable-git-ignore` is set,
    Semgrep walks the file system to find all target files.
    Semgrep now walks the file system 30% faster compared to previous versions.

- The output format has been updated to visually separate lines
  with headings and indentation.

## [0.84.0](https://github.com/returntocorp/semgrep/releases/tag/v0.84.0) - 2022-03-09

### Added

- new --show-supported-languages CLI flag to display the list of languages
  supported by semgrep. Thanks to John Wu for his contribution! (#4754)
- `--validate` will check that metavariable-x doesn't use an invalid
  metavariable
- Add r2c-internal-project-depends on support for Java, Go, Ruby, and Rust
- PHP: .tpl files are now considered PHP files (#4763)
- Scala: Support for custom string interpolators (#4655)
- Scala: Support parsing Scala scripts that contain plain definitions outside
  an Object or Class
- JSX: JSX singleton elements (a.k.a XML elements), e.g., `<foo />` used to
  match also more complex JSX elements, e.g., `<foo >some child</foo>`.
  This can now be disabled via rule `options:`
  with `xml_singleton_loose_matching: false` (#4730)
- JSX: new matching option `xml_attrs_implicit_ellipsis` that allows
  disabling the implicit `...` that was added to JSX attributes patterns.
- new focus-metavariable: experimental operator (#4735) (the syntax may change
  in the near futur)

### Fixed

- Report parse errors even when invoked with `--strict`
- Show correct findings count when using `--config auto` (#4674)
- Kotlin: store trailing lambdas in the AST (#4741)
- Autofix: Semgrep no longer errors during `--dry-run`s where one fix changes the line numbers in a file that also has a second autofix.
- Performance regression when running with --debug (#4761)
- SARIF output formatter not handling lists of OWASP or CWE metadata (#4673)
- Allow metrics flag and metrics env var at the same time if both are set to the same value (#4703)
- Scan `yarn.lock` dependencies that do not specify a hash
- Run `project-depends-on` rules with only `pattern-inside` at their leaves
- Dockerfile patterns no longer need a trailing newline (#4773)

## [0.83.0](https://github.com/returntocorp/semgrep/releases/tag/v0.83.0) - 2022-02-24

### Added

- semgrep saves logs of last run to `~/.semgrep/last.log`
- A new recursive operator, `-->`, for join mode rules for recursively chaining together Semgrep rules based on metavariable contents.
- A new recursive operator, `-->`, for join mode rules for recursively
  chaining together Semgrep rules based on metavariable contents.
- Semgrep now lists the scanned paths in its JSON output under the
  `paths.scanned` key.
- When using `--verbose`, the skipped paths are also listed under the
  `paths.skipped` key.
- C#: added support for typed metavariables (#4657)
- Undocumented, experimental `metavariable-analysis` feature
  supporting two kinds of analyses: prediction of regular expression
  denial-of-service vulnerabilities (ReDoS, `redos` analyzer, #4700)
  and high-entropy string detection (`entropy` analyzer, #4672).
- A new subcommand `semgrep publish` allows users to upload private,
  unlisted, or public rules to the Semgrep Registry

### Fixed

- Configure the PCRE engine with lower match-attempts and recursion limits in order
  to prevent regex matching from potentially "hanging" Semgrep
- Terraform: Parse heredocs respecting newlines and whitespaces, so that it is
  possible to correctly match these strings with `metavariable-regex` or
  `metavariable-pattern`. Previously, Semgrep had problems analyzing e.g. embedded
  YAML content. (#4582)
- Treat Go raw string literals like ordinary string literals (#3938)
- Eliminate zombie uname processes (#4466)
- Fix for: semgrep always highlights one extra character

### Changed

- Improved constant propagation for global constants
- PHP: Constant propagation now has built-in knowledge of `escapeshellarg` and
  `htmlspecialchars_decode`, if these functions are given constant arguments,
  then Semgrep assumes that their output is also constant
- The environment variable used by Semgrep login changed from `SEMGREP_LOGIN_TOKEN` to `SEMGREP_APP_TOKEN`

## [0.82.0](https://github.com/returntocorp/semgrep/releases/tag/v0.82.0) - 2022-02-08

### Added

- Experimental baseline scanning. Run with `--baseline-commit GIT_COMMIT` to only
  show findings that currently exist but did not exist in GIT_COMMIT

### Changed

- Performance: send all rules directly to semgrep-core instead of invoking semgrep-core
- Scans now report a breakdown of how many target paths were skipped for what reason.
  - `--verbose` mode will list all skipped paths along with the reason they were skipped
- Performance: send all rules directly to semgrep-core instead of invoking semgrep-core
  for each rule, reducing the overhead significantly. Other changes resulting from this:
  Sarif output now includes all rules run. Error messages use full path of rules.
  Progress bar reports by file instead of by rule
- Required minimum version of python to run semgrep now 3.7 instead of EOL 3.6
- Bloom filter optimization now considers `import` module file names, thus
  speeding up matching of patterns like `import { $X } from 'foo'`
- Indentation is now removed from matches to conserve horizontal space

### Fixed

- Typescript: Patterns `E as T` will be matched correctly. E.g. previously
  a pattern like `v as $T` would match `v` but not `v as any`, now it
  correctly matches `v as any` but not `v`. (#4515)
- Solidity: ellipsis in contract body are now supported (#4587)
- Highlighting has been restored for matching code fragments within a finding

## [0.81.0](https://github.com/returntocorp/semgrep/releases/tag/v0.81.0) - 2022-02-02

### Added

- Dockerfile language: metavariables and ellipses are now
  supported in most places where it makes sense (#4556, #4577)

### Fixed

- Gracefully handle timeout errors with missing rule_id
- Match resources in Java try-with-resources statements (#4228)

## [0.80.0](https://github.com/returntocorp/semgrep/releases/tag/v0.80.0) - 2022-01-26

### Added

- Autocomplete for CLI options
- Dockerfile: add support for metavariables where argument expansion is already supported

### Changed

- Ruby: a metavariable matching an atom can also be used to match an identifier
  with the same name (#4550)

### Fixed

- Handle missing target files without raising an exception (#4462)

## [0.79.0](https://github.com/returntocorp/semgrep/releases/tag/v0.79.0) - 2022-01-20

### Added

- Add an experimental key for internal team use: `r2c-internal-project-depends-on` that
  allows rules to filter based on the presence of 3rd-party dependencies at specific
  version ranges.
- Experimental support for Dockerfile syntax.
- Support nosemgrep comments placed on the line before a match,
  causing such match to be ignored (#3521)
- Add experimental `semgrep login` and `semgrep logout` to store API token from semgrep.dev
- Add experimenntal config key `semgrep --config policy` that uses stored API token to
  retrieve configured rule policy on semgrep.dev

### Changed

- CLI: parse errors (reported with `--verbose`) appear once per file,
  not once per rule/file

### Fixed

- Solidity: add support for `for(...)` patterns (#4530)

## [0.78.0](https://github.com/returntocorp/semgrep/releases/tag/v0.78.0) - 2022-01-13

### Added

- Pre-alpha support for Dockerfile as a new target language
- Semgrep is now able to symbolically propagate simple definitions. E.g., given
  an assignment `x = foo.bar()` followed by a call `x.baz()`, Semgrep will keep
  track of `x`'s definition, and it will successfully match `x.baz()` with a
  pattern like `foo.bar().baz()`. This feature should help writing simple yet
  powerful rules, by letting the dataflow engine take care of any intermediate
  assignments. Symbolic propagation is still experimental and it is disabled by
  default, it must be enabled in a per-rule basis using `options:` and setting
  `symbolic_propagation: true`. (#2783, #2859, #3207)
- `--verbose` outputs a timing and file breakdown summary at the end
- `metavariable-comparison` now handles metavariables that bind to arbitrary
  constant expressions (instead of just code variables)
- YAML support for anchors and aliases (#3677)

### Fixed

- Rust: inner attributes are allowed again inside functions (#4444) (#4445)
- Python: return statement can contain tuple expansions (#4461)
- metavariable-comparison: do not throw a Not_found exn anymore (#4469)
- better ordering of match results with respect to captured
  metavariables (#4488)
- Go, JavaScript, Java, Python, TypeScript: correct matching of
  multibyte characters (#4490)

## [0.77.0](https://github.com/returntocorp/semgrep/releases/tag/v0.77.0) - 2021-12-16

### Added

- New language Solidity with experimental support.
- Scala: Patterns like List(...) now correctly match against patterns in code
- A default set of .semgrepignore patterns (in semgrep/templates/.semgrepignore) are now used if no .semgrepignore file is provided
- Java: Ellipsis metavariables can now be used for parameters (#4420)
- `semgrep login` and `semgrep logout` commands to save api token

### Fixed

- Go: fixed bug where using an ellipsis to stand for a list of key-value pairs
  would sometimes cause a parse error
- Scala: Translate definitions using patterns like
  `val List(x,y,z) = List(1,2,3)` to the generic AST
- Allow name resolution on imported packages named just vN, where N is a number
- The -json option in semgrep-core works again when used with -e/-f
- Python: get the correct range when matching comprehension (#4221)
- Python and other languages: allow matches of patterns containing
  non-ascii characters, but still with possibly many false positives (#4336)
- Java: parse correctly constructor method patterns (#4418)
- Address several autofix output issues (#4428, #3577, #3338) by adding per-
  file line/column offset tracking

### Changed

- Constant propagation is now a proper must-analysis, if a variable is undefined
  in some path then it will be considered as non-constant
- Dataflow: Only consider reachable nodes, which prevents some FPs/FNs
- Timing output handles errors and reports profiling times
- semgrep-core will log a warning when a worker process is consuming above 400 MiB
  of memory, or reached 80% of the specified memory limit, whatever happens first.
  This is meant to help diagnosing OOM-related crashes.

## [0.76.2](https://github.com/returntocorp/semgrep/releases/tag/v0.76.2) - 2021-12-08

## [0.76.2](https://github.com/returntocorp/semgrep/releases/tag/v0.76.2) - 2021-12-08

### Fixed

- Python: set the right scope for comprehension variables (#4260)
- Fixed bug where the presence of .semgrepignore would cause reported targets
  to have absolute instead of relative file paths

## [0.76.1](https://github.com/returntocorp/semgrep/releases/tag/v0.76.1) - 2021-12-07

### Fixed

- Fixed bug where the presence of .semgrepignore would cause runs to fail on
  files that were not subpaths of the directory where semgrep was being run

## [0.76.0](https://github.com/returntocorp/semgrep/releases/tag/v0.76.0) - 2021-12-06

### Added

- Improved filtering of rules based on file content (important speedup
  for nodejsscan rules notably)
- Semgrep CLI now respects .semgrepignore files
- Java: support ellipsis in generics, e.g., `class Foo<...>` (#4335)

### Fixed

- Java: class patterns not using generics will match classes using generics
  (#4335), e.g., `class $X { ...}` will now match `class Foo<T> { }`
- TS: parse correctly type definitions (#4330)
- taint-mode: Findings are now reported when the LHS of an access operator is
  a sink (e.g. as in `$SINK->method`), and the LHS operand is a tainted
  variable (#4320)
- metavariable-comparison: do not throw a NotHandled exn anymore (#4328)
- semgrep-core: Fix a segmentation fault on Apple M1 when using
  `-filter_irrelevant_rules` on rules with very large `pattern-either`s (#4305)
- Python: generate proper lexical exn for unbalanced braces (#4310)
- YAML: fix off-by-one in location of arrays
- Python: generate proper lexical exn for unbalanced braces (#4310)
- Matching `"$MVAR"` patterns against string literals computed by constant folding
  no longer causes a crash (#4371)

### Changed

- semgrep-core: Log messages are now tagged with the process id
- Optimization: change bloom filters to use sets, move location of filter
- Reduced the size of `--debug` dumps
- Given `--output` Semgrep will no longer print search results to _stdout_,
  but it will only save/post them to the specified file/URL

## [0.75.0](https://github.com/returntocorp/semgrep/releases/tag/v0.75.0) - 2021-11-23

### Fixed

- semgrep-ci relies on `--disable-nosem` still tagging findings with `is_ignored`
  correctly. Reverting optimization in 0.74.0 that left this field None when said
  flag was used

## [0.74.0](https://github.com/returntocorp/semgrep/releases/tag/v0.74.0) - 2021-11-19

### Added

- Support for method chaining patterns in Python, Golang, Ruby,
  and C# (#4300), so all GA languages now have method chaining
- Scala: translate infix operators to generic AST as method calls,
  so `$X.map($F)` matches `xs map f`
- PHP: support method patterns (#4262)

### Changed

- Add `profiling_times` object in `--time --json` output for more fine
  grained visibility into slow parts of semgrep
- Constant propagation: Any kind of Python string (raw, byte, or unicode) is
  now evaluated to a string literal and can be matched by `"..."` (#3881)

### Fixed

- Ruby: blocks are now represented with an extra function call in Generic so that
  both `f(...)` and `f($X)` correctly match `f(x)` in `f(x) { |n| puts n }` (#3880)
- Apply generic filters excluding large files and binary files to
  'generic' and 'regex' targets as it was already done for the other
  languages.
- Fix some Stack_overflow when using -filter_irrelevant_rules (#4305)
- Dataflow: When a `switch` had no other statement following it, and the last
  statement of the `switch`'s `default` case was a statement, such as `throw`,
  that can exit the execution of the current function, this caused `break`
  statements within the `switch` to not be resolved during the construction of
  the CFG. This could led to e.g. constant propagation incorrectly flagging
  variables as constants. (#4265)

## [0.73.0](https://github.com/returntocorp/semgrep/releases/tag/v0.73.0) - 2021-11-12

### Added

- experimental support for C++

### Changed

- Dataflow: Assume that any function/method call inside a `try-catch` could
  be raising an exception (#4091)
- cli: if an invalid config is passed to semgrep, it will fail immediately, even
  if valid configs are also passed

### Fixed

- Performance: Deduplicate rules by rule-id + behavior so rules are not being run
  twice
- Scala: recognize metavariables in patterns
- Scala: translate for loops to the generic ast properly
- Catch PCRE errors
- Constant propagation: Avoid "Impossible" errors due to unhandled cases

## [0.72.0](https://github.com/returntocorp/semgrep/releases/tag/v0.72.0) - 2021-11-10

### Added

- Java: Add partial support for `synchronized` blocks in the dataflow IL (#4150)
- Dataflow: Add partial support for `await`, `yield`, `&`, and other expressions
- Field-definition-as-assignemnt equivalence that allows matching expression
  patterns against field definitions. It is disabled by default but can be
  enabled via rule `options:` with `flddef_assign: true` (#4187)
- Arrows (a.k.a short lambdas) patterns used to match also regular function
  definitions. This can now be disabled via rule `options:` with
  `arrow_is_function: false` (#4187)
- Javascript variable patterns using the 'var' keyword used to also
  match variable declarations using 'let' or 'const'. This can now be
  disabled via rule `options:` with `let_is_var: false`

### Fixed

- Constant propagation: In a method call `x.f(y)`, if `x` is a constant then
  it will be recognized as such
- Go: match correctly braces in composite literals for autofix (#4210)
- Go: match correctly parens in cast for autofix (#3387)
- Go: support ellipsis in return type parameters (#2746)
- Scala: parse `case object` within blocks
- Scala: parse typed patterns with variables that begin with an underscore:
  `case _x : Int => ...`
- Scala: parse unicode identifiers
- semgrep-core accepts `sh` as an alias for bash
- pattern-regex: Hexadecimal notation of Unicode code points is now
  supported and assumes UTF-8 (#4240)
- pattern-regex: Update documentation, specifying we use PCRE (#3974)
- Scala: parse nullary constructors with no arguments in more positions
- Scala: parse infix type operators with tuple arguments
- Scala: parse nested comments
- Scala: parse `case class` within blocks
- `metavariable-comparison`: if a metavariable binds to a code variable that
  is known to be constant, then we use that constant value in the comparison (#3727)
- Expand `~` when resolving config paths

### Changed

- C# support is now GA
- cli: Only suggest increasing stack size when semgrep-core segfaults
- Semgrep now scans executable scripts whose shebang interpreter matches the
  rule's language

## [0.71.0](https://github.com/returntocorp/semgrep/releases/tag/v0.71.0) - 2021-11-01

### Added

- Metavariable equality is enforced across sources/sanitizers/sinks in
  taint mode, and these metavariables correctly appear in match messages
- Pre-alpha support for Bash as a new target language
- Pre-alpha support for C++ as a new target language
- Increase soft stack limit when running semgrep-core (#4120)
- `semgrep --validate` runs metachecks on the rule

### Fixed

- text_wrapping defaults to MAX_TEXT_WIDTH if get_terminal_size reports
  width < 1
- Metrics report the error type of semgrep core errors (Timeout,
  MaxMemory, etc.)
- Prevent bad settings files from crashing Semgrep (#4164)
- Constant propagation: Tuple/Array destructuring assignments now correctly
  prevent constant propagation
- JS: Correctly parse metavariables in template strings
- Scala: parse underscore separators in number literals, and parse
  'l'/'L' long suffix on number literals
- Scala: parse by name arguments in arbitary function types,
  like `(=> Int) => Int`
- Bash: various fixes and improvements
- Kotlin: support ellipsis in class body and parameters (#4141)
- Go: support method interface pattern (#4172)

### Changed

- Report CI environment variable in metrics for better environment
  determination
- Bash: a simple expression pattern can now match any command argument rather
  than having to match the whole command

## [0.70.0](https://github.com/returntocorp/semgrep/releases/tag/v0.70.0) - 2021-10-19

### Added

- Preliminary support for bash

### Fixed

- Go: support ... in import list (#4067),
  for example `import (... "error" ...)`
- Java: ... in method chain calls can now match also 0 elements, to be
  consistent with other use of ... (#4082), so `o. ... .foo()` will now
  also match just `o.foo()`.
- Config files with only a comment give bad error message (#3773)
- Does not crash if user does not have write permissions on home directory

### Changed

- Resolution of rulesets use legacy registry instead of cdn registry
- Benchmark suite is easier to modify

## [0.69.1](https://github.com/returntocorp/semgrep/releases/tag/v0.69.1) - 2021-10-14

### Fixed

- The `--enable-metrics` flag is now always a flag, does not optionally
  take an argument

## [0.69.0](https://github.com/returntocorp/semgrep/releases/tag/v0.69.0) - 2021-10-13

### Added

- C: support ... in parameters and sizeof arguments (#4037)
- C: support declaration and function patterns
- Java: support @interface pattern (#4030)

### Fixed

- Reverted change to exclude minified files from the scan (see changelog for
  0.66.0)
- Java: Fixed equality of metavariables bounded to imported classes (#3748)
- Python: fix range of tuples (#3832)
- C: fix some wrong typedef inference (#4054)
- Ruby: put back equivalence on old syntax for keyword arguments (#3981)
- OCaml: add body of functor in AST (#3821)

### Changed

- taint-mode: Introduce a new kind of _not conflicting_ sanitizer that must be
  declared with `not_conflicting: true`. This affects the change made in 0.68.0
  that allowed a sanitizer like `- pattern: $F(...)` to work, but turned out to
  affect our ability to specify sanitization by side-effect. Now the default
  semantics of sanitizers is reverted back to the same as before 0.68.0, and
  `- pattern: $F(...)` is supported via the new not-conflicting sanitizers.

## [0.68.2](https://github.com/returntocorp/semgrep/releases/tag/v0.68.2) - 2021-10-07

### Fixed

- Respect --skip-unknown-extensions even for files with no extension
  (treat no extension as an unknown extension)
- taint-mode: Fixed (another) bug where a tainted sink could go unreported when
  the sink is a specific argument in a function call

## [0.68.1](https://github.com/returntocorp/semgrep/releases/tag/v0.68.1) - 2021-10-07

### Added

- Added support for `raise`/`throw` expressions in the dataflow engine and
  improved existing support for `try-catch-finally`

### Fixed

- Respect rule level path filtering

## [0.68.0](https://github.com/returntocorp/semgrep/releases/tag/v0.68.0) - 2021-10-06

### Added

- Added "automatic configuration" (`--config auto`), which collaborates with
  the Semgrep Registry to customize rules to a project; to support this, we
  add support for logging-in to the Registry using the project URL; in
  a future release, this will also perform project analysis to determine
  project languages and frameworks
- Input can be derived from subshells: `semgrep --config ... <(...)`
- Java: support '...' in catch (#4002)

### Changed

- taint-mode: Sanitizers that match exactly a source or a sink are filtered out,
  making it possible to use `- pattern: $F(...)` for declaring that any other
  function is a sanitizer
- taint-mode: Remove built-in source `source(...)` and built-in sanitizer
  `sanitize(...)` used for convenience during early development, this was causing
  some unexpected behavior in real code that e.g. had a function called `source`!
- When enabled, metrics now send the hashes of rules that yielded findings;
  these will be used to tailor rules on a per-project basis, and also will be
  used to improve rules over time
- Improved Kotlin parsing from 77% to 90% on our Kotlin corpus.
- Resolution of rulesets (i.e. `p/ci`) use new rule cdn and do client-side hydration
- Set pcre recursion limit so it will not vary with different installations of pcre
- Better pcre error handling in semgrep-core

### Fixed

- taint-mode: Fixed bug where a tainted sink could go unreported when the sink is
  a specific argument in a function call
- PHP: allows more keywords as valid field names (#3954)

## [0.67.0](https://github.com/returntocorp/semgrep/releases/tag/v0.67.0) - 2021-09-29

### Added

- Added support for break and continue in the dataflow engine
- Added support for switch statements in the dataflow engine

### Changed

- Taint no longer analyzes dead/unreachable code
- Improve error message for segmentation faults/stack overflows
- Attribute-expression equivalence that allows matching expression patterns against
  attributes, it is enabled by default but can be disabled via rule `options:` with
  `attr_expr: false` (#3489)
- Improved Kotlin parsing from 35% to 77% on our Kotlin corpus.

### Fixed

- Fix CFG dummy nodes to always connect to exit node
- Deep ellipsis `<... x ...>` now matches sub-expressions of statements
- Ruby: treat 'foo' as a function call when alone on its line (#3811)
- Fixed bug in semgrep-core's `-filter_irrelevant_rules` causing Semgrep to
  incorrectly skip a file (#3755)

## [0.66.0](https://github.com/returntocorp/semgrep/releases/tag/v0.66.0) - 2021-09-22

### Added

- HCL (a.k.a Terraform) experimental support

### Changed

- **METRICS COLLECTION CHANGES**: In order to target development of Semgrep features, performance improvements,
  and language support, we have changed how metrics are collected by default
  - Metrics collection is now controlled with the `--metrics` option, with possible values: `auto`, `on`, or `off`
  - `auto` will send metrics only on runs that include rules are pulled from the Semgrep Registry.
    It will not send metrics when rules are only read from local files or passed directly as
    strings
  - `auto` is now the default metrics collection state
  - `on` forces metrics collection on every run
  - `off` disables metrics collection entirely
  - Metrics collection may still alternatively be controlled with the `SEMGREP_SEND_METRICS`
    environment variable, with the same possible values as the `--metrics` option. If both
    are set, `--metrics` overrides `SEMGREP_SEND_METRICS`
  - See `PRIVACY.md` for more information
- Constant propagation now assumes that void methods may update the callee (#3316)
- Add rule message to emacs output (#3851)
- Show stack trace on fatal errors (#3876)
- Various changes to error messages (#3827)
- Minified files are now automatically excluded from the scan, which
  may result in shorter scanning times for some projects.

### Fixed

- Dataflow: Recognize "concat" method and interpret it in a language-dependent manner (#3316)
- PHP: allows certain keywords as valid field names (#3907)

## [0.65.0](https://github.com/returntocorp/semgrep/releases/tag/v0.65.0) - 2021-09-13

### Added

- Allow autofix using the command line rather than only with the fix: YAML key
- Vardef-assign equivalence can now be disabled via rule `options:` with `vardef_assign: false`

### Changed

- Grouped semgrep CLI options and added constraints when useful (e.g. cannot use `--vim` and `--emacs` at the same time)

### Fixed

- Taint detection with ternary ifs (#3778)
- Fixed corner-case crash affecting the `pattern: $X` optimization ("empty And; no positive terms in And")
- PHP: Added support for parsing labels and goto (#3592)
- PHP: Parse correctly constants named PUBLIC or DEFAULT (#3589)
- Go: Added type inference for struct literals (#3622)
- Fix semgrep-core crash when a cache file exceeds the file size limit
- Sped up Semgrep interface with tree-sitter parsing

## [0.64.0](https://github.com/returntocorp/semgrep/releases/tag/v0.64.0) - 2021-09-01

### Added

- Enable associative matching for string concatenation (#3741)

### Changed

- Add logging on failure to git ls-files (#3777)
- Ignore files whose contents look minified (#3795)
- Display semgrep-core errors in a better way (#3774)
- Calls to `semgrep --version` now check if Semgrep is up-to-date; this can
  cause a ~ 100 ms delay in run time; use --disable-version-check if you
  don't want this

### Fixed

- Java: separate import static from regular imports during matching (#3772)
- Taint mode will now benefit from semgrep-core's -filter_irrelevant_rules
- Taint mode should no longer report duplicate matches (#3742)
- Only change source directory when running in docker context (#3732)

## [0.63.0](https://github.com/returntocorp/semgrep/releases/tag/v0.63.0) - 2021-08-25

### Added

- C#: support ellipsis in declarations (#3720)

### Fixed

- Hack: improved support for metavariables (#3716)
- Dataflow: Disregard type arguments but not the entire instruction

### Changed

- Optimize ending `...` in `pattern-inside`s to simply match anything left

## [0.62.0](https://github.com/returntocorp/semgrep/releases/tag/v0.62.0) - 2021-08-17

### Added

- OCaml: support module aliasing, so looking for `List.map` will also
  find code that renamed `List` as `L` via `module L = List`.
- Add help text to sarif formatter output if defined in metadata field.
- Update shortDescription in sarif formatter output if defined in metadata field.
- Add tags as defined in metadata field in addition to the existing tags.

### Fixed

- core: Fix parsing of numeric literals in rule files
- Java: fix the range and autofix of Cast expressions (#3669)
- Generic mode scanner no longer tries to open submodule folders as files (#3701)
- `pattern-regex` with completely empty files (#3705)
- `--sarif` exit code with suppressed findings (#3680)
- Fixed fatal errors when a pattern results in a large number of matches
- Better error message when rule contains empty pattern

### Changed

- Add backtrace to fatal errors reported by semgrep-core
- Report errors during rule evaluation to the user
- When anded with other patterns, `pattern: $X` will not be evaluated on its own, but will look at the context and find `$X` within the metavariables bound, which should be significantly faster

## [0.61.0](https://github.com/returntocorp/semgrep/releases/tag/v0.61.0) - 2021-08-04

### Added

- Hack: preliminary support for hack-lang
  thanks to David Frankel, Nicholas Lin, and more people at Slack!
- OCaml: support for partial if, match, and try patterns
  (e.g., `if $X = $Y`)
- OCaml: you can match uppercase identifiers (constructors, module names) by
  using a metavariable with an uppercase letter followed by an underscore,
  followed by uppercase letters or digits (e.g. `$X_`, `$F_OO`).
  Instead, `$FOO` will match everything else (lowercase identifiers,
  full expressions, types, patterns, etc.).
- OCaml: match cases patterns are now matched in any order, and ellipsis are
  handled correctly
- Improved error messages sent to the playground

### Changed

- Run version check and print upgrade message after scan instead of before
- OCaml: skip ocamllex and ocamlyacc files. Process only .ml and .mli files.
- Memoize range computation for expressions and speed up taint mode
- Report semgrep-core's message upon a parse error
- Deprecated the following experimental features:
  - pattern-where-python
  - taint-mode
  - equivalences
  - step-by-step evaluation output
- Deduplicate findings that fire on the same line ranges and have the same message.

### Fixed

- Go: Match import module paths correctly (#3484)
- OCaml: use latest ocamllsp 1.7.0 for the -lsp option
- OCaml: include parenthesis tokens in the AST for tuples and constructor
  calls for better range matching and autofix
- OCaml: fixed many matching bugs with ellipsis
- core: Do not crash when is not possible to compute range info
- eliminate 6x slowdown when using the '--max-memory' option

## [0.60.0](https://github.com/returntocorp/semgrep/releases/tag/v0.60.0) - 2021-07-27

### Added

- Detect duplicate keys in YAML dictionaries in semgrep rules when parsing a rule
  (e.g., detect multiple 'metavariable' inside one 'metavariable-regex')

### Fixed

- C/C++: Fixed stack overflows (segmentation faults) when processing very large
  files (#3538)
- JS: Fixed stack overflows (segmentation faults) when processing very large
  files (#3538)
- JS: Detect numeric object keys `1` and `0x1` as equal (#3579)
- OCaml: improved parsing stats by using tree-sitter-ocaml (from 25% to 88%)
- taint-mode: Check nested functions
- taint-mode: `foo.x` is now detected as tainted if `foo` is a source of taint
- taint-mode: Do not crash when is not possible to compute range info
- Rust: recognize ellipsis in macro calls patterns (#3600)
- Ruby: represent correctly a.(b) in the AST (#3603)
- Rust: recognize ellipsis in macro calls patterns

### Changed

- Added precise error location for the semgrep metachecker, to detect for example
  duplicate patterns in a rule

## [0.59.0](https://github.com/returntocorp/semgrep/releases/tag/v0.59.0) - 2021-07-20

### Added

- A new experimental 'join' mode. This mode runs multiple Semgrep rules
  on a codebase and "joins" the results based on metavariable contents. This
  lets users ask questions of codebases like "do any 3rd party
  libraries use a dangerous function, and do I import that library directly?" or
  "is this variable passed to an HTML template, and is it rendered in that template?"
  with several Semgrep rules.

### Fixed

- Improve location reporting of errors
- metavariable-pattern: `pattern-not-regex` now works (#3503)
- Rust: correctly parse macros (#3513)
- Python: imports are unsugared correctly (#3940)
- Ruby: `pattern: $X` in the presence of interpolated strings now works (#3560)

## [0.58.2](https://github.com/returntocorp/semgrep/releases/tag/v0.58.2) - 2021-07-15

### Fixed

- Significant speed improvements, but the binary is now 95MB (from 47MB
  in 0.58.1, but it was 170MB in 0.58.0)

## [0.58.1](https://github.com/returntocorp/semgrep/releases/tag/v0.58.1) - 2021-07-15

### Fixed

- The --debug option now displays which files are currently processed incrementally;
  it will not wait until semgrep-core completely finishes.

### Changed

- Switch from OCaml 4.10.0 to OCaml 4.10.2 (and later to OCaml 4.12.0) resulted in
  smaller semgrep-core binaries (from 170MB to 47MB) and a smaller docker
  image (from 95MB to 40MB).

## [0.58.0](https://github.com/returntocorp/semgrep/releases/tag/v0.58.0) - 2021-07-14

### Added

- New iteration of taint-mode that allows to specify sources/sanitizers/sinks
  using arbitrary pattern formulas. This provides plenty of flexibility. Note
  that we breaks compatibility with the previous taint-mode format, e.g.
  `- source(...)` must now be written as `- pattern: source(...)`.
- HTML experimental support. This does not rely on the "generic" mode
  but instead really parses the HTML using tree-sitter-html. This allows
  some semantic matching (e.g., matching attributes in any order).
- Vue.js alpha support (#1751)
- New matching option `implicit_ellipsis` that allows disabling the implicit
  `...` that are added to record patterns, plus allow matching "spread fields"
  (JS `...x`) at any position (#3120)
- Support globstar (`**`) syntax in path include/exclude (#3173)

### Fixed

- Apple M1: Semgrep installed from HomeBrew no longer hangs (#2432)
- Ruby command shells are distinguished from strings (#3343)
- Java varargs are now correctly matched (#3455)
- Support for partial statements (e.g., `try { ... }`) for Java (#3417)
- Java generics are now correctly stored in the AST (#3505)
- Constant propagation now works inside Python `with` statements (#3402)
- Metavariable value replacement in message/autofix no longer mixes up short and long names like $X vs $X2 (#3458)
- Fixed metavariable name collision during interpolation of message / autofix (#3483)
  Thanks to Justin Timmons for the fix!
- Revert `pattern: $X` optimization (#3476)
- metavariable-pattern: Allow filtering using a single `pattern` or
  `pattern-regex`
- Dataflow: Translate call chains into IL

### Changed

- Faster matching times for generic mode

## [0.57.0](https://github.com/returntocorp/semgrep/releases/tag/v0.57.0) - 2021-06-29

### Added

- new `options:` field in a YAML rule to enable/disable certain features
  (e.g., constant propagation). See https://github.com/returntocorp/semgrep/blob/develop/semgrep-core/src/core/Config_semgrep.atd
  for the list of available features one can enable/disable.
- Capture groups in pattern-regex: in $1, $2, etc. (#3356)
- Support metavariables inside atoms (e.g., `foo(:$ATOM)`)
- Support metavariables and ellipsis inside regexp literals
  (e.g., `foo(/.../)`)
- Associative-commutative matching for bitwise OR, AND, and XOR operations
- Add support for $...MVAR in generic patterns.
- metavariable-pattern: Add support for nested Spacegrep/regex/Comby patterns
- C#: support ellipsis in method parameters (#3289)

### Fixed

- C#: parse `__makeref`, `__reftype`, `__refvalue` (#3364)
- Java: parsing of dots inside function annotations with brackets (#3389)
- Do not pretend that short-circuit Boolean AND and OR operators are commutative (#3399)
- metavariable-pattern: Fix crash when nesting a non-generic pattern within
  a generic rule
- metavariable-pattern: Fix parse info when matching content of a metavariable
  under a different language
- generic mode on Markdown files with very long lines will now work (#2987)

### Changed

- generic mode: files that don't look like nicely-indented programs
  are no longer ignored, which may cause accidental slowdowns in setups
  where excessively large files are not excluded explicitly (#3418).
- metavariable-comparison: Fix crash when comparing integers and floats
  Thanks to Justin Timmons for the fix!
- Do not filter findings with the same range but different metavariable bindings (#3310)
- Set parsing_state.have_timeout when a timeout occurs (#3438)
- Set a timeout of 10s per file (#3434)
- Improvements to contributing documentation (#3353)
- Memoize getting ranges to speed up rules with large ranges
- When anded with other patterns, `pattern: $X` will not be evaluated on its own, but will look at the context and find `$X` within the metavariables bound, which should be significantly faster

## [0.56.0](https://github.com/returntocorp/semgrep/releases/tag/v0.56.0) - 2021-06-15

### Added

- Associative-commutative matching for Boolean AND and OR operations
  (#3198)
- Support metavariables inside strings (e.g., `foo("$VAR")`)
- metavariable-pattern: Allow matching the content of a metavariable under
  a different language.

### Fixed

- C#: Parse attributes for local functions (#3348)
- Go: Recognize other common package naming conventions (#2424)
- PHP: Support for associative-commutative matching (#3198)

### Changed

- Upgrade TypeScript parser (#3102)

### Changed

- `--debug` now prints out semgrep-core debug logs instead of having this
  behavior with `--debugging-json`

## [0.55.1](https://github.com/returntocorp/semgrep/releases/tag/v0.55.1) - 2021-06-9

### Added

- Add helpUri to sarif output if rule source metadata is defined

### Fixed

- JSON: handle correctly metavariables as field (#3279)
- JS: support partial field definitions pattern, like in JSON
- Fixed wrong line numbers for multi-lines match in generic mode (#3315)
- Handle correctly ellipsis inside function types (#3119)
- Taint mode: Allow statement-patterns when these are represented as
  statement-expressions in the Generic AST (#3191)

## [0.55.0](https://github.com/returntocorp/semgrep/releases/tag/v0.55.0) - 2021-06-8

### Added

- Added new metavariable-pattern operator (available only via --optimizations),
  thanks to Kai Zhong for the feature request (#3257).

### Fixed

- Scala: parse correctly symbol literals and interpolated strings containing
  double dollars (#3271)
- Dataflow: Analyze foreach body even if we do not handle the pattern yet (#3155)
- Python: support ellipsis in try-except (#3233)
- Fall back to no optimizations when using unsupported features: pattern-where-python,
  taint rules, and `--debugging-json` (#3265)
- Handle regexp parse errors gracefully when using optimizations (#3266)
- Support equivalences when using optimizations (#3259)
- PHP: Support ellipsis in include/require and echo (#3191, #3245)
- PHP: Prefer expression patterns over statement patterns (#3191)
- C#: Support unsafe block syntax (#3283)

### Changed

- Run rules in semgrep-core (rather than patterns) by default (aka optimizations all)

## [0.54.0](https://github.com/returntocorp/semgrep/releases/tag/v0.54.0) - 2021-06-2

### Added

- Per rule parse times and per rule-file parse and match times added to opt-in metrics
- $...MVAR can now match a list of statements (not just a list of arguments) (#3170)

### Fixed

- JavaScript parsing: [Support decorators on
  properties](https://github.com/tree-sitter/tree-sitter-javascript/pull/166)
- JavaScript parsing: [Allow default export for any declaration](https://github.com/tree-sitter/tree-sitter-javascript/pull/168)
- Metavariables in messages are filled in when using `--optimizations all`
- Python: class variables are matched in any order (#3212)
- Respect `--timeout-threshold` option in `--optimizations all` mode

### Changed

- Moved some debug logging to verbose logging
- $...ARGS can now match an empty list of arguments, just like ... (#3177)
- JSON and SARIF outputs sort keys for predictable results

## [0.53.0](https://github.com/returntocorp/semgrep/releases/tag/v0.53.0) - 2021-05-26

### Added

- Scala alpha support
- Metrics collection of project_hash in cases where git is not available
- Taint mode now also analyzes top-level statements.

### Fixed

- Running with `--strict` will now return results if there are `nosem` mismatches. Semgrep will report a nonzero exit code if `--strict` is set and there are `nosem` mismathces. [#3099](https://github.com/returntocorp/semgrep/issues/3099)
- PHP: parsing correctly ... and metavariables in parameters
- PHP: parsing correctly functions with a single statement in their body
- Evaluate interpolated strings during constant propagation (#3127)
- Fixed #3084 - Semgrep will report an InvalidRuleSchemaError for dictionaries with duplicate key names.
- Basic type inference also for implicit variable declarations (Python, Ruby, PHP, and JS)
- JS/TS: differentiating tagged template literals in the AST (#3187)
- Ruby: storing parenthesis in function calls in the AST (#3178)

## [0.52.0](https://github.com/returntocorp/semgrep/releases/tag/v0.52.0) - 2021-05-18

### Added

- C# alpha support
- Let meta-variables match both a constant variable occurrence and that same
  constant value (#3058)

### Fixed

- OCaml: fix useless-else false positives by generating appropriate AST for
  if without an else.
- JS/TS: Propagate constant definitions without declaration
- Python: Make except ... match except _ as _

## [0.51.0](https://github.com/returntocorp/semgrep/releases/tag/v0.51.0) - 2021-05-13

### Added

- Keep track of and report rule parse time in addition to file parse time.
- v0 of opt-in anonymous aggregate metrics.
- Improved cheatsheet for generic mode, now recommending indented
  patterns (#2911, #3028).

### Fixed

- JS/TS: allow the deep expression operator <... ...> in expression
  statement position, for example:

```
$ARG = [$V];
...
<... $O[$ARG] ...>; // this works now
```

- PHP arrays with dots inside parse
- Propagate constants in nested lvalues such as `y` in `x[y]`
- C# experimental support

### Changed

- Show log messages from semgrep-core when running semgrep with
  `--debug`.
- By default, targets larger than 1 MB are now excluded from semgrep
  scans. New option `--max-target-bytes 0` restores the old behavior.
- Report relative path instead of absolute when using `--time`

## [0.50.1](https://github.com/returntocorp/semgrep/releases/tag/v0.50.1) - 2021-05-06

### Changed

- Reinstate `--debugging-json` to avoid stderr output of `--debug`

## [0.50.0](https://github.com/returntocorp/semgrep/releases/tag/v0.50.0) - 2021-05-06

### Added

- JS/TS: Infer global constants even if the `const` qualifier is missing (#2978)
- PHP: Resolve names and infer global constants in the same way as for Python

### Fixed

- Empty yaml files do not crash
- Autofix does not insert newline characters for patterns from semgrep.live (#3045)
- Autofix printout is grouped with its own finding rather than the one below it (#3046)
- Do not assign constant values to assigned variables (#2805)
- A `--time` flag instead of `--json-time` which shows a summary of the
  timing information when invoked with normal output and adds a time field
  to the json output when `--json` is also present

### Changed

- .git/ directories are ignored when scanning
- External Python API (`semgrep_main.invoke_semgrep`) now takes an
  optional `OutputSettings` argument for controlling output
- `OutputSettings.json_time` has moved to `OutputSettings.output_time`,
  this and many other `OutputSettings` arguments have been made optional

### Removed

- `--debugging-json` flag in favor of `--json` + `--debug`
- `--json-time` flag in favor of `--json` + `--time`

## [0.49.0](https://github.com/returntocorp/semgrep/releases/tag/v0.49.0) - 2021-04-28

### Added

- Support for matching multiple arguments with a metavariable (#3009)
  This is done with a 'spread metavariable' operator that looks like
  `$...ARGS`. This used to be available only for JS/TS and is now available
  for the other languages (Python, Java, Go, C, Ruby, PHP, and OCaml).
- A new `--optimizations [STR]` command-line flag to turn on/off some
  optimizations. Use 'none' to turn off everything and 'all' to turn on
  everything.
  Just using `--optimizations` is equivalent to `--optimizations all`, and
  not using `--optimizations` is equivalent to `--optimizations none`.
- JS/TS: Support '...' inside JSX text to match any text, as in
  `<a href="foo">...</a>` (#2963)
- JS/TS: Support metavariables for JSX attribute values, as in
  `<a href=$X>some text</a>` (#2964)

### Fixed

- Python: correctly parsing fstring with multiple colons
- Ruby: better matching for interpolated strings (#2826 and #2949)
- Ruby: correctly matching numbers

### Changed

- Add required executionSuccessful attribute to SARIF output (#2983)
  Thanks to Simon Engledew
- Remove jsx and tsx from languages, just use javascript or typescript (#3000)
- Add limit max characters in output line (#2958) and add
  flag to control maxmium characters (defaults to 160).
  Thanks to Ankush Menat

## [0.48.0](https://github.com/returntocorp/semgrep/releases/tag/v0.48.0) - 2021-04-20

### Added

- Taint mode: Basic cross-function analysis (#2913)
- Support for the new Java Record extension and Java symbols with accented characters (#2704)

### Fixed

- Capturing functions when used as both expressions and statements in JS (#1007)
- Literal for ocaml tree sitter (#2885)
- Ruby: interpolated strings match correctly (#2967)
- SARIF output now contains the required runs.invocations.executionSuccessful property.

### Changed

- The `extra` `lines` data is now consistent across scan types
  (e.g. `semgrep-core`, `spacegrep`, `pattern-regex`)

## [0.47.0](https://github.com/returntocorp/semgrep/releases/tag/v0.47.0) - 2021-04-15

### Added

- support `for(...)` for Java
- Ability to match lambdas or functions in Javascript with ellipsis after
  the function keyword, (e.g., `function ...(...) { ... }`)
- Rust: Semgrep patterns now support top-level statements (#2910)
- support for utf-8 code with non-ascii chars (#2944)
- Java switch expressions

### Fixed

- fixed single field pattern in JSON, allow `$FLD: { ... }` pattern
- Config detection in files with many suffix delimiters, like `this.that.check.yaml`.
  More concretely: configs end with `.yaml`, YAML language tests end with `.test.yaml`,
  and everything else is handled by its respective language extension (e.g. `.py`).
- Single array field in yaml in a pattern is parsed as a field, not a one element array

## [0.46.0](https://github.com/returntocorp/semgrep/releases/tag/v0.46.0) - 2021-04-08

### Added

- YAML language support to --test
- Ability to list multiple, comma-separated rules on the same line when in --test mode
- Resolve alias in require/import in Javascript

```
child_process.exec(...)
```

will now match

```javascript
var { exec } = require("child_process");
exec("dangerous");
```

- Taint mode: Pattern-sources can now be arbitrary expressions (#2881)

### Fixed

- SARIF output now nests invocations inside runs.
- Go backslashed carets in regexes can be parsed

### Changed

- Deep expression matches (`<... foo ...>`) now match within records, bodies of
  anonymous functions (a.k.a. lambda-expressions), and arbitrary language-specific
  statements (e.g. the Golang `go` statement)

## [0.45.0](https://github.com/returntocorp/semgrep/releases/tag/v0.45.0) - 2021-03-30

### Added

- New `--experimental` flag for passing rules directly to semgrep-core (#2836)

### Fixed

- Ellipses in template strings don't match string literals (#2780)
- Go: correctly parse select/switch clauses like in tree-sitter (#2847)
- Go: parse correctly 'for ...' header in Go patterns (#2838)

## [0.44.0](https://github.com/returntocorp/semgrep/releases/tag/v0.44.0) - 2021-03-25

### Added

- Support for YAML! You can now write YAML patterns in rules
  to match over YAML target files (including semgrep YAML rules, inception!)
- A new Bloomfilter-based optimisation to speedup matching (#2816)
- Many benchmarks to cover semgrep advertised packs (#2772)
- A new semgrep-dev docker container useful for benchmarking semgrep (#2800)
- Titles to rule schema definitions, which can be leveraged in
  the Semgrep playground (#2703)

### Fixed

- Fixed taint mode and added basic test (#2786)
- Included formatted errors in SARIF output (#2748)
- Go: handle correctly the scope of Go's short assignment variables (#2452)
- Go: fixed the range of matched slices (#2763)
- PHP: correctly match the PHP superglobal `$_COOKIE` (#2820)
- PHP: allow ellipsis inside array ranges (#2819)
- JSX/TSX: fixed the range of matched JSX elements (#2685)
- Javascript: allow ellipsis in arrow body (#2802)
- Generic: correctly match the same metavariable when used in different
  generic patterns

#### Fixed in `semgrep-core` only

These features are not yet available via the `semgrep` CLI,
but have been fixed to the internal `semgrep-core` binary.

- Fixed all regressions on semgrep-rules when using -fast
- Handle pattern-not: and pattern-not-inside: as in semgrep
- Handle pattern: and pattern-inside: as in semgrep (#2777)

## [0.43.0](https://github.com/returntocorp/semgrep/releases/tag/v0.43.0) - 2021-03-16

### Added

- Official Python 3.9 support
- Support for generating patterns that will match multiple given code targets
- Gitignore for compiled binaries

### Fixed

- Parsing enum class patterns (#2715)
- Ocaml test metavar_equality_var (#2755)

### Changed

- Pfff java parser and tree-sitter-java parser are now more similar
- Octal numbers parsed correctly in tree-sitter parsers

## [0.42.0](https://github.com/returntocorp/semgrep/releases/tag/v0.42.0) - 2021-03-09

### Added

- Added propagation of metavariables to clauses nested under `patterns:`. Fixes (#2548)[https://github.com/returntocorp/semgrep/issues/2548].
- `--json-time` flag which reports runtimes for (rule, target file)
- `--vim` flag for Syntastic
- PHP - Support for partial if statements
- CSharp - Many improvements to parsing

### Fixed

- Rust can be invoked with `rs` or `rust` as a language

### Changed

- The timeout for downloading config files from a URL was extended from 10s to 20s.

## [0.41.1](https://github.com/returntocorp/semgrep/releases/tag/v0.41.1) - 2021-02-24

### Fixed

- Statically link pcre in semgrep-core for MacOS releases

## [0.41.0](https://github.com/returntocorp/semgrep/releases/tag/v0.41.0) - 2021-02-24

### Added

- Added basic typed metavariables for javascript and typescript (#2588)
- Ability to match integers or floats by values
  e.g., the pattern '8' will now match code like 'x = 0x8'
- Start converting the tree-sitter CST of R to the generic AST
  thx to Ross Nanopoulos!
- Allow 'nosem' in HTML. (#2574)

#### Added in `semgrep-core` only

These features are not yet available via the `semgrep` CLI,
but have been added to the internal `semgrep-core` binary.

- ability to process a whole rule in semgrep-core; this will allow
  whole-rule optimisations and avoid some fork and communication with the
  semgrep Python wrapper
- handling the none (regexp) and generic (spacegrep) patterns in a rule
- handling the metavariable-regexp, metavariable-comparison
- correctly handle boolean formula using inclusion checks on metavariables
- new semgrep-core -test_rules action to test rules; it reports only
  28/2800 mismatches on the semgrep-rules repository

### Changed

- update C# to latest tree-sitter-csharp
  thx to Sjord for the huge work adapting to the new C# grammar
- Improve --generate-config capabilities (#2562)
- optimise the matching of blocks with ellipsis (#2618)
  e.g., the pattern 'function(...) { ... }' will now be more efficient
- Change pattern-not-regex to filter when regex overlaps with a match (#2572)

### Fixed

- remove cycle in named AST for Rust 'fn foo(self)' (#2584)
  and also typescript, which could cause semgrep to use giga bytes of memory
- fix missing token location on Go type assertion (#2577)

## [0.40.0](https://github.com/returntocorp/semgrep/releases/tag/v0.40.0) - 2021-02-17

### Added

- Documentation for contributing new languages.
- New language Kotlin with experimental support.
- Work on caching improvements for semgrep-core.
- Work on bloom filters for matching performance improvement.

### Changed

- Typescript grammar upgraded.
- Ruby parser updated from the latest tree-sitter-ruby.
- New Semgrep logo!
- metavariable_regex now supported with PCRE.
- Rust macros now parsed. Thanks Ruin0x11!

### Fixed

- Constant propagaion support covers `:=` short assignment in Go. (#2440)
- Functions now match against functions inside classes for PHP. (#2470)
- Import statements for CommonJS Typescript modules now supported. (#2234)
- Ellipsis behave consistently in nested statements for PHP. (#2453)
- Go Autofix does not drop closing parenthesis. (#2316)
- Helpful errors added for Windows installation. (#2533)
- Helpful suggestions provided on output encoding error. (#2514)
- Import metavariables now bind to the entire Java path. (#2502)
- Semgrep matches the short name for a type in Java. (#2400)
- Interface types explicitly handled in Go patterns. (#2376)
- TooManyMatches error generated instead of Timeout error when appropriate. (#2411)

## [0.39.1](https://github.com/returntocorp/semgrep/releases/tag/v0.39.1) - 2021-01-26

No new changes in this version.
This is a re-release of 0.39.0 due to an error in the release process.

## [0.39.0](https://github.com/returntocorp/semgrep/releases/tag/v0.39.0) - 2021-01-26

### Added

- Typed metavariables in C.
  Patterns like `$X == $Y` can now match specific types like so: `(char *$X) == $Y`. (#2431)

#### Added in `semgrep-core` only

These features are not yet available via the `semgrep` CLI,
but have been added to the internal `semgrep-core` binary.

- `semgrep-core` supports rules in JSON and Jsonnet format. (#2428)
- `semgrep-core` supports a new nested format
  for combining patterns into a boolean query. (#2430)

### Changed

- When an unknown language is set on a rule,
  the error message now lists all supported languages. (#2448)
- When semgrep is executed without a config specified,
  the error message now includes some suggestions on how to pick a config. (#2449)
- `-c` is the new shorthand for `--config` in the CLI.
  `-f` is kept as an alias for backward-compatibility. (#2447)

### Fixed

- Disable timeouts if timeout setting is 0 (#2423).
- Typed metavariables in go match literal strings (#2401).
- Fix bug that caused m_compatible_type to only bind the type (#2441).

## [0.38.0](https://github.com/returntocorp/semgrep/releases/tag/v0.38.0) - 2021-01-20

### Added

- Added a new language: Rust. Support for basic semgrep patterns (#2391)
  thanks to Ruin0x11!
- Added a new language: R. Just parsing for now (#2407)
  thanks to Ross Nanopoulos!
- Parse more Rust constructs: Traits, type constraints (#2393, #2413)
  thanks to Ruin0x11!
- Parse more C# constructs: Linq queries, type parameter constraints (#2378, #2408)
  thanks to Sjord!
- new experimental semgrep rule (meta)linter (#2420) with semgrep-core -check_rules

### Changed

- new controlflow-sensitive intraprocedural dataflow-based constant propagation
  (#2386)

### Fixed

- matching correctly Ruby functions with rescue block (#2390)
- semgrep crashing on permission error on a file (#2394)
- metavariable interpolation for pattern-inside (#2361)
- managing Lua assignment correctly (#2406) thanks to Ruin0x11!
- correctly parse metavariables in PHP, and ellipsis in fields (#2419)

## [0.37.0](https://github.com/returntocorp/semgrep/releases/tag/v0.37.0) - 2021-01-13

### Added

- pattern-not-regex added so findings can be filtered using regular expression (#2364)
- Added a new language: Lua. Support for basic semgrep patterns (#2337, #2312)
  thanks to Ruin0x11!
- C# support for basic semgrep patterns (#2336)
- Parse event access, conditional access, async-await in C# (#2314, #2329, #2358)
  thanks to Sjord

### Changed

- Java and Javascript method chaining requires extra "." when using ellipsis (#2354)

### Fixed

- Semgrep crashing due to missing token information in AST (#2380)

## [0.36.0](https://github.com/returntocorp/semgrep/releases/tag/v0.36.0) - 2021-01-05

### Added

- Typed metavariables can now match field access when we can propagate
  the type of a field
- Constant propagation for Java final fields (using this.field syntax)

### Changed

- Packaging and `setup.py` functionality (`.whl` and `pip` install unchanged):
  `SEMGREP_SKIP_BIN`, `SEMGREP_CORE_BIN`, and `SPACEGREP_BIN` now available

### Fixed

- correctly match the same metavariable for a field when used at a definition
  site and use site for Java
- add classname attribute to junit.xml report

## [0.35.0](https://github.com/returntocorp/semgrep/releases/tag/v0.35.0) - 2020-12-16

### Added

- Support for `...` in chains of method calls in JS, e.g. `$O.foo() ... .bar()`
- Official Ruby GA support

### Fixed

- Separate out test and pattern files with `--test` (#1796)

## [0.34.0](https://github.com/returntocorp/semgrep/releases/tag/v0.34.0) - 2020-12-09

### Added

- Experimental support for matching multiple arguments in JS/TS.
  This is done with a 'spread metavariable' operator,
  that looks like `$...ARGS`.
- Support for using `...` inside a Golang `switch` statement.
- Support for matching only
  the `try`, the `catch`, or the `finally` part
  of a `try { } catch (e) { } finally { }` construct in JS/TS.
- Support for matching only
  the `if ()` part of
  an `if () { }` construct in Java
- Support for metavariables inside dictionary keys in Ruby.
  This looks like `{..., $KEY: $VAL, ...}`.
- An experimental `--json-stats` flag.
  The stats output contains
  the number of files and lines of code scanned,
  broken down by language.
  It also contains profiling data broken down by rule ID.
  Please note that as this is an experimental flag,
  the output format is subject to change in later releases.
- Regex-only rules can now use `regex` as their language.
  The previously used language `none` will keep working as well.

### Changed

- Matches are now truncated to 10 lines in Semgrep's output.
  This was done to avoid filling the screen with output
  when a rule captures a whole class or function.
  If you'd like to adjust this behavior,
  you can set the new `--max-lines-per-finding` option.
- Fans of explicit & verbose code can now ignore findings
  with a `// nosemgrep` comment instead of the original `// nosem`.
  The two keywords have identical behavior.
- Generic pattern matching is now 10-20% faster
  on large codebases.

### Fixed

- Semgrep would crash when tens of thousands of matches were found
  for the same rule in one file.
  A new internally used `semgrep-core` flag named `-max_match_per_file`
  prevents these crashes by forcing a 'timeout' state
  when 10,000 matches are reached.
  Semgrep can then gracefully report
  what combination of rules and paths causes too much work.
- `semgrep --debug` works again,
  and now outputs even more debugging information from `semgrep-core`.
  The new debugging output is especially helpful to discover
  which rules have too many matches.
- A pattern that looks like `$X & $Y`
  will now correctly match bitwise AND operations in Ruby.
- Metavariables can now capture the name of a class
  and match its occurrences later in the class definition.
- Semgrep used to crash when a metavariable matched
  over text that cannot be read as UTF-8 text.
  Such matches will now try to recover what they can
  from apparent broken unicode text.

## [0.33.0](https://github.com/returntocorp/semgrep/releases/tag/v0.33.0) - 2020-12-01

### Added

- Allow selecting rules based on severity with the `--severity` flag. Thanks @kishorbhat!

### Changed

- In generic mode, shorter matches are now always preferred over
  longer ones. This avoids matches like `def bar def foo` when the
  pattern is `def ... foo`, instead matching just `def foo`
- In generic mode, leading dots must now match at the beginning of a
  block, allowing patterns like `... foo` to match what comes before `foo`
- Disabled link following for parity with other LINUX tools (e.g. ripgrep)
- spacegrep timeouts are now reported as timeouts instead of another error

### Fixed

- Correctly bind a metavariable in an import to the fully-qualified name. [Issue](https://github.com/returntocorp/semgrep/issues/1771)
- Fix invalid match locations on target files containing both CRLF line
  endings UTF-8 characters (#2111)
- Fix NoTokenLocation error when parsing Python f-strings
- [C] Support `include $X`
- [Go] Fix wrong order of imports

## [0.32.0](https://github.com/returntocorp/semgrep/releases/tag/v0.32.0) - 2020-11-18

### Added

- JSON output now includes an attribute of findings named `is_ignored`.
  This is `false` under regular circumstances,
  but if you run with `--disable-nosem`,
  it will return `true` for findings
  that normally would've been excluded by a `// nosem` comment.

### Changed

- `// nosemgrep` can now also be used to ignore findings,
  in addition to `// nosem`
- Added a default timeout of 30 seconds per file instead of none (#1981).

## [0.31.1](https://github.com/returntocorp/semgrep/releases/tag/v0.31.1) - 2020-11-11

### Fixed

- Regression in 0.31.0 where only a single file was being used when `--config`
  was given a directory with multiple rules (#2019).
- Cheatsheet's html functionality now has correct output.

## [0.31.0](https://github.com/returntocorp/semgrep/releases/tag/v0.31.0) - 2020-11-10

### Fixed

- Gracefully handle empty configuration file.
- Gracefully handle LexicalErrors from semgrep-core.
- Fix stack overflows in spacegrep on large input files (#1944).
- Fix extension-based file selection when the language is `generic` (#1968).
- Fix semgrep error when no valid config on path provided (#1912).
- Fix NO_FILE_INFO_YET error which causes the python wrapper to crash (#1925).
- Fix usage of '...' in special builtin arguments for PHP (#1963).
- Fix automatic semicolon insertion parse error in javascript (#1960).

### Added

- kotlin-tree-sitter integration into semgrep-core. Can now call
  dump-tree-sitter-cst on kotlin files.
- c++ tree-sitter integration into semgrep-core (#1952).
- More documents for language porting.
- Error handling in spacegrep to print stderr when CalledProcessError occurs.

## [0.30.0](https://github.com/returntocorp/semgrep/releases/tag/v0.30.0) - 2020-11-03

### Added

- Better examples for the generic mode aka spacegrep (#1951).

### Fixed

- Fix matching of trailing dots in spacegrep (#1939).
- Allow matching on one-line files with spacegrep (#1929).
- Fix incorrect number of lines matched by dots with spacegrep (#1918).
- Other subtle spacegrep matching bugs (#1913).
- Metavariable for method call should be matched against corresponding
  metavariable in method definition (#1861).
- Typescript class properties/declarations not recognized (#1846).
- Can't match inside Python try/except clause (#1902).

## [0.29.0](https://github.com/returntocorp/semgrep/releases/tag/v0.29.0) - 2020-10-27

### Added

- Semgrep will now partially parse files with parse errors and report findings detected before the parse errors was encountered.
- Allow user to specify registry path without having to add semgrep.dev url
  i.e.: instead of `--config https://semgrep.dev/p/r2c-ci` users can use `--config p/r2c-ci`
- Allow user to specify snippet id without having to add semgrep.dev url
  i.e.: instead of `--config https://semgrep.dev/s/username:snippetname`
  users can use `--config username:snippetname`
- `--test` will now error out if `ruleid` or `ok` is not in reported IDs
- Semgrep will run JavaScript rules on TypeScript files automatically.

### Fixed

- More off by one fixes in autofix
- Support for matching dynamic class names in Ruby
- Removed `nosem` findings from the final findings count
- Matching nested JSX elements works properly. See https://semgrep.dev/s/erlE?version=0.29.0.
- Can now match partial class definitions with annotations in Java. See https://github.com/returntocorp/semgrep/issues/1877.
- Fixed errors in TypeScript "implements" keyword. See https://github.com/returntocorp/semgrep/issues/1850.

## [0.28.0](https://github.com/returntocorp/semgrep/releases/tag/v0.28.0) - 2020-10-21

### Added

- A `metavariable-comparison` operator
  for evaluating numeric comparisons on metavariable values,
  such as `comparison: $KEY_SIZE < 2048`.
  This is a safe alternative to `pattern-where-python` snippets.
  Check the [full documentation of this feature](https://github.com/returntocorp/semgrep/blob/12d25a5c/docs/experimental.md#metavariable-comparison)!
- Matching 1-to-N attributes with a `...` wildcard
  in JSX tags' attribute lists,
  such as `<$TAG attr="1" ... />`
- Matching only the function signature
  without the function body,
  such as `function foo(...)`.
  This is useful to have cleaner match output
  when the body content doesn't matter in a rule.
  This works on JavaScript, TypeScript, and Java code currently.
- SARIF output now includes the exact CWE and OWASP categories as tags.
  Thanks @hunt3rkillerz!
- Matching of annotation patterns for Java (like `@SomeAnnot(...)`) in any context.

### Fixed

- PHP superglobals such as `$_GET`,
  which start with a dollar sign just like Semgrep metavariables,
  are now correctly interpreted as PHP code instead of Semgrep pattern code.
- Calls to `isset(...)` in PHP look like function calls,
  but technically are not functions calls.
  Now you can match them anyway!
- It's now possible to write unit tests for OCaml rules.
- JavaScript's special identifiers,
  such as `this`, can now be captured into a metavariable.
- A Java pattern for `implements B`
  will now also match code that does `implements A, B, C`.
- Addressed off by one errors when applying autofixes
- Missing characters in metavariable interpolation in messages
- And many more minor code parser fixes!

## [0.27.0](https://github.com/returntocorp/semgrep/releases/tag/v0.27.0) - 2020-10-06

### Added

- Added a `--debug` flag and moved most of the output under `--verbose` to it.
- Can run multiple rule configs by repeating `--config` option
- Jenkins information added to integrations
- Added matching with partial patterns for function signatures for Go.

### Changed

- Parse and other errors are mentioned at final output, but not individually displayed unless --verbose is passed
- tree-sitter parse error exceptions converted to parsing_error, improving error location

### Fixed

- Dislayed types using the `message` key are more complete.
- Triple token repeat for EncodedString in semgrep messages fixed.
- Crashes on 3 or more layered jsonschema errors fixed.

## [0.26.0](https://github.com/returntocorp/semgrep/releases/tag/v0.26.0) - 2020-09-30

### Fixed

- Metavariables are able to match empty tuples
- Correctly parse optional chaining (?.) in Typescript
- Correctly parse logical assignment operators (&&=, ||=, ??=) in Typescript
- Some type constraing matching in Typescript

### Changed

- Added default timeout of 5 seconds to javascript parsing (related to ?. on large minified files stalling)

## [0.25.0](https://github.com/returntocorp/semgrep/releases/tag/v0.25.0) - 2020-09-23

### Added

- Added support for the JUnit XML report format (`--junit-xml`)
- C now supports the deep expression operator: `<... $X ...>`. See [this example](https://semgrep.dev/s/boKP/?version=develop).
- Added support for ellipses `...` in PHP. (https://github.com/returntocorp/semgrep/issues/1715). See [this example](https://semgrep.dev/s/NxRn/?version=develop).

### Fixed

- JavaScript will parse empty yields (https://github.com/returntocorp/semgrep/issues/1688).
- In JavaScript, arrow functions are now considered lambdas (https://github.com/returntocorp/semgrep/issues/1691). This allows [matching](https://semgrep.dev/s/Kd1j/?version=develop) arrow functions in `var` assignments.
- `tsx` and `typescript` are now properly recognized in the `languages` key. (https://github.com/returntocorp/semgrep/issues/1705)

## [0.24.0](https://github.com/returntocorp/semgrep/releases/tag/v0.24.0) - 2020-09-16

### Added

- The `--test` functionality now supports the `--json` flag
- Alpha support for TypeScript
- Alpha support for PHP
- PyPI artifacts are now compatible with Alpine Linux

### Fixed

- Can now parse ECMAScript object patterns with ellipses in place of fields

## [0.23.0](https://github.com/returntocorp/semgrep/releases/tag/v0.23.0) - 2020-09-09

### Added

- Experimental support for Typescript (with -lang ts). You can currently
  mainly use the Javascript subset of Typescript in patterns, as well
  as type annotations in variable declarations or parameters.
- Ability to read target contents from stdin by specifying "-" target.

### Changed

- You can now specify timeouts using floats instead of integers
  (e.g., semgrep -timeout 0.5 will timeout after half a second)

### Fixed

- We now respect the -timeout when analyzing languages which have
  both a Tree-sitter and pfff parser (e.g., Javascript, Go).

## [0.22.0](https://github.com/returntocorp/semgrep/releases/tag/v0.22.0) - 2020-09-01

### Added

- The 'languages' key now supports 'none' for running `pattern-regex` on arbitrary files. See [this file](https://github.com/returntocorp/semgrep/blob/develop/semgrep/tests/e2e/rules/regex-any-language.yaml) for an example.
- You can now use the '...' ellipsis operator in OCaml.
- True negatives to '--test' functionality via the 'ok:<rule-id>' annotation.

### Changed

- Groups of rules are now called "Rulesets" in the Semgrep ecosystem,
  instead of their previous name, "Packs".
- We now use also the tree-sitter-javascript Javascript parser, which
  can parse quickly minified files. Thus, we also removed the 5 seconds
  parsing timeout we were using for Javascript.
- We should correctly report ranges when matching array access expressions
  (e.g., 'foo[$X]').
- Breaking: regular expressions in semgrep string patterns (e.g., `"=~/foo/"`)
  are now using the PCRE (Perl Compatible Regular Expressions) syntax instead of
  the OCaml syntax. This means you should not escape parenthesis for grouping
  or escape pipes for dijunctions (e.g., use simply `"=~/foo|bar/"` instead of
  `"=~/foo\|bar/"`). You can also use more advanced regexp features available
  in PCRE such as case-insensitive regexps with '/i' (e.g., `"=~/foo/i"`).
  The semantic of matching changes also to look for the regexp anywhere
  in the string, not just at the beginning, which means if you want to
  enforce a format for the whole string, you will now need to use the '^' anchor
  character (e.g., `"=~/^o+$/"` to check if a string contains only a sequence
  of 'o').

### Removed

- Breaking: install script installation procedure (semgrep-<version>-ubuntu-generic.sh).
  Please use 'pip install' for equivalent Linux installation.

## [0.21.0](https://github.com/returntocorp/semgrep/releases/tag/v0.21.0) - 2020-08-25

### Added

- Parsing JSX (JavaScript React) files is now supported as a beta feature!
  In this release, you need to target .jsx files one by one explicitly to have them be scanned.
  We're planning to scan all .jsx files in targeted directories in our next release
- We now bundle a [json-schema](https://json-schema.org/) spec for rules YAML syntax.

### Changed

- Our custom-made rules YAML validator has been replaced with a jsonschema standard one.
  This results in more reliable and comprehensive error messages
  to help you get back on track when bumping into validation issues.
- Calling `semgrep --validate` now includes more information,
  such as the number of rules validation ran on.

### Fixed

- Fixed a bug where multiple assignment,
  also known as tuple unpacking assignment in Python,
  such as `a, b = foo`,
  could be misinterpreted by semgrep.
- Fixed a bug that would cause a crash when trying to get debug steps output as JSON.
- `.mly` and `.mll` files are no longer targeted implicitly by OCaml scans.
- Fixed the `--skip-unknown-extensions` flag skipping files even with recognized extensions.
- Fixed JavaScript conditionals without braces,
  such as `if (true) return;`,
  not being matched by patterns such as `if (true) { return; }`.

## [0.20.0](https://github.com/returntocorp/semgrep/releases/tag/v0.20.0) - 2020-08-18

### Added

- Support for JSX tag metavariables (e.g., <$TAG />) and ellipsis inside
  JSX attributes (e.g., <foo attr=... />)
- By default Semgrep treats explicitly passed files with unknown extension as possibly any language and so runs all rules on said files. Add a flag `--skip-unknown-extensions` so that Semgrep will treat these files as if they matched no language and will so run no rules on them. [Link: PR](https://github.com/returntocorp/semgrep/pull/1507)

### Fixed

- Python patterns do not have to end with a newline anymore.
- Pattern `$X = '...';` in JavaScript matches `var $X = '...'`. Additionally, semicolon is no longer required to match. [Link: Issue](https://github.com/returntocorp/semgrep/issues/1497); [Link: Example](https://semgrep.dev/7g0Q?version=0.20.0)
- In JavaScript, can now match destructured object properties inside functions. [Link: Issue](https://github.com/returntocorp/semgrep/issues/1005); [Link: Example](https://semgrep.dev/d72E/?version=0.20.0)
- Java annotations can be matched with fully qualified names. [Link: Issue](https://github.com/returntocorp/semgrep/issues/1508); [Link: Example](https://semgrep.dev/vZqY/?version=0.20.0)
- Ensure `/src` exists in Dockerfile; [Link: PR](https://github.com/returntocorp/semgrep/pull/1512)

## [0.19.1](https://github.com/returntocorp/semgrep/releases/tag/v0.19.1) - 2020-08-13

### Fixed

- Update Docker container to run successfully without special volume
  permissions

## [0.19.0](https://github.com/returntocorp/semgrep/releases/tag/v0.19.0) - 2020-08-11

### Added

- `--timeout-threshold` option to set the maximum number of times a file can timeout before it is skipped
- Alpha support for C#

### Fixed

- Match against JavaScript unparameterized catch blocks
- Parse and match against Java generics
- Add ability to match against JSX attributes using ellipses
- Add ability to use ellipses in Go struct definitions
- No longer convert Go expressions with a newline to a statement

## [0.18.0](https://github.com/returntocorp/semgrep/releases/tag/v0.18.0) - 2020-08-04

### Added

- Match arbitrary content with `f"..."`
- Performance improvements by filtering rules if file doesn't contain string needed for match
- Match "OtherAttribute" attributes in any order
- Support Python 3.8 self-documenting fstrings
- `--max-memory` flag to set a maximum amount of memory that can be used to apply a rule to a file

## [0.17.0](https://github.com/returntocorp/semgrep/releases/tag/v0.17.0) - 2020-07-28

### Added

- The `metavariable-regex` operator, which filters finding's by metavariable
  value against a Python re.match compatible expression.
- `--timeout` flag to set maximum time a rule is applied to a file
- Typed metavariables moved to official support. See [docs](https://github.com/returntocorp/semgrep/blob/develop/docs/pattern-features.md#typed-metavariables)

### Changed

- Improved `pattern-where-python` error messages

## [0.16.0](https://github.com/returntocorp/semgrep/releases/tag/v0.16.0) - 2020-07-21

### Added

- Match file-name imports against metavariables using `import "$X"` (most
  useful in Go)
- Support for taint-tracking rules on CLI using the key-value pair 'mode: taint'
  (defaults to 'mode: search')

### Changed

- Don't print out parse errors to stdout when using structured output formats

### Fixed

- Parse nested object properties in parameter destructuring in JavaScript
- Parse binding patterns in ECMAScript 2021 catch expressions
- Was mistakenly reporting only one of each type of issue even if multiple issues exist

## [0.15.0](https://github.com/returntocorp/semgrep/releases/tag/v0.15.0) - 2020-07-14

### Added

- Alpha level support for Ruby

### Changed

- Show semgrep rule matches even with --quiet flag

### Fixed

- Fixed a crash when running over a directory with binary files in it.
- Fix SARIF output format
- Parse nested destructured parameters in JavaScript
- True and False are not keywords in Python2
- Send informative error message when user tries to use semgrep on missing files

## [0.14.0](https://github.com/returntocorp/semgrep/releases/tag/v0.14.0) - 2020-07-07

### Changed

- Default Docker code mount point from `/home/repo` to `/src` - this is also
  configurable via the `SEMGREP_SRC_DIRECTORY` environment variable

### Removed

- `--precommit` flag - this is no longer necessary after defaulting to
  `pre-commit`'s code mount point `/src`

### Fixed

- Parse python files with trailing whitespace
- Parse python2 tuple as parameter in function/lambda definition
- Parse python3.8 positional only parameters (PEP 570)
- Parse python2 implicit array in comprehension
- Cache timeout errors in semgrep-core so running multiple rules does not
  retry parsing

## [0.13.0](https://github.com/returntocorp/semgrep/releases/tag/v0.13.0) - 2020-06-30

### Added

- Const propagation now works with Java 'final' keyword and for Python globals
  which were assigned only once in the program

### Fixed

- Parsing Ocaml open overriding
- Parse raise in Python2 can take up to three arguments
- Metavariable matching now works with variables with global scope:

```yaml
$CONST = "..."
---
def $FUNC(...): return foo($CONST)
```

will match

```python
GLOBAL_CONST = "SOME_CONST"

def fetch_global_const():
    return foo(GLOBAL_CONST)
```

### Changed

- More clear Parse error message

## [0.12.0](https://github.com/returntocorp/semgrep/releases/tag/v0.12.0) - 2020-06-23

### Added

- Support for a new configuration language: JSON. You can now write
  JSON semgrep patterns with -lang json
- Support for '...' inside set and dictionaries
- Version check to recommend updating when out-of-date, disable with `--disable-version-check`
- Support for multiline pattern-where-python
- `--dryrun` flag to show result of autofixes without modifying any files
- Add capability to use regex replacement for autofixing. See documentaion [here](https://github.com/returntocorp/semgrep/blob/develop/docs/experimental.md#autofix-using-regular-expression-replacement)
- Add version check to recommend upgrading when applicable

### Fixed

- The range of function calls and statement blocks now includes the closing
  `}` and `)`. The range for expression statements now includes the closing
  ';' when there's one. The range of decorators now includes '@'.
- Do not convert certain parenthesized expressions in tuples in Python
- Returned warning when improperly mounting volume in docker container
- Correctly handle uncommited file deletions when using git aware file targeting

### Changed

- Progress bar only displays when in interactive terminal, more than one
  rule is being run, and not being run with `-v` or `-q`
- Colapsed `--include-dir` and `--exclude-dir` functionaity into `--include` and
  `--exclude` respectively

## [0.11.0](https://github.com/returntocorp/semgrep/releases/tag/v0.11.0) - 2020-06-16

### Added

- Support for another programming language: OCaml. You can now write
  OCaml semgrep patterns with -lang ocaml
- Inline whitelisting capabilities via `nosem` comments and the
  `--disable-nosem` flag [#900](https://github.com/returntocorp/semgrep/issues/900)
- Show a progress bar when using semgrep in an interactive shell
- More understandable error messages

### Changed

- If scanning a directory in a git project then skip files that are ignored by the
  project unless `--no-git-ignore` flag is used
- Show aggregate parse errors unless `--verbose` flag is used

### Fixed

- Handle parsing unicode characters

## [0.10.1](https://github.com/returntocorp/semgrep/releases/tag/v0.10.1) - 2020-06-10

### Fixed

- Value of `pattern_id` when using nested pattern operators [#828](https://github.com/returntocorp/semgrep/issues/828)
- `...` now works inside for loops in javascript
- Metavariable
- Infinite loop in python [#923](https://github.com/returntocorp/semgrep/issues/923)
- Treat field definition (`{x: 1}`) differently from assignment (`{x = 1}`)
- Support triple-quoted f-strings in python
- Fix ubuntu build error [#965](https://github.com/returntocorp/semgrep/pull/965)

## [0.10.0](https://github.com/returntocorp/semgrep/releases/tag/v0.10.0) - 2020-06-09

### Fixed

- Support immediately indexed arrays with initializers in Java
- Support object rest parameters in ECMAScript 6+
- Support various array destructuring calls with ommitted arguments in
  ECMAScript 6+
- Fix an issue where an error was raised when matching to Python else
  blocks with a metavariable
- Don't filter out files that are explicitly passed as arguments to semgrep
  even if they do not have expected extension

### Added

- Java imports can now be searched with patterns written like `import javax.crypto.$ANYTHING`
- `--debugging-json` flag for use on semgrep.dev

### Changed

- Pattern matches now distinguish between conditionals without `else` blocks
  and those with empty `else` blocks; write two patterns to capture both
  possibilities
- Print output before exiting when using --strict

## [0.9.0](https://github.com/returntocorp/semgrep/releases/tag/v0.9.0) - 2020-06-02

### Fixed

- Performance optimizations in deep statement matching
- Disable normalization of != ==> !(==)
- Support empty variable declaration in javasript
- Support "print expr," in Python 2.X
- Support "async" keyword on inner arrow functions for ECMAScript 7+
- Support optional catch bindings for ECMAScript 2019+
- Support non-ASCII Unicode whitespace code points as lexical whitespace in JavaScript code
- Support assignment expressions in Python 3.8
- Emtpty block in if will only match empty blocks

### Removed

- `--exclude-tests` flag - prefer `--exclude` or `--exclude-dir`
- `--r2c` flag - this was completely unused

## [0.8.1](https://github.com/returntocorp/semgrep/releases/tag/v0.8.1) - 2020-05-26

### Fixed

- `semgrep --version` on ubuntu was not returning the correct version

## [0.8.0](https://github.com/returntocorp/semgrep/releases/tag/v0.8.0) - 2020-05-20

### Added

- `pattern-regex` functionality - see docs for more information.
- Ellipsis used in the final position of a sequence of binary operations
  will match any number of additional arguments:
  ```
  $X = 1 + 2 + ...
  ```
  will match
  ```python
  foo = 1 + 2 + 3 + 4
  ```
- Per rule configuration of paths to include/exclude. See docs for more information.

### Changed

- fstring pattern will only match fstrings in Python:
  ```
  f"..."
  ```
  will match
  ```python
  f"foo {1 + 1}"
  ```
  but not
  ```python
  "foo"
  ```
- Change location of r2c rule config to https://semgrep.live/c/r/all which filters out
  pattern-where-python rules

## [0.7.0](https://github.com/returntocorp/semgrep/releases/tag/v0.7.0) - 2020-05-12

### Added

- `--exclude`, `--include`, `--exclude-dir`, and `--include-dir` flags
  for targeting specific paths with command line options.
  The behavior of these flags mimics `grep`'s behavior.
- A `--sarif` flag to receive output formatted according to the
  [SARIF v2.1.0](https://docs.oasis-open.org/sarif/sarif/v2.1.0/cs01/sarif-v2.1.0-cs01.html)
  specification for static analysis tools.
- Metavariables are now checked for equality across pattern clauses. For example, in the following pattern, `$REQ` must be the same variable name for this to match:
  ```yaml
  - patterns:
      - pattern-inside: |
          $TYPE $METHOD(..., HttpServletRequest $REQ, ...) {
            ...
          }
      - pattern: $REQ.getQueryString(...);
  ```

### Fixed

- Correclty parse implicit tuples in python f-strings
- Correctly parse `%` token in python f-string format
- Correctly parse python fstrings with spaces in interpolants

## [0.6.1](https://github.com/returntocorp/semgrep/releases/tag/v0.6.1) - 2020-05-06

### Fix

- Message field in output was not using proper interpolated message

## [0.6.0](https://github.com/returntocorp/semgrep/releases/tag/v0.6.0) - 2020-05-05

### Added

- The `-j/--jobs` flag for specifying number of subprocesses to use to run checks in parallel.
- expression statements will now match by default also return statements
  ```
  foo();
  ```
  will now match
  ```javascript
  return foo();
  ```
- You can now use regexps for field names:
  ```
  var $X = {"=~/[lL]ocation/": $Y};
  ```
  will now match
  ```javascript
  var x = { Location: 1 };
  ```
- Add severity to json output and prepend the rule line with it. Color yellow if `WARNING`, and red if `ERROR`. e.g. WARNING rule:tests.equivalence-tests
- For languages not allowing the dollar sign in identifiers (e.g., Python),
  semgrep will return an error if your pattern contains an identifier
  starting with a dollar that is actually not considered a metavariable
  (e.g., `$x`)
- Support top level `metadata` field in rule.yaml. Entire metadata object is attached to
  all things that match the rule when using json output format.

### Changed

- Config files in hidden paths can now be used by explicitly specifying
  the hidden path:
  ```
  semgrep --config some/hidden/.directory
  ```
- Metavariables can now contain digits or `_`. `$USERS_2` is now
  a valid metavariable name. A metavariable must start with a letter
  or `_` though.
- Simple calls of the `semgrep` CLI, such as `semgrep --version`, are now 60% faster.
- Display autofix suggestion in regular and json output mode.
- Update command line help texts.

### Fixed

- Correctly parse `f"{foo:,f}"` in Python
- Correctly parse Python files where the last line is a comment

## [0.5.0](https://github.com/returntocorp/semgrep/releases/tag/v0.5.0) - 2020-04-28

### Changed

- Rename executable to semgrep
- Deep expression matching in conditionals requires different syntax:
  ```
  if <... $X = True ...>:
      ...
  ```
  will now match
  ```python
  if foo == bar and baz == True:
      return 1
  ```
- Deduplicate semgrep output in cases where there are multiple ways
  a rule matches section of code
- Deep statement matchings goes into functions and classes:

  ```
  $X = ...
  ...
  bar($X)
  ```

  now matches with

  ```javascript
  QUX = "qux";

  function baz() {
    function foo() {
      bar(QUX);
    }
  }
  ```

### Added

- `python2` is a valid supported language

### Fixed

- Expression will right hand side of assignment/variable definition in javascript. See #429
  ```
  foo();
  ```
  will now match
  ```
  var x = foo();
  ```
- Regression where `"..."` was matching empty list
  ```
  foo("...")
  ```
  does _not_ match
  ```
  foo()
  ```

## [0.4.9](https://github.com/returntocorp/semgrep/releases/tag/v0.4.9) - 2020-04-07

### Changed

- Only print out number of configs and rules when running with verbose flag
- Match let and const to var in javascript:
  ```
  var $F = "hello"
  ```
  will now match any of the following expressions:
  ```javascript
  var foo = "hello";
  let bar = "hello";
  const baz = "hello";
  ```

### Added

- Print out --dump-ast
- Print out version with `--version`
- Allow ... in arrays
  ```
  [..., 1]
  ```
  will now match
  ```
  [3, 2, 1]
  ```
- Support Metavariable match on keyword arguments in python:
  ```
  foo(..., $K=$B, ...)
  ```
  will now match
  ```
  foo(1, 2, bar=baz, 3)
  ```
- Support constant propogation in f-strings in python:
  ```
  $M = "..."
  ...
  $Q = f"...{$M}..."
  ```
  will now match
  ```python
  foo = "bar"
  baz = f"qux {foo}"
  ```
- Constant propogation in javascript:

  ```
  api("literal");
  ```

  will now match with any of the following:

  ```javascript
  api("literal");

  const LITERAL = "literal";
  api(LITERAL);

  const LIT = "lit";
  api(LIT + "eral");

  const LIT = "lit";
  api(`${LIT}eral`);
  ```

- Deep statement matching:
  Elipsis operator (`...`) will also include going deeper in scope (i.e. if-else, try-catch, loop, etc.)
  ```
  foo()
  ...
  bar()
  ```
  will now match
  ```python
  foo()
  if baz():
      try:
          bar()
      except Exception:
          pass
  ```
- Unified import resolution in python:

  ```
  import foo.bar.baz
  ```

  will now match any of the following statements:

  ```python
  import foo.bar.baz
  import foo.bar.baz.qux
  import foo.bar.baz as flob
  import foo.bar.baz.qux as flob
  from foo.bar import baz
  from foo.bar.baz import qux
  from foo.bar import baz as flob
  from foo.bar.bax import qux as flob
  ```

- Support for anonymous functions in javascript:
  ```
  function() {
      ...
  }
  ```
  will now match
  ```javascript
  var bar = foo(
    //matches the following line
    function () {
      console.log("baz");
    }
  );
  ```
- Support arrow function in javascript

  ```
  (a) => { ... }
  ```

  will now match:

  ```javascript
  foo((a) => {
    console.log("foo");
  });
  foo((a) => console.log("foo"));

  // arrows are normalized in regular Lambda, so an arrow pattern
  // will match also old-style anynonous function.
  foo(function (a) {
    console.log("foo");
  });
  ```

- Python implicit string concatenation
  ```
  $X = "..."
  ```
  will now match
  ```python
  # python implicitly concatenates strings
  foo = "bar"       "baz"              "qux"
  ```
- Resolve alias in attributes and decorators in python

  ```
  @foo.bar.baz
  def $X(...):
      ...
  ```

  will now match

  ```python
  from foo.bar import baz

  @baz
  def qux():
      print("hello")
  ```

### Fixed

- Handle misordered multiple object destructuring assignments in javascript:
  ```
  var {foo, bar} = qux;
  ```
  will now match
  ```
  var {bar, baz, foo} = qux;
  ```
- Defining properties/functions in different order:

  ```
  var $F = {
      two: 2,
      one: 1
  };
  ```

  will now match both

  ```javascript
  var foo = {
    two: 2,
    one: 1,
  };

  var bar = {
    one: 1,
    two: 2,
  };
  ```

- Metavariables were not matching due to go parser adding empty statements in golang

## [0.4.8](https://github.com/returntocorp/semgrep/releases/tag/0.4.8) - 2020-03-09

### Added

- Constant propagation for some languages. Golang example:

```
pattern: dangerous1("...")
will match:

const Bar = "password"
func foo() {
     dangerous1(Bar);
}
```

- Import matching equivalences

```
pattern: import foo.bar.a2
matches code: from foo.bar import a1, a2
```

- Deep expression matching - see (#264)

```
pattern: bar();
matches code: print(bar())
```
