# Changelog

This project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## Unreleased

### Added
- Keep track of and report rule parse time in addition to file parse time

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
  `<a href="foo">...</a>`  (#2963)
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

- remove cycle in named AST for Rust 'fn foo(self)'  (#2584)
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
...
def $FUNC(...):
  return foo($CONST)
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

- Java imports can now be searched with patterns written like `import
  javax.crypto.$ANYTHING`
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
  var x = {"Location": 1};
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
    QUX = "qux"

    function baz() {
        function foo() {
            bar(QUX)
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
        function () { console.log("baz"); }
    );
    ```
- Support arrow function in javascript
    ```
    (a) => { ... }
    ```
    will now match:

    ```javascript
    foo( (a) => { console.log("foo"); });
    foo( a => console.log("foo"));

    // arrows are normalized in regular Lambda, so an arrow pattern
    // will match also old-style anynonous function.
    foo(function (a) { console.log("foo"); });
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
      one: 1
    };

    var bar = {
        one: 1,
        two: 2
    };
    ```
- Metavariables were not matching due to go parser adding empty statements in golang


## [0.4.8](https://github.com/returntocorp/semgrep/releases/tag/0.4.8) - 2020-03-09

### Added
* Constant propagation for some langauges. Golang example:
```
pattern: dangerous1("...")
will match:

const Bar = "password"
func foo() {
     dangerous1(Bar);
}
```

* Import matching equivalences
```
pattern: import foo.bar.a2
matches code: from foo.bar import a1, a2
```

* Deep expression matching - see (#264)
```
pattern: bar();
matches code: print(bar())
```
