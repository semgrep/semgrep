# Changelog

This project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## Unreleased

### Changed

- Made error message for resource exhausion (exit code -11/-9) more actionable

## [0.92.1](https://github.com/returntocorp/semgrep/releases/tag/v0.92.1) - 2022-05-13

### Added

- `r2c-internal-project-depends-on`: support for Gradle and Poetry lockfiles

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
- Processing large rule files is now 30% faster.
- The dot access ellipsis now matches field accesses in addition to method
  calls.

### Fixed

- TS: support for template literal types after upgrading to a more recent
  tree-sitter-typescript (Oct 2021)
- TS: support for `override` keyword (#4220, #4798)
- TS: better ASI (#4459) and accept code like `(null)(foo)` (#4468)
- TS: parse correctly private properties (#5162)
- Go: Support for ellipsis in multiple return values
  (e.g., `func foo() (..., error, ...) {}`) (#4896)
- semgrep-core: you can use again rules stored in JSON instead of YAML (#5268)

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
