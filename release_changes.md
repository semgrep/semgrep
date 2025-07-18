## [1.129.0](https://github.com/semgrep/semgrep/releases/tag/v1.129.0) - 2025-07-17


### Added


- A warning is now printed for each exclude or include pattern found in rules
  that is considered ambiguous (`paths.exclude`, `paths.include`).

  Currently, a pattern that contains a middle slash such as `src/*.c`
  is considered floating or unanchored by our implementation. In order to
  be compliant with Gitignore and Semgrepignore, `src/*.c`
  should be treated as anchored. Since many programmers are unaware of this
  subtlety in the Gitignore specification, Semgrep now prints a warning asking
  the user to lift the ambiguity. A user will now be asked to
  change their pattern `src/*.c` into either `/src/*.c` (anchored) or
  `**/src/*.c` (floating). This clarifies the expected behavior for readers
  of Semgrep rules and will avoid problems when Semgrep rules adopt
  the Gitignore/Semgrepignore behavior. (rule-paths-middle-slash-patterns)
- Secrets: Validation for AWS credentials which failed due to possibly transient
  reasons is now retried (3 attempts max). (scrt-917)


### Fixed


- When running `semgrep scan` in a docker container without an argument
  and no target project was mounted under `/src`,
  instead of a silent exit with code 2, a helpful error message is
  now printed before exiting. (docker-mount-error)
- In-rule path filters (`paths.exclude`, `paths.include`) now apply to
  normalized file paths relative to the project root. This makes rule selection
  independent from the current work folder.
  Patterns with a leading slash such as `/src` are now anchored instead
  of being floating. For example, `exclude: [ "/src" ]` will exclude the target
  file `src/main.c` but no longer excludes `misc/src/main.c`. (rule-paths-leading-slash-patterns)
- Fixed a bug where a `Unix.Unix_error` would occasionally crash the experimental language server
  on startup. (saf-2133)
- CLI: Only log a sample of the response from the `get_targets` endpoint.
  Previously, scanning large repos with the debug flag significantly ballooned
  the size of the output log. (saf-2145)
