## [1.118.0](https://github.com/semgrep/semgrep/releases/tag/v1.118.0) - 2025-04-09


### Fixed


- Pro: Failure to parse a `package.json` file when analysing JavaScript or
  TypeScript is no longer a fatal error. (code-8227)
- taint-mode: Fixed bug in taint "auto-cleaning" where we automatically clean the
  LHS of an assigmnet if the RHS is clean, provided that the LHS is not subject to
  any "side-effects". In some cases, this could cause the taint analysis to timeout.
  Some combinations of rules and repos will see a major perf improvement, in other
  cases it may not be noticeable. (code-8288)
- In a Semgrep rule's `metadata` section, two fields may provide URLs:

  - `source`: populated dynamically by the Semgrep registry serving the rule, it's a URL that
    offers information about the rule.
  - `source-rule-url`: optional string, a URL for the source of inspiration for the rule.

  The SARIF format supports only one URL under the field `helpUri`.
  Previously, Semgrep populated the SARIF `helpUri` field only with `metadata.source`.
  This fix is to use `metadata.source` if available, otherwise falling back to `metadata.source-rule-url`.

  Contributed by @candrews. (gh-10891)
