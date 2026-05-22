## [1.163.0](https://github.com/semgrep/semgrep/releases/tag/v1.163.0) - 2026-05-13

### ### Added

- Updated PHP target parsing to support grammar changes from PHP 8.1-8.5 (LANG-380)

### ### Changed

- Improved `semgrep ci` startup time with App-provided rules by avoiding duplicate semgrep-core rule validation during CLI rule loading while preserving config-style failures for invalid rules. (ci-rule-validation-startup)
- Semgrep now validates dependency aware rules only on the core side, improving startup time (validate-skip-dep-aware)
- Rule validation now runs in parallel across cores on large rulesets, reducing scan startup time. (gh-6279)
- Rule parsing now runs in parallel across shards on multi-core machines, reducing scan startup time on large rulesets. (gh-6281)

### ### Fixed

- Improved name resolution for fully-qualified names in Java, Kotlin, and Scala. This could lead to fewer false positives and more true positives when the code under analysis uses fully-qualified names instead of imports. (java-qualified)
- Optimised rule prefiltering and parsing to improve engine startup time. (rule-parse-cache)
- Reduced peak memory usage when scanning repos with large rulesets. (rules-json-compact)
- Fixed transitive reachability rule parsing performance: the temporary rule
  file written for each transitive-reachability RPC call is JSON content
  (`json.dumps([rule.raw])`) but was being created with a `.yaml` suffix.
  OCaml's `Parse_rule.parse_file` dispatches purely on file extension, so this
  routed every TR rule through `Yaml_to_generic.parse_yaml_file` (the slow YAML
  path) instead of `Fast_json.parse_program` (the new hand-written RFC 8259
  parser). Switching the suffix to `.json` lines the suffix up with the actual
  content and lets every TR rule parse take the fast path. (tr-json-suffix)
- Pro: Fixed a naming resolution bug in Java. (LANG-274)
