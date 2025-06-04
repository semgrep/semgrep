## [1.124.0](https://github.com/semgrep/semgrep/releases/tag/v1.124.0) - 2025-06-04


### Added


- Parallelizes rule validation to improve performance when scanning with many rule files. (SAF-2061)
- Semgrep should now respect `ALL_PROXY`, `HTTP_PROXY`, `HTTPS_PROXY`,
  `NO_PROXY`, `PROXY_USERNAME` and `PROXY_PASSWORD` for all networking (including
  that done via the OCaml components). Moreover, the environment variable
  `OCAML_EXTRA_CA_CERTS` should now allow additional CA certs to be used for
  network operations done by OCaml components. (code-8157)


### Fixed


- Stop attempting to parse `build.gradle.kts` files as `build.gradle`. (SC-2209)
- Taint rules using the **experimental** feature _labels_, and specifying sinks
  with a `requires:` of the form `not A`, could produce findings with an empty
  list of traces, potentially causing a crash. We now recognize the issue and
  prevent the crash from happening. (code-8531)
- Fixed inconsistency where the empty Python fstring `f""` was not matched by the pattern "...". (gh-10047)
- Fixed bug where dev depenencies (and their dependencies, and so on) were incorrectly marked as "transitivity: unknown" when parsing package-lock.json projects, specifically v3 lockfiles. (gh-4003)
- Fixed scenario where a multiplication expression of ints was not considered an int. This will help with `metavariable-type`. Concretely, "2 * groups" was not considered an int, where groups is an int. Additionally adds type inference for mod, floor division, and pow. (gh-9855)
- pro: python: Fixed a regression that could (in rare cases) cause naming to take a
  disproportionate amount of time significantly slowing down scans. (saf-1978)
