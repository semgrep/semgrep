## [1.107.0](https://github.com/semgrep/semgrep/releases/tag/v1.107.0) - 2025-02-04


### Added


- More testing of pnpm-lock.yaml dependency parsing. (gh-2999)
- Added a progress indicator during dependency resolution for supply chain scans. (sc-2045)


### Fixed


- The pro engine now respects the correct order of field resolution in Scala's
  multiple inheritance. The type that appears later takes precedence when
  resolving fields. For example, in `class A extends B with C with D`, the order
  of precedence is D, C, B, and A. (code-7891)
- pro: taint: Fixed bug in callback support, see https://semgrep.dev/playground/s/oqobX (code-7976)
- pro: python: Fixed resolution of calls to the implementation of abstract methods.
  See https://semgrep.dev/playground/s/X5kZ4. (code-7987)
- Fixed the semgrep ci --help to not include experimental options
  like --semgrep-branch (saf-1746)
- Peer dependency relationships in package-lock.json files are tracked when parsing a dependency graph (sc-2032)
- Peer dependency relationships in pnpm-lock.yaml files are tracked when parsing a dependency graph (sc-2033)


### Infra/Release Changes


- Upgrade from OCaml 4.14.0 to OCaml 5.2.1 for our Docker images (ocaml5-docker)
