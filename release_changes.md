## [1.110.0](https://github.com/semgrep/semgrep/releases/tag/v1.110.0) - 2025-02-26


### Added


- pro: Inter-file analysis will now process Javascript and Typescript files
  together, so that taint can be tracked across both languages. (code-8076)
- Pro: new `metavariable-name` operator which allows for expressing a constraint
  against the fully qualified name or nearest equivalent of a metavariable
  (useful mainly in JavaScript and TypeScript, where there is no first-class
  syntax for this, or where such names or pseudo-names containt symbols which
  cannot appear in identifiers). Requires pro naming passes and works best with
  interfile naming.

  Additional documentation forthcoming. (code-8121)


### Changed


- Upgrade from OCaml 4.14.0 to OCaml 5.2.1 for our PyPI and Homebrew distributions. Our Docker images have been built with OCaml 5.2.1 since Semgrep 1.107.0. (ocaml5)


### Fixed


- Fixed a regression in pro interfile mode where type inference for the `var`
  keyword in Java was not functioning correctly. (code-7991)
- PRO: Fix the `range not found` error when using a metavariable pattern match on
  a typed metavariable. For example, the following metavariable pattern rule will
  no longer trigger the error:

  ```
  patterns:
    - pattern: ($FOO $VAR).bar()
    - metavariable-pattern:
        metavariable: $FOO
        pattern-either:
          - pattern: org.foo.Foo
  ``` (code-8007)
- lsp will no longer send diagnostics where the message is `MarkupContent` since
  our current implementation does not discriminate on the client capability for
  recieiving such diagnostics (to-be-added in 3.18). (code-8120)
- Yarn.lock parser now correctly denotes NPM organization scope. (sc-2107)
- Packages in `Package.resolved` without a version are now ignored. (sc-2116)
- Updated `Package.swift` parser to support:
  - The url value in a .package entry doesn't have to end with .git
  - You can have an exact field that looks like exact: "1.0.0" instead of .exact("1.0.0")
  - The exact version can be an object like Version(1,2,3) instead of a string
  - You can have .package values with no url, like this: .package(name: "package", path: "foo/bar") (sc-2117)
