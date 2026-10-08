## [1.179.0](https://github.com/semgrep/semgrep/releases/tag/v1.179.0) - 2026-10-01

### ### Added

- Semgrep Supply Chain now parses Bun `bun.lock` lockfiles (text format, `lockfileVersion` 1) paired with a `package.json` manifest, including the dependency relationships between packages. The deprecated binary `bun.lockb` format is still unsupported. (SC-2419)

### ### Changed

- Pro: Stabilize file processing order in inter-file analysis, which was previously arbitrary but deterministic. We believe this is very unlikely to affect findings but there is a theoretical path by which it could in certain rare cases. (stabilize)
- c grammar update (v0.24.2):

  - No verified user-facing improvements: Agent feature assessment was not run for this grammar.

  (c-v0.24.2)
- cpp grammar update (v0.23.4):

  - C++ target parsing now supports attributes on namespace definitions and preserves their arguments in the AST.
  - C++ target parsing now supports reference typedef declarators, including references to arrays and functions.
  - C++ target parsing now supports C++20 lambda pack init-captures such as [...values = args] and [&...values = args]. Capture-sensitive matching remains an existing limitation.
  - C++ target parsing now supports GNU inline-assembly output operands that are expressions, including pointer dereferences and array accesses.
  - C++ target parsing now supports composite type arguments in alignas declarations, such as alignas(int *), while preserving the alignment argument.
  - C++ target parsing now accepts single-digit hexadecimal escapes in string literals, such as "\xA", while retaining support for longer hexadecimal escapes.

  (cpp-v0.23.4)
- Update the html grammar to v0.23.2. (html-v0.23.2)
- r grammar update (v1.3.0):

  - R target parsing now supports unary and binary help expressions (?) and exponentiation written with **.
  - R target parsing correctly represents hexadecimal fractional and exponent literals, including imaginary forms, and decimal or exponent notation with the L suffix.
  - R target parsing now supports quoted namespace and slot names, such as "base"::"mean" and x@"slot".
  - R parsing preserves else branches placed on a new line within braced expressions.
  - R string parsing preserves literal newlines and excludes raw-string delimiters from string values. Short hexadecimal and Unicode escape spellings are accepted; escape decoding remains unchanged.
  - R parsing preserves omitted argument positions in calls and subscripts, and retains the indexed expression for single-index subscripts.

  (r-v1.3.0)
- sfapex grammar update (v2.3):

  - Apex target parsing now supports null-coalescing expressions using ??, preserving both operands.
  - Apex target parsing now supports DML operations with explicit as user and as system security modes, retaining the mode, operation targets, and optional upsert key.
  - Apex target parsing now supports numeric package-version expressions such as Package.Version.1.2.
  - Apex target parsing now recognizes the webservice modifier and annotations with empty argument lists.
  - Apex target parsing now accepts whitespace within relational and shift operators, including compound shift assignments.
  - Embedded SOQL parsing in Apex now supports dotted TYPEOF operands, functions such as convertTimezone, and HAVING comparisons on grouped fields.
  - Apex target parsing now supports multiple top-level method declarations followed by executable statements in anonymous Apex.
  - Apex parsing now preserves native java: qualifiers on types and supports java:-prefixed field-access expressions.
  - Apex patterns now distinguish safe navigation (?.) from ordinary field and method access (.).

  (sfapex-v2.3)

### ### Fixed

- Python: `except A, B:` and `except A, B, C:` (PEP 758, Python 3.14) and the
  `except*` forms of both are now parsed as a tuple of exception types, like
  `except (A, B):`, rather than as the Python 2 `except A as B:` or a parse
  error. `except*` is now handled by Semgrep's primary Python parser too, so
  exception groups no longer depend on the fallback parser. Note that Python
  2's `except A, e:` is no longer read as `except A as e:`, even for `python2`.
  One limitation remains: the vendored tree-sitter Python grammar still accepts
  only a single comma, so `except A, B, C:` is still a parse error there. The
  menhir parser runs first and handles it, so this only surfaces if the
  tree-sitter fallback is reached; upgrading the vendored grammar is follow-up
  work. (gh-11906)
- Fix Dockerfile parsing for environment-expanded `EXPOSE` ports with TCP/UDP protocols. (gh-11934)
- Semgrep now allows PyJWT 2.15 and later (`pyjwt[crypto]>=2.15.0,<3`) instead of
  pinning `~=2.13.0`, so installs pick up a PyJWT release with fixes for known
  vulnerabilities, and projects that require a newer PyJWT can install Semgrep. (gh-11953)
