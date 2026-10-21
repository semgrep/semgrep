## [1.182.0](https://github.com/semgrep/semgrep/releases/tag/v1.182.0) - 2026-10-20

### ### Added

- Added Objective-C language support (`objc`, alpha): tree-sitter based parsing of `.m` files with semgrep pattern syntax (metavariables, ellipsis in argument/statement/message-keyword positions, deep ellipsis), enabling search-mode and taint-mode rules on Objective-C code. Message sends, blocks, control flow, class/method definitions, and Objective-C literals are modeled; remaining constructs parse and degrade gracefully. Objective-C++ (`.mm`) support will follow separately. (gh-9265)

### ### Changed

- c-sharp-pro grammar update (v0.23.1):

  - C# (Pro) parsing and rule patterns now support Unicode escape sequences in identifiers (e.g. x\u0062), and matching follows C# identifier equality: `@class`, `cl\u0061ss`, and `class` (or `a\u200Cb` and `ab`) are the same name.
  - C# (Pro) now parses identifiers using Unicode XID characters such as `℘` (U+2118) and `·` (U+00B7), and still accepts formatting characters (e.g. soft hyphen) inside identifiers.

  (c-sharp-pro-v0.23.1)
- hcl grammar update (v1.2.0):

  - Terraform target parsing now supports namespaced function calls such as provider::local::fn(...).
  - Terraform parsing now represents conditional and loop template directives with their conditions, binders, and bodies.
  - Terraform parsing now binds attribute and index traversal more tightly than unary operators.

  (hcl-v1.2.0)
- lua grammar update (v0.5.0):

  - Lua parsing now supports Lua 5.4 variable attributes and Lua 5.5 leading attributes, global declarations, and named vararg parameters.
  - Lua parsing now accepts Unicode identifiers, hashbang lines, and additional numeric forms including LuaJIT binary, LL/ULL, and imaginary literals.
  - Lua AST conversion now preserves elseif branches in source order, including when an else branch is present, and correctly retains member-assignment receivers and colon-method declaration names.
  - Lua string ASTs now separate single-quoted and long-bracket delimiters from their content, consistently with double-quoted strings. Escape contents remain uninterpreted.
  - Lua parsing now handles builtin names such as next as ordinary callable identifiers.

  (lua-v0.5.0)

### ### Fixed

- Fix C++ range-based for loops with structured bindings so they parse and preserve findings in the rest of the file. (CODE-10028)
- Python only: a `requirements.txt` line declared as a version range or bare name is now reported as a direct dependency even when a `requirements.in` manifest is present. Such a line can only have been written by hand, since `pip-compile` emits exact pins, so it is a direct declaration regardless of whether the manifest names the package. Exact pins keep using the manifest to decide between direct and transitive. (SC-4242)

### ### Infra/Release Changes

- Update the bundled Tree-sitter Elixir grammar to v0.3.5 and regenerate its parser with Tree-sitter 0.26.3; no language syntax changes. (elixir-v0.3.5)
