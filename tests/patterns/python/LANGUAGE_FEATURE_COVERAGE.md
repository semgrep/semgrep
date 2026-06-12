# Python language-feature pattern-matching coverage

Companion to `tests/parsing/python/LANGUAGE_FEATURE_COVERAGE.md` (parser acceptance).
This file maps each syntactic feature to a Semgrep **pattern** test (`.sgrep` +
annotated `.py` target). Patterns use the pfff parser and may lag tree-sitter targets.

| Version | Feature | Pattern test |
|---------|---------|--------------|
| 3.0 | `nonlocal` | `nonlocal_stmt` |
| 3.0 | `raise X from Y` | `raise_from` |
| 3.0 | Function / parameter / return annotations | `metavar_typed`, `metavar_anno`, `less_typehint` |
| 3.0 | Keyword-only parameters — bare `*` | `keyword_only_params` |
| 3.0 | Extended iterable unpacking — `a, *b = xs` | `star_assignment_target` |
| 3.0 | Bytes literals `b"…"` | `bytes_literal` |
| 3.0 | Unicode identifiers | `unicode_identifiers` |
| 3.3 | `yield from` | `yield_from` |
| 3.5 | `async def` / `await` / `async with` / `async for` | `async_await`, `async_with`, `async_for` |
| 3.5 | Generalized `*`/`**` unpacking in literals & calls | `generalized_unpacking` |
| 3.5 | Starred expression in `return`/`yield` | `starred_return` |
| 3.5 | `*`/`**` unpacking in class bases | `dots_inherit`, `less_inherits` |
| 3.5 | Matrix multiplication `@` / `@=` | `matrix_multiplication` |
| 3.6 | f-strings | `dots_fstring`, `concrete_fstring`, `equivalence_f_string` |
| 3.6 | Underscores in numeric literals | `numeric_underscores` |
| 3.6 | Variable annotations — class body | UNTESTED |
| 3.6 | Variable annotations — module & function scope | `module_variable_annotation` (module scope only) |
| 3.6 | Async generators | `async_generator` |
| 3.6 | Async comprehensions | `async_comprehension` |
| 3.8 | Walrus `:=` | `assignment_expression` |
| 3.8 | Positional-only parameters `/` | `positional_only_params` |
| 3.8 | f-string debug `f"{x=}"` | `fstring_debug` |
| 3.9 | Relaxed decorator grammar | `pip614-extended-decorator-grammer1`, `pip614-extended-decorator-grammer2` |
| 3.10 | `match`/`case` | `metavar_match`, `misc_match_stmt` |
| 3.10 | Parenthesized context managers | `parenthesized_with`, `misc_with_parens` |
| 3.11 | `except*` | `except_star` |
| 3.11 | Variadic-generic `*Ts` in subscript | `variadic_generic_subscript` |
| 3.12 | Type parameters — `type X = …`, `class C[T]`, `def f[T]` | `type_alias_stmt`, `generic_class`, `generic_function` |
| 3.12 | f-string nested quotes (PEP 701) | `fstring_pep701` |
| 3.13 | Type-parameter defaults | UNSUPPORTED — `type X[T = …] = …` → `ERROR` (LANG-545) |
| 3.14 | t-strings | UNSUPPORTED — `t"…"` → `ERROR` (`parsing_todo/python/t_strings.py`, LANG-544) |

## Pattern-language limitations

- **`def f[T]`** — pfff rejects `def $F[$T](...)`; use `def $F[$T]($X): ...` (`generic_function`).
- **Variadic `*Ts`** — no metavar in star; use `tuple[*Ts]` (`variadic_generic_subscript`).
- **Async comprehensions** — `async for` in comp clauses is not preserved in the generic AST; patterns match sync comprehensions too.
- **`:=` / `except*`** — normalized to `=` / `except`; patterns match both forms.
- **Numeric underscores** — literals match by value (`1_000_000` ≡ `1000000`).
