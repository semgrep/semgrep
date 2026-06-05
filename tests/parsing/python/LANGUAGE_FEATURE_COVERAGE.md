# Python language-feature parsing coverage

Python **syntactic** language features (new grammar the parser must accept) and
the parsing test that covers each. Features without a test show `UNTESTED`.

Feature names link to the [Python language reference](https://docs.python.org/3/reference/)
(canonical grammar). PEP numbers note the original proposal and are not linked —
many are now [historical documents](https://peps.python.org/pep-0012/#pep-types)
superseded by the language reference.

| Version | Feature | Test file |
|---------|---------|-----------|
| 3.0 | [`nonlocal` declarations](https://docs.python.org/3/reference/simple_stmts.html#the-nonlocal-statement) | `nonlocal_and_class_bases.py` |
| 3.0 | [Exception chaining `raise X from Y`](https://docs.python.org/3/reference/simple_stmts.html#the-raise-statement) (PEP 3134) | `raise_from.py` |
| 3.0 | [Function / parameter / return annotations](https://docs.python.org/3/reference/compound_stmts.html#function-definitions) (PEP 3107) | `function_annotations.py` |
| 3.0 | [Keyword-only parameters — bare `*`](https://docs.python.org/3/reference/compound_stmts.html#function-definitions) (PEP 3102, incidental) | `function_annotations.py` |
| 3.0 | [Extended iterable unpacking in assignment targets — `a, *b = xs`](https://docs.python.org/3/reference/expressions.html#expression-lists) (PEP 3132) | UNTESTED |
| 3.0 | [Bytes literals `b"…"`](https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals) | UNTESTED |
| 3.0 | [Non-ASCII (Unicode) identifiers](https://docs.python.org/3/reference/lexical_analysis.html#identifiers) (PEP 3131) | UNTESTED |
| 3.3 | [`yield from` delegation](https://docs.python.org/3/reference/expressions.html#yield-expressions) (PEP 380) | `yield_from.py` |
| 3.5 | [`async def` / `await` / `async with` / `async for`](https://docs.python.org/3/reference/compound_stmts.html#async-def) (PEP 492) | `async_basics.py` |
| 3.5 | [Generalized `*`/`**` unpacking in literals & calls](https://docs.python.org/3/reference/expressions.html#displays-for-lists-sets-and-dictionaries) (PEP 448) | `generalized_unpacking.py` |
| 3.5 | [Starred expression in `return`/`yield`](https://docs.python.org/3/reference/simple_stmts.html#the-return-statement) (PEP 448) | `tuple_expansion.py` |
| 3.5 | [`*`/`**` unpacking in class base list](https://docs.python.org/3/reference/compound_stmts.html#class-definitions) (PEP 448) | `nonlocal_and_class_bases.py` |
| 3.5 | [Matrix-multiplication operator `@` / `@=`](https://docs.python.org/3/reference/expressions.html#binary-arithmetic-operations) (PEP 465) | UNTESTED |
| 3.6 | [f-strings](https://docs.python.org/3/reference/lexical_analysis.html#f-strings) (PEP 498) | `f_strings.py` |
| 3.6 | [Underscores in numeric literals](https://docs.python.org/3/reference/lexical_analysis.html#integer-literals) (PEP 515) | `numeric_underscores.py` |
| 3.6 | [Variable annotations — class body](https://docs.python.org/3/reference/simple_stmts.html#annassign) (PEP 526) | `field.py` |
| 3.6 | [Variable annotations — module & function scope](https://docs.python.org/3/reference/simple_stmts.html#annassign) (PEP 526) | `variable_annotations.py` |
| 3.6 | [Async generators](https://docs.python.org/3/reference/compound_stmts.html#async-def) (PEP 525) | `async_generators_and_comprehensions.py` |
| 3.6 | [Async comprehensions](https://docs.python.org/3/reference/expressions.html#displays-for-lists-sets-and-dictionaries) (PEP 530) | `async_generators_and_comprehensions.py` |
| 3.8 | [Assignment expressions — walrus `:=`](https://docs.python.org/3/reference/expressions.html#assignment-expressions) (PEP 572) | UNTESTED |
| 3.8 | [Positional-only parameters `/`](https://docs.python.org/3/reference/compound_stmts.html#function-definitions) (PEP 570) | UNTESTED |
| 3.8 | [f-string self-documenting `=` (`f"{x=}"`)](https://docs.python.org/3/reference/lexical_analysis.html#f-strings) | UNTESTED |
| 3.9 | [Relaxed decorator grammar](https://docs.python.org/3/reference/compound_stmts.html#function-definitions) (PEP 614) | UNTESTED |
| 3.10 | [Structural pattern matching `match`/`case`](https://docs.python.org/3/reference/compound_stmts.html#the-match-statement) (PEP 634–636) | `pattern_matching.py`, `dict_structural pattern.py` |
| 3.10 | [Parenthesized context managers](https://docs.python.org/3/reference/compound_stmts.html#the-with-statement) (PEP 617) | `as_pattern.py` |
| 3.11 | [Exception groups & `except*`](https://docs.python.org/3/reference/compound_stmts.html#except-star) (PEP 654) | `as_pattern.py` |
| 3.11 | [Variadic-generic `*Ts` unpacking in subscript](https://docs.python.org/3/reference/expressions.html#subscriptions) (PEP 646) | UNTESTED |
| 3.12 | [Type-parameter syntax — `type X = …`, `def f[T]`, `class C[T]`](https://docs.python.org/3/reference/compound_stmts.html#type-params) (PEP 695) | UNTESTED |
| 3.12 | [f-string formalization — nested quotes, multiline, backslashes](https://docs.python.org/3/reference/lexical_analysis.html#f-strings) (PEP 701) | UNTESTED |
| 3.13 | [Type-parameter defaults](https://docs.python.org/3/reference/compound_stmts.html#type-params) (PEP 696) | UNTESTED |
| 3.14 | [Template strings — t-strings](https://docs.python.org/3/library/string.templatelib.html#template-strings) (PEP 750) | UNTESTED |

## Other parsing tests in this directory

Parsing tests that target parser regressions or Semgrep pattern-syntax parsing
rather than a specific Python version's feature:

| File | Purpose |
|------|---------|
| `docstrings.py` | Parsing quotes inside triple-quoted docstrings |
| `re.py` | Regression for parsing a `re.match` call with `\|` in a string |
| `ellipsis_in_call_chain.py` | Parsing the Semgrep `...` ellipsis in dot-access chain patterns (gh-11545) |
