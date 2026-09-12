Code generation pipeline
==

This is a high-level view of the transformations of an input grammar.
It is meant for contributors. The grammar exists in the following
successive states:

1. `grammar.js`
2. `grammar.json`
3. simplified `grammar.json` with inlined rules &rarr; `parser.c`
4. ocaml types with inlined rules &rarr; `Parse.ml`
5. ocaml types with deinlined rules &rarr; `CST.ml`, `Boilerplate.ml`

Preparing the input of `tree-sitter generate`
--

The source code for a tree-sitter grammar is normally a `grammar.js`
file with optional javascript dependencies and optionally a
`scanner.c` or `scanner.cc` file.
Consult the tree-sitter documentation for details.

The tree-sitter CLI (`tree-sitter generate`) operates in two steps:

1. Execute `grammar.js` and produce `grammar.json`.
2. Take `grammar.json` and produce `parser.c` and some other files.

Between these two steps, we take the opportunity to rewrite
`grammar.json` with our own `simplify-grammar` command. It's important
that this step doesn't change how an input file is interpreted, but
only affects how the parse tree is presented to us. Our
simplifications include:

* Remove aliases, which are alternate names to show instead of the
  original rule name.
* Unhide the hidden rules which start with an underscore, by removing
  the leading underscores.
* Inline the rules listed in the `inline:` field but leave their
  definitions in the grammar (even though they're unreachable from
  the grammar's entry point; see next section).

Generating OCaml code
--

The generated OCaml code in `Parse.ml` must be able to read a parse
tree produced by `parser.c`. It should also preserve the original rule
names as much as possible and avoid duplications in type definitions
(`CST.ml`) and in the generated boilerplate code (`Boilerplate.ml`).

The main difficulty is that some rules in `grammar.json` are declared
as inline using the `inline:` field. This makes those rules hidden, like any
rule whose name starts with an underscore, but that's not the
problem. The problem is we can't declare them as not inline because
it introduces parsing conflicts reported by `tree-sitter generate`.

So, our `simplify-grammar` rewriter gives us a `grammar.json` that's a
bit weird:
* Some rules have been inlined, resulting in duplicated subtrees
  occurring in various rule bodies.
* The definitions for the inlined rules are still there.

We use the definitions for the rules that were inlined to deinline
them. For example, the `Parse.ml` file that parses tree-sitter's parse
tree is based on the inlined view of the grammar, e.g.

```ocaml
type opt_id = [ `Var of string | `Dollar_ref of string ] option
type id_list = [ `Var of string | `Dollar_ref of string ] list
```

but in `CST.ml` we factor out the known type definition `identifier`,
resulting in

```ocaml
type identifier = [ `Var of string | `Dollar_ref of string ]
type opt_id = identifier option
type id_list = identifier list
```

The `Boilerplate.ml` file is derived from the same view of the grammar
as `CST.ml`, allowing for less repetitive code and good names.
