Tips for converting CST to generic AST
==

Once you have copied the generated `Boilerplate.ml` for your language
`foo` into `Parse_foo_tree_sitter.ml`, you can start editing it. The
goal is replace all the calls like `todo env x` by the construction
of a node of the generic AST.

This is a collection of tips to make this tedious task somewhat easier.

Use editor/IDE with good OCaml support
--

Make sure to set up your editor with a proper ocaml mode, so that you
can see the inferred type of expressions and get the ability to jump
to the definitions.

Popular editors include emacs, vim, vscode. They all have their own
OCaml extension or plugin which relies on merlin.

Editing the boilerplate
--

`Parse_foo_tree_sitter.ml` is copied from the generated file
`Boilerplate.ml`. The `todo env x` calls are typically replaced by the
construction of a node of the generic AST (`AST_generic`, which you
can alias to `AST` for brevity).
See how it's done for example in `Parse_go_tree_sitter.ml`.
For example `TStruct (v1, v2)` constructs a node of kind `TStruct` from
`v1` and `v2`. The type of `TStruct` is shown in the editor as `tok *
struct_field stack bracket -> type_` which is a little strange. `stack`
is an alias for `list`. So, `struct_field stack` is the same as
`struct_field list`, which in Java syntax would be
`list<struct_field>`.
It's best to study `AST_generic.ml` a bit to get a sense of the
different constructs that you're going to map to.

Compile regularly
--

Compile regularly so as to perform type checking. This is general
advice for OCaml development. If you're working on a single file, you
don't need to recompile the project, though. Merlin will take care of
checking types when you save the file.

The initial template with all the todos should compile successfully,
but of course will fail at runtime. Type errors produced by the
compiler can be tricky to understand but it's good to learn how to
interpret them. Sometimes they're just too long, though.

Keep the boilerplate structure intact
--

Leave the original structure in place as much as possible. This is
important for later when we want to update the grammar and need to
compare the new boilerplate with the old/edited one.

Consult the original `grammar.js`
--

The original `grammar.js`, or sometimes another javascript file,
contains the bulk of the original rules for the grammar. This is
usually a better reference than the generated code.

The generated boilerplate `Boilerplate.ml` is similar to the type definitions
`CST.ml` which is our interpretation of the original
`grammar.js`. So, it is useful to consult `CST.ml` as well.

What tends to work well is to keep 4 windows open:
* `Parse_foo_tree_sitter.ml` (2 windows)
* `grammar.js`
* `AST_generic.ml`

The last file `AST_generic.ml` contains the type definitions of the
AST we're mapping to.

Extend the generic AST with moderation
--

Each programming language comes with a few features that other
languages don't offer, but typically not that many. The generic AST is
designed to accommodate all the constructs for all the programming
languages. So, we sometimes have to extend the generic AST with new
kinds of nodes. We try to do so sparingly, since we try to keep the
generic AST as simple as possible and it's already very rich.
