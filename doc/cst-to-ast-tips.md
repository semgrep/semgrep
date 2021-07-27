Tips for converting CST to AST
==

Once you have copied the generated `Boilerplate.ml` for your language
`foo` into `Parse_foo_tree_sitter.ml`, you can start editing it. The
goal is replace all the calls like `todo env x` by the construction
of a node of the AST. The destination AST can be a language-specific
AST or directly the generic AST. If we're mapping to a
language-specific AST, this language-specific AST needs to be created
first. The advantage of going through a language-specific AST is more
visibility into which constructs are valid for the language, compared
to the generic AST which supports many more constructs.

Besides writing and updating the tree-sitter grammar, this step is
where the most time will be spent to integrate a language in semgrep.
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

### Study examples

`Parse_foo_tree_sitter.ml` is copied from the generated file
[`Boilerplate.ml`](https://github.com/returntocorp/semgrep-go/blob/main/lib/Boilerplate.ml). The `todo env x` calls are typically replaced by the
construction of a node of the AST.
See how it's done for example in [`Parse_go_tree_sitter.ml`](https://github.com/returntocorp/semgrep/blob/develop/semgrep-core/src/parsing/tree_sitter/Parse_go_tree_sitter.ml).

### Learn OCaml basics

CST and AST type definitions make heavy use of algebraic data types to
accommodate nodes of different kinds under the same type.
Those are known as variants (e.g. `Expr e`) and
polymorphic variants in OCaml jargon (e.g. `` `Expr e``).

Parametrized types in OCaml are like generics in languages like Java.
The OCaml type for a list of ints is denoted `int list`, which would
be denoted `List<Int>` in a Java-like language.

Run `utop` (`opam install utop`) and go over [this tutorial about OCaml
types at ocaml.org](https://ocaml.org/learn//tutorials/data_types_and_matching.html).

### Preserve structure, assign useful names

Consider this example of typical generated code in `Boilerplate.ml` file:

```ocaml
let rec anon_choice_type_id_42c0412 (env : env) (x : CST.anon_choice_type_id_42c0412) =
  match x with
  | `Id tok -> [ identifier env tok ] (* identifier *)
  | `Nested_id x -> nested_identifier env x
```

The name `anon_choice_type_id_42c0412` was generated from an anonymous
node in the grammar and it's not meaningful. However, it's used in multiple
spots, which is why it has its own function definition. It occurs for example
here:
```ocaml
    | `Choice_id_opt_type_args (v1, v2) ->
        let v1 = anon_choice_type_id_42c0412 env v1 in
        let id = concat_nested_identifier v1 in
        let _v2 =
```

Instead, it works better to give it a meaningful name like `id_or_nested_id` as
follows:

```ocaml
let rec id_or_nested_id (env : env) (x : CST.anon_choice_type_id_42c0412) =
  match x with
  | `Id tok -> [ identifier env tok ] (* identifier *)
  | `Nested_id x -> nested_identifier env x
```

and replace it at every point of use. Now, the snippet where it's used
makes more sense:

```ocaml
    | `Choice_id_opt_type_args (v1, v2) ->
        let v1 = id_or_nested_id env v1 in
        let id = concat_nested_identifier v1 in
        let _v2 =
```

Another helpful transformation is to assign a name to every new
value instead of `v1`, `v2`, etc. The above snippet becomes something like

```ocaml
    | `Choice_id_opt_type_args (v1, v2) ->
        let ids = id_or_nested_id env v1 in
        let id = concat_nested_identifier ids in
        let type_args =
```

Note that we're still keeping the original `v1` and `v2`, because it's
not very useful to find names for them.

Finally, it is very useful to specify the return type of the function
so as to figure out type errors a lot more easily.
```ocaml
let rec id_or_nested_id (env : env) (x : CST.anon_choice_type_id_42c0412) =
```
becomes
```ocaml
let rec id_or_nested_id (env : env) (x : CST.anon_choice_type_id_42c0412) : ident =
```

Summary:

* Replace generated function names by something meaningful.
* Replace `let v1 =` by a meaningful name.
* Specify the return type of functions that map CST to AST.
* Preserve the general structure of the generated functions.

This all helps with updating the code when the grammar changes.

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

Add type annotations
---

The generated boilerplate looks like this, i.e. the return type is left
unspecified.

```ocaml
and formal_parameter (env : env) (x : CST.formal_parameter) =
  ...
```

The return type will be an AST node determined by the programmer.
OCaml performs full type inference, so it's not technically required
to specify a return type.
However, given the peculiar nature of this exercise, we recommend specifying
return types. It makes it easier to see expectations and get clear error
messages when the time comes to upgrade the grammar. A return type annotation
looks like this:

```ocaml
and formal_parameter (env : env) (x : CST.formal_parameter) : AST.parameter =
  ...
```

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
