Generating good CST type definitions
==

A generated CST will have to be consulted by a programmer who
will translate it into an AST. This is a list of issues that have
to be taken care of to get there. For details, see the implementation.

Name uniqueness
--

OCaml types within a module should be unique in order to be
usable. However, contructors used for polymorphic variants or even
classic variants don't need to be unique within a module.

* tree-sitter rule names are lowercase alphanumeric identifiers, which
  in general are valid OCaml type identifiers.
* The only types defined in a generated `CST.ml` file are derived from
  rule names so as to avoid conflicts. For example, the predefined
  token type is in a separate `Token` module rather than being put
  directly into `CST`.
* OCaml keywords (`if`, `let`, ...) and helper types (`list`,
  `option`) are forbidden to use as types representing rules.
   For this, we append a suffix to the rule name, going through a list
   of candidate suffixes (`_`, `2`, `3`, ...) until the name is
   unique. A rule named `if` would end up as `type if_ = ...`.

Assigning names to alternatives
--

The following tree-sitter constructs get mapped to OCaml types:

* terminal symbol: `Token.t` or alias for `Token.t`
* symbol: type name
* `seq()`: tuple
* `choice()`: variant
* `repeat()`, `repeat1()`: `list`
* `optional()`: `option`

The mapping to OCaml types is mostly straightforward except for the
members of a `choice()`, which aren't given names in the tree-sitter
grammar. Simply numbering them `Case0`, `Case1`, etc. isn't good for
two reasons:

* Readability: the name "Case" is uninformative.
* Stability: inserting new cases shifts the numbering, making it hard
  to fix code that depends on it.

Instead of numbering the cases, they're given names derived from the
containing type if any, and from their contents. Here's an example:

```ocaml
type pattern = [ `Pat_arg of arg | `Pat_splat_arg of splat_argument ]
```

Simplifications
--

Note that tree-sitter grammars support other constructs than those
mentioned above. These are either ignored or discarded from the
original grammar. In particular:

* `alias(orig, new)` is replaced by `orig` in the original
  `grammar.js`.
* Hidden rules, whose name starts with an underscore, are unhidden in
  `grammar.js` by removing the leading underscore. This makes the
  recovery of the CST from the json output simpler, faster and less
  ambiguous.
