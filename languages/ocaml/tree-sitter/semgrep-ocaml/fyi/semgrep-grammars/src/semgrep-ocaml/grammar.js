/*
  semgrep-ocaml

  Extends the standard ocaml grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-ocaml/ocaml/grammar');

module.exports = grammar(base_grammar, {
  name: 'ocaml',

  conflicts: ($, previous) => previous.concat([
    // those conflicts are because of the $._signature in compilation_unit
    [$._structure_item, $._signature_item],
    [$._structure_item_ext, $._signature_item_ext],
    [$._structure, $._signature],
    [$.include_module, $.include_module_type],
    [$.parenthesized_module_type, $.parenthesized_module_expression],
    [$.functor_type, $.functor],
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     if they're not already part of the base grammar.
  */
  rules: {
    // We should use _signature when parsing .mli and _structure
    // when parsing .ml but it's tedious in tree-sitter and ocaml-tree-sitter
    // to have multiple entry points in the grammar, so simpler
    // to merge both grammars and allow both structures and signatures.
    compilation_unit: ($, previous) =>
       choice(
         previous,
         $._signature
       )
  /*
    semgrep_ellipsis: $ => '...',

    _expression: ($, previous) => {
      return choice(
        $.semgrep_ellipsis,
        ...previous.members
      );
    }
  */
  }
});
