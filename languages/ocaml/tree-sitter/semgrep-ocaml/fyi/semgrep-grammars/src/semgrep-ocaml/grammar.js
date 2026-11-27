/*
  semgrep-ocaml

  Combines OCaml structures and signatures for Semgrep target parsing.
  OCaml Semgrep patterns are parsed separately by the Menhir parser.
*/

const base_grammar = require('tree-sitter-ocaml/grammars/ocaml/grammar');

module.exports = grammar(base_grammar, {
  name: 'ocaml',

  conflicts: ($, previous) => previous.concat([
    // those conflicts are because of the $._signature in compilation_unit
    [$._structure_item, $._signature_item],
    [$._structure, $._signature],
    [$.module_path, $.module_type_path],
    [$.module_path, $.extended_module_path],
    [$._module_type, $._simple_module_expression],
    [$.module_parameter, $.module_path],
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     if they're not already part of the base grammar.
  */
  rules: {
    // `effect` is also an ordinary name in code predating effect handlers.
    _simple_pattern: ($, previous) => choice(
      previous,
      alias('effect', $.value_pattern)
    ),

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
