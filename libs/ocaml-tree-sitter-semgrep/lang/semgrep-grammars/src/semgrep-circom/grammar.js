/*
  semgrep-circom

  Extends the standard circom grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-circom/grammar');

module.exports = grammar(base_grammar, {
  name: 'circom',

  conflicts: ($, previous) => previous.concat([
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     if they're not already part of the base grammar.
  */
  rules: {

    source_file: ($, previous) => {
      return choice(
        previous,
        repeat1($._statement),
        $._expression,
      );
    },

    // Metavariables for pragma version
    circom_pragma_token: ($, previous) => {
      return choice(
        previous,
        seq(
          $._circom,
          $.identifier
        )
      )
    },
    
    _expression: ($, previous) => {
      return choice(
          previous,
          $.ellipsis,
          $.deep_ellipsis
      );
    },

    expression_statement: ($, previous) => {
      return choice(
            previous,
            prec.right(100, seq($.ellipsis, ';')),  // expression ellipsis
            prec.right(100, $.ellipsis),  // statement ellipsis
      );
    },

    parameter: ($, previous) => {
      return choice(
         previous,
         $.ellipsis
      );
    },

    for_statement: ($, previous) => {
      return choice(
         previous,
         seq('for', '(', $.ellipsis, ')', $._statement)
      );
    },
  
    ellipsis: $ => '...',

    deep_ellipsis: $ => seq(
      '<...', $._expression, '...>'
    ),
  
  }
});
