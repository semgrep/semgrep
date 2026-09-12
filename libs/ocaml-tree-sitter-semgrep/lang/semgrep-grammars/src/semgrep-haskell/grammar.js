/*
  semgrep-haskell

  Extends the standard Haskell grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-haskell/grammar');
const util = require('tree-sitter-haskell/grammar/util');

module.exports = grammar(base_grammar, {
  name: 'haskell',

  conflicts: ($, previous) => previous.concat([
    [$.variable, $.constructor],
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     if they're not already part of the base grammar.
  */
  rules: {
    /* unused for now. it's really complicated to try and modify
       decl or stmt with semgrep ellipses, and the haskell grammar
       is both complicated and uses a sophisticated scanner, so this
       probably isnt' a goood idea

       for now, let's try to just work around it
     */
    semgrep_ellipsis: $ => '...',

    semgrep_metavariable: $ => token(/\$[A-Z_][A-Z_0-9]*/),

    variable: ($, previous) => choice(
      previous,
      $.semgrep_metavariable,
    ),

    constructor: ($, previous) => choice(
      previous,
      $.semgrep_metavariable,
    ),

    /*
    _decl: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
    ),

    stmt: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
    ),
    */
  }
});
