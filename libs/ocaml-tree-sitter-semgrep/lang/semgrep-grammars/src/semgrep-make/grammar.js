/*
  semgrep-make

  Extends the standard make grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-make/grammar');

module.exports = grammar(base_grammar, {
  name: 'make',

  conflicts: ($, previous) => previous.concat([
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     if they're not already part of the base grammar.
  */
  rules: {
  /*
    semgrep_ellipsis: $ => '...',

    _expression: ($, previous) => choice(
      $.semgrep_ellipsis,
      ...previous.members
    ),
  */
  }
});
