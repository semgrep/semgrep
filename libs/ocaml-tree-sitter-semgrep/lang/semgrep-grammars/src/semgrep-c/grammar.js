/*
  semgrep-c

  Extends the standard c grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-c/grammar');

module.exports = grammar(base_grammar, {
  name: 'c',

  conflicts: ($, previous) => previous.concat([
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     if they're not already part of the base grammar.
  */
  rules: {
    // C hexadecimal escapes consume all following hexadecimal digits.
    escape_sequence: ($, previous) => choice(
      previous,
      token.immediate(prec(1, seq('\\', /x[0-9a-fA-F]+/))),
    ),
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
