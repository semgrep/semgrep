/*
  semgrep-jsonnet

  Extends the standard jsonnet grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-jsonnet/grammar');

module.exports = grammar(base_grammar, {
  name: 'jsonnet',

  conflicts: ($, previous) => previous.concat([
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
  */
  rules: {
    // No need for _SEMGREP_EXPRESSION hack here, because jsonnet allows
    // toplevel expressions.
      
    // Metavariables
    id: ($, previous) => {
      return choice(
        previous,
        $._semgrep_metavariable
      );
    },

    _semgrep_metavariable: $ => token(/\$[A-Z_][A-Z_0-9]*/),

    // Ellipsis
    expr: ($, previous) => choice(
      $.semgrep_ellipsis,
      $.deep_ellipsis,
      previous
    ),
    field: ($, previous) => choice(
      $.semgrep_ellipsis,
      previous
    ),
    param: ($, previous) => choice(
      $.semgrep_ellipsis,
      previous
    ),

    // The actual ellipsis rules
    semgrep_ellipsis: $ => '...',
      
    deep_ellipsis: $ => seq(
            '<...', $.expr, '...>'
    ),
  }
});
