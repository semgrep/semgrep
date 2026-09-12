/*
  semgrep-r

  Extends the standard r grammar with semgrep pattern constructs.
*/

const standard_grammar = require('tree-sitter-r/grammar');

module.exports = grammar(standard_grammar, {
  name: 'r',

    /* No need for ellipsis in expressions or paramaters; R already supports those
       constructs.
       No need for __SEMGREP_EXPRESSION trick, R allows toplevel single expressions
    */
    rules: {

    // Metavariables
    identifier: ($, previous) => {
      return choice(
        previous,
        $._semgrep_metavariable
      );
    },

    _semgrep_metavariable: $ => token(/\$[A-Z_][A-Z_0-9]*/),

    // Expression ellipsis
    //_expression: ($, previous) => {
    //  return choice(
    //    ...previous,
    //    $.deep_ellipsis,
    //  );
    //},
    //deep_ellipsis: $ => seq(
    //  '<...', $._expression, '...>'
    //),
    }
});
