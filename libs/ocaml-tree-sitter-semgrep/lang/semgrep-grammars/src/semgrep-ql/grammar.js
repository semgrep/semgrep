/*
  semgrep-ql

  Extends the standard ql grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-ql/grammar');

module.exports = grammar(base_grammar, {
  name: 'ql',

  conflicts: ($, previous) => previous.concat([
    // This conflict makes it so we prefer to parse a metavariable into an annotation
    // as an annotated aggregate, not an annotated expression.
    // I don't think it should matter much in practice.
    [$.annotName, $.aggId]
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     if they're not already part of the base grammar.
  */
  rules: {
    semgrep_ellipsis: $ => '...',
    semgrep_ellipsis_metavar : $ => /\$\.\.\.[a-zA-Z_][a-zA-Z_0-9]*/,
    semgrep_metavariable: $ => token(/\$[A-Z_][A-Z_0-9]*/),
    semgrep_deep_expression: $ => seq('<...', $._exprOrTerm, '...>'),

    // typed metavars
    par_expr: ($, previous) => choice(
      seq("(", $.simpleId, $.semgrep_metavariable, ")"),
      previous
    ),

    // This gets slightly more precedence to make it win over cases where
    // annotName and predicateName are also possible.
    _upper_id: ($, previous) => prec(1, choice(
      previous,
      $.semgrep_metavariable
    )),
    // These must be choice'd separarately, as the only use sites of
    // _lower_id which are not also _upper_id.
    annotName: ($, previous) => choice(
      previous,
      $.semgrep_metavariable
    ),
    predicateName: ($, previous) => choice(
      previous,
      $.semgrep_metavariable
    ),
    aggId: ($, previous) => choice(
      $.semgrep_metavariable,
      ...previous.members,
    ),

    // ellipses
    classMember: ($, previous) => choice(
      $.semgrep_ellipsis,
      ...previous.members
    ),
    moduleMember: ($, previous) => choice(
      $.semgrep_ellipsis,
      ...previous.members
    ),
    _call_arg: ($, previous) => choice(
      $.semgrep_ellipsis,
      previous
    ),

    _primary: ($, previous) => choice(
      // Gives a slight edge when disambiguating foo(...)
      // this looks kind of similar to an exists(... | e), but could also be
      // a regular call foo(..., 2)
      // let's prefer to parse foo(...) as a real call
      prec(1, $.semgrep_ellipsis),
      $.semgrep_ellipsis_metavar,
      $.semgrep_deep_expression,
      ...previous.members
    ),
    varDecl: ($, previous) => choice(
      $.semgrep_ellipsis,
      previous,
    ),

    // Alternate "entry point". Allows parsing a standalone expression.
    semgrep_expression: $ => seq('__SEMGREP_EXPRESSION', $._exprOrTerm),

    ql: ($, previous) => choice(
      $.semgrep_expression,
      previous
    )
  }
});
