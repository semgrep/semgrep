/*
  semgrep-cfscript

  Extends the standard cfscript grammar with semgrep pattern constructs.

  Metavariables ($FOO) work out of the box because $ is a valid identifier
  character in cfscript.
*/

const base_grammar = require('tree-sitter-cfml/cfscript/grammar');

module.exports = grammar(base_grammar, {
  name: 'cfscript',

  conflicts: ($, previous) => previous.concat([
    [$.expression, $.expression_statement],
    [$.semgrep_expression_ellipsis, $.spread_element],
    [$.semgrep_expression_ellipsis, $.rest_pattern],
    [$.semgrep_expression_ellipsis, $.spread_element, $.rest_pattern],
    [$.semgrep_expression_ellipsis, $.semgrep_ellipsis],
    [$.spread_element, $.rest_pattern, $.semgrep_ellipsis],
    [$.spread_element, $.rest_pattern, $.semgrep_ellipsis, $.semgrep_expression_ellipsis],
  ]),

  rules: {
    // Allow a standalone expression as a valid program (pattern).
    program: ($, previous) => choice(
      previous,
      $.semgrep_expression,
    ),

    // Alternate "entry point" for standalone expression patterns.
    semgrep_expression: $ => seq('__SEMGREP_EXPRESSION', $.expression),

    // Statement-level ellipsis (for matching sequences of statements).
    semgrep_ellipsis: $ => '...',

    // Expression-level ellipsis (separate to reduce conflicts).
    semgrep_expression_ellipsis: $ => '...',

    deep_ellipsis: $ => seq('<...', $.expression, '...>'),

    statement: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
    ),

    expression: ($, previous) => choice(
      previous,
      $.semgrep_expression_ellipsis,
      $.deep_ellipsis,
    ),

    _formal_parameter: ($, previous) => choice(
      $.semgrep_ellipsis,
      previous,
    ),
  }
});
