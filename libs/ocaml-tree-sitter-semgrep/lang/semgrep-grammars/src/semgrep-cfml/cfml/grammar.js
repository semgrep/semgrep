/*
  semgrep-cfml

  Extends the standard cfml grammar with semgrep pattern constructs.

  Metavariables ($FOO) work natively because $ is valid in identifiers.
*/

const base_grammar = require('tree-sitter-cfml/cfml/grammar');

module.exports = grammar(base_grammar, {
  name: 'cfml',

  conflicts: ($, previous) => previous.concat([
    [$.semgrep_expression_ellipsis, $.spread_element],
    [$.semgrep_expression_ellipsis, $.rest_pattern],
    [$.semgrep_expression_ellipsis, $.spread_element, $.rest_pattern],
    [$.semgrep_expression_ellipsis, $.semgrep_ellipsis],
    [$.spread_element, $.rest_pattern, $.semgrep_ellipsis],
    [$.spread_element, $.rest_pattern, $.semgrep_ellipsis, $.semgrep_expression_ellipsis],
  ]),

  rules: {
    semgrep_ellipsis: $ => '...',
    semgrep_expression_ellipsis: $ => '...',
    deep_ellipsis: $ => seq('<...', $.expression, '...>'),

    _node: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
    ),

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
