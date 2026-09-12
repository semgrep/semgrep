/*
 * semgrep-c-sharp-pro
 *
 * Extend the original tree-sitter C# grammar with semgrep-specific constructs
 * used to represent semgrep patterns.
 *
 * c-sharp-pro is the same as c-sharp but tracks a more recent version
 * of tree-sitter-c-sharp.
 *
 */

const standard_grammar = require('./original-grammar');

module.exports = grammar(standard_grammar, {
  name: 'c_sharp_pro',

  conflicts: ($, previous) => [
    ...previous,
    [$.expression, $.parameter],
  ],

  rules: {

    // Entry point
    compilation_unit: ($, previous) => {
      return choice(
        previous,
        $.semgrep_expression
      );
    },

    // Alternate "entry point". Allows parsing a standalone expression.
    semgrep_expression: $ => seq('__SEMGREP_EXPRESSION', $.expression),

    // Metavariables
    identifier: ($, previous) => {
      return choice(
        previous,
        $._semgrep_metavariable
      );
    },

    _semgrep_metavariable: $ => token(/\$[A-Z_][A-Z_0-9]*/),
    semgrep_variadic_metavariable: ($) => /\$\.\.\.[A-Z_][A-Z_0-9]*/,

    parameter: ($, previous) => {
      return choice(
        previous,
        $.ellipsis
      );
    },

    // alt: we could also extend 'identifier' above and allow 'semgrep_variadic_metavariable'
    // there to be more general
    argument: ($, previous) => {
      return choice(
        previous,
        $.semgrep_variadic_metavariable
      );
    },

    declaration: ($, previous) => {
      return choice(
        ...previous.members,
        $.ellipsis
      );
    },

    // We want ellipses to be interchangeable with namespace member declarations, so
    // we need to add them in to `type_declaration` here. 
    type_declaration: ($, previous) => { 
      return choice(
        ...previous.members,
        $.ellipsis
      )
    },

    enum_member_declaration: ($, previous) => choice(
          previous,
	  $.ellipsis,
    ),

    // Statement ellipsis: '...' not followed by ';'
    expression_statement: ($, previous) => {
      return choice(
        previous,
        prec.right(100, seq($.ellipsis, ';')),  // expression ellipsis
        prec.right(100, $.ellipsis),  // statement ellipsis
        seq($.deep_ellipsis, ';'),
        seq($.member_access_ellipsis_expression, ';'),
        seq($._semgrep_metavariable, ';'),
        seq($.typed_metavariable, ';')
      );
    },

    // Expression ellipsis
    expression: ($, previous) => {
      return choice(
        ...previous.members,
        $.ellipsis,
        $.deep_ellipsis,
        $.member_access_ellipsis_expression,
        $.typed_metavariable
      );
    },

    // TODO: how to use PREC.DOT from original grammar instead of 18 below?
    member_access_ellipsis_expression : $ => prec(18, seq(
      field('expression', choice($.expression, $.predefined_type, $._name)),
      choice('.', '->'),
      $.ellipsis
     )),

    // use syntax similar to a cast_expression, but with metavar
    //TODO: use PREC.CAST from original grammar instead of 17 below
    typed_metavariable: $ => prec(17, prec.dynamic(1, seq(
      '(',
      field('type', $.type),
      field('metavar', $._semgrep_metavariable),
      ')',
    ))),

    deep_ellipsis: $ => seq(
      '<...', $.expression, '...>'
    ),

    ellipsis: $ => '...',
  }
});
