/*
 * semgrep-c-sharp
 *
 * Extend the original tree-sitter C# grammar with semgrep-specific constructs
 * used to represent semgrep patterns.
 *
 */

const standard_grammar = require('tree-sitter-c-sharp/grammar');

module.exports = grammar(standard_grammar, {
  /*
    We can't rename the grammar to 'csharp' because it relies on a custom
    lexer ('scanner.c') which comes with the inherited grammar and provides
    a function 'tree_sitter_c_sharp_external_scanner_create'.
    The code generator ('tree-sitter generate') however expects this function
    name to match the grammar name, so if we rename it here to 'csharp',
    it will generate glue code that calls
    'tree_sitter_csharp_external_scanner_create'
    while only 'tree_sitter_c_sharp_external_scanner_create' is available.
  */
  name: 'c_sharp',

  conflicts: ($, previous) => [
    ...previous,
    [$._expression, $.parameter]
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
    semgrep_expression: $ => seq('__SEMGREP_EXPRESSION', $._expression),

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

    _declaration: ($, previous) => {
      return choice(
        ...previous.members,
        $.ellipsis
      );
    },

    // We want ellipses to be interchangeable with namespace member declarations, so
    // we need to add them in to `type_declaration` here. 
    _type_declaration: ($, previous) => { 
      return choice(
        ...previous.members,
        $.ellipsis
      )
    },

    // We do this because a file scoped namespace declaration is a top-level 
    // thing, but ellipses are more particular. We want ellipses to be used 
    // in conjunction with file scoped namespace declarations.
    // Unfortunately, in the base grammar, we can either have ellipsis 
    // statements, or a file scoped declaration! That's no good. To play 
    // around the previous grammar, we simply allow what came before to also 
    // occur before a file scoped namespace declaration.
    file_scoped_namespace_declaration: ($, previous) => {
      return seq(
        seq(repeat($.global_statement), repeat($._namespace_member_declaration)),
        previous,
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
    _expression: ($, previous) => {
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
      field('expression', choice($._expression, $.predefined_type, $._name)),
      choice('.', '->'),
      $.ellipsis
     )),

    // use syntax similar to a cast_expression, but with metavar
    //TODO: use PREC.CAST from original grammar instead of 17 below
    typed_metavariable: $ => prec.right(17, seq(
      '(',
      field('type', $._type),
      field('metavar', $._semgrep_metavariable),
      ')',
    )),

    deep_ellipsis: $ => seq(
      '<...', $._expression, '...>'
    ),

    ellipsis: $ => '...',
  }
});
