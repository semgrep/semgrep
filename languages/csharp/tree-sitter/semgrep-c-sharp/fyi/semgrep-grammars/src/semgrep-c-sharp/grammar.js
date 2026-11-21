/*
 * semgrep-c-sharp
 *
 * Extend the original tree-sitter C# grammar with semgrep-specific constructs
 * used to represent semgrep patterns.
 *
 */

// tree-sitter-c-sharp 0.23.6+ switched grammar.js to ESM (`export default`);
// older versions used `module.exports`. Handle both shapes.
const _imported = require('tree-sitter-c-sharp/grammar');
const standard_grammar = _imported.default || _imported;

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
    [$.expression, $.parameter]
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

    // (Older versions of this extension wrapped `file_scoped_namespace_declaration`
    // to allow a prefix of statements/declarations before it. In the post-C#-14
    // grammar `compilation_unit` already accepts arbitrary `_top_level_item`s
    // — including `global_statement` and `_namespace_member_declaration` —
    // before `file_scoped_namespace_declaration`, so the wrapper is redundant
    // and now conflicts with `preproc_if_in_top_level`. Removed.)

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

    // Same shape as cast_expression but with a metavariable in place of the
    // cast value. We need prec.dynamic > cast_expression's (which is 1) so the
    // typed_metavariable interpretation wins over an ambiguous parse like
    // `(List<T> $X)` which can otherwise be read as `parenthesized_expression`
    // containing the binary chain `List < T > $X`.
    //TODO: use PREC.CAST from original grammar instead of 17 below
    typed_metavariable: $ => prec.dynamic(2, prec.right(17, seq(
      '(',
      field('type', $.type),
      field('metavar', $._semgrep_metavariable),
      ')',
    ))),

    // Real-world C# code occasionally contains technically-invalid
    // escape sequences inside string literals (`"\\share\images"` where
    // `\i` is not one of the spec's recognized escapes). Roslyn rejects
    // these, but the upstream tree-sitter grammar follows Roslyn and
    // produces an ERROR node, which then fails semgrep's parse-error
    // gate. Semgrep is a static analyzer that wants to scan as much
    // code as it can, so we broaden `escape_sequence` here to accept
    // any `\<single-char>` after the spec-recognized escapes have had a
    // chance to match.
    escape_sequence: ($, previous) => choice(previous, token(/\\./)),

    deep_ellipsis: $ => seq(
      '<...', $.expression, '...>'
    ),

    ellipsis: $ => '...',
  }
});
