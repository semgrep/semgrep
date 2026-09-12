/*
 * semgrep-kotlin
 *
 * Extend the standard kotlin grammar with metavariable pattern constructs.
 * Ellipsis are in the kotlin grammar, so no need to extend for ellipsis.
 */

// INVARIATN: Make sure that you are merging any commits into the `semgrep`
// branch of `tree-sitter-kotlin`! This is because our version of
// `tree-sitter-kotlin` is forked from the original repository, and we
// want our branch to be kept separate.

const standard_grammar = require("tree-sitter-kotlin/grammar");

module.exports = grammar(standard_grammar, {
  name: "kotlin",

  rules: {
    // Entry point
    source_file: ($, previous) => {
      return choice(previous, $.semgrep_expression);
    },

    // Alternate "entry point". Allows parsing a standalone expression.
    semgrep_expression: ($) => seq("__SEMGREP_EXPRESSION", $._expression),

    semgrep_named_ellipsis: ($) => /\$\.\.\.[a-zA-Z_][a-zA-Z_0-9]*/,

    // Metavariables
    simple_identifier: ($, previous) => {
      return choice(previous, "constructor", /\$[a-zA-Z_][a-zA-Z_0-9]*/);
    },

    typed_metavar: ($) => seq("(", $.simple_identifier, ":", $._type, ")"),

    // Statement ellipsis: '...' not followed by ';'
    _expression: ($, previous) => {
      return choice(
        previous,
        $.ellipsis, // statement ellipsis
        $.deep_ellipsis,
        $.typed_metavar,
        $.semgrep_named_ellipsis,
      );
    },

    navigation_suffix: ($, previous) => {
      return choice(previous, seq($._member_access_operator, $.ellipsis));
    },

    _class_member_declaration: ($, previous) => {
      return choice(previous, $.ellipsis);
    },

    _function_value_parameter: ($, previous) => {
      return choice(previous, $.ellipsis);
    },

    class_parameter: ($, previous) => {
      return choice(previous, $.ellipsis);
    },

    deep_ellipsis: ($) => seq("<...", $._expression, "...>"),

    ellipsis: ($) => "...",
  },
});
