/*
 * semgrep-rust
 *
 * Extend the original tree-sitter Rust grammar with semgrep-specific constructs
 * used to represent semgrep patterns.
 */

const standard_grammar = require("tree-sitter-cairo/grammar.js");

module.exports = grammar(standard_grammar, {
  name: "cairo",

  inline: ($, previous) => [...previous, $.name],

  rules: {
    // Entry point
    source_file: ($, previous) =>
      choice(previous, $.semgrep_expression, $.semgrep_statement),

    // Alternate "entry point". Allows parsing a standalone expression.
    semgrep_expression: ($) => seq("__SEMGREP_EXPRESSION", $._expression),

    // Alternate "entry point". Allows parsing a standalone list of statements.
    semgrep_statement: ($) => seq("__SEMGREP_STATEMENT", repeat1($._statement)),

    // Metavariables
    semgrep_var: ($) => /\$[A-Z_][A-Z_0-9]*/,

    deep_ellipsis: ($) => seq("<...", $._expression, "...>"),

    ellipsis: ($) => "...",

    _declaration: ($, previous) => choice(...previous.members, $.ellipsis),

    // Expression ellipsis
    _expression: ($, previous) =>
      choice(...previous.members, $.ellipsis, $.deep_ellipsis),

    _simple_expression: ($, previous) =>
      choice(...previous.members, $.ellipsis, $.deep_ellipsis),

    _statement: ($, previous) =>
      choice(...previous.members, $.ellipsis, $.deep_ellipsis),

    name: ($, previous) => choice(alias(previous, $.name), $.semgrep_var),

    attribute_argument: ($, previous) => choice(...previous.members, $.ellipsis),

    selector_expression: ($, previous) => choice(
      previous, 
      seq(field("value", $._expression), ".", $.ellipsis)
    ),

    parameter_declaration: ($, previous) => choice(previous, $.ellipsis),

    member_declaration: ($, previous) => choice(previous, $.ellipsis),
  },
});
