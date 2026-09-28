/*
  semgrep-sfapex

  Extends the standard sfapex grammar:
  - with semgrep pattern constructs ('$FOO', '...', ...)
  - with alternate entrypoints allowing simple code fragments to be parsed
    as semgrep patterns

  This work derives from what we have for Java in
  ../semgrep-java/grammar.js
*/

// Import utilities
const {
  ci,
  commaJoined,
  commaJoined1,
  joined,
} = require("tree-sitter-sfapex/common/common.js");

const base_grammar = require('tree-sitter-sfapex/apex/grammar');

module.exports = grammar(base_grammar, {
  name: 'apex',

  // See explanations in semgrep-java/grammar.js
  conflicts: ($, previous) => previous.concat([
    [$.primary_expression, $.formal_parameter],
    [$.primary_expression, $.statement],
    [$.argument_list, $.formal_parameter],
    // A separated '?' may start either navigation or a ternary expression.
    [$.expression, $.field_access, $.method_invocation],
    [$.primary_expression, $.version_expression],
  ]),

  // This is the so-called "word token". It must be a terminal symbol.
  // Originally: $.identifier
  word: ($) => $._apex_identifier,

  rules: {
    // Entrypoint. We add alternate entrypoints for Semgrep patterns.
    parser_output: ($, previous) => choice(
      // Replace repeat($.declaration) which is too limited. (need to clarify)
      repeat($.statement),

      $.constructor_declaration,
      $.expression,
      $.annotation,
      prec(100, $.local_variable_declaration),

      ///// Partial definitions
      $._class_header,
      $._full_method_header,

      // Partial statements
      $.partial_if,
      $.partial_try,
      $.partial_catch,
      $.partial_finally
    ),

    partial_if: ($) => seq(ci("if"), $.parenthesized_expression),
    partial_try: ($) => seq(ci("try"), $.block),
    partial_catch: ($) => $.catch_clause,
    partial_finally: ($) => $.finally_clause,

    semgrep_ellipsis: $ => '...',
    semgrep_metavar_ellipsis: $ => /\$\.\.\.[A-Z_][A-Z_0-9]*/,
    semgrep_deep_expression: $ => seq('<...', $.expression, '...>'),
    semgrep_any_ellipsis: $ => choice(
      $.semgrep_ellipsis,
      $.semgrep_metavar_ellipsis
    ),

    // Ordinary identifiers already can start with a dollar sign.
    // This is for spots where we want to support metavariables but
    // an identifier is not already allowed.
    semgrep_metavar: $ => /\$[A-Z_][A-Z_0-9]*/,

    _apex_identifier: $ => /[\p{L}_$][\p{L}\p{Nd}_$]*/,

    // Split 'identifier' into two cases. This allows us to use
    // metavariables only where regular identifiers aren't allowed.
    identifier: ($) => choice(
      // We assume the tokenizer will prefer to match 'semgrep_metavar'
      // over the original pattern.
      $.semgrep_metavar,
      $._apex_identifier
    ),

    ////////////////////////////////////////////////////////////////////
    ///// Add Semgrep ellipsis and deep expressions in several places

    primary_expression: ($, previous) => choice(
      previous,
      $.semgrep_deep_expression,
      // Keep generic field/method patterns reachable after the reserved prefix.
      alias(ci("Package"), $.identifier),
    ),

    statement: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
    ),

    formal_parameter: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
      $.semgrep_metavar_ellipsis
    ),

    argument_list: ($) => seq(
      "(",
      commaJoined(
        choice(
          $.semgrep_ellipsis,
          $.semgrep_metavar_ellipsis,
          $.expression
        )
      ),
      ")"
    ),

    // class Foo<...,T1,...> {}
    type_parameter: ($, previous) => choice(
      $.semgrep_ellipsis,
      previous
    ),

    // foo.bar
    // foo. ... .bar
    // TODO. See method_invocation

    // Keep pattern bodies reachable even where Apex requires a block.
    do_statement: ($) => seq(
      ci("do"),
      field("body", $.statement),
      ci("while"),
      field("condition", $.parenthesized_expression),
      ";"
    ),

    // Prefer the dedicated native tree when a generic field access also fits.
    version_expression: ($, previous) => prec.dynamic(1, previous),

    // Navigation and multi-word keywords must allow intervening extras.
    safe_navigation_operator: ($) => seq("?", "."),
    before_insert: ($) => seq(ci("before"), ci("insert")),
    before_update: ($) => seq(ci("before"), ci("update")),
    before_delete: ($) => seq(ci("before"), ci("delete")),
    after_insert: ($) => seq(ci("after"), ci("insert")),
    after_update: ($) => seq(ci("after"), ci("update")),
    after_delete: ($) => seq(ci("after"), ci("delete")),
    after_undelete: ($) => seq(ci("after"), ci("undelete")),
    with_sharing: ($) => seq(ci("with"), ci("sharing")),
    without_sharing: ($) => seq(ci("without"), ci("sharing")),
    inherited_sharing: ($) => seq(ci("inherited"), ci("sharing")),

    // for(...) {}
    for_statement: ($) => seq(
      ci("for"),
      "(",
      choice(
        $.semgrep_ellipsis,
        seq(
          choice(
            field("init", $.local_variable_declaration),
            seq(commaJoined(field("init", $.expression)), ";"),
          ),
          field("condition", optional($.expression)),
          ";",
          commaJoined(field("update", $.expression))
        )
      ),
      ")",
      field("body", $.statement)
    ),

    // class X { ... }
    _class_body_declaration: ($, previous) => choice(
      $.semgrep_ellipsis,
      previous
    ),

    // interface X { ... }
    interface_body: ($) => seq(
      "{",
      repeat(
        choice(
          $.semgrep_ellipsis,
          $.constant_declaration,
          $.enum_declaration,
          $.method_declaration,
          $.class_declaration,
          $.interface_declaration,
          ";"
        )
      ),
      "}"
    ),

    // Commas are intentional in Semgrep annotation patterns, alongside Apex's
    // whitespace-separated key/value arguments.
    annotation_argument_list: ($) => seq(
      "(",
      choice(
        field("value", $._element_value),
        optional(joined(optional(","), $.annotation_key_value))
      ),
      ")"
    ),

    // @SomeAnnot(...)
    annotation_key_value: ($, previous) => choice(
      $.semgrep_ellipsis,
      previous
    ),

    // enum X { ... }
    enum_constant: ($, previous) => choice(
      $.semgrep_ellipsis,
      previous
    ),

    // switch on foo { ... }
    switch_rule: ($, previous) => choice(
      $.semgrep_ellipsis,
      previous
    ),

    /////////////////////////////////////////////////////////////////////
    ///// Add support for partial constructs used in Semgrep patterns

    // Redefine class_declaration, splitting it into header and body.
    class_declaration: ($) =>
      seq($._class_header, $.class_body),

    // Derived from class_declaration.
    // (hidden so that it doesn't change the original text expectations)
    _class_header: ($) => seq(
      optional($.modifiers),
      ci("class"),
      $.identifier,
      optional($.type_parameters),
      optional($.superclass),
      optional($.interfaces)
    ),

    // Derived from method_declaration.
    _full_method_header: ($) => seq(
      optional($.modifiers),
      $._method_header,
    ),

/*
    // Typed metavariable (TODO).
    // Should parse the following: (Point $X)
    primary_expression: ($, previous) => choice(
      $.semgrep_typed_metavar,
      previous
    ),

    semgrep_typed_metavar: ($) => seq(
      "(", $._type, ")"
    )
*/

    ////////////// Semgrep extensions for SOQL

    // Embedded queries share Apex's comment extras between keyword words.
    soql_using_clause: ($) => seq(ci("USING"), ci("SCOPE"), $.using_scope_type),
    group_by_clause: ($) => seq(
      ci("GROUP"), ci("BY"), $._group_by_expression, optional($.having_clause)
    ),
    with_data_cat_expression: ($) => seq(
      ci("DATA"), ci("CATEGORY"), joined(ci("AND"), $.with_data_cat_filter)
    ),
    order_by_clause: ($) => seq(ci("ORDER"), ci("BY"), commaJoined1($.order_expression)),
    order_null_direction: ($) => seq(ci("NULLS"), choice(ci("FIRST"), ci("LAST"))),
    all_rows_clause: ($) => seq(ci("ALL"), ci("ROWS")),
    set_comparison_operator: ($) => choice(
      ci("IN"), seq(ci("NOT"), ci("IN")), ci("INCLUDES"), ci("EXCLUDES")
    ),

    // SELECT ...
    _selectable_expression: ($, previous) => choice(
      $.semgrep_ellipsis,
      $.semgrep_metavar_ellipsis,
      previous
    ),

    // FROM ...
    storage_identifier: ($, previous) => choice(
      $.semgrep_ellipsis,
      $.semgrep_metavar_ellipsis,
      previous
    ),

    // WHERE $A
    // This is what requires 'identifier' to be a choice between
    // the terminals 'semgrep_metavar' and '_apex_identifier' such that
    // the tokenizer prefers the former. Without this, it would not
    // see 'semgrep_metavar' here but an ordinary identifier
    // (the original 'identifier' that we renamed '_apex_identifier'),
    // interpreting the '$A' in 'WHERE $A' incorrectly and causing a syntax
    // error.
    _condition_expression: ($, previous) => choice(
      $.semgrep_metavar,
      previous
    )
  }
});
