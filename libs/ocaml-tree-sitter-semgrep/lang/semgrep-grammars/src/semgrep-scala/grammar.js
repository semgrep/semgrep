/*
  semgrep-scala

  Extends the standard Scala grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-scala/grammar');

module.exports = grammar(base_grammar, {
    name: 'scala',

    conflicts: ($, previous) => [
        ...previous,
        [$._simple_expression, $.semgrep_val_or_var_definition],
    ],

    rules: {
        semgrep_expression: $ => seq(token(prec(100, '__SEMGREP_EXPRESSION')), $.expression),
        semgrep_statement: $ => seq(token(prec(100, '__SEMGREP_STATEMENT')), choice($.expression, $._definition)),
        semgrep_member_decl: $ => seq(token(prec(100, '__SEMGREP_MEMBER_DECL')), choice($.function_definition, $.function_declaration, $.val_definition, $.val_declaration, $.var_definition, $.var_declaration)),
        semgrep_metavariable: _ => token(prec(1, /\$[A-Z][A-Za-z0-9_]*/)),
        semgrep_ellipsis: _ => token(prec(1, '...')),
        semgrep_ellipsis_metavariable: _ => token(prec(1, /\$\.\.\.[A-Za-z_][A-Za-z0-9_]*/)),
        deep_expression: $ => prec(10, seq('<...', $.expression, '...>')),

        _top_level_definition: ($, previous) => choice(
            $.semgrep_expression,
            $.semgrep_statement,
            $.semgrep_member_decl,
            previous,
        ),
        _simple_expression: ($, previous) => choice(
            previous,
            $.symbol_literal,
            $.semgrep_metavariable,
            $.deep_expression,
            $.semgrep_ellipsis_metavariable,
            $.semgrep_ellipsis,
        ),
        _pattern: ($, previous) => choice(previous, $.semgrep_ellipsis),
        parameter: ($, previous) => choice(previous, $.semgrep_ellipsis),
        class_parameter: ($, previous) => choice(previous, $.semgrep_ellipsis),
        enumerator: ($, previous) => choice(previous, prec(1, $.semgrep_ellipsis)),

        // Semgrep pattern: $DECL $NAME = expr
        // Matches val/var definitions where the keyword is abstracted by a
        // metavariable, e.g. `$DECL $SECRET = "..."`.
        semgrep_val_or_var_definition: $ =>
            seq(
                field("keyword", $.semgrep_metavariable),
                field("pattern", choice($._pattern, $.identifiers)),
                optional(seq(":", field("type", $._type))),
                "=",
                field("value", $._indentable_expression),
            ),

        _definition: ($, previous) => choice(previous, $.semgrep_val_or_var_definition),

        // Upstream grammar fixes (until merged upstream)

        // Fix: limit constructor annotation to single argument list.
        // The base `annotation` rule uses `repeat($.arguments)` which greedily
        // consumes class_parameters as annotation arguments in constructors
        // like `class Foo @Inject()(val x: Int)`.
        _constructor_annotation: $ =>
            prec.right(
                seq(
                    "@",
                    field("name", $._simple_type),
                    field("arguments", optional($.arguments)),
                ),
            ),

        // Fix: allow empty blocks in quote expressions.
        quote_expression: ($, _previous) =>
            prec.left(
                10, // PREC.macro
                seq(
                    "'",
                    choice(seq("{", optional($._block), "}"), seq("[", $._type, "]"), $.identifier),
                ),
            ),

        // Fix: support by-name repeated parameter types (=> T*).
        repeated_parameter_type: ($, _previous) =>
            prec.left(
                5, // PREC.postfix
                seq(field("type", choice($._type, $.lazy_parameter_type)), "*"),
            ),

        _class_constructor: ($, _previous) =>
            seq(
                field("name", $._identifier),
                field("type_parameters", optional($.type_parameters)),
                optional($._constructor_annotation),
                optional($.access_modifier),
                field(
                    "class_parameters",
                    repeat(seq(optional($._automatic_semicolon), $.class_parameters)),
                ),
            ),

        // Feat: Scala 2 symbol literal support.
        // Deprecated in Scala 3 but still accepted. Lower precedence so
        // quote_expression wins in Scala 3 contexts.
        symbol_literal: $ =>
            prec(-1, seq("'", field("name", $.identifier))),
    },
});
