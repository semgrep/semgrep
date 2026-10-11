/*
  semgrep-go

  Accepts Go source snippets for Semgrep.
*/

const base_grammar = require('tree-sitter-go/grammar');
const commaSep1 = rule => seq(rule, repeat(seq(',', rule)));

module.exports = grammar(base_grammar, {
  name: 'go',

  rules: {
    // Snippets may end in a statement without a newline. Avoid synthetic EOF
    // tokens, which have no source text for typed-CST recovery.
    source_file: $ => seq(
      repeat(seq(
        choice($._statement, $._top_level_declaration),
        choice(/\n/, ';'),
      )),
      optional(choice($._statement, $._top_level_declaration)),
    ),

    // Go 1.24 permits parameters on aliases as well as defined types.
    type_alias: $ => seq(
      field('name', $._type_identifier),
      field('type_parameters', optional($.type_parameter_list)),
      '=',
      field('type', $._type),
    ),

    // Keep function parameter parsing unchanged; unions apply only here.
    type_parameter_list: $ => seq(
      '[',
      seq(
        alias($.type_parameter_spec, $.parameter_declaration),
        repeat(seq(',', alias($.type_parameter_spec, $.parameter_declaration))),
      ),
      optional(','),
      ']',
    ),

    type_parameter_spec: $ => prec.right(1, seq(
      commaSep1(field('name', $.identifier)),
      field('type', choice($._type, $.type_parameter_union, $.type_parameter_approx)),
    )),

    // A standalone identifier is already handled by _type: union syntax
    // must actually include a pipe to avoid changing ordinary parameters.
    type_parameter_union: $ => seq(
      choice($.constraint_term, $.type_parameter_composite),
      '|',
      choice($.constraint_term, $.type_parameter_composite),
      repeat(seq('|', choice($.constraint_term, $.type_parameter_composite))),
    ),

    type_parameter_composite: $ => seq(
      optional('~'),
      choice(
        $.slice_type, $.map_type, $.array_type, $.struct_type,
        $.pointer_type, $.channel_type, $.function_type,
      ),
    ),

    type_parameter_approx: $ => seq(
      '~',
      choice(
        $._type_identifier, $.qualified_type, $.slice_type, $.map_type,
        $.array_type, $.struct_type, $.pointer_type,
        $.channel_type, $.function_type,
      ),
    ),

    // Qualified names are valid as terms of a Go type set.
    constraint_term: $ => prec(-1, seq(
      optional('~'),
      choice($._type_identifier, $.qualified_type),
    )),

  }
});
