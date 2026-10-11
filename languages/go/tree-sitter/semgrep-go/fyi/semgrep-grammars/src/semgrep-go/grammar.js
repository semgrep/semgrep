/*
  semgrep-go

  Accepts Go source snippets for Semgrep.
*/

const base_grammar = require('tree-sitter-go/grammar');
const commaSep1 = rule => seq(rule, repeat(seq(',', rule)));

module.exports = grammar(base_grammar, {
  name: 'go',

  // Preserve the old alternatives while allowing longer type-set terms.
  conflicts: ($, previous) => previous.concat([
    [$.interface_complex_type_set, $.interface_named_struct_type_set],
    [$.constraint_elem, $.interface_complex_type_set, $.interface_named_struct_type_set],
    [$.struct_elem, $.interface_complex_type_set, $.interface_named_struct_type_set],
    [$.interface_named_struct_type_set],
  ]),

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
      choice($.constraint_term, $.type_parameter_composite, $.generic_type),
      '|',
      choice($.constraint_term, $.type_parameter_composite, $.generic_type),
      repeat(seq('|', choice($.constraint_term, $.type_parameter_composite, $.generic_type))),
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

    // Interface type sets can mix composite and instantiated generic types.
    // Preserve the old constraint_elem and struct_elem CST shapes by using a
    // distinct production that requires at least one non-struct composite term.
    _interface_body: $ => choice(
      $.method_spec, $.interface_type_name, $.constraint_elem,
      $.struct_elem, $.interface_complex_type_set,
      $.interface_named_struct_type_set,
    ),

    interface_complex_type_set: $ => prec(-2, seq(
      repeat(seq(choice($.constraint_term, $.struct_term), '|')),
      $.interface_nonstruct_type_term,
      repeat(seq('|', choice(
        $.constraint_term, $.struct_term, $.interface_nonstruct_type_term,
      ))),
    )),

    // A named type and a struct may also form a type set without any other
    // composite type. Require at least one of each so homogeneous legacy
    // constraint_elem / struct_elem nodes keep their existing CST shape.
    interface_named_struct_type_set: $ => seq(
      repeat(seq(choice($.constraint_term, $.struct_term), '|')),
      choice(
        seq($.constraint_term, '|', $.struct_term),
        seq($.struct_term, '|', $.constraint_term),
      ),
      repeat(seq('|', choice($.constraint_term, $.struct_term))),
    ),

    interface_nonstruct_type_term: $ => choice(
      seq(optional('~'), choice(
        $.slice_type, $.map_type, $.array_type,
        $.channel_type, $.function_type,
      )),
      $.generic_type,
      $.interface_nonstruct_pointer,
      seq('~', $.pointer_type),
      // Go permits parenthesized type terms and interfaces used as terms.
      seq(optional('~'), choice($.parenthesized_type, $.interface_type)),
    ),

    // Keep *struct{...} in the legacy struct_term branch. Otherwise
    // Tree-sitter would change the CST for existing Go interfaces.
    interface_nonstruct_pointer: $ => seq(
      '*',
      choice(
        $._type_identifier, $.qualified_type, $.generic_type,
        $.slice_type, $.map_type, $.array_type,
        $.channel_type, $.function_type, $.pointer_type,
        $.parenthesized_type,
      ),
    ),

  }
});
