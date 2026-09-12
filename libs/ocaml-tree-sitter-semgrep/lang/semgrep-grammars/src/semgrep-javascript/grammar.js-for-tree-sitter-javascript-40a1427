/*
  semgrep-javascript

  Extends the standard javascript grammar with semgrep pattern constructs.

  Semgrep metavariables are already valid javascript
  identifiers so we do nothing for them.

  Maintenance:

  - Most tests are not in the tree-sitter format, but are under
    /lang/javascript/test. To test the grammar, use:

      cd lang
      ./test-lang javascript

  - Some of the rules aren't really extended but were copy-pasted then edited.
    They may have to be updated more frequently. Tests should catch problems.
*/

const javascript_grammar = require('tree-sitter-javascript/grammar');

module.exports = grammar(javascript_grammar, {
  name: 'javascript',


  conflicts: ($, previous) => previous.concat([

    // conflicts between semgrep ellipsis and spread elements
    [$.spread_element, $.rest_pattern, $.semgrep_dots],
    [$.rest_pattern, $.semgrep_dots],
    [$.spread_element, $.semgrep_dots],

    // conflict occurring when extending '_formal_parameter'
    [$.primary_expression, $.formal_parameters],
  ]),

  rules: {

    /*
       We create multiple "entry points" so as to allow different kinds of
       semgrep patterns, prefixed with some new keyword.
    */

    // Original:
    //
    // program: $ => seq(
    //   optional($.hash_bang_line),
    //   repeat($.statement)
    // ),
    //
    program: $ => seq(
      optional($.hash_bang_line),
      choice (
        repeat($.statement),
        seq('__SEMGREP_PARTIAL', $.semgrep_partial)
      )
    ),

    semgrep_dots: $ => '...',
    semgrep_ldots: $ => '<...',
    semgrep_rdots: $ => '...>',
    semgrep_metavar: $ => /\$[a-zA-Z_][a-zA-Z_0-9]*/,

    // Constructs valid as whole semgrep patterns only.
    //
    // pfff: sgrep_spatch_pattern
    semgrep_partial: $ => choice(
      // truncated 'function_declaration'
      seq(
        optional('async'),
        'function',
        $.identifier,
        $._call_signature
      ),
      // truncated 'class_declaration'
      seq(
        repeat($.decorator),
        'class',
        $.identifier,
        optional($.class_heritage),
      ),
      seq('if', $.parenthesized_expression),
      seq('try', $.statement_block),
      $.catch_clause,
      $.finally_clause
    ),

    // pfff: stmt
    statement: ($, previous) => {
      return choice(
        ...previous.members,
        // prec(100, $.semgrep_dots), // higher precedence than expression
        $.semgrep_for
      );
    },

    primary_expression: ($, previous) => {
      return choice(
        ...previous.members,

        // pfff: assignment_expr
        $.semgrep_dots, // conflict

        // pfff: primary_expr_no_braces
        $.semgrep_deep_expression,
      );
    },

    semgrep_deep_expression: $ => seq(
      $.semgrep_ldots, $.expression, $.semgrep_rdots
    ),

    // pfff: formal_parameter
    _formal_parameter: ($, previous) => {
      return choice(
        ...previous.members,
        $.semgrep_dots
      );
    },

    // pfff: class_element (repeated in the class body)
    class_body: $ => seq(
      '{',
      repeat(choice(
        seq(field('member', $.method_definition), optional(';')),
        seq(field('member', $.public_field_definition), $._semicolon),
        $.semgrep_dots, // added
      )),
      '}'
    ),

    // pfff: iteration_stmt
    semgrep_for: $ => seq(
      'for',
      '(',
      $.semgrep_dots,
      ')',
      $.statement // pfff: stmt1 (inline version of 'stmt')
    ),

    // pfff: xhp_attribute
    _jsx_attribute: ($, previous) => {
      return choice(
        ...previous.members,
        $.semgrep_dots,
      );
    },

    // pfff: xhp_attribute_value
    _jsx_attribute_value: ($, previous) => {
      return choice(
        ...previous.members,
        $.semgrep_dots,
        $.semgrep_metavar, // pfff: "$" XHPATTR  (in lexer_js.mll)
      );
    },

    // The original grammar was the following. Unfortunately, it's not really
    // extensible. TODO: It would be more maintainable if the repeated item had
    // its own rule in tree-sitter-javascript.
    //
    //     object: $ => prec('object', seq(
    //       '{',
    //       commaSep(optional(choice(
    //         $.pair,
    //         $.spread_element,
    //         $.method_definition,
    //         alias(
    //           choice($.identifier, $._reserved_identifier),
    //           $.shorthand_property_identifier
    //         )
    //       ))),
    //       '}'
    //     )),
    //
    // pfff: object_literal, property_name_and_value
    object: $ => prec('object', seq(
      '{',
      commaSep(optional(choice(
        $.pair,
        $.spread_element,
        $.method_definition,
        alias(
          choice($.identifier, $._reserved_identifier),
          $.shorthand_property_identifier
        ),
        $.semgrep_dots, // added
      ))),
      '}'
    )),

    // Original:
    //
    // member_expression: $ => prec('member', seq(
    //   field('object', choice($.expression, $.primary_expression)),
    //   choice('.', '?.'),
    //   field('property', alias($.identifier, $.property_identifier))
    // )),
    //
    // pfff: call_expr
    member_expression: $ => prec('member', seq(
      field('object', choice($.expression, $.primary_expression)),
      choice('.', '?.'),
      choice(
        field('property', alias($.identifier, $.property_identifier)),
        $.semgrep_dots // added
      )
    )),

    // Original:
    // _from_clause: $ => seq(
    //   "from", field('source', $.string)
    // ),
    //
    // pfff: module_specifier
    _from_clause: $ => seq(
      "from",
      choice(
        field('source', $.string),
        $.semgrep_metavar // added
      )
    ),
  }
});

// copy-pasted from the original grammar
function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}

// copy-pasted from the original grammar
function commaSep(rule) {
  return optional(commaSep1(rule));
}
