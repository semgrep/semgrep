// Precedence is used by the parser to determine which rule to apply when there are two rules that can be applied.
// We use the PREC dict to globally define rule pprecidence
const PREC = {
  COMMENT: 1,
  STRING: 2,

  COMMA: -1,
  OBJECT: -1,
  USER_TYPE: 1,
  DECLARATION: 1,
  ASSIGN: 0,
  TERNARY: 1,
  OR: 2,
  AND: 3,
  REL: 4,
  PLUS: 5,
  TIMES: 6,
  EXP: 7,
  TYPEOF: 8,
  DELETE: 8,
  VOID: 8,
  NOT: 9,
  NEG: 10,
  INC: 11,
  CALL: 12,
  NEW: 13,
  MEMBER: 1
}


module.exports = grammar({
  name: 'circom',

  extras: $ => [
    /\s/,
    $.comment
  ],

  conflicts: $ => [
    [$.member_expression, $._expression],
  ],

  rules: {
    //  -- [ Program ] --
    source_file: $ => seq(
      repeat($._source_unit),
    ),

    //  -- [ Source Element ] --
    _source_unit: $ =>  choice(
      $._directive,
      $._definition,
    ),

    //  -- [ Directives ] --
    _directive: $ => choice(
      $.pragma_directive,
      $.include_directive,
    ),

    // Pragma
    pragma_directive: $ => seq(
      "pragma",
      choice($.circom_pragma_token, $.circom_custom_templates_token),
      $._semicolon,
    ),

    circom_custom_templates_token: $ => prec(1, "custom_templates"),

    circom_pragma_token: $ => seq(
      $._circom,
      $.circom_version
    ),

    _circom: $ => prec(1, "circom"),
    circom_version: $ => /"?\.? ?(\d|\*)+(\. ?(\d|\*)+ ?(\.(\d|\*)+)?)?"?/,

    // Include
    include_directive: $ => seq(
      "include",
      field('source', $.string),
      $._semicolon
    ),

    //  -- [ Definition ] --
    _definition: $ => choice(
      $.function_definition,
      $.template_definition,
      $.main_component_definition
    ),


    template_definition: $ => seq(
      "template",
      optional($.template_type),
      field("name", $.identifier),
      $.parameter_list,
      $.template_body
    ),

    template_type: $ => choice(
      $.custom,
      $.parallel
    ),

    custom: $ => "custom",
    parallel: $ => "parallel",

    template_body: $ => seq(
      "{",
          repeat($._statement),
      "}",
    ),

    function_definition: $ => seq(
      "function",
      field("name", $.identifier),
      $.parameter_list,
      $.function_body
    ),

    function_body: $ => seq(
      "{",
          repeat($._statement),
      "}",
    ),

    main_component_definition: $ => seq(
      "component",
      "main",
      optional($.main_component_public_signals),
      "=",
      $.call_expression,
      $._semicolon

    ),

    main_component_public_signals: $ => seq(
      "{",
      "public",
      "[",
      commaSep1($.parameter),
      "]",
      "}"
    ),

    // -- [ Statements ] --
    _statement: $ => choice(
      $.return_statement,
      $.block_statement,
      $.if_statement,
      $.for_statement,
      $.while_statement,
      $.variable_declaration_statement,
      $.expression_statement
    ),

    block_statement: $ => seq('{', repeat($._statement), "}"),
    
    expression_statement: $ => seq($._expression, $._semicolon),

    if_statement: $ => prec.right(seq(
      'if', '(',$._expression, ')', $._statement, optional(seq('else', $._statement)),
    )),

    for_statement: $ => seq(
      'for', '(',
      choice($.variable_declaration_statement, $.expression_statement, $._semicolon),
      choice($.expression_statement, $._semicolon),
      optional($._expression),
      ')', $._statement,
    ),

    while_statement: $ => seq(
      'while', '(',$._expression, ')', $._statement,
    ),

    return_statement: $ => seq(
      'return', $._expression, $._semicolon
    ),

    variable_declaration_statement: $ => seq(
      $._type,
      commaSep1($._variable_initialization),
      $._semicolon
    ),

    _variable_initialization: $ => prec.left(1, seq(
      commaSep1($.identifier),
      optional($.array_definition),
      optional(seq(
        choice(
          '=',
          '<==',
          '==>',
          '<--',
          '-->'
        ), 
        field("value", $._expression)
      )))
    ),

    // Expressions

    _expression: $ => choice(
      $.int,
      $.identifier,
      $.array,
      $.tuple,
      $.unary_expression,
      $.binary_expression,
      $.ternary_expression,
      $.parenthesized_expression,
      $.call_expression,
      $.increment_expression,
      $.decrement_expression,
      $.member_expression, 
      $.array_access_expression,
      $.assignment_expression,
    ),

    assignment_expression: $ => prec.right(1, seq(
      choice(
        $._expression
      ),
      choice(
        '<==',
        '==>',
        '<--',
        '-->',
        '&=',
        '+=',
        '-=',
        '*=',
        '**=',
        '/=',
        '\\=',
        '%=',
        '|=',
        '^=',
        '>>=',
        '<<=',
        '===',
        '='
      ),
      $._expression,
    )),

    increment_expression: $ => seq(
      choice(
        $.identifier,
        $.member_expression,
        $.array_access_expression
      ),
      '++',
    ),

    decrement_expression: $ => seq(
      choice(
        $.identifier,
        $.member_expression,
        $.array_access_expression
      ),
      '--',
    ),

    anonymous_inputs: $ => seq(
      '(',
      optional($.argument_list),
      ')'
    ),

    call_expression: $ => seq(
      optional($.parallel),
      $.identifier,
      '(',
      optional($.argument_list),
      ')',
      optional($.anonymous_inputs)
    ),

    argument_list: $ => seq(
      $._expression,
      repeat(seq(
        ',',
        $._expression
      ))
    ),

    member_expression: $ => prec.dynamic(1, seq(
      field('object', choice(
          $._expression,
          $.identifier,
      )),
      '.',
      field('property', alias($.identifier, $.property_identifier))
    )),

    array_access_expression: $ => seq(
      field('base', $._expression),
      '[',
      optional(field('index', $._expression)),
      ']'
    ),

    parenthesized_expression: $ => prec(2, seq('(', $._expression, ')')),

    ternary_expression: $ => prec.left(
      seq($._expression, "?", $._expression, ':', $._expression)
    ),

    unary_expression: $ => choice(...[
          ['!', PREC.NOT],
          ['~', PREC.NOT],
          ['-', PREC.NEG],
          ['+', PREC.NEG],
      ].map(([operator, precedence]) =>
          prec.left(precedence, seq(
              field('operator', operator),
              field('argument', $._expression)
          ))
      ),
    ),

    binary_expression: $ => choice(
      ...[
      ['&&', PREC.AND],
      ['&', PREC.AND],
      ['||', PREC.OR],
      ['|', PREC.OR],
      ['>>', PREC.TIMES],
      ['<<', PREC.TIMES],
      ['^', PREC.OR],
      ['+', PREC.PLUS],
      ['-', PREC.PLUS],
      ['*', PREC.TIMES],
      ['/', PREC.TIMES],
      ['\\', PREC.TIMES],
      ['%', PREC.TIMES],
      ['**', PREC.EXP],
      ['<', PREC.REL],
      ['<=', PREC.REL],
      ['==', PREC.REL],
      ['!=', PREC.REL],
      ['>=', PREC.REL],
      ['>', PREC.REL],
      ].map(([operator, precedence]) =>
          prec.left(precedence, seq(
              field('left', $._expression),
              field('operator', operator),
              field('right', $._expression)
          ))
      )
    ),

    _semicolon: $ => ';',

    identifier: $ => /[a-zA-Z$_][a-zA-Z0-9$_]*/,

    int: $ => /\d+/,

    array: $ => seq(
      "[",
      commaSep1($._expression),
      "]"
    ),

    tuple: $ => seq(
      '(',
      commaSep1($._expression),
      ')'
    ),

    _type: $ => choice(
      $.signal,
      $.var,
      $.component
    ),

    array_definition: $ => repeat1(
      seq(
        "[",
        $._expression,
        "]",
      )
    ),

    var: $ => "var",

    component: $ => "component",

    signal: $ => seq(
      'signal',
      optional($.signal_visability),
      optional($.signal_tags)
    ),

    signal_visability: $ => choice(
      "input",
      "output"
    ),

    signal_tags: $ => seq(
      "{",
      $.identifier,
      repeat(
        seq(
          ",",
          $.identifier
        )
      ),
      "}"
    ),

    parameter_list: $ => seq(
      '(', commaSep($.parameter), ')'
    ),

    parameter: $ => seq(
      $.identifier
    ),

    _escape_sequence: $ => token.immediate(seq(
      '\\',
      choice(
        /[^xu0-7]/,
        /[0-7]{1,3}/,
        /x[0-9a-fA-F]{2}/,
        /u[0-9a-fA-F]{4}/
      )
    )),

    _string_immediate_elt_inside_double_quote: $ =>
      token.immediate(prec(PREC.STRING, /[^"\\\n]+|\\\r?\n/)),

    _string_immediate_elt_inside_quote: $ =>
      token.immediate(prec(PREC.STRING, /[^'\\\n]+|\\\r?\n/)),

    string: $ => choice(
      seq( 
        '"',
        repeat(choice(
            $._string_immediate_elt_inside_double_quote,
            $._escape_sequence
        )),
        '"'
      ),
      seq(
        "'",
        repeat(choice(
            $._string_immediate_elt_inside_quote,
            $._escape_sequence
        )),
        "'"
      )
    ),

    comment: $ => token(
      prec(PREC.COMMENT,
          choice(
              seq('//', /([^\r\n])*/),
              seq(
                  '/*',
                  /[^*]*\*+([^/*][^*]*\*+)*/,
                  '/'
              )
          )
      )
    ),

  }
});


function commaSep1(rule) {
  return seq(
      rule,
      repeat(
          seq(
              ',',
              rule
          )
      ),
      optional(','),
  );
}

function commaSep(rule) {
  return optional(commaSep1(rule));
}