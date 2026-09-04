const PREC = {
    name: 8,
    primary: 7,
    unary: 6,
    multiplicative: 5,
    additive: 4,
    comparative: 3,
    and: 2,
    or: 1,
  },
  unary_operator = ['!', '*', '-'],
  multiplicative_operators = ["*", "/", "%"],
  additive_operators = ["+", "-"],
  comparative_operators = ["==", "!=", "<", "<=", ">", ">="],
  assignment_operators = multiplicative_operators
    .concat(additive_operators)
    .map((op) => op + "=")
    .concat("="),
  terminator = ";";

module.exports = grammar({
  name: "cairo",

  extras: ($) => [$.comment, /\s/],

  conflicts: ($) => [[$.block], [$.qualified_name, $.qualified_name_segment]],

  inline: ($) => [
    $._declaration,
    $._type,
    $._pattern,
    $._pattern_struct_binding,
    $._impl_declaration,
    $._statement,
    $._expression,
    $._simple_expression,
    $._match_case_pattern,
    $._pattern_var,
    $._literal_expression,
    $._pattern,
    $._modifier,
    $._identifier,
  ],

  rules: {
    source_file: ($) => repeat($._declaration),

    _declaration: ($) =>
      choice(
        $.import_declaration,

        $.module_declaration,
        $.typealias_declaration,
        $.const_declaration,

        $.trait_declaration,
        $.struct_declaration,
        $.enum_declaration,
        $._impl_declaration,
        $.function_declaration
      ),

    // ******************************************
    // Declaration
    // ******************************************
    path: ($) => prec.left(list1($.name, "::")),

    module_declaration: ($) =>
      seq(
        field("attributes", repeat($.attribute_list)),
        "mod",
        field("name", $.name),
        choice(terminator, $.module_body)
      ),

    module_body: ($) => seq("{", repeat($._declaration), "}"),

    import_declaration: ($) =>
      seq(
        "use",
        field("path", $.path),
        field("alias", optional(seq("as", $.name))),
        terminator
      ),

    const_declaration: ($) =>
      seq(
        field("attributes", repeat($.attribute_list)),
        "const",
        field("name", $.name),
        ":",
        field("type", $._type),
        "=",
        field("value", $._expression),
        terminator
      ),

    typealias_declaration: ($) =>
      seq(
        "type",
        field("name", $.name),
        field("type_parameters", optional($.type_parameter_list)),
        "=",
        field("type", $._type),
        terminator
      ),

    trait_declaration: ($) =>
      seq(
        field("attributes", repeat($.attribute_list)),
        "trait",
        field("name", $.name),
        field("type_parameters", optional($.type_parameter_list)),
        field("body", $.trait_body)
      ),

    trait_body: ($) => seq("{", repeat($.trait_function), "}"),

    // Allow either fully defined function (with body)
    // or signature only
    trait_function: ($) =>
      seq(
        field("signature", $.function_signature),
        choice(field("body", $.block), terminator)
      ),

    struct_declaration: ($) =>
      seq(
        field("attributes", repeat($.attribute_list)),
        "struct",
        field("name", $.name),
        field("type_parameters", optional($.type_parameter_list)),
        choice(field("fields", $.member_declaration_list), terminator)
      ),

    enum_declaration: ($) =>
      seq(
        field("attributes", repeat($.attribute_list)),
        "enum",
        field("name", $.name),
        field("type_parameters", optional($.type_parameter_list)),
        field("variants", $.member_declaration_list)
      ),

    member_declaration_list: ($) =>
      seq("{", list($.member_declaration, ","), "}"),

    member_declaration: ($) =>
      seq(
        field("attributes", repeat($.attribute_list)),
        field("name", $.name),
        ":",
        field("type", $._type)
      ),

    _impl_declaration: ($) => choice($.impl_base, $.impl_trait),

    // Impl MyType
    impl_base: ($) =>
      seq(
        field("attributes", repeat($.attribute_list)),
        "impl",
        field("name", $.name),
        field("type_parameters", optional($.type_parameter_list)),
        field("body", $.impl_body)
      ),

    // Impl MyType of MyTrait
    impl_trait: ($) =>
      seq(
        field("attributes", repeat($.attribute_list)),
        "impl",
        field("name", $.name),
        field("type_parameters", optional($.type_parameter_list)),
        "of",
        field("trait", $.qualified_name),
        field("body", $.impl_body)
      ),

    impl_body: ($) => seq("{", repeat($._declaration), "}"),

    function_declaration: ($) =>
      seq(field("signature", $.function_signature), field("body", $.block)),

    function_signature: ($) =>
      seq(
        field("attributes", repeat($.attribute_list)),
        "fn",
        field("name", $.name),
        field("type_parameters", optional($.type_parameter_list)),
        field("parameters", $.parameter_list),
        field("return_type", optional(seq("->", $._type)))
      ),

    parameter_list: ($) => seq("(", list($.parameter_declaration, ","), ")"),

    parameter_declaration: ($) =>
      seq(
        field("modifiers", repeat($._modifier)),
        field("name", $.name),
        ":",
        field("type", $._type)
      ),

    type_parameter_list: ($) =>
      seq("<", list1($._type_parameter_declaration, ","), ">"),

    _type_parameter_declaration: ($) =>
      choice($.name, $.type_const_declaration, $.type_impl_declaration),

    type_const_declaration: ($) =>
      seq("const", field("name", $.name), ":", field("type", $._type)),

    type_impl_declaration: ($) =>
      seq("impl", field("name", $.name), ":", field("type", $._type)),

    // ******************************************
    // Expression - Statements
    // ******************************************
    block: ($) =>
      seq("{", seq(repeat(seq($._statement)), optional($._expression)), "}"),

    _statement: ($) =>
      choice(
        $.let_statement,
        $.assignment_statement,
        $.return_statement,
        $.break_statement,

        // Depending on the context, these expressions can be interpreted as
        // statement
        $.if_expression,
        $.loop_expression,
        $.match_expression,

        seq($._expression, terminator)
      ),

    let_statement: ($) =>
      seq(
        "let",
        field("modifier", optional($.modifier_mut)),
        field("pattern", $._pattern),
        field("type", optional(seq(":", $._type))),
        "=",
        field("value", $._expression),
        terminator
      ),

    assignment_statement: ($) =>
      seq(
        field("lhs", choice($.name, $.wildcard)),
        field("operator", choice(...assignment_operators)),
        field("rhs", $._expression),
        terminator
      ),

    return_statement: ($) => seq("return", optional($._expression), terminator),

    break_statement: ($) => seq("break", optional($._expression), terminator),

    _expression: ($) =>
      choice(
        $.tuple_expression,
        $.block,

        $.unary_expression,
        $.binary_expression,

        $.if_expression,
        $.loop_expression,
        $.match_expression,
        $.selector_expression,

        $.call_expression,

        $.qualified_name,
        $._literal_expression
      ),

    // Expression that cannot be surrounded by brackets (Ex. if condition)
    _simple_expression: ($) =>
      choice(
        $.tuple_expression,

        $.unary_expression,
        $.binary_expression,

        $.if_expression,
        $.loop_expression,
        $.match_expression,
        $.selector_expression,

        $.call_expression,

        $.qualified_name,
        $._literal_expression
      ),

    unary_expression: ($) =>
      prec(
        PREC.unary,
        seq(
          field("operator", choice(...unary_operator)),
          field("operand", $._expression)
        )
      ),

    binary_expression: ($) => {
      const table = [
        [PREC.multiplicative, choice(...multiplicative_operators)],
        [PREC.additive, choice(...additive_operators)],
        [PREC.comparative, choice(...comparative_operators)],
        [PREC.and, "&&"],
        [PREC.or, "||"],
      ];

      return choice(
        ...table.map(([precedence, operator]) =>
          prec.left(
            precedence,
            seq(
              field("lhs", $._expression),
              field("operator", operator),
              field("rhs", $._expression)
            )
          )
        )
      );
    },

    tuple_expression: ($) => seq("(", list1($._expression, ","), ")"),

    if_expression: ($) =>
      prec.right(
        seq(
          "if",
          field("condition", $._simple_expression),
          field("then", $.block),
          field("else", optional(seq("else", choice($.if_expression, $.block))))
        )
      ),

    loop_expression: ($) => seq("loop", $.block),

    match_expression: ($) =>
      seq(
        "match",
        field("condition", $._expression),
        field("cases", $.match_cases)
      ),

    match_cases: ($) => seq("{", list($.match_case, ","), "}"),

    match_case: ($) =>
      seq(
        field("pattern", $._match_case_pattern),
        "=>",
        field("value", $._expression)
      ),

    _match_case_pattern: ($) => choice($._pattern, $._literal_expression),

    call_expression: ($) =>
      prec(
        PREC.primary,
        seq(
          field("operand", $._expression),
          field("arguments", $.argument_list)
        )
      ),

    argument_list: ($) => seq("(", list($._expression, ","), ")"),

    selector_expression: ($) =>
      seq(field("value", $._expression), ".", field("field", $.name)),

    _literal_expression: ($) =>
      choice($.true, $.false, $.number, $.string, $.unit),

    // ******************************************
    // Attributes
    //
    // Example: #[derive(Eq)], #[test]
    // ******************************************
    attribute_list: ($) => seq("#", "[", repeat1($.attribute), "]"),

    attribute: ($) => seq($.path, optional($._attribute_argument_list)),

    _attribute_argument_list: ($) =>
      seq("(", list1($.attribute_argument, ","), ")"),

    attribute_argument: ($) =>
      choice(
        // Simple literal value
        $._literal_expression,
        // Simple name
        $.name,
        // Key - Value
        seq($.path, ":", $._literal_expression),
        // Key - Tuple of arguments
        seq($.path, ":", $._attribute_argument_list)
      ),

    // ******************************************
    // Modifiers
    // ******************************************
    _modifier: ($) => choice($.modifier_ref, $.modifier_mut),

    modifier_ref: ($) => "ref",

    modifier_mut: ($) => "mut",

    // ******************************************
    // Names
    // ******************************************
    name: ($) => /[a-zA-Z][a-zA-Z0-9_]*/,

    qualified_name: ($) =>
      seq(
        repeat(seq($.qualified_name_segment, "::")),
        $.name,
        optional(seq("::", $.type_argument_list))
      ),
    qualified_name_segment: ($) => seq($.name, optional($.type_argument_list)),
    type_argument_list: ($) => seq("<", list1($._literal_expression, ","), ">"),

    wildcard: ($) => "_",

    // Represents a value binding (ex. val)

    // ******************************************
    // Types
    // ******************************************
    _type: ($) => choice($.type_tuple, $.unit_type, $.type_identifier),

    type_tuple: ($) => seq("(", list($._type, ","), ")"),

    unit_type: ($) => token(seq("(", ")")),

    type_identifier: ($) => seq($.qualified_name, optional($.type_parameters)),

    type_parameters: ($) => seq("<", list($._type, ","), ">"),

    // ******************************************
    // Patterns
    // ******************************************
    _pattern: ($) =>
      choice($._pattern_var, $.pattern_struct, $.pattern_enum, $.pattern_tuple),

    _pattern_var: ($) => choice($.wildcard, alias($.name, $.identifier)),

    pattern_struct: ($) =>
      seq($.qualified_name, "{", list1($._pattern_struct_binding, ","), "}"),

    _pattern_struct_binding: ($) =>
      choice($.name, seq($.name, ":", $._pattern)),

    pattern_enum: ($) =>
      seq($.qualified_name, "(", list1($._pattern, ","), ")"),

    pattern_tuple: ($) => seq("(", list1($._pattern, ","), ")"),

    // ******************************************
    // Literals
    // ******************************************
    true: ($) => "true",
    false: ($) => "false",
    unit: ($) => token(seq("(", ")")),
    number: ($) =>
      seq(
        field("value", choice($.integer, $.hex, $.octal, $.binary)),
        field("suffix", optional($._number_suffix))
      ),

    integer: ($) => /[0-9]+/,
    hex: ($) => /0x[0-9a-f]+/,
    octal: ($) => /0o[0-7]+/,
    binary: ($) => /0b[01]+/,
    _number_suffix: ($) => /_[a-z][a-z0-9]+/,

    string: ($) => token(seq("'", /[^']*/, "'")),

    // ******************************************
    // Extra
    // ******************************************

    comment: ($) => token(seq("//", /.*/, "\n")),
  },
});

function list(rule, separator) {
  return seq(repeat(seq(rule, separator)), optional(rule));
}

function list1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)), optional(separator));
}