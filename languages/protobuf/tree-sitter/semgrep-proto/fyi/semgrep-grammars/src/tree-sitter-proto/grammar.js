const
  letter = /[a-zA-Z]/,
  decimal_digit = /[0-9]/
  octal_digit = /[0-7]/
  hex_digit = /[0-9A-Fa-f]/

module.exports = grammar({
  name: 'proto',

  extras: $ => [$.comment, /\s/],

  rules: {
    // proto = syntax { import | package | option | topLevelDef | emptyStatement }
    // topLevelDef = message | enum | service
    source_file: $ => seq(
      $.syntax,
      optional(repeat(choice(
        $.import,
        $.package,
        $.option,
        $.enum,
        $.message,
        $.service,
        $.empty_statement,
      ))),
    ),

    empty_statement: $ => ';',

    // syntax = "syntax" "=" quote "proto3" quote ";"
    syntax: $ => seq('syntax', '=', '"proto3"', ';'),

    // import = "import" [ "weak" | "public" ] strLit ";"
    import: $ => seq(
      'import',
      optional(choice('weak', 'public')),
      field('path', $.string),
      ';',
    ),

    // package = "package" fullIdent ";"
    package: $ => seq(
      'package',
      $.full_ident,
      ';',
    ),

    // option = "option" optionName  "=" constant ";"
    // optionName = ( ident | "(" fullIdent ")" ) { "." ident }
    option: $ => seq(
      'option',
      $._option_name,
      '=',
      $.constant,
      ';',
    ),

    _option_name: $ => seq(
      choice(
        $.identifier,
        seq('(', $.full_ident, ')'),
      ),
      repeat(seq(
        '.',
        $.identifier,
      )),
    ),

    // enum = "enum" enumName enumBody
    // enumBody = "{" { option | enumField | emptyStatement } "}"
    // enumField = ident "=" [ "-" ] intLit [ "[" enumValueOption { ","  enumValueOption } "]" ]";"
    // enumValueOption = optionName "=" constant
    enum: $ => seq(
      'enum',
      $.enum_name,
      $.enum_body,
    ),

    enum_name: $ => $.identifier,

    enum_body: $ => seq(
      '{',
      repeat(choice(
        $.option,
        $.enum_field,
        $.empty_statement,
      )),
      '}',
    ),

    enum_field: $ => seq(
      $.identifier,
      '=',
      optional('-'),
      $.int_lit,
      optional(seq(
        '[',
        $.enum_value_option,
        repeat(seq(',', $.enum_value_option)),
        ']',
      )),
      ';',
    ),

    enum_value_option: $ => seq(
      $._option_name,
      '=',
      $.constant,
    ),

    // message = "message" messageName messageBody
    // messageBody = "{" { field | enum | message | option | oneof | mapField | reserved | emptyStatement } "}"
    message: $ => seq(
      'message',
      $.message_name,
      $.message_body,
    ),

    message_body: $ => seq(
      '{',
      repeat(choice(
        $.field,
        $.enum,
        $.message,
        $.option,
        $.oneof,
        $.map_field,
        $.reserved,
        $.empty_statement,
      )),
      '}',
    ),

    message_name: $ => $.identifier,

    // field = [ "repeated" ] type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
    // fieldOptions = fieldOption { ","  fieldOption }
    // fieldOption = optionName "=" constant
    field: $ => seq(
      // This isn't allowed according to the spec and yet the proto3 compiler
      // accepts it so we put it here for parsing.
      optional('optional'),

      optional('repeated'),
      $.type,
      $.identifier,
      '=',
      $.field_number,
      optional(seq('[', $.field_options, ']')),
      ';',
    ),

    field_options: $ => seq(
      $.field_option,
      repeat(seq(',', $.field_option)),
    ),

    field_option: $ => seq(
      $._option_name,
      '=',
      $.constant,
    ),

    // oneof = "oneof" oneofName "{" { option | oneofField | emptyStatement } "}"
    // oneofField = type fieldName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
    oneof: $ => seq(
      'oneof',
      $.identifier,
      '{',
      repeat(choice(
        $.option,
        $.oneof_field,
        $.empty_statement,
      )),
      '}',
    ),

    oneof_field: $ => seq(
      $.type,
      $.identifier,
      '=',
      $.field_number,
      optional(seq('[', $.field_options, ']')),
    ),

    // mapField = "map" "<" keyType "," type ">" mapName "=" fieldNumber [ "[" fieldOptions "]" ] ";"
    // keyType = "int32" | "int64" | "uint32" | "uint64" | "sint32" | "sint64" |
    //        "fixed32" | "fixed64" | "sfixed32" | "sfixed64" | "bool" | "string"
    map_field: $ => seq(
      'map',
      '<',
      $.key_type,
      ',',
      $.type,
      '>',
      $.identifier,
      '=',
      $.field_number,
      optional(seq('[', $.field_options, ']')),
      ';',
    ),

    key_type: $ => choice(
      'int32',
      'int64',
      'uint32',
      'uint64',
      'sint32',
      'sint64',
      'fixed32',
      'fixed64',
      'sfixed32',
      'sfixed64',
      'bool',
      'string',
    ),

    // type = "double" | "float" | "int32" | "int64" | "uint32" | "uint64"
    //    | "sint32" | "sint64" | "fixed32" | "fixed64" | "sfixed32" | "sfixed64"
    //    | "bool" | "string" | "bytes" | messageType | enumType
    type: $ => choice(
      'double',
      'float',
      'int32',
      'int64',
      'uint32',
      'uint64',
      'sint32',
      'sint64',
      'fixed32',
      'fixed64',
      'sfixed32',
      'sfixed64',
      'bool',
      'string',
      'bytes',
      $.message_or_enum_type,
    ),

    // reserved = "reserved" ( ranges | fieldNames ) ";"
    // ranges = range { "," range }
    // range =  intLit [ "to" ( intLit | "max" ) ]
    // fieldNames = fieldName { "," fieldName }
    reserved: $ => seq(
      'reserved',
      choice($.ranges, $.field_names),
      ';',
    ),

    ranges: $ => seq($.range, repeat(seq(',', $.range))),

    range: $ => seq(
      $.int_lit,
      optional(seq(
        'to',
        choice($.int_lit, 'max'),
      )),
    ),

    field_names: $ => seq(
      $.identifier,
      repeat(seq(',', $.identifier)),
    ),

    // messageType = [ "." ] { ident "." } messageName
    message_or_enum_type: $ => seq(
      optional('.'),
      repeat(seq(
        $.identifier,
        '.',
      )),
      $.identifier,
    ),

    // fieldNumber = intLit;
    field_number: $ => $.int_lit,

    // service = "service" serviceName "{" { option | rpc | emptyStatement } "}"
    // rpc = "rpc" rpcName "(" [ "stream" ] messageType ")" "returns" "(" [ "stream" ]
    //          messageType ")" (( "{" {option | emptyStatement } "}" ) | ";")
    service: $ => seq(
      'service',
      $.service_name,
      '{',
      repeat(choice(
        $.option,
        $.rpc,
        $.empty_statement,
      )),
      '}',
    ),

    service_name: $ => $.identifier,

    rpc: $ => seq(
      'rpc',
      $.rpc_name,
      '(',
      optional('stream'),
      $.message_or_enum_type,
      ')',
      'returns',
      '(',
      optional('stream'),
      $.message_or_enum_type,
      ')',
      choice(
        seq(
          '{',
          repeat(choice(
            $.option,
            $.empty_statement,
          )),
          '}',
        ),
        ';',
      ),
    ),

    rpc_name: $ => $.identifier,

    // constant = fullIdent | ( [ "-" | "+" ] intLit ) | ( [ "-" | "+" ] floatLit ) | strLit | boolLit
    constant: $ => choice(
      $.full_ident,
      seq(
        optional(choice('-', '+')),
        $.int_lit,
      ),
      seq(
        optional(choice('-', '+')),
        $.float_lit,
      ),
      $.string,
      $.bool,

      // block_lit is not specified but is used in the real world
      // (i.e. grpc-gateway) so we define it
      $.block_lit,
    ),

    // block_lit is completely unspecified. I determined what is allowed
    // based on the "a bit of everything" grpc-gateway example which has
    // wildly inconsistent syntax and yet it actually parses and compiles
    // with protoc.
    block_lit: $ => seq(
      '{',
      repeat(seq(
        $.identifier,
        optional(':'),
        choice(
          $.constant,
          seq(
            '[',
            $.constant,
            repeat(seq(',', $.constant)),
            ']',
          ),
        ),
        optional(choice(',', ';')),
      )),
      '}',
    ),

    // ident = letter { letter | decimalDigit | "_" }
    identifier: $ => token(seq(
      letter,
      optional(repeat(choice(
        letter,
        decimal_digit,
        '_',
      ))),
    )),

    // fullIdent = ident { "." ident }
    full_ident: $ => seq(
      $.identifier,
      optional(repeat(seq('.', $.identifier))),
    ),

    // boolLit = "true" | "false"
    bool: $ => choice($.true, $.false),
    true: $ => 'true',
    false: $ => 'false',

    // intLit     = decimalLit | octalLit | hexLit
    int_lit: $ => choice(
      $.decimal_lit,
      $.octal_lit,
      $.hex_lit,
    ),

    // decimalLit = ( "1" … "9" ) { decimalDigit }
    decimal_lit: $ => token(seq(
      /[1-9]/,
      repeat(decimal_digit),
    )),

    // octalLit   = "0" { octalDigit }
    octal_lit: $ => token(seq(
      '0',
      repeat(octal_digit),
    )),

    // hexLit     = "0" ( "x" | "X" ) hexDigit { hexDigit }
    hex_lit: $ => token(seq(
      '0',
      choice('x', 'X'),
      hex_digit,
      repeat(hex_digit),
    )),

    // floatLit = ( decimals "." [ decimals ] [ exponent ] | decimals exponent | "."decimals [ exponent ] ) | "inf" | "nan"
    // decimals  = decimalDigit { decimalDigit }
    // exponent  = ( "e" | "E" ) [ "+" | "-" ] decimals
    float_lit: $ => {
      const decimals = seq(
        decimal_digit,
        repeat(decimal_digit),
      );

      const exponent = seq(
        choice('e', 'E'),
        optional(choice('+', '-')),
        decimals,
      );

      return token(choice(
        seq(
          decimals,
          '.',
          optional(decimals),
          optional(exponent),
        ),
        seq(
          decimals,
          exponent,
        ),
        seq(
          '.',
          decimals,
          optional(exponent),
        ),
        'inf',
        'nan',
      ));
    },

    string: $ => choice(
      seq(
        '"',
        repeat(choice(
          token.immediate(prec(1, /[^"\\]+/)),
          $.escape_sequence
        )),
        '"'
      ),

      seq(
        "'",
        repeat(choice(
          token.immediate(prec(1, /[^'\\]+/)),
          $.escape_sequence
        )),
        "'",
      ),
    ),

    escape_sequence: $ => token.immediate(seq(
      '\\',
      choice(
        /[^xuU]/,
        /\d{2,3}/,
        /x[0-9a-fA-F]{2,}/,
        /u[0-9a-fA-F]{4}/,
        /U[0-9a-fA-F]{8}/
      )
    )),

    comment: $ => token(choice(
      seq('//', /.*/),
      seq(
        '/*',
        /[^*]*\*+([^/*][^*]*\*+)*/,
        '/'
      )
    ))
  }
});
