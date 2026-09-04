// Precedence is used by the parser to determine which rule to apply when there are two rules that can be applied.
// We use the PREC dict to globally define rule precedence
// [N] corresponds to precedence table at https://docs.soliditylang.org/en/v0.8.24/cheatsheet.html#order-of-precedence-of-operators
const PREC = {
    COMMENT: 1,
    STRING: 2,

    COMMA: -1, // [15] Comma operator
    OBJECT: -1,
    USER_TYPE: 1,
    DECLARATION: 1,
    TERNARY: 0, // [14] Ternary operator
    ASSIGN: 0, // [14] Assignment operators
    LOGICAL_OR: 1, // [13] Logical OR
    LOGICAL_AND: 2, // [12] Logical AND
    REL_EQ: 3, // [11] Equality operators
    REL_INEQ: 4, // [10] Inequality operators
    BITWISE_OR: 5, // [9] Bitwise OR
    BITWISE_XOR: 6, // [8] Bitwise XOR
    BITWISE_AND: 7, // [7] Bitwise AND
    BITWISE_SHIFT: 8, // [6] Bitwise shift operators
    PLUS: 9, // [5] Addition and substraction
    TIMES: 10, // [4] Multiplication, division and modulo
    EXP: 11, // [3] Exponentiation
    PREFIX_UNARY: 12, // [2]
    POSTFIX_UNARY: 13, // [1]
    CALL: 13, // ??
    REVERT: 13, // ??
}

// The following is the core grammar for Solidity. It accepts Solidity smart contracts between the versions 0.4.x and 0.8.x.
module.exports = grammar({
    name: 'solidity',

    // Extras is an array of tokens that is allowed anywhere in the document.
    extras: $ => [
        // Allow comments to be placed anywhere in the file
        $.comment,
        // Allow characters such as whitespaces to be placed anywhere in the file
        /[\s\uFEFF\u2060\u200B\u00A0]/
    ],

    // The word token allows tree-sitter to appropriately handle scenario's where an identifier includes a keyword.
    // Documentation: https://tree-sitter.github.io/tree-sitter/creating-parsers#keywords
    word: $ => $.identifier,

    conflicts: $ => [
        // The following conflicts are all due to the array type and array access expression ambiguity
        [$._primary_expression, $.type_name],
        [$._primary_expression, $._identifier_path],
        [$._primary_expression, $.member_expression, $._identifier_path],
        [$.member_expression, $._identifier_path],
        [$.layout_specifier, $.struct_expression],
        [$._nameless_parameter, $.parameter],

        [$._primary_expression, $.type_cast_expression],
        [$.variable_declaration_tuple, $.tuple_expression],

        [$._yul_expression, $.yul_assignment],
        // Ambiguity: identifier ':'
        [$.yul_label, $.yul_identifier],

        // This is to deal with ambiguities arising from different fallback styles
        [$.fallback_receive_definition, $._function_type]
    ],

    rules: {
        //  -- [ Program ] --
        source_file: $ => seq(
            repeat($._source_unit),
        ),

        //  -- [ Source Element ] --
        _source_unit: $ =>  choice(
            $._directive,
            $._declaration,
        ),

        //  -- [ Directives ] --
        _directive: $ => choice(
            $.pragma_directive,
            $.import_directive,
        ),

        // Pragma
        pragma_directive: $ => seq(
            "pragma",
            choice($.solidity_pragma_token, $.any_pragma_token),
            $._semicolon,
        ),

        solidity_pragma_token: $ => prec(10, seq(
            $._solidity,
            repeat(seq(field("version_constraint", $._pragma_version_constraint), optional(choice("||", "-")))),
        )),

        any_pragma_token: $ => seq(
            $.identifier,
            $.pragma_value,
        ),

        _solidity: $ => prec(1, "solidity"),
        pragma_value: $ =>  prec(0, /[^;]+/),

        _pragma_version_constraint: $ => seq(
            optional($.solidity_version_comparison_operator),
            $.solidity_version,
        ),
        solidity_version: $ => /"?\.? ?(\d|\*)+(\. ?(\d|\*)+ ?(\.(\d|\*)+)?)?"?/,
        solidity_version_comparison_operator: $ => choice("<=", "<", "^", ">", ">=", "~", "="),

        // Import
        import_directive: $ => seq(
            'import',
            choice(
                $._source_import,
                seq($._import_clause, $._from_clause)
            ),
            $._semicolon,
        ),

        _source_import: $ => seq(
            field('source', $.string),
            optional($._import_alias)
        ),

        _import_clause: $ => choice(
            $._single_import,
            $._multiple_import,
        ),

        _from_clause: $ => seq(
            "from", field('source', $.string)
        ),

        _single_import: $ => seq(
            choice("*", field("import_name", $.identifier)),
            optional($._import_alias)
        ),

        _multiple_import: $ => seq(
            '{',
            commaSep($._import_declaration),
            '}'
        ),

        _import_declaration: $  => seq(
            field("import_name", $.identifier),
            optional($._import_alias)
        ),

        _import_alias: $ => seq("as", field("alias", $.identifier)),

        //  -- [ Declarations ] --
        _declaration: $ => choice(
            $.contract_declaration,
            $.interface_declaration,
            $.error_declaration,
            $.library_declaration,
            $.struct_declaration,
            $.enum_declaration,
            $.function_definition,
            $.constant_variable_declaration,
            $.user_defined_type_definition,
            $.event_definition,
            $.using_directive,
        ),

        user_defined_type_definition: $ => seq(
            'type',
            field("name", $.identifier),
            'is',
            $.primitive_type,
            $._semicolon
        ),

        constant_variable_declaration: $ => seq(
            field("type", $.type_name),
            "constant",
            field("name", $.identifier),
            '=',
            field("value", $.expression),
            $._semicolon
        ),

        // Contract Declarations
        contract_declaration: $ => seq(
            optional('abstract'),
            'contract',
            field("name", $.identifier),
            repeat(choice(
                $._class_heritage,
                $.layout_specifier,
            )),
            field('body', $.contract_body),
        ),

        error_declaration: $ => seq(
            'error',
            field("name", $.identifier),
            '(', commaSep($.error_parameter), ')',
            $._semicolon
        ),

        error_parameter: $ => seq(
            field("type", $.type_name),
            field("name", optional($.identifier)),
        ),

        interface_declaration: $ => seq(
            'interface',
            field("name", $.identifier),
            optional($._class_heritage),
            field('body', $.contract_body),
        ),

        library_declaration: $ => seq(
            'library',
            field("name", $.identifier),
            field('body', $.contract_body),
        ),

        _class_heritage: $ => seq(
            "is",
            commaSep1($.inheritance_specifier)
        ),

        layout_specifier: $ => seq(
            "layout",
            "at",
            $.expression,
        ),

        inheritance_specifier: $ => seq(
            field("ancestor", $.user_defined_type),
            optional(field("ancestor_arguments", $._call_arguments)),
        ),

        contract_body: $  => seq(
            "{",
            repeat($._contract_member),
            "}",
        ),

        _contract_member: $ => choice(
            $.function_definition,
            $.modifier_definition,
            $.error_declaration,
            $.state_variable_declaration,
            $.struct_declaration,
            $.enum_declaration,
            $.event_definition,
            $.using_directive,
            $.constructor_definition,
            $.fallback_receive_definition,
            $.user_defined_type_definition,
        ),

        struct_declaration: $ =>  seq(
            'struct',
            field("name", $.identifier),
            field('body', $.struct_body),
        ),

        struct_member: $ => seq(
            field("type", $.type_name),
            field("name", $.identifier),
            $._semicolon
        ),

        struct_body: $ => seq(
            '{',
            repeat1($.struct_member),
            '}',
        ),

        enum_declaration: $ =>  seq(
            'enum',
            field("name", $.identifier),
            field('body', $.enum_body),
        ),

        enum_body: $ => seq(
            '{',
            commaSep(alias($.identifier, $.enum_value)),
            '}',
        ),

        event_definition: $ => seq(
            'event',
            field('name', $.identifier),
            $._event_parameter_list ,
            optional('anonymous'),
            $._semicolon
        ),

        _event_parameter_list: $ => seq(
            "(",
            commaSep($.event_parameter),
            ")"
        ),

        event_parameter: $ => seq(
            field("type", $.type_name),
            optional("indexed"),
            optional(field("name", $.identifier)),
        ),

        user_definable_operator: $ => choice(
            "&",
            "~",
            "|",
            "^",
            "+",
            "-",
            "/",
            "%",
            "*",
            "-",
            "==",
            ">",
            ">=",
            "<",
            "<=",
            "!=",
        ),

        using_directive: $ => seq(
            'using',
            choice(
                alias($.user_defined_type, $.type_alias),
                seq('{', commaSep1($.using_alias), '}')
            ),
            'for',
            field("source", choice($.any_source_type, $.type_name)),
            optional('global'),
            $._semicolon
        ),

        using_alias: $ => seq(
            $.user_defined_type,
            optional(seq(
                "as",
                $.user_definable_operator,
            ))
        ),

        any_source_type: $ => '*',

        // -- [ Statements ] --
        statement: $ => choice(
            $.block_statement,
            $.expression_statement,
            $.variable_declaration_statement,
            $.if_statement,
            $.for_statement,
            $.while_statement,
            $.do_while_statement,
            $.continue_statement,
            $.break_statement,
            $.try_statement,
            $.return_statement,
            $.emit_statement,
            $.assembly_statement,
            $.revert_statement,
        ),

        assembly_statement: $ => seq(
            'assembly',
            optional('"evmasm"'),
	        optional($.assembly_flags),
            "{",
            repeat($._yul_statement),
            "}"
        ),

	    assembly_flags: $ => seq("(", commaSep($.string), ")"),

        // -- [ Yul ] --
        _yul_statement: $ => choice(
            $.yul_block,
            $.yul_variable_declaration,
            $.yul_assignment,
            $.yul_function_call,
            $.yul_if_statement,
            $.yul_for_statement,
            $.yul_switch_statement,
            $.yul_leave,
            $.yul_break,
            $.yul_continue,
            $.yul_function_definition,
            $.yul_label,
            $._yul_literal
        ),

        yul_label: $ => seq($.identifier, ":"),
        yul_leave: $ => "leave",
        yul_break: $ => "break",
        yul_continue: $ => "continue",

        yul_identifier: $ => $.identifier, ///[a-zA-Z$_]+/,
        _yul_expression: $ => choice($.yul_path, $.yul_function_call, $._yul_literal),
        yul_path: $ => prec.left(dotSep1($.yul_identifier)),

        // -- Yul Literals --
        _yul_literal: $ =>  choice(
            $.yul_decimal_number,
            $.yul_string_literal,
            $.yul_hex_number,
            $.yul_boolean,
            $.yul_hex_string_literal,
        ),
        yul_decimal_number: $ => /0|([1-9][0-9]*)/,
        yul_string_literal: $ => $.string,
        yul_hex_number: $ => /0x[0-9A-Fa-f]*/,
        yul_boolean: $ => choice('true', 'false'),
        yul_hex_string_literal: $ => seq(
            'hex',
            choice(
                seq('"', optional(optionalDashSeparation($._hex_digit)), '"'),
                seq("'", optional(optionalDashSeparation($._hex_digit)), "'"),
            )),

        // -- Yul Statements --
        yul_block: $ => seq('{', repeat($._yul_statement), '}'),
        yul_variable_declaration: $ => prec.left(PREC.DECLARATION, choice(
            seq('let', field("left", $.yul_identifier), optional(seq(':=', field("right", $._yul_expression)))),
            seq(
                'let', field("left", choice(
                    commaSep1($.yul_identifier),
                    seq('(', commaSep1($.yul_identifier), ')')
                )),
                optional(seq(':=', field("right", $.yul_function_call)))),
        )),
        _yul_assignment_operator: $ => choice(":=", seq(":", "=")),
        yul_assignment: $ => prec.left(PREC.ASSIGN, choice(
            seq($.yul_path, $._yul_assignment_operator, $._yul_expression),
            seq(commaSep1($.yul_path), optional(seq($._yul_assignment_operator, $.yul_function_call))),
        )),
        yul_function_call: $ => choice(
            seq(
                field("function", choice($.yul_identifier, $.yul_evm_builtin)), '(', commaSep($._yul_expression), ')'
            ),
            field("function", $.yul_evm_builtin)
        ),
        yul_if_statement: $ => seq('if', $._yul_expression, $.yul_block),
        yul_for_statement: $ => seq('for', $.yul_block, $._yul_expression, $.yul_block, $.yul_block),
        yul_switch_statement: $ => seq(
            'switch', $._yul_expression,
            choice(
                seq('default', $.yul_block),
                seq(
                    repeat1(seq('case', $._yul_literal, $.yul_block)),
                    optional(seq('default', $.yul_block)),
                )
            ),
        ),
        yul_function_definition: $ => seq(
            'function', $.yul_identifier, '(', commaSep($.yul_identifier), ')',
            optional(seq('->', commaSep1($.yul_identifier))),
            $.yul_block
        ),

        yul_evm_builtin: $ => prec(1, choice(
            'stop',
            'add',
            'sub',
            'mul',
            'div',
            'sdiv',
            'mod',
            'smod',
            'exp',
            'not',
            'lt',
            'gt',
            'slt',
            'sgt',
            'eq',
            'iszero',
            'and',
            'or',
            'xor',
            'byte',
            'shl',
            'shr',
            'sar',
            'addmod',
            'mulmod',
            'signextend',
            'keccak256',
            'pop',
            'mload',
            'mcopy',
            'tload',
            'tstore',
            'mstore',
            'mstore8',
            'sload',
            'sstore',
            'msize',
            'gas',
            'address',
            'balance',
            'selfbalance',
            'caller',
            'callvalue',
            'calldataload',
            'calldatasize',
            'calldatacopy',
            'extcodesize',
            'extcodecopy',
            'returndatasize',
            'returndatacopy',
            'extcodehash',
            'create',
            'create2',
            'call',
            'callcode',
            'delegatecall',
            'staticcall',
            'return',
            'revert',
            'selfdestruct',
            'invalid',
            'log0',
            'log1',
            'log2',
            'log3',
            'log4',
            'chainid',
            'origin',
            'gasprice',
            'blockhash',
            'blobhash',
            'basefee',
            'blobfee',
            'coinbase',
            'timestamp',
            'number',
            'difficulty',
            'gaslimit',
            'prevrandao',
            'blobbasefee',
            'blobfee',
        )),

        // -- [ Statements ] --
        unchecked: $ => "unchecked",
        block_statement: $ => seq(
            optional($.unchecked),
            '{',
            repeat($.statement),
            "}"
        ),
        variable_declaration_statement: $ => prec(1,seq(
                choice(
                    seq($.variable_declaration, optional(seq('=', field("value", $.expression)))),
                    seq($.variable_declaration_tuple, '=', field("value", $.expression)),
                ),
                $._semicolon
        )),

        variable_declaration: $ => seq(
            field("type", $.type_name),
            field("location", optional(choice('memory', 'storage', 'calldata'))),
            field('name', $.identifier)
        ),

        variable_declaration_tuple: $ => prec(3, choice(
            seq(
                '(',
                commaSep(optional($.variable_declaration)),
                ')'
            ),
            seq('var',
                '(',
                commaSep(optional($.identifier)),
                ')'
            )
        )),

        expression_statement: $ => seq($.expression, $._semicolon),

        if_statement: $ => prec.right(seq(
            'if', '(',
            field("condition", $.expression),
            ')',
            field("body", $.statement),
            field("else",
                optional(
                    seq(
                        'else',
                        field("body", $.statement)
                ))
            ),
        )),

        for_statement: $ => seq(
            'for', '(',
            field("initial", choice($.variable_declaration_statement, $.expression_statement, $._semicolon)),
            field("condition", choice($.expression_statement, $._semicolon)),
            field("update", optional($.expression)),
            ')', field("body", $.statement),
        ),

        while_statement: $ => seq(
            'while', '(',field("condition", $.expression), ')', field("body", $.statement),
        ),
        do_while_statement: $ => seq(
            'do', field("body", $.statement), 'while', '(', field("condition", $.expression), ')', $._semicolon,
        ),
        continue_statement: $ => seq('continue', $._semicolon),
        break_statement: $ => seq('break', $._semicolon),

        revert_statement: $ => prec(PREC.REVERT, seq(
            'revert',
            optional(field("error", $.expression)),
            optional(alias($._call_arguments, $.revert_arguments)),
            $._semicolon
        )),

        try_statement: $ => seq(
            'try',
            field("attempt", $.expression),
            optional(seq('returns', $._parameter_list)),
            field("body", $.block_statement),
            repeat1($.catch_clause),
        ),

        catch_clause: $ => seq(
            'catch',
            optional(seq(optional($.identifier), $._parameter_list)),
            field("body", $.block_statement),
        ),

        return_statement: $ => seq(
            'return',
            optional($.expression),
            $._semicolon
        ),

        emit_statement: $ => seq(
            'emit',
            field("name", $.expression),
            $._call_arguments,
            $._semicolon
        ),


        //  -- [ Definitions ] --

        // Definitions
        state_variable_declaration: $ => seq(
            field("type", $.type_name),
            repeat(choice(
                field('visibility', $.visibility), // FIXME: this also allows external
                "constant",
                $.override_specifier,
                $.immutable,
                field('location', $.state_location)
            )),
            field("name", $.identifier),
            optional(seq(
                '=', field("value", $.expression)
            )),
            $._semicolon
        ),
         visibility: $ => choice(
            'public',
            'internal',
            'private',
            'external',
        ),

        state_mutability: $ => choice(
            'pure',
            'view',
            'payable'
        ),

        state_location: $ => choice(
          "transient"
        ),

        immutable: $ => 'immutable',

        override_specifier: $ => seq(
            'override',
            optional(seq(
                '(',
                commaSep1($.user_defined_type),
                ')',
            ))
        ),

        modifier_definition: $ => seq(
            "modifier",
            field("name", $.identifier),
            optional($._parameter_list),
            repeat(choice(
                $.virtual,
                $.override_specifier,
            )),
            choice($._semicolon, field("body", $.function_body)),
        ),

        constructor_definition: $ => seq(
            'constructor',
            $._parameter_list,
            repeat(choice(
                $.modifier_invocation,
                'payable',
                choice('internal', 'public'),
            )),
            field('body', $.function_body),
        ),

        fallback_receive_definition: $ => seq(
            choice(seq(
                // optional("function"),
                choice('fallback', 'receive', 'function'),
                ),
                "function"
            ),
            // #todo: only fallback should get arguments
            $._parameter_list,
            // FIXME: We use repeat to allow for unorderedness. However, this means that the parser
            // accepts more than just the solidity language. The same problem exists for other definition rules.
            repeat(choice(
                $.visibility,
                $.modifier_invocation,
                $.state_mutability,
                $.virtual,
                $.override_specifier,
            )),
            optional(seq(
                'returns',
                $._parameter_list,
            )),
            choice($._semicolon, field('body', $.function_body))
        ),

        function_definition: $ => seq(
            "function",
            field("name", $.identifier),
            $._parameter_list,
            repeat(choice(
                $.modifier_invocation,
                $.visibility,
                $.state_mutability,
                $.virtual,
                $.override_specifier,
            )),
            field("return_type", optional($.return_type_definition)),
            choice($._semicolon, field('body', $.function_body))
        ),

        return_type_definition: $ => seq(
            'returns',
            $._parameter_list,
        ),

        virtual: $ => "virtual",
        modifier_invocation: $ => seq($._identifier_path, optional($._call_arguments)),

        _call_arguments: $ => prec(4,
            seq(
                '(',
                commaSep($.call_argument),
                ')'
            ),
        ),

        call_argument: $ => choice(
            $.expression,
            seq("{", commaSep($.call_struct_argument), "}"),
        ),
        call_struct_argument: $ => seq(
            field("name", $.identifier),
            ":",
            field("value", $.expression)
        ),

        function_body: $ => seq(
            "{",
                repeat($.statement),
            "}",
        ),

        // Expressions
        expression: $ => choice(
            $.binary_expression,
            $.unary_expression,
            $.update_expression,
            $.call_expression,
            // TODO: $.function_call_options_expression,
            $.payable_conversion_expression,
            $.meta_type_expression,
            $._primary_expression,
            $.struct_expression,
            $.ternary_expression,
            $.type_cast_expression,
        ),

        _primary_expression: $ => choice(
            $.parenthesized_expression,
            $.member_expression,
            $.array_access,
            $.slice_access,
            $.primitive_type,
            $.assignment_expression,
            $.augmented_assignment_expression,
            $.user_defined_type,
            $.tuple_expression,
            $.inline_array_expression,
            $.identifier,
            $._literal,
            $.new_expression,
        ),

        // TODO: back this up with official documentation
        type_cast_expression: $ => prec.left(seq($.primitive_type,  $._call_arguments)),

        ternary_expression: $ => prec.left(seq($.expression, "?", $.expression, ':', $.expression)),

        // TODO: make sure call arguments are part of solidity
        new_expression: $ => prec.left(seq('new', field("name", $.type_name), optional($._call_arguments))),

        tuple_expression: $ => prec(1, seq('(', commaSep(optional($.expression)), ')' )),

        inline_array_expression: $ => seq('[', commaSep($.expression), ']' ),

        binary_expression: $ => choice(
            ...[
            // [13] Logical OR
            ['||', PREC.LOGICAL_OR],
            // [12] Logical AND
            ['&&', PREC.LOGICAL_AND],
            // [11] Equality operators
            ['==', PREC.REL_EQ],
            ['!=', PREC.REL_EQ],
            // [10] Inequality operators
            ['<', PREC.REL_INEQ],
            ['>', PREC.REL_INEQ],
            ['<=', PREC.REL_INEQ],
            ['>=', PREC.REL_INEQ],
            // [9] Bitwise OR
            ['|', PREC.BITWISE_OR],
            // [8] Bitwise XOR
            ['^', PREC.BITWISE_XOR],
            // [7] Bitwise AND
            ['&', PREC.BITWISE_AND],
            // [6] Bitwise shift operators
            ['<<', PREC.BITWISE_SHIFT],
            ['>>', PREC.BITWISE_SHIFT],
            // [5] Addition and subtraction
            ['+', PREC.PLUS],
            ['-', PREC.PLUS],
            // [4] Multiplication, division and modulo
            ['*', PREC.TIMES],
            ['/', PREC.TIMES],
            ['%', PREC.TIMES],
            // [3] Exponentiation
            ['**', PREC.EXP],
            ].map(([operator, precedence]) =>
                prec.left(precedence, seq(
                    field('left', $.expression),
                    field('operator', operator),
                    field('right', $.expression)
                ))
            )
        ),

        unary_expression: $ => choice(...[
                // [2] Unary minus
                ['-', PREC.PREFIX_UNARY],
                // [2] Unary operations
                ['delete', PREC.PREFIX_UNARY],
                // [2] Bitwise NOT
                ['!', PREC.PREFIX_UNARY],
                // [2] Bitwise NOT
                ['~', PREC.PREFIX_UNARY],
            ].map(([operator, precedence]) =>
                prec.left(precedence, seq(
                    field('operator', operator),
                    field('argument', $.expression)
                ))
        )),

        update_expression: $ => choice(
            prec.left(PREC.POSTFIX_UNARY, seq(
                field('argument', $.expression),
                field('operator', choice('++', '--'))
            )),
            prec.left(PREC.PREFIX_UNARY, seq(
                field('operator', choice('++', '--')),
                field('argument', $.expression)
            )),
        ),

        member_expression: $ => prec.dynamic(1, seq(
            field('object', choice(
                $.expression,
                $.identifier,
            )),
            '.',
            field('property', $.identifier)
        )),

        array_access: $ => seq(
            field('base', $.expression),
            '[',
            optional(field('index', $.expression)),
            ']'
        ),

        slice_access: $ => seq(
            field('base', $.expression),
            '[',
            optional(field('from', $.expression)),
            ':',
            optional(field('to', $.expression)),
            ']'
        ),

        struct_expression: $ => seq(
            field("type", $.expression),
            "{",
            commaSep($.struct_field_assignment),
            "}"
        ),

        struct_field_assignment: $ => seq(
            field("name", $.identifier),
            ":",
            field("value", $.expression),
        ),

        parenthesized_expression: $ => prec(2, seq('(', $.expression, ')')),

        assignment_expression: $ => prec.right(PREC.ASSIGN, seq(
            field('left', $.expression),
            '=',
            field('right', $.expression)
        )),

        augmented_assignment_expression: $ => prec.right(PREC.ASSIGN, seq(
            field('left', $.expression),
            choice('+=', '-=', '*=', '/=', '%=', '^=', '&=', '|=', '>>=', '<<=',),
            field('right', $.expression)
        )),

        call_expression: $ => prec.right(PREC.CALL, seq(
            field("function", $.expression),
            $._call_arguments
        )),

        payable_conversion_expression: $ => seq('payable', $._call_arguments),
        meta_type_expression: $ => seq('type', '(', $.type_name, ')'),

        type_name: $ => choice(
            $.primitive_type,
            $.user_defined_type,
            $._mapping,
            $._array_type,
            $._function_type,
        ),

        _array_type: $ => prec(1, seq($.type_name, '[', optional($.expression), ']')),

        _function_type: $ => prec.right(seq(
            'function',
            field("parameters", $._parameter_list),
            repeat(choice(
                $.visibility,
                $.state_mutability,
            )),
            optional($._return_parameters),
        )),

        _parameter_list: $ => seq(
            '(', commaSep($.parameter), ')'
        ),

        _return_parameters: $ => seq(
            'returns', '(', commaSep1(alias($._nameless_parameter, $.return_parameter)), ')'
        ),

        _nameless_parameter: $ =>  seq(
            field("type", $.type_name),
            field("location", optional($._storage_location)),
        ),

        parameter: $ =>  seq(
            field("type", $.type_name),
            optional(field("location", $._storage_location)),
            optional(field("name", $.identifier)),
        ),

        _storage_location: $ => choice(
            'memory',
            'storage',
            'calldata'
        ),

        user_defined_type: $ => $._identifier_path,

        _identifier_path: $ => prec.left(dotSep1( $.identifier)),

        _mapping: $ => seq(
            'mapping', '(',
            field("key_type", $._mapping_key),
            optional(field('key_identifier', $.identifier)),
            '=>',
            field("value_type", $.type_name),
            optional(field('value_identifier', $.identifier)),
            ')',
        ),

        _mapping_key: $ => choice(
            $.primitive_type,
            $.user_defined_type
        ),

        primitive_type: $ => prec.left(choice(
            seq('address', optional('payable')),
            'bool',
            'string',
            'var',
            $._int,
            $._uint,
            $._bytes,
            $._fixed,
            $._ufixed,
        )),

        _int: $ => choice (
            'int', 'int8', 'int16', 'int24', 'int32', 'int40', 'int48', 'int56', 'int64', 'int72', 'int80', 'int88', 'int96', 'int104', 'int112', 'int120', 'int128', 'int136', 'int144', 'int152', 'int160', 'int168', 'int176', 'int184', 'int192', 'int200', 'int208', 'int216', 'int224', 'int232', 'int240', 'int248', 'int256'
        ),
        _uint: $ => choice (
            'uint', 'uint8', 'uint16', 'uint24', 'uint32', 'uint40', 'uint48', 'uint56', 'uint64', 'uint72', 'uint80', 'uint88', 'uint96', 'uint104', 'uint112', 'uint120', 'uint128', 'uint136', 'uint144', 'uint152', 'uint160', 'uint168', 'uint176', 'uint184', 'uint192', 'uint200', 'uint208', 'uint216', 'uint224', 'uint232', 'uint240', 'uint248', 'uint256'
        ),
        _bytes: $ => choice (
            'byte', 'bytes', 'bytes1', 'bytes2', 'bytes3', 'bytes4', 'bytes5', 'bytes6', 'bytes7', 'bytes8', 'bytes9', 'bytes10', 'bytes11', 'bytes12', 'bytes13', 'bytes14', 'bytes15', 'bytes16', 'bytes17', 'bytes18', 'bytes19', 'bytes20', 'bytes21', 'bytes22', 'bytes23', 'bytes24', 'bytes25', 'bytes26', 'bytes27', 'bytes28', 'bytes29', 'bytes30', 'bytes31', 'bytes32'
        ),

        _fixed: $ => choice (
            'fixed',
            /fixed([0-9]+)x([0-9]+)/
        ),
        _ufixed: $ => choice (
            'ufixed',
            /ufixed([0-9]+)x([0-9]+)/
        ),

        _semicolon: $ => ';',

        identifier: $ => /[a-zA-Z$_][a-zA-Z0-9$_]*/,

        number: $ => /\d+/,

        _literal: $ => choice(
            $.string_literal,
            $.number_literal,
            $.boolean_literal,
            $.hex_string_literal,
            $.unicode_string_literal,
        ),

        string_literal: $ => prec.left(repeat1($.string)),
        number_literal: $ => seq(choice($._decimal_number, $._hex_number), optional($.number_unit)),
        _decimal_number: $ =>  choice(
            /(\d|_)+(\.(\d|_)+)?([eE](-)?(\d|_)+)?/,
            /\.(\d|_)+([eE](-)?(\d|_)+)?/,
        ),
        _hex_number: $ => prec(10, /0[xX]([a-fA-F0-9][a-fA-F0-9]?_?)+/),
        // _hex_number: $ => seq(/0[xX]/, optional(optionalDashSeparation($._hex_digit))),
        _hex_digit: $ => /([a-fA-F0-9][a-fA-F0-9])/,
        number_unit: $ => choice(
            'wei','szabo', 'finney', 'gwei', 'ether', 'seconds', 'minutes', 'hours', 'days', 'weeks', 'years'
        ),
        true: $ => "true",
        false: $ => "false",
        boolean_literal: $ => choice($.true, $.false),

        hex_string_literal: $ => prec.left(repeat1(seq(
            'hex',
            choice(
                seq('"', optional(optionalDashSeparation($._hex_digit)), '"'),
                seq("'", optional(optionalDashSeparation($._hex_digit)), "'"),
            )))),

        _escape_sequence: $ => token.immediate(seq(
            '\\',
            choice(
              /[^xu0-7]/,
              /[0-7]{1,3}/,
              /x[0-9a-fA-F]{2}/,
              /u[0-9a-fA-F]{4}/,
              /u\{[0-9a-fA-F]+\}/
            )
        )),
        _single_quoted_unicode_char: $ =>
            token.immediate(prec(PREC.STRING, /[^'\\\n]+|\\\r?\n/)),
        _double_quoted_unicode_char: $ =>
            token.immediate(prec(PREC.STRING, /[^"\\\n]+|\\\r?\n/)),
        unicode_string_literal: $ => prec.left(repeat1(seq(
            'unicode',
            choice(
                seq('"', repeat($._double_quoted_unicode_char), '"'),
                seq("'", repeat($._single_quoted_unicode_char), "'"),
        )))),

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
	    // We need to name those elts for ocaml-tree-sitter-semgrep.
        _string_immediate_elt_inside_double_quote: $ =>
            token.immediate(prec(PREC.STRING, /[^"\\\n]+|\\\r?\n/)),
        _string_immediate_elt_inside_quote: $ =>
            token.immediate(prec(PREC.STRING, /[^'\\\n]+|\\\r?\n/)),



        // Based on: https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js#L965
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
  }
);

function dotSep1(rule) {
    return seq(
        rule,
        repeat(
            seq(
                '.',
                rule
            )
        ),
    );
}

function dotSep(rule) {
    return optional(dotSep1(rule))
}

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

function optionalDashSeparation(rule) {
    return seq(
        rule,
        repeat(
            seq(
                optional('_'),
                rule
            )
        ),
    );
}
