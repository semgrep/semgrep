/// <reference types="tree-sitter-cli/dsl" />

// Using the informal draft spec to support the newest features of dart
// https://spec.dart.dev/DartLangSpecDraft.pdf

const DIGITS = token(sep1(/[0-9]+/, /_+/))
const HEX_DIGITS = token(sep1(/[A-Fa-f0-9]+/, '_'))

//Everything above RelationalTypeCast was incremented from its original value
//This was to get type casting issues finally fixed.

const DART_PREC = {
    IMPORT_EXPORT: 19,
    TYPE_IDENTIFIER: 18, //was: 17
    DOT_IDENTIFIER: 19, //was: 18
    UNARY_POSTFIX: 17,
    UNARY_PREFIX: 16,
    Multiplicative: 15, // *, /, ˜/, % Left
    Additive: 14, // +, - Left
    Shift: 13, // <<, >>, >>> Left
    TYPE_ARGUMENTS: 13,
    Bitwise_AND: 12, // & Left
    Bitwise_XOR: 11, // ˆ Left
    Bitwise_Or: 10, // | Left
    RelationalTypeCast: 9, // <, >, <=, >=, as, is, is! None 8
    RelationalTypeTest: 9,
    Relational: 8, // <, >, <=, >=, as, is, is! None 8
    Equality: 7, // ==, != None 7
    Logical_AND: 6, // AND && Left
    Logical_OR: 5, // Or || Left
    If: 4, //-null ?? Left
    Conditional: 3, // e1?e2:e3 Right 3
    Cascade: 2, // .. Left
    Assignment: 1, // =, *=, /=, +=, -=, &=, ˆ=, etc. Right
    BUILTIN: 0,
    TRY: 0,
    // Added by Ben for experimentation.
    SELECTOR_IN_PRIMARY: 1,
    SELECTOR_IN_ASSIGNMENT: 0,
    TYPE_ARGS: 1
};

// TODO: general things to add
// both string types
//get protocols in classes?
// todo: type test operators: as, is, and is!
//todo: assignment operators: ??=, and ~/=
//todo: ?? operator
// todo: cascade notation: dot dot accesses each object
//todo: conditional member access: blah?.foo
//todo: rethrow keyword
//todo: override operator notations
//todo: correct import statements to be strings
//todo: sync* and async* functions, plus yields

//DONE: override shorter constructor notations?


module.exports = grammar({
    name: 'dart',

    externals: $ => [
        $._template_chars_double,
        $._template_chars_single,
        $._template_chars_double_single,
        $._template_chars_single_single,
        $._template_chars_raw_slash,
        $._block_comment,
        $._documentation_block_comment,
    ],

    extras: $ => [
        $.comment,
        $.documentation_comment,
        /\s/
    ],

    supertypes: $ => [
        // $._expression,
        $._declaration,
        $._statement,
        $._literal,
        // $._element
        // $._primary_pattern
        // $._primary,
        // $._type,
    ],

    inline: $ => [
        $._ambiguous_name,
        $._class_member_definition,
        $._if_null_expression,
    ],

    conflicts: $ => [
        // `@override (X, Y) foo() { ... }` — the parser needs to keep
        // both stacks alive (annotation-with-args vs bare-annotation +
        // record-return method) until it sees what comes after `)`.
        [$.annotation, $._bare_annotation],
        [$._record_literal_no_const, $.record_field],
        [$._record_literal_no_const, $.record_type],
        [$._record_literal_no_const, $._strict_formal_parameter_list],
        [$.getter_signature, $.function_signature, $._var_or_type],
        [$.setter_signature, $.function_signature, $._var_or_type],
        [$.operator_signature, $._var_or_type],
        [$._primary, $._type_not_void_not_function, $._function_type_tail],
        [$._primary, $._function_type_tail],
        [$.block, $.set_or_map_literal],
        [$._type_name, $._primary, $.function_signature],
        [$._primary, $.function_signature],
        [$._primary, $._type_name],
        [$._primary, $._simple_formal_parameter],
        [$._primary, $._type_name, $._function_formal_parameter],
        [$._primary, $.constructor_param],
        [$._normal_formal_parameters],
        [$._declared_identifier],
        [$.equality_expression],
        [$.record_type_field, $._function_formal_parameter, $._var_or_type],
        [$.typed_identifier, $._var_or_type, $._function_formal_parameter],
        [$._type_name, $._simple_formal_parameter],
        [$._type_not_function, $._type_not_void],
        [$.switch_statement_case],
        // [$._argument_list],
        [$.declaration, $._external_and_static],
        [$.constructor_signature, $._formal_parameter_part],
        // [$._type_not_function, $._type_not_void],
        [$._cascade_subsection],
        [$._expression],
        // [$._real_expression, $._below_relational_expression],
        [$._postfix_expression],
        [$.pattern_variable_declaration, $._var_or_type],
        [$._final_const_var_or_type, $.pattern_variable_declaration],
        [$.type_arguments, $.relational_operator],
        [$.prefix_operator, $.constant_pattern],
        [$._primary, $.constant_pattern, $._type_name],
        [$._literal, $.constant_pattern],
        [$._primary, $.constant_pattern],
        [$._final_var_or_type],
        [$._primary, $.constant_pattern, $._type_name, $._simple_formal_parameter],
        [$._parenthesized_pattern, $._pattern_field],
        [$.record_type_field, $._var_or_type, $._final_var_or_type, $._function_formal_parameter],
        [$._var_or_type, $._final_var_or_type],
        [$._final_const_var_or_type, $._final_var_or_type],
        [$._var_or_type, $._for_loop_parts, $.pattern_variable_declaration],
        [$.pattern_variable_declaration, $._for_loop_parts, $._final_const_var_or_type],
        [$._var_or_type, $._final_var_or_type, $._function_formal_parameter],
        [$.set_or_map_literal, $.map_pattern],
        [$.list_literal, $.list_pattern],
        [$.constant_pattern, $._type_name],
        [$._pattern_field, $.label],
        [$.constructor_tearoff, $._identifier_or_new],
        [$._primary, $.constant_pattern, $._simple_formal_parameter],
        [$.record_type_field, $._final_var_or_type],
        [$.set_or_map_literal, $.constant_pattern],
        [$.list_literal, $.constant_pattern],
        [$._var_or_type, $.function_signature],
        [$._var_or_type, $._function_formal_parameter],
        [$.relational_operator, $.type_arguments, $.type_parameters],
        [$._var_or_type],
        [$._final_const_var_or_type, $.const_object_expression],
        [$._final_const_var_or_type],
        [$.type_parameter, $._type_name],
        [$._normal_formal_parameter],
        [$._assignable_selector_part, $.selector],
        [$._assignable_selector_part, $._postfix_expression],
        [$._primary, $.assignable_expression],
        [$._simple_formal_parameter, $.assignable_expression],
        // [$._type_name, $._primary, $.assignable_expression],
        [$.assignable_expression, $._postfix_expression],
        // [$._type_name, $.assignable_expression],
        // [$._type_name, $.function_signature],
        [$._type_name, $._function_formal_parameter],
        [$._type_name],
        // [$.assignment_expression, $._expression],
        [$._primary, $._type_name, $.assignable_expression],
        [$._type_name, $.function_signature],
        // [$.relational_operator, $._shift_operator],
        [$.declaration, $._external],
        [$.relational_expression],
        [$._function_type_tail],
        [$._type_not_void_not_function, $._function_type_tail],
        [$._type_not_void],
        [$._type_not_void_not_function],
        [$.super_formal_parameter, $.unconditional_assignable_selector],
    ],

    word: $ => $.identifier,

    rules: {

        // Page 188 libraryDeclaration
        program: $ => seq(
            optional($.script_tag),
            optional($.library_name),
            repeat($.import_or_export),
            repeat($.part_directive),
            repeat($.part_of_directive),
            repeat($._top_level_definition),
        ),

        // Page 187 topLevelDefinition
        _top_level_definition: $ => choice(
            $.class_definition,
            $.mixin_declaration,
            $.extension_declaration,
            $.extension_type_declaration,
            $.enum_declaration,
            $.type_alias,
            seq(
                optional($._metadata),
                optional($._external_builtin),
                $.function_signature,
                $._semicolon
            ),
            seq(
                optional($._metadata),
                optional($._external_builtin),
                $.getter_signature,
                $._semicolon
            ),
            seq(
                optional($._metadata),
                optional($._external_builtin),
                $.setter_signature,
                $._semicolon
            ),
            seq(
                optional($._metadata),
                $.getter_signature,
                $.function_body
            ),
            seq(
                optional($._metadata),
                $.setter_signature,
                $.function_body
            ),
            seq(
                optional($._metadata),
                $.function_signature,
                $.function_body
            ),
            // Top-level function with a record return type preceded by
            // bare annotations. See `_record_return_class_member` for the
            // rationale; this is the top-level analogue. Negative dynamic
            // precedence so the standard annotation-with-args path wins
            // when both are valid.
            prec.dynamic(-10, seq(
                repeat1(alias($._bare_annotation, $.annotation)),
                alias(
                    $._record_return_function_signature,
                    $.function_signature
                ),
                $.function_body
            )),
            //    final or const static final declaration list
            seq(
                optional($._metadata),
                choice(
                    $.final_builtin,
                    $.const_builtin
                ),
                optional($._type),
                $.static_final_declaration_list,
                $._semicolon
            ),
            seq(
                optional($._metadata),
                $._late_builtin,
                $.final_builtin,
                optional($._type),
                $.initialized_identifier_list,
                $._semicolon
            ),
            seq(
                optional($._metadata),
                optional($._late_builtin),
                choice($._type, $.inferred_type),
                $.initialized_identifier_list,
                $._semicolon
            ),
            // Top-level external variable declaration (Dart 3 / static
            // interop, e.g. `@JS() external _DdLogs? DD_LOGS;`).
            seq(
                optional($._metadata),
                $._external_builtin,
                choice(
                    seq($.final_builtin, optional($._type), $.identifier_list),
                    seq(optional($._late_builtin), $._var_or_type, $.identifier_list)
                ),
                $._semicolon
            )
        ),

        /**************************************************************************************************
        *********************************Literals**********************************************************
        ***************************************************************************************************
        ****These are the Literals from section 16.4-9 (Page 84-110) of the dart specification*************
        ***************************************************************************************************
        ***************************************************************************************************/

        _bool_literal: $ => choice($.true, $.false),

        _numeric_literal: $ => choice(
            $.decimal_integer_literal,
            $.decimal_floating_point_literal,
            $.hex_integer_literal,
        ),

        _literal: $ => choice(
            $.null_literal,
            $._bool_literal,
            $._numeric_literal,
            $.string_literal,
            $.symbol_literal,
            $.set_or_map_literal,
            $.list_literal,
            $.record_literal,
        ),

        /****This is the symbol literals from section 16.8 (Page 99) of the dart specification****************/
        // Dart symbols accept a dotted identifier list (e.g. `#foo.bar.baz`)
        // or a single operator (e.g. `#+`, `#==`). See Dart language spec
        // section 'Symbols'.
        symbol_literal: $ => prec.right(seq(
            '#',
            choice(
                sep1($.identifier, '.'),
                $.equality_operator,
                $.relational_operator,
                $.shift_operator,
                $.additive_operator,
                $.multiplicative_operator,
                '~',
                '|',
                '&',
                '^',
                '[]',
                '[]=',
            ),
        )),

        /**************************************************************************************************
        *********************************Numeric Literals**************************************************
        ***************************************************************************************************
        ****These are the Numeric Literals from section 16.5 (Page 84-85) of the dart specification********
        ***************************************************************************************************
        ***************************************************************************************************/

        decimal_integer_literal: $ => token(DIGITS),

        hex_integer_literal: $ => token(seq(
            choice('0x', '0X'),
            HEX_DIGITS
        )),

        decimal_floating_point_literal: $ => token(choice(
            seq(DIGITS, '.', DIGITS, optional(seq((/[eE]/), optional(choice('-', '+')), DIGITS))),
            seq('.', DIGITS, optional(seq((/[eE]/), optional(choice('-', '+')), DIGITS))),
            seq(DIGITS, /[eE]/, optional(choice('-', '+')), DIGITS),
            seq(DIGITS, optional(seq((/[eE]/), optional(choice('-', '+')), DIGITS)))
        )),

        /**************************************************************************************************
        *********************************Boolean Literals**************************************************
        ***************************************************************************************************
        ****These are the boolean from section 16.6 (Page 86) of the dart specification********************
        ***************************************************************************************************
        ***************************************************************************************************/
        true: $ => prec(
            DART_PREC.BUILTIN,
            'true',
        ),

        false: $ => prec(
            DART_PREC.BUILTIN,
            'false',
        ),

        /**************************************************************************************************
        *********************************String Parts******************************************************
        ***************************************************************************************************
        ****These are the parts of String from section 16.7 (Page 86-92) of the dart specification*********
        ***************************************************************************************************
        ***************************************************************************************************/
        string_literal: $ => repeat1(
            choice(
                $._string_literal_double_quotes,
                $._string_literal_single_quotes,
                $._string_literal_double_quotes_multiple,
                $._string_literal_single_quotes_multiple,
                //raw, separate later
                $._raw_string_literal_double_quotes,
                $._raw_string_literal_single_quotes,
                $._raw_string_literal_double_quotes_multiple,
                $._raw_string_literal_single_quotes_multiple,
            ),
        ),
        _string_literal_double_quotes: $ => seq(
            '"',
            repeat(
                choice(
                    $._template_chars_double_single,
                    '\'',
                    $.escape_sequence,
                    $._sub_string_test,
                    $.template_substitution
                )
            ),
            '"'
        ),
        _string_literal_single_quotes: $ => seq(
            '\'',
            repeat(choice(
                $._template_chars_single_single,
                '"',
                $.escape_sequence,
                $._sub_string_test,
                $.template_substitution
            )),
            '\''
        ),
        _string_literal_double_quotes_multiple: $ => prec.left(
            seq(
                '"""',
                repeat(choice(
                    $._template_chars_double,
                    '\'',
                    '\"',
                    $.escape_sequence,
                    $._sub_string_test,
                    $.template_substitution
                )),
                '"""'
            ),
        ),
        _string_literal_single_quotes_multiple: $ => prec.left(
            seq(
                '\'\'\'',
                repeat(choice(
                    $._template_chars_single,
                    '"',
                    '\'',
                    $.escape_sequence,
                    $._sub_string_test,
                    $.template_substitution
                )),
                '\'\'\''
            ),
        ),
        _raw_string_literal_double_quotes: $ => seq(
            'r"',
            repeat(choice(
                $._template_chars_double_single,
                // /[^\n"]*/,
                '\'',
                $._template_chars_raw_slash,
                // '\\',
                $._unused_escape_sequence,
                $._sub_string_test,
                '$'
            )),
            '"'
        ),
        _raw_string_literal_single_quotes: $ => seq(
            'r\'',
            repeat(choice(
                $._template_chars_single_single,
                // /[^\n']/,
                '"',
                $._template_chars_raw_slash,
                // '\\',
                $._unused_escape_sequence,
                $._sub_string_test,
                '$'
            )),
            '\''
        ),
        _raw_string_literal_double_quotes_multiple: $ => prec.left(
            seq(
                'r"""',
                // $._triple_double_quote_end,
                repeat(choice(
                    $._template_chars_double,
                    '\'',
                    // '\\',
                    $._template_chars_raw_slash,
                    '"',
                    $._unused_escape_sequence,
                    $._sub_string_test,
                    '$'
                )),
                '"""'
                // $._triple_double_quote_end
            ),
        ),
        _raw_string_literal_single_quotes_multiple: $ => prec.left(
            seq(
                'r\'\'\'',
                // $._triple_quote_end,
                repeat(choice(
                    $._template_chars_single,
                    '"',
                    '\'',
                    // '\\',
                    $._template_chars_raw_slash,
                    $._unused_escape_sequence,
                    $._sub_string_test,
                    '$'
                )),
                '\'\'\''
                // $._triple_quote_end
            ),
        ),
        _triple_quote_end: $ => token('\'\'\''),
        _triple_double_quote_end: $ => token('"""'),
        template_substitution: $ => seq(
            '$',
            choice(
                seq('{',
                    $._expression,
                    '}'),
                $.identifier_dollar_escaped
            )
        ),
        _sub_string_test: $ => seq('$', /[^a-zA-Z_{]/),
        _string_interp: $ => /\$((\w+)|\{([^{}]+)\})/, // represents $word or ${word} for now
        _unused_escape_sequence: $ => token.immediate(seq(
            '\\',
            choice(
                /[^xu0-7]/,
                /[0-7]{1,3}/,
                /x[0-9a-fA-F]{2}/,
                /u[0-9a-fA-F]{4}/,
                /u\{[0-9a-fA-F]+\}/
            )
        )),
        escape_sequence: $ => $._unused_escape_sequence,


        /**************************************************************************************************
        *********************************Collection Literals***********************************************
        ***************************************************************************************************
        ****These are the collection literals from section 16.9 (Page 92-108) of the dart specification****
        ***************************************************************************************************
        ***************************************************************************************************/
        list_literal: $ => seq(
            optional($.const_builtin), optional($.type_arguments), '[',
            commaSepTrailingComma($._element),
            ']'
        ),
        set_or_map_literal: $ => seq(
            optional($.const_builtin), optional($.type_arguments), '{',
            commaSepTrailingComma(
                $._element
            ),
            '}'
        ),

        // Per Dart 3 (null-aware elements), a map literal entry may use `?`
        // before the key, the value, or both, e.g. `?key: value`,
        // `key: ?value`, `?key: ?value`. Each `?` causes the entry to be
        // omitted when the corresponding sub-expression is `null`.
        pair: $ => seq(
            field('key', seq(optional('?'), $._expression)),
            ':',
            field('value', seq(optional('?'), $._expression))
        ),
        // pair_or_element: $ => seq(
        //     field('key', $._expression),
        //     optional(
        //         seq(
        //             ':',
        //             field('value', $._expression)
        //         )
        //     )
        // ),

        _element: $ => choice(
            seq(optional('?'), $._expression),
            $.pair,
            $.spread_element,
            $.if_element,
            $.for_element
        ),

        /****This is the null literal from section 16.4 (Page 84) of the dart specification****/
        null_literal: $ => prec(
            DART_PREC.BUILTIN,
            'null',
        ),

        /// Record literal (from Dart.g)
        record_literal: $ => seq(
            optional($.const_builtin),
            $._record_literal_no_const,
        ),

        // Per Dart spec (Records, recordLiteral): a record literal may be
        // the empty record `()` (zero positional, zero named fields), a
        // single-named-field record `(name: e)`, a single-positional record
        // requiring a trailing comma `(e,)`, or two-or-more comma-separated
        // record fields. Disambiguates against parenthesized_expression which
        // matches `(e)` (no trailing comma, exactly one expression). The empty
        // form gets a negative dynamic precedence so the parser prefers the
        // existing `arguments`/type-arguments path (`f<T>()`) when ambiguous.
        _record_literal_no_const: $ => choice(
            prec.dynamic(-1, seq('(', ')')),
            seq(
                '(',
                choice(
                    // `(label: expr,)` — single named field with mandatory
                    // trailing comma to disambiguate from a labeled paren-expr.
                    seq($.label, $._expression, ','),
                    seq($.label, $._expression),
                    seq($._expression, ','),
                    commaSep2TrailingComma($.record_field),
                ),
                ')'
            ),
        ),

        record_field: $ => seq(optional($.label), $._expression),

        /**************************************************************************************************
        *********************************Expressions*******************************************************
        ***************************************************************************************************
        ****These are the expressions from section 16.9 (Page 110-166) of the dart specification***********
        ***************************************************************************************************
        ***************************************************************************************************/
        _expression: $ => choice(
            $.pattern_assignment,
            $.assignment_expression,
            $.throw_expression,
            $.rethrow_expression,
            seq(
                $._real_expression,
                repeat($.cascade_section)
            )
        ),
        _expression_without_cascade: $ => choice(
            $.assignment_expression_without_cascade,
            $._real_expression,
            $.throw_expression_without_cascade
        ),
        _real_expression: $ => choice(
            $.conditional_expression,
            $.logical_or_expression,
            $.if_null_expression,
            $.additive_expression,
            $.multiplicative_expression,
            $.relational_expression,
            $.equality_expression,
            $.logical_and_expression,
            $.bitwise_and_expression,
            $.bitwise_or_expression,
            $.bitwise_xor_expression,
            $.shift_expression,
            $.type_cast_expression,
            $.type_test_expression,
            $._unary_expression
        ),

        // _below_relational_expression: $ => choice(
        //     // UNARY_POSTFIX: 16,
        //     // UNARY_PREFIX: 15,
        //     // Multiplicative: 14, // *, /, ˜/, % Left
        //     // Additive: 13, // +, - Left
        //     // Shift: 12, // <<, >>, >>> Left
        //     // Bitwise_AND: 11, // & Left
        //     // Bitwise_XOR: 10, // ˆ Left
        //     // Bitwise_Or: 9 , // | Left
        //     // $.type_cast_expression,
        //     $._unary_expression,
        //     $.multiplicative_expression,
        //     $.additive_expression,
        //     $.shift_expression,
        //     $.bitwise_and_expression,
        //     $.bitwise_or_expression,
        //     $.bitwise_xor_expression,
        //
        // ),
        //
        // _below_relational_type_cast_expression: $ => prec(
        //     DART_PREC.RelationalTypeCast,
        //     choice(
        //         // UNARY_POSTFIX: 16,
        //         // UNARY_PREFIX: 15,
        //         // Multiplicative: 14, // *, /, ˜/, % Left
        //         // Additive: 13, // +, - Left
        //         // Shift: 12, // <<, >>, >>> Left
        //         // Bitwise_AND: 11, // & Left
        //         // Bitwise_XOR: 10, // ˆ Left
        //         // Bitwise_Or: 9 , // | Left
        //         $._unary_expression,
        //         $.multiplicative_expression,
        //         $.additive_expression,
        //         $.shift_expression,
        //         $.bitwise_and_expression,
        //         $.bitwise_or_expression,
        //         $.bitwise_xor_expression,
        //
        //     )
        // ),

        throw_expression: $ => seq(
            'throw',
            $._expression
        ),
        throw_expression_without_cascade: $ => seq(
            'throw',
            $._expression_without_cascade
        ),

        rethrow_expression: $ => $.rethrow_builtin,

        /**************************************************************************************************
         ***********************Assignment Expressions*****************************************************
         ***************************************************************************************************
         ****These are the assignment expressions from section 16.34 (Page 159) of the dart DRAFT**********
         * specification. (Very different from the formal spec in this instance)****************************
         ***************************************************************************************************
         ***************************************************************************************************/

        assignment_expression: $ => prec.right(DART_PREC.Assignment, seq( //right
            field('left', $.assignable_expression),
            field('operator', $._assignment_operator),
            field('right', $._expression)
        )),

        assignment_expression_without_cascade: $ => prec.right(DART_PREC.Assignment, seq( //right
            field('left', $.assignable_expression),
            field('operator', $._assignment_operator),
            field('right', $._expression_without_cascade)
        )),

        assignable_expression: $ => choice(
            seq($._primary, $._assignable_selector_part), // dart issue?
            seq($.super, $.unconditional_assignable_selector),
            seq($.constructor_invocation, $._assignable_selector_part),
            // Per Dart spec, `get`, `set`, and `Function` are built-in
            // identifiers and may appear on the left-hand side of an
            // assignment when used as a field or local variable name
            // (e.g. `set = expr;`).
            $.identifier,
            alias($._get, $.identifier),
            alias($._set, $.identifier),
            alias($._function_builtin_identifier, $.identifier),
        ),
        _assignable_selector_part: $ => seq(
            repeat($.selector),
            $._assignable_selector
        ),
        //'+=', '-=', '*=', '/=', '&=', '|=', '^=', '%=', '<<=', '>>=', '>>>=', '??='
        //todo: use the op names in place of these.
        _assignment_operator: $ => choice(
            '=',
            // additive operator
            '+=',
            '-=',
            // multiplicative operator
            '*=',
            '/=',
            '%=',
            '~/=',
            // shift operator
            '<<=',
            '>>=',
            '>>>=',
            '&=',
            '^=',
            '|=',
            '??=',
        ),

        // binary_expression: $ => choice(
        //     ...[
        //         ['>', PREC.REL],
        //         ['<', PREC.REL],
        //         ['==', PREC.REL],
        //         ['>=', PREC.REL],
        //         ['<=', PREC.REL],
        //         ['!=', PREC.REL],
        //         ['&&', PREC.AND],
        //         ['||', PREC.OR],
        //         ['+', PREC.PLUS],
        //         ['-', PREC.PLUS],
        //         ['*', PREC.TIMES],
        //         ['/', PREC.TIMES],
        //         ['&', PREC.AND],
        //         ['|', PREC.OR],
        //         ['^', PREC.OR],
        //         ['%', PREC.TIMES],
        //         ['<<', PREC.TIMES],
        //         ['>>', PREC.TIMES],
        //         ['>>>', PREC.TIMES],
        //     ].map(([operator, precedence]) =>
        //         prec.left(precedence, seq(
        //             field('left', $._expression),
        //             field('operator', operator),
        //             field('right', $._expression)
        //         ))
        //     )),

        // instanceof_expression: $ => prec(PREC.REL, seq(
        //     field('left', $._expression),
        //     'instanceof',
        //     field('right', $._type)
        // )),

        lambda_expression: $ => seq(
            field('parameters', $.function_signature),
            field(
                'body',
                $.function_body
            )
        ),

        function_expression: $ => seq(
            field('parameters', $._formal_parameter_part),
            field(
                'body',
                $.function_expression_body
            )
        ),

        inferred_parameters: $ => seq(
            '(',
            commaSep1($.identifier),
            ')'
        ),

        if_null_expression: $ => prec.left( //left
            DART_PREC.If,
            seq(
                field('first',
                    $._real_expression // logical_or_expression
                ),
                $._if_null_expression
                // optional(
                //     $._if_null_expression
                // )
            )
        ),

        _if_null_expression: $ => repeat1(
            seq(
                '??',
                field('second', $._real_expression)
            )
        ),

        conditional_expression: $ => prec.left( //left
            DART_PREC.Conditional,
            seq(
                // $.if_null_expression,
                $._real_expression,
                seq(
                    '?',
                    field('consequence', $._expression_without_cascade),
                    ':',
                    field('alternative', $._expression_without_cascade)
                )
            )
        ),

        logical_or_expression: $ => prec.left( //left
            DART_PREC.Logical_OR,
            sep2($._real_expression, $.logical_or_operator)
        ),

        logical_and_expression: $ => prec.left( //left
            DART_PREC.Logical_AND,
            sep2($._real_expression, $.logical_and_operator)
        ),

        equality_expression: $ => prec( //neither
            DART_PREC.Equality,
            choice(
                seq(
                    // $.relational_expression,
                    $._real_expression,
                    // optional(
                    //
                    // )

                    $.equality_operator,
                    $._real_expression
                    // $.relational_expression

                ),
                seq(
                    $.super,
                    $.equality_operator,
                    // $.relational_expression
                    $._real_expression
                )
            )
        ),

        equality_operator: $ => token(
            choice(
                '==',
                '!='
            )
        ),
        type_cast_expression: $ => prec.left(
            DART_PREC.RelationalTypeCast,
            seq(
                // $._below_relational_type_cast_expression,
                $._real_expression,
                $.type_cast,
            )
        ),
        type_test_expression: $ => prec(
            DART_PREC.RelationalTypeTest,
            seq(
                // $._below_relational_type_cast_expression,
                $._real_expression,
                $.type_test,
            )
        ),
        // _raw_type_cast: $ => prec.right(
        //     seq(
        //         $._below_relational_type_cast_expression,
        //         $.type_cast,
        //     )
        // ),

        relational_expression: $ => prec( // neither
            DART_PREC.Relational,
            choice(
                // $._raw_type_cast,
                seq(
                    // $.bitwise_or_expression,
                    // $._below_relational_type_cast_expression,
                    // TODO: The spec says optional but it breaks tests, and I'm not sure in a good way.
                    // Modified to account for type casts being compared relationally!
                    // I am not certain this is what designers intended. (see other comments on github)
                    // optional(
                    $._real_expression,
                    $.relational_operator,
                    $._real_expression
                    // choice(
                    //     $.type_test,
                    //     $.type_cast,
                    //     seq(
                    //         $.relational_operator,
                    //         $._real_expression
                    //     )
                    // )
                    // ),
                ),
                // seq(
                //     // $.bitwise_or_expression,
                //     choice(
                //         $._raw_type_cast,
                //         $._below_relational_type_cast_expression
                //     ),
                //     $.relational_operator,
                //     choice(
                //         $._raw_type_cast,
                //         $._below_relational_type_cast_expression
                //     )
                // ),
                seq(
                    $.super,
                    $.relational_operator,
                    $._real_expression
                ),
            )
        ),

        relational_operator: $ => choice(
            '<',
            '>',
            '<=',
            '>='
        ),

        //BITWISE EXPRESSIONS
        bitwise_or_expression: $ => binaryRunLeft($._real_expression, '|', $.super, DART_PREC.Bitwise_Or),
        bitwise_xor_expression: $ => binaryRunLeft($._real_expression, '^', $.super, DART_PREC.Bitwise_XOR),
        bitwise_and_expression: $ => binaryRunLeft($._real_expression, '&', $.super, DART_PREC.Bitwise_AND),
        shift_expression: $ => binaryRunLeft($._real_expression, $.shift_operator, $.super, DART_PREC.Shift),
        additive_expression: $ => binaryRunLeft($._real_expression, $.additive_operator, $.super, DART_PREC.Additive),
        multiplicative_expression: $ => binaryRunLeft($._unary_expression, $.multiplicative_operator, $.super, DART_PREC.Multiplicative),
        bitwise_operator: $ => $._bitwise_operator,
        _bitwise_operator: $ => choice(
            '&',
            '^',
            '|'
        ),
        shift_operator: $ => $._shift_operator,
        _shift_operator: $ => choice(
            '<<',
            '>>',
            '>>>'
        ),
        additive_operator: $ => $._additive_operator,
        _additive_operator: $ => token(
            choice(
                '+',
                '-'
            )
        ),
        multiplicative_operator: $ => $._multiplicative_operator,
        _multiplicative_operator: $ => choice(
            '*',
            '/',
            '%',
            '~/'
        ),

        _unary_expression: $ => prec(
            DART_PREC.UNARY_PREFIX,
            choice(
                $._postfix_expression,
                $.unary_expression,
            )
        ),

        unary_expression: $ => prec( //neither
            DART_PREC.UNARY_PREFIX,
            choice(

                seq($.prefix_operator, $._unary_expression),
                $.await_expression,
                // prec(DART_PREC.UNARY_POSTFIX, $._postfix_expression),
                seq(
                    choice(
                        $.minus_operator,
                        $.tilde_operator
                    ),
                    $.super
                ),
                seq(
                    $.increment_operator,
                    $.assignable_expression
                )
            )
        ),

        _postfix_expression: $ => choice(
            seq(
                $._primary,
                repeat(
                    $.selector
                )
            ),
            $.postfix_expression
        ),

        postfix_expression: $ => prec.right(choice(
            seq(
                $.assignable_expression,
                $.postfix_operator
            ),
            seq(
                $.constructor_invocation,
                repeat(
                    $.selector
                )
            )
        )),

        postfix_operator: $ => $.increment_operator,

        increment_operator: $ => token(choice(
            '++',
            '--'
        )),


        spread_element: $ => seq(
            '...',
            optional('?'),
            $._expression
        ),

        selector: $ => prec.right(choice(
            // '!',
            $._exclamation_operator,
            $._assignable_selector,
            $.argument_part,
            $.type_arguments,
        )),

        prefix_operator: $ => choice(
            $.minus_operator,
            $.negation_operator,
            $.tilde_operator
        ),

        minus_operator: $ => '-',
        negation_operator: $ => $._exclamation_operator,
        _exclamation_operator: $ => '!',
        tilde_operator: $ => '~',

        await_expression: $ => seq(
            'await',
            $._unary_expression
        ),

        type_test: $ => seq(
            $.is_operator,
            $._type_not_void
        ),

        is_operator: $ => seq(
            token('is'),
            optional(
                $._exclamation_operator
            )
        ),

        type_cast: $ => seq(
            $.as_operator,
            $._type_not_void
        ),

        as_operator: $ => token('as'),

        new_expression: $ => seq(
            $._new_builtin,
            $._type_not_void,
            optional(
                $._dot_identifier
            ),
            $.arguments
        ),

        _dot_identifier: $ => prec.dynamic(
            DART_PREC.DOT_IDENTIFIER,
            seq(
                '.',
                $.identifier
            )
        ),
        const_object_expression: $ => choice(
            seq(
                $.const_builtin,
                $._type_not_void,
                optional(
                    $._dot_identifier
                ),
                $.arguments
            ),
            // Dart 3.10 allows `const` with a dot-shorthand constructor
            // invocation: `const .new(...)` / `const .named(...)`. The
            // receiver type is inferred from the context type.
            seq(
                $.const_builtin,
                $.dot_shorthand,
                $.arguments,
            ),
        ),


        _primary: $ => choice(
            $._literal,
            $.identifier,
            alias($._get, $.identifier),
            alias($._set, $.identifier),
            // Per Dart spec, `Function` is a built-in identifier and may
            // appear as an ordinary identifier (e.g. `Function.apply(...)`,
            // referring to the `Function` class).
            alias($._function_builtin_identifier, $.identifier),
            $.function_expression,
            $.new_expression,
            $.const_object_expression,
            $.parenthesized_expression,
            // $.class_literal,
            $.this,
            seq(
                $.super,
                $.unconditional_assignable_selector
            ),
            $.constructor_tearoff,
            $.switch_expression,
            $.dot_shorthand,
            // $.object_creation_expression,
            // $.field_access,
            // $.array_access,
            // $.method_invocation,
            // $.method_reference,
        ),


        // Dart 3.10 dot shorthands: .identifier or .new, optionally followed by
        // type arguments and arguments. The context type provides the prefix.
        // e.g. Color c = .red;  ButtonStyle s = .fromSeed(Colors.blue);
        dot_shorthand: $ => prec.right(seq(
            '.',
            choice($.identifier, $._new_builtin),
        )),


        parenthesized_expression: $ => seq('(', $._expression, ')'),

        _compound_access: $ => choice('.', '?.'),

        constructor_invocation: $ => prec.right(choice(
            seq($._type_name, $.type_arguments, '.', $.identifier, $.arguments),
            seq($._type_name, '.', $._new_builtin, $.arguments),
        )),

        constructor_tearoff: $ => prec.right(seq(
            $._type_name, optional($.type_arguments), '.', $._new_builtin,
        )),

        arguments: $ => seq('(', optional($._argument_list), ')'),

        _argument_list: $ => prec.right(commaSep1TrailingComma($._any_argument)),

        _any_argument: $ => choice($.argument, $.named_argument),

        argument: $ => $._expression,

        named_argument: $ => seq($.label, $._expression),

        cascade_section: $ => prec.left(
            DART_PREC.Cascade,
            seq(
                choice('..', '?..'),
                $.cascade_selector,
                // After the selector, allow argument parts and `!` null
                // assertions interleaved before the subsequent subsections
                // begin (e.g. `.._parent!.onChildrenChanged(...)`).
                repeat(choice(
                    $.argument_part,
                    $._exclamation_operator,
                )),
                repeat(
                    $._cascade_subsection
                ),
                optional(
                    $._cascade_assignment_section
                )
            )
        ),

        // prec.left(
        // DART_PREC.Cascade,
        // ),
        // After the cascade selector, allow chained selectors (assignable
        // selectors `.x` / `?.x` / `[i]`, the postfix `!` null assertion,
        // and argument parts) — matches the general `selector` form used in
        // non-cascade method chains. Without `!`, code like
        // `obj.._parent!.onChildrenChanged(...)` fails to parse.
        _cascade_subsection: $ => seq(
            $._assignable_selector,
            repeat(choice(
                $.argument_part,
                $._exclamation_operator,
            ))
        ),
        _cascade_assignment_section: $ => seq(
            $._assignment_operator,
            $._expression_without_cascade
        ),
        index_selector: $ => seq('[', $._expression, ']'),
        cascade_selector: $ => choice(
            seq(
                optional($.nullable_selector),
                $.index_selector,
            ),
            $.identifier
        ),
        argument_part: $ => seq(
            optional(
                $.type_arguments
            ),
            // seq(
            //     $.type_arguments,
            //     $.arguments
            // ),
            $.arguments
        ),

        unconditional_assignable_selector: $ => choice(
            $.index_selector,
            seq('.', $.identifier)
        ),

        conditional_assignable_selector: $ => choice(
            seq('?.', $.identifier),
            seq('?', $.index_selector)
        ),

        _assignable_selector: $ => choice(
            $.unconditional_assignable_selector,
            $.conditional_assignable_selector
        ),

        type_arguments: $ => choice( // was prec.right
            // seq(
            //     '<',
            //     '>',
            //     optional($.nullable_type)
            // ),
            seq(
                '<',
                commaSep($._type),
                '>',
                // optional($.nullable_type)
            )
        ),

        wildcard: $ => seq(
            optional($._metadata),
            '?',
            optional($._wildcard_bounds)
        ),

        _wildcard_bounds: $ => choice(
            seq('extends', $._type),
            seq($.super, $._type)
        ),

        dimensions: $ => prec.right(repeat1(
            seq(optional($._metadata), '[', ']')
        )),

        // Statements
        _statement: $ => choice(
            $.block,
            prec.dynamic(1, $.local_function_declaration),
            prec.dynamic(2, $.local_variable_declaration),
            $.for_statement,
            $.while_statement,
            $.do_statement,
            $.switch_statement,
            $.if_statement,
            //TODO: add rethrow statement.
            // $._declaration,

            $.try_statement,
            $.break_statement,
            $.continue_statement,
            $.return_statement,
            $.yield_statement,
            $.yield_each_statement,
            $.expression_statement,
            $.empty_statement,
            $.assert_statement,
            // $.labeled_statement,
        ),

        local_function_declaration: $ => seq(
            optional($._metadata),
            $.lambda_expression
        ),

        block: $ => seq(
            '{', repeat($._statement), '}'
        ),

        expression_statement: $ => seq(
            $._expression,
            $._semicolon
        ),

        // Per Dart spec, `;` alone is the empty (null) statement, e.g.
        // `if (cond) ;` or as a no-op body.
        empty_statement: $ => ';',

        labeled_statement: $ => seq(
            $.identifier, ':', $._statement
        ),

        assert_statement: $ => seq($.assertion, ';'),

        assertion: $ => seq(
            $.assert_builtin,
            $.assertion_arguments,
        ),

        assertion_arguments: $ => seq(
            '(',
            $._expression,
            optional(
                seq(
                    ',',
                    $._expression
                ),
            ),
            optional(','),
            ')',
        ),

        switch_statement: $ => seq(
            'switch',
            field('condition', $.parenthesized_expression),
            field('body', $.switch_block)
        ),

        switch_expression: $ => seq(
            'switch',
            field('condition', $.parenthesized_expression),
            field('body',
                seq('{',
                    commaSep1TrailingComma($.switch_expression_case),
                    '}'
                ))
        ),

        switch_expression_case: $ => seq($._guarded_pattern, '=>', $._expression),

        _guarded_pattern: $ => seq(
            $._pattern, optional(seq('when', $._expression))
        ),

        _pattern: $ => choice(
            $._logical_or_pattern,
        ),

        _logical_or_pattern: $ => seq($._logical_and_pattern, repeat(seq($.logical_or_operator, $._logical_and_pattern))),
        _logical_and_pattern: $ => seq($._relational_pattern, repeat(seq($.logical_and_operator, $._relational_pattern))),
        _relational_pattern: $ =>
            prec(DART_PREC.Relational, choice(
                seq(choice($.relational_operator, $.equality_operator), $._real_expression),
                $._unary_pattern,
            )
            ),

        _unary_pattern: $ => choice(
            $.cast_pattern,
            $.null_check_pattern,
            $.null_assert_pattern,
            $._primary_pattern,
        ),

        _primary_pattern: $ => choice(
            $.constant_pattern,
            $.variable_pattern,
            $._parenthesized_pattern,
            $.list_pattern,
            $.map_pattern,
            $.record_pattern,
            $.object_pattern,
        ),

        cast_pattern: $ => seq($._primary_pattern, 'as', $._type),

        null_check_pattern: $ => seq($._primary_pattern, '?'),

        null_assert_pattern: $ => seq($._primary_pattern, '!'),

        constant_pattern: $ => choice(
            $._bool_literal,
            $.null_literal,
            seq(optional($.minus_operator), $._numeric_literal),
            $.string_literal,
            $.symbol_literal,
            $.identifier,
            $.qualified,
            $.const_object_expression,
            // Dart 3.10 dot-shorthand may appear as a constant pattern
            // (e.g. `case .red:`, `case .new():`, `case .fromRGB(1,2,3):`).
            // The context type of the matched value supplies the receiver
            // type. The `const` form (`const .new(...)` / `const .named(...)`)
            // is reached via `const_object_expression` above.
            seq($.dot_shorthand, optional(seq(optional($.type_arguments), $.arguments))),
            seq($.const_builtin, optional($.type_arguments), '[', commaSep1TrailingComma($._element), ']'),
            seq($.const_builtin, optional($.type_arguments), '{', commaSep1TrailingComma($._element), '}'),
            seq($.const_builtin, '(', $._expression, ')'),
        ),

        variable_pattern: $ => seq($._final_var_or_type, $.identifier),

        _parenthesized_pattern: $ => seq('(', $._pattern, ')'),

        list_pattern: $ => seq(optional($.type_arguments), '[', commaSepTrailingComma($._list_pattern_element), ']'),

        _list_pattern_element: $ => choice($._pattern, $.rest_pattern),

        rest_pattern: $ => seq('...', optional($._pattern)),

        map_pattern: $ => seq(optional($.type_arguments), '{', commaSepTrailingComma($._map_pattern_entry), '}'),

        _map_pattern_entry: $ => choice(seq($._expression, ':', $._pattern), '...'),

        record_pattern: $ => seq('(', commaSep1TrailingComma($._pattern_field), ')'),

        _pattern_field: $ => seq(optional(seq(optional($.identifier), ':')), $._pattern),

        object_pattern: $ => seq($._type_name, optional($.type_arguments), '(', commaSepTrailingComma($._pattern_field), ')'),

        pattern_variable_declaration: $ => seq(choice($.final_builtin, $.inferred_type), $._outer_pattern, '=', $._expression),

        _outer_pattern: $ => choice($._parenthesized_pattern, $.list_pattern, $.map_pattern, $.record_pattern, $.object_pattern),

        pattern_assignment: $ => seq($._outer_pattern, '=', $._expression),

        switch_block: $ => seq(
            '{',
            repeat($.switch_statement_case),
            optional($.switch_statement_default),
            '}'
        ),

        switch_statement_case: $ => seq(
            repeat($.label), $.case_builtin, $._guarded_pattern, ':', repeat($._statement),
        ),

        switch_statement_default: $ => seq(
            repeat($.label), 'default', ':', repeat($._statement),
        ),

        switch_case: $ => choice(
            seq(repeat($.label), $.case_builtin, $._guarded_pattern, ':', repeat1($._statement)),
        ),

        default_case: $ => choice(
            seq(repeat($.label), 'default', ':', repeat1($._statement)),
        ),

        switch_label: $ => seq(
            repeat($.label),
            choice(
                seq($.case_builtin, $._expression, ':'),
                seq('default', ':')
            )),

        do_statement: $ => seq(
            'do',
            field('body', $._statement),
            'while',
            field('condition', $.parenthesized_expression),
            $._semicolon
        ),

        break_statement: $ => seq($.break_builtin, optional($.identifier), $._semicolon),

        continue_statement: $ => seq('continue', optional($.identifier), $._semicolon),

        yield_statement: $ => seq('yield', $._expression, $._semicolon),

        yield_each_statement: $ => seq('yield', '*', $._expression, $._semicolon),

        return_statement: $ => seq(
            'return',
            optional($._expression),
            $._semicolon
        ),

        throw_statement: $ => seq('throw', $._expression, $._semicolon),

        try_statement: $ => seq(
            $._try_head,
            optional(choice(
                $.finally_clause,
                seq(repeat1($._on_part), optional($.finally_clause))
            ))
        ),
        _on_part: $ => choice(
            seq(
                $.catch_clause,
                $.block
            ),
            seq(
                'on',
                $._type_not_void,
                optional($.catch_clause),
                $.block
            )
        ),
        _try_head: $ => seq(
            'try',
            field('body', $.block),
        ),
        catch_clause: $ => seq(
            'catch',
            $.catch_parameters,
            // field('body', $.block)
        ),

        catch_parameters: $ => seq(
            '(',
            $.identifier,
            optional(
                seq(
                    ',',
                    $.identifier
                ),
            ),
            ')',
        ),

        catch_type: $ => sep1($._type, '|'),

        finally_clause: $ => seq('finally', $.block),

        if_element: $ => prec.right(seq(
            'if',
            '(', $._expression, optional(seq('case', $._guarded_pattern)), ')',
            field('consequence', $._element),
            optional(seq('else', field('alternative', $._element)))
        )),

        if_statement: $ => prec.right(seq(
            'if',
            '(', $._expression, optional(seq('case', $._guarded_pattern)), ')',
            field('consequence', $._statement),
            optional(seq('else', field('alternative', $._statement)))
        )),


        while_statement: $ => seq(
            'while',
            field('condition', $.parenthesized_expression),
            field('body', $._statement)
        ),

        for_statement: $ => seq(
            optional('await'),
            'for',
            $.for_loop_parts,
            field('body', $._statement)
        ),

        for_loop_parts: $ => seq('(', $._for_loop_parts, ')'),

        _for_loop_parts: $ => choice(
            seq(
                choice(
                    $._declared_identifier,
                    $.identifier
                ),
                'in',
                field('value', $._expression),
            ),
            seq(
                optional(choice(
                    field('init', $.local_variable_declaration),
                    seq(
                        commaSep(field('init', $._expression)),
                        $._semicolon
                    )
                ),),
                field('condition', optional($._expression)), $._semicolon,
                commaSepTrailingComma(field('update', $._expression)),
            ),
            seq(
                choice($.final_builtin, $.inferred_type),
                $._outer_pattern,
                'in',
                field('value', $._expression)
            )
        ),

        // support map weirdness?
        for_element: $ => seq(
            optional('await'),
            'for',
            $.for_loop_parts,
            field('body', $._element)
        ),

        // Annotations
        annotation: $ => prec.right(seq(
            '@',
            field('name', choice($.identifier, $.scoped_identifier)),
            choice(
                optional(seq($.type_arguments, $.arguments)),
                optional($.arguments)
            )
        )),

        // A "bare" annotation: just `@name` with no argument list. This is
        // structurally a strict subset of `annotation` and is used to
        // disambiguate `@override (X, Y) foo() { ... }` from
        // `@override(X, Y)`. Aliased to `annotation` at the use site so the
        // resulting AST is indistinguishable from a normal annotation.
        // Negative dynamic precedence so the standard `annotation` rule
        // (which can also produce `@name` with no args) wins ties — this
        // matters for inputs like `@Foo() bar() {}` where both
        // `@Foo()` (annotation with empty args) and `@Foo` (bare) +
        // `()` (empty-record return type) are syntactically valid.
        _bare_annotation: $ => prec.dynamic(-1, seq(
            '@',
            field('name', choice($.identifier, $.scoped_identifier))
        )),

        // Declarations

        _declaration: $ => prec(1, choice(
            $.import_specification,
            $.class_definition,
            // $.annotation_type_declaration,
            $.enum_declaration,
        )),

        import_or_export: $ => prec(
            DART_PREC.IMPORT_EXPORT,
            choice(
                $.library_import,
                $.library_export
            )
        ),

        library_import: $ => seq(
            optional($._metadata),
            $.import_specification
        ),

        library_export: $ => seq(
            optional($._metadata),
            $._export,
            $.configurable_uri,
            repeat($.combinator),
            $._semicolon
        ),

        import_specification: $ => choice(
            seq(
                $._import,
                $.configurable_uri,
                optional(
                    seq(
                        $._as,
                        $.identifier
                    )
                ),
                repeat($.combinator),
                $._semicolon
            ),
            seq(
                $._import,
                $.uri,
                $._deferred,
                $._as,
                $.identifier,
                repeat($.combinator),
                $._semicolon
            )
        ),

        part_directive: $ => seq(
            optional($._metadata),
            'part',
            $.uri,
            $._semicolon
        ),

        part_of_directive: $ => seq(
            optional($._metadata),
            $.part_of_builtin,
            choice($.dotted_identifier_list, $.uri),
            $._semicolon
        ),

        uri: $ => $.string_literal,

        configurable_uri: $ => seq(
            $.uri,
            repeat($.configuration_uri)
        ),

        configuration_uri: $ => seq(
            'if',
            $.configuration_uri_condition,
            $.uri
        ),

        configuration_uri_condition: $ => seq('(', $.uri_test, ')'),

        uri_test: $ => seq(
            $.dotted_identifier_list,
            optional(
                seq(
                    '==',
                    $.string_literal
                )
            )
        ),

        combinator: $ => choice(
            seq('show', $._identifier_list),
            seq('hide', $._identifier_list)
        ),

        _identifier_list: $ => commaSep1($.identifier),

        asterisk: $ => '*',

        enum_declaration: $ => seq(
            optional($._metadata),
            'enum',
            field('name', $.identifier),
            optional($.type_parameters),
            optional($.mixins),
            optional($.interfaces),
            field('body', $.enum_body),
        ),

        enum_body: $ => seq(
            '{',
            commaSep1TrailingComma($.enum_constant),
            optional(
                seq(';', repeat(choice(
                    seq(optional($._metadata), $._class_member_definition),
                    prec.dynamic(-10, $._record_return_class_member),
                )))
            ),
            '}'
        ),

        enum_constant: $ => choice(
            seq(
                optional($._metadata),
                field('name', $.identifier),
                optional($.argument_part),
            ),
            seq(
                optional($._metadata),
                field('name', $.identifier),
                optional($.type_arguments),
                '.',
                choice($.identifier, $._new_builtin),
                $.arguments,
            )),

        type_alias: $ => choice(
            seq(
                optional($._metadata),
                $._typedef,
                optional($._type),
                $._type_name,
                $._formal_parameter_part, ';'),
            seq(
                optional($._metadata),
                $._typedef,
                $._type_name,
                optional($.type_parameters),
                '=', $._type, ';'),
        ),

        _class_modifiers: $ => seq(choice($.sealed, seq(optional($.abstract), optional(choice($.base, $.interface, 'final', 'inline')))), 'class'),

        _mixin_class_modifiers: $ => seq(optional($.abstract), optional($.base), $.mixin, 'class'),

        class_definition: $ => choice(
            seq(
                optional($._metadata),
                choice($._class_modifiers, $._mixin_class_modifiers),
                field('name', $.identifier),
                optional(field('type_parameters', $.type_parameters)),
                optional(field('superclass', $.superclass)),
                optional(field('interfaces', $.interfaces)),
                field('body', $.class_body)
            ),
            seq(
                optional($._metadata),
                $._class_modifiers,
                $.mixin_application_class
            )
        ),

        extension_declaration: $ => choice(
            seq(
                optional($._metadata),
                'extension',
                optional(field('name', $.identifier)),
                optional(field('type_parameters', $.type_parameters)),
                'on',
                field('class', $._type),
                field('body', $.extension_body)
            ),
        ),

        extension_type_declaration: $ => seq(
            optional($._metadata),
            'extension',
            'type',
            optional($.const_builtin),
            field('name', $.identifier),
            optional(field('type_parameters', $.type_parameters)),
            field('representation', $.representation_declaration),
            optional(field('interfaces', $.interfaces)),
            field('body', $.class_body)
        ),

        representation_declaration: $ => seq(
            optional(seq('.', choice($.identifier, $._new_builtin))),
            '(',
            optional($._metadata),
            field('type', $._type),
            field('name', $.identifier),
            ')'
        ),

        _metadata: $ => prec.right(repeat1($.annotation)),


        type_parameters: $ => seq(
            '<', commaSep1($.type_parameter), '>'
        ),

        type_parameter: $ => seq(
            optional($._metadata),
            choice(alias(
                $.identifier,
                $.type_identifier),
                $.nullable_type
            ),
            // This is a comment
            // comment with a link made in https://github.com/flutter/flutter/pull/48547
            // Changes made in https://github.com/flutter/flutter/pull/48547
            /* This is also a comment */
            /* this comment /* // /** ends here: */

            optional($.nullable_type),
            optional($.type_bound)
        ),

        type_bound: $ => seq('extends', $._type_not_void),

        superclass: $ => choice(
            seq(
                'extends',
                $._type_not_void,
                optional($.mixins)
            ),
            $.mixins
        ),

        mixins: $ => seq(
            'with',
            $._type_not_void_list
        ),

        mixin_application_class: $ => seq(
            $.identifier,
            optional($.type_parameters),
            '=',
            $.mixin_application,
            $._semicolon
        ),

        mixin_application: $ => seq(
            $._type_not_void,
            $.mixins,
            optional($.interfaces)
        ),
        mixin_declaration: $ => seq(
            optional($._metadata),
            optional($.base),
            $.mixin,
            $.identifier,
            optional($.type_parameters),
            optional(seq(
                'on',
                $._type_not_void_list
            )),
            optional($.interfaces),
            $.class_body
        ),
        interfaces: $ => seq(
            $._implements,
            $._type_not_void_list
        ),

        interface_type_list: $ => seq(
            $._type,
            repeat(seq(',', $._type))
        ),

        class_body: $ => seq(
            '{',
            repeat(
                choice(
                    seq(
                        optional($._metadata),
                        $._class_member_definition
                    ),
                    // Methods whose return type is a record type, preceded by
                    // bare annotations (no argument list). The standard arm
                    // can't reach this case because `annotation`'s
                    // `optional(arguments)` greedily eats the leading `(`,
                    // which then mis-parses as the annotation's argument
                    // list. The conflict between `annotation` and
                    // `_bare_annotation` (declared at the top of this
                    // grammar) lets GLR explore both stacks; the negative
                    // dynamic precedence makes the standard path win when
                    // both interpretations are valid (e.g. `@Foo() x() {}`
                    // — empty annotation args + plain function — should
                    // not be reinterpreted as bare-annotation + record
                    // return; the standard `record_type` rule allows the
                    // empty form so this can otherwise tie).
                    prec.dynamic(-10, $._record_return_class_member),
                )
            ),
            '}'
        ),

        _record_return_class_member: $ => seq(
            repeat1(alias($._bare_annotation, $.annotation)),
            // Hidden rule aliased to `method_signature` at this use site,
            // so the AST is identical to a normal record-return method —
            // downstream tools (highlight queries, OCaml mapper) don't
            // need to learn a new node type.
            alias(
                $._record_return_method_signature,
                $.method_signature
            ),
            $.function_body
        ),

        _record_return_method_signature: $ => seq(
            optional($._static),
            alias(
                $._record_return_function_signature,
                $.function_signature
            ),
        ),

        // Same shape as `function_signature` but with the return type
        // pinned to `record_type`. Note that the empty record form `()` is
        // intentionally excluded here: `@Foo() bar() {}` is unambiguously
        // an annotation with empty args followed by a function with no
        // return type, not a bare annotation followed by an empty-record
        // return. Aliased to `record_type` so the AST shape is the same.
        _record_return_function_signature: $ => seq(
            alias(
                choice(
                    seq(
                        '(',
                        commaSep1($.record_type_field),
                        ',',
                        '{',
                        commaSep1TrailingComma($.record_type_named_field),
                        '}',
                        ')'
                    ),
                    seq(
                        '(',
                        commaSep1TrailingComma($.record_type_field),
                        ')'
                    ),
                    seq(
                        '(',
                        '{',
                        commaSep1TrailingComma($.record_type_named_field),
                        '}',
                        ')'
                    ),
                ),
                $.record_type
            ),
            optional($.nullable_type),
            field('name', $.identifier),
            $._formal_parameter_part,
            optional($._native),
        ),
        extension_body: $ => seq(
            '{',
            repeat(
                choice(
                    seq(optional($._metadata), $.declaration, $._semicolon),
                    seq(
                        optional($._metadata),
                        seq(
                            $.method_signature,
                            $.function_body
                        ),
                    ),
                    prec.dynamic(-10, $._record_return_class_member),
                )
            ),
            '}'
        ),

        _class_member_definition: $ => choice(
            seq($.declaration, $._semicolon),
            seq(
                $.method_signature,
                $.function_body
            ),
        ),

        getter_signature: $ => seq(
            optional($._type),
            $._get,
            field('name', $.identifier),
            optional($._native)
        ),
        setter_signature: $ => seq(
            optional($._type),
            $._set,
            field('name', $.identifier),
            $._formal_parameter_part,
            optional($._native)
        ),
        method_signature: $ => choice(
            seq($.constructor_signature, optional($.initializers)),
            $.factory_constructor_signature,

            seq(
                optional($._static),
                choice(
                    $.function_signature,
                    $.getter_signature,
                    $.setter_signature
                )
            ),
            $.operator_signature
        ),

        declaration: $ => choice(
            seq($.constant_constructor_signature, optional(choice($.redirection, $.initializers))),
            seq($.constructor_signature, optional(choice($.redirection, $.initializers))),
            seq($._external,
                optional($.const_builtin),
                $.factory_constructor_signature
            ),
            seq(
                optional($.const_builtin),
                $.factory_constructor_signature, $._native
            ),
            seq($._external,
                $.constant_constructor_signature
            ),
            $.redirecting_factory_constructor_signature,
            seq($._external,
                $.constructor_signature
            ),
            seq(
                optional($._external_builtin),
                optional($._static),
                $.getter_signature,
            ),
            seq(
                optional($._external_and_static),
                $.setter_signature,
            ),

            seq(
                optional($._external),
                $.operator_signature
            ),
            seq(
                optional($._external_and_static),
                $.function_signature,
            ),
            seq(
                $._external_and_static,
                $._type,
                // Allow built-in identifiers (`get`, `set`, `operator`) as
                // the declared name — used by `dart:ffi` Struct fields like
                // `external SomeType get;` (`colibri_stateless` etc.).
                choice(
                    $.identifier,
                    alias($._get, $.identifier),
                    alias($._set, $.identifier),
                    alias($._operator, $.identifier),
                )
            ),
            // TODO: This should only work with native?
            seq(
                $._static,
                $.function_signature,
            ),
            // | static const 〈type〉? 〈staticFinalDeclarationList〉
            // | static final 〈type〉? 〈staticFinalDeclarationList〉
            // | static late final 〈type〉? 〈initializedIdentifierList〉
            // | static late? 〈varOrType〉 〈initializedIdentifierList
            seq(
                $._static,
                choice(
                    seq(
                        $._final_or_const,
                        optional($._type),
                        $.static_final_declaration_list
                    ),
                    seq(
                        $._late_builtin,
                        choice(
                            seq(
                                $.final_builtin,
                                optional($._type),
                                $.initialized_identifier_list
                            ),
                            seq(
                                choice(
                                    $._type,
                                    $.inferred_type,
                                ),
                                $.initialized_identifier_list
                            )
                        )
                    ),
                    seq(
                        choice(
                            $._type,
                            $.inferred_type,
                        ),
                        $.initialized_identifier_list
                    )
                )
            ),
            // | covariant late final 〈type〉? 〈identifierList〉
            // | covariant late? 〈varOrType〉 〈initializedIdentifierList〉
            seq(
                $._covariant,
                choice(
                    seq(
                        $._late_builtin,
                        choice(
                            seq(
                                $.final_builtin,
                                optional($._type),
                                $.identifier_list
                            ),
                            seq(
                                choice(
                                    $._type,
                                    $.inferred_type,
                                ),
                                $.initialized_identifier_list
                            )
                        )
                    ),
                    seq(
                        choice(
                            $._type,
                            $.inferred_type,
                        ),
                        $.initialized_identifier_list
                    )
                )
            ),
            seq(
                optional($._late_builtin), $.final_builtin,
                optional($._type),
                $.initialized_identifier_list
            ),
            seq(
                optional($._late_builtin),
                $._var_or_type,
                $.initialized_identifier_list
            ),
            // External instance fields (Dart 3 / static interop): per the
            // language spec a class member may be declared with the
            // `external` modifier together with `final`/`covariant`. The
            // unmodified `external <type> <id>` form is already handled by
            // the `_external_and_static + _type + identifier` rule above.
            seq(
                $._external,
                choice(
                    seq($.final_builtin, optional($._type), $.identifier_list),
                    seq($._covariant, $._var_or_type, $.identifier_list)
                )
            ),
            // Abstract instance fields (Dart 3): per the language spec, an
            // instance field in an abstract class may be declared without an
            // initializer, with the modifier `abstract`. The field has no
            // implementation; subclasses must provide a concrete
            // getter/setter. Supports `abstract final <type>? <ids>`,
            // `abstract covariant <type>? <ids>`, and the bare
            // `abstract <type> <ids>` form.
            seq(
                $.abstract,
                choice(
                    seq($.final_builtin, optional($._type), $.identifier_list),
                    seq($._covariant, $._var_or_type, $.identifier_list),
                    seq($._var_or_type, $.identifier_list)
                )
            )
            //    TODO: add in the 'late' keyword from the informal draft spec:
            //    |static late final〈type〉?〈initializedIdentifierList〉
            //    |static late?〈varOrType〉 〈initializedIdentifierList〉
            //    |covariant late?〈varOrType〉 〈initializedIdentifierList〉
            //    |late?final〈type〉?〈initializedIdentifierList〉
            //    |late?〈varOrType〉 〈initializedIdentifierList〉
        ),

        identifier_list: $ => commaSep1(
            $.identifier
        ),
        initialized_identifier_list: $ => commaSep1(
            $.initialized_identifier
        ),
        // Per Dart spec, `get`, `set`, and `operator` are built-in
        // identifiers and may be used as ordinary identifiers (e.g. as a
        // field name or local variable). The `_declared_identifier` rule
        // aliases the same set; mirror that here so `int set = 0;`,
        // `var get = ...;`, and `String operator;` parse.
        initialized_identifier: $ => seq(
            choice(
                $.identifier,
                alias($._get, $.identifier),
                alias($._set, $.identifier),
                alias($._operator, $.identifier),
            ),
            optional(seq(
                '=',
                $._expression
            ))
        ),
        static_final_declaration_list: $ => commaSep1(
            $.static_final_declaration
        ),
        binary_operator: $ => choice(
            $.multiplicative_operator,
            $.additive_operator,
            $.shift_operator,
            $.relational_operator,
            '==',
            $.bitwise_operator
        ),
        operator_signature: $ => seq(
            optional($._type),
            $._operator,
            choice(
                '~',
                $.binary_operator,
                '[]',
                '[]='
            ),
            $.formal_parameter_list,
            optional($._native)
        ),
        static_final_declaration: $ => seq(
            $.identifier,
            '=',
            $._expression
        ),

        _external_and_static: $ => seq(
            $._external,
            optional($._static)),
        _static_or_covariant: $ => choice(
            $._covariant,
            $._static
        ),
        _final_or_const: $ => choice(
            $.final_builtin,
            $.const_builtin
        ),

        static_initializer: $ => seq(
            $._static,
            $.block
        ),

        initializers: $ => seq(
            ':',
            commaSep1($.initializer_list_entry)
        ),
        initializer_list_entry: $ => choice(
            seq($.super, $.arguments),
            seq($.super,
                seq('.', choice($.identifier, $._new_builtin), $.arguments),
            ),
            $.field_initializer,
            $.assertion
        ),

        field_initializer: $ => seq(
            optional(seq($.this, '.')),
            $.identifier,
            '=',
            // Per Dart spec the field initializer's right-hand side is a
            // full expression (including compound assignments like `??=`),
            // not just a conditional expression.
            $._expression
        ),

        // constructor_signature: $ => seq(
        //      $._constructor_declarator,
        //      // optional($.throws),
        //      // field('body', choice(
        //      //     $.constructor_body,
        //      //     $._semicolon
        //      // ))
        //  ),

        factory_constructor_signature: $ => seq(
            $._factory,
            sep1($.identifier, '.'),
            $.formal_parameter_list,
        ),

        redirecting_factory_constructor_signature: $ => seq(
            optional($.const_builtin),
            $._factory,
            sep1($.identifier, '.'),
            $.formal_parameter_list,
            '=',
            $._type_not_void,
            optional(seq('.', $.identifier)),
        ),

        redirection: $ => seq(
            ':',
            $.this,
            optional(seq(
                '.',
                $._identifier_or_new
            )),
            $.arguments
        ),

        constructor_signature: $ => seq(
            field('name', seq($.identifier, optional(
                seq(
                    '.',
                    $._identifier_or_new
                )
            ))),
            field('parameters', $.formal_parameter_list)
        ),
        constant_constructor_signature: $ => seq(
            $.const_builtin,
            seq($.identifier, optional(seq('.', $._identifier_or_new))),
            $.formal_parameter_list
        ),

        constructor_body: $ => seq(
            '{',
            optional($.explicit_constructor_invocation),
            repeat($._statement),
            '}'
        ),

        explicit_constructor_invocation: $ => seq(
            choice(
                seq(
                    field('type_arguments', optional($.type_arguments)),
                    field('constructor', choice($.this, $.super)),
                ),
                seq(
                    field('object', choice($._ambiguous_name, $._primary)),
                    '.',
                    field('type_arguments', optional($.type_arguments)),
                    field('constructor', $.super),
                )
            ),
            field('arguments', $.arguments),
            $._semicolon
        ),

        _ambiguous_name: $ => choice(
            $.identifier,
            $.scoped_identifier
        ),

        scoped_identifier: $ => seq(
            field('scope', choice($.identifier, $.scoped_identifier)),
            '.',
            field('name', $.identifier)
        ),

        variable_declaration: $ => seq(
            $._declared_identifier,
            optional(seq(
                ',',
                commaSep1($.identifier)
            ))
        ),

        initialized_variable_definition: $ => seq(
            $._declared_identifier,
            optional(seq(
                prec(DART_PREC.BUILTIN, '='),
                field('value', $._expression)
            )),
            repeat(seq(',', $.initialized_identifier))
        ),
        // initialized_identifier: $ => seq(
        //   $.identifier,
        //   optional(seq('=', $._expression))
        // ),

        _declared_identifier: $ => seq(
            optional($._metadata),
            optional($._covariant),
            $._final_const_var_or_type,
            field('name', choice(
                $.identifier,
                alias($._get, $.identifier),
                alias($._set, $.identifier),
                alias($._operator, $.identifier),
            ))
        ),

        // Types

        _final_const_var_or_type: $ => choice(
            seq(optional($._late_builtin), $.final_builtin, optional($._type)),
            seq($.const_builtin, optional(
                $._type
            )),
            seq(optional($._late_builtin),
                $._var_or_type)
        ),

        _type: $ => choice(
            seq(
                $.function_type,
                optional($.nullable_type)
            ),
            $._type_not_function
            // $._function_type_tails,
            // seq(
            //     $._type_not_function,
            //     $._function_type_tails
            // ),
            // $._type_not_function
            // $._unannotated_type,
            // $.annotated_type
        ),
        _type_not_function: $ => choice(
            $._type_not_void_not_function,
            seq($.record_type, optional($.nullable_type)),
            $.void_type
        ),
        _type_not_void_not_function: $ => choice(
            seq(
                $._type_name,
                optional($.type_arguments),
                optional($.nullable_type)
            ),
            // rewritten in accordance with the draft spec page 198
            seq(
                $._function_builtin_identifier,
                optional($.nullable_type)
            )
        ),

        function_type: $ => choice(
            $._function_type_tails,
            seq(
                $._type_not_function,
                $._function_type_tails
            )
        ),
        _function_type_tails: $ => repeat1($._function_type_tail),

        _function_type_tail: $ => seq(
            $._function_builtin_identifier,
            optional($.type_parameters),
            optional($.nullable_type),
            optional($.parameter_type_list),
            optional($.nullable_type),
        ),

        parameter_type_list: $ => seq(
            '(',
            optional(choice(
                commaSep1TrailingComma($.normal_parameter_type),
                seq(
                    commaSep1($.normal_parameter_type),
                    ',',
                    $.optional_parameter_types,
                ),
                $.optional_parameter_types
            ),),
            ')'
        ),

        normal_parameter_type: $ => seq(
            optional($._metadata),
            choice(
                $.typed_identifier,
                $._type
            )
        ),

        optional_parameter_types: $ => choice(
            $.optional_positional_parameter_types,
            $.named_parameter_types
        ),

        optional_positional_parameter_types: $ => seq(
            '[',
            commaSep1TrailingComma($.normal_parameter_type),
            ']'
        ),
        named_parameter_types: $ => seq(
            '{',
            commaSep1TrailingComma($._named_parameter_type),
            '}'
        ),

        _named_parameter_type: $ => seq(
            optional($._metadata),
            optional($._required),
            $.typed_identifier
        ),

        _type_not_void: $ => choice(
            seq(
                $.function_type,
                optional($.nullable_type)
            ),
            seq($.record_type, optional($.nullable_type)),
            // $.function_type,
            $._type_not_void_not_function
            // alias($.identifier, $.type_identifier),
            // // $.scoped_type_identifier,
            // $.generic_type
        ),

        record_type: $ => choice(
            // The empty record type `()` gets a strongly negative dynamic
            // precedence so e.g. `@Foo() bar() {}` keeps parsing as
            // (annotation with empty args) + (plain function). Without
            // this, the bare-annotation/record-return path added below
            // can win the GLR fork because `()` is also a valid empty
            // record_type as the return type.
            prec.dynamic(-20, seq('(', ')')),
            seq('(', commaSep1($.record_type_field), ',', '{', commaSep1TrailingComma($.record_type_named_field), '}', ')'),
            seq('(', commaSep1TrailingComma($.record_type_field), ')'),
            seq('(', '{', commaSep1TrailingComma($.record_type_named_field), '}', ')'),
        ),

        record_type_field: $ => seq(
            optional($._metadata),
            $._type,
            optional($.identifier),
        ),

        record_type_named_field: $ => seq(
            optional($._metadata),
            $.typed_identifier,
        ),

        _type_not_void_list: $ => commaSep1(
            $._type_not_void
        ),

        _type_name: $ => seq(
            alias(
                $.identifier,
                $.type_identifier
            ),
            optional(
                $._type_dot_identifier
            ),
            // optional($.nullable_type),
        ),

        // _type_name: $ => prec.right( // changed from above?
        //     seq(
        //         alias(
        //             $.identifier,
        //             $.type_identifier
        //         ),
        //         optional(
        //             $._type_dot_identifier
        //         ),
        //         optional($.nullable_type),
        //     )
        // ),

        _type_dot_identifier: $ => prec.right(
            DART_PREC.IMPORT_EXPORT,
            seq(
                '.',
                alias(
                    $.identifier,
                    $.type_identifier
                )
            )
        ),

        typed_identifier: $ => seq(
            $._type,
            $.identifier
        ),

        nullable_type: $ => prec(DART_PREC.BUILTIN, '?'),
        nullable_selector: $ => prec(DART_PREC.BUILTIN, '?'),

        floating_point_type: $ => token(
            'double'
        ),

        boolean_type: $ => prec(
            DART_PREC.BUILTIN,
            'bool',
        ),

        void_type: $ => token('void'),

        _var_or_type: $ => choice(
            $._type,
            seq(
                $.inferred_type,
                optional($._type)
            )
        ),

        _final_var_or_type: $ => choice($.inferred_type, $.final_builtin, seq(optional($.final_builtin), $._type)),

        inferred_type: $ => prec(
            DART_PREC.BUILTIN,
            'var',
        ),

        function_body: $ => choice(
            seq(
                optional('async'),
                '=>',
                $._expression,
                $._semicolon
            ),
            seq(
                optional(choice(
                    'async',
                    'async*',
                    'sync*',
                )),
                $.block
            )
        ),

        function_expression_body: $ => choice(
            seq(
                optional('async'),
                '=>',
                $._expression
            ),
            seq(
                optional(choice(
                    'async',
                    'async*',
                    'sync*',
                )),
                $.block
            )
        ),
        function_signature: $ => seq(
            // optional($._metadata),
            optional($._type),
            field('name', choice(
                alias(
                    $._get,
                    $.identifier, // this way the syntax still highlights consistently.
                ),
                alias(
                    $._set,
                    $.identifier, // this way the syntax still highlights consistently.
                ),
                // $._get,
                // $._set,
                $.identifier
            )),
            $._formal_parameter_part,
            optional($._native),
        ),

        // _get_identifier: $ => alias(
        //         $.identifier, // this way the syntax still highlights consistently.
        //         $._get
        //     ),

        _formal_parameter_part: $ => seq(
            optional($.type_parameters),
            $.formal_parameter_list
        ),


        formal_parameter_list: $ => $._strict_formal_parameter_list,

        _strict_formal_parameter_list: $ => choice(
            seq(
                '(',
                ')'
            ),
            seq(
                '(',
                $._normal_formal_parameters,
                optional(
                    ','
                ),
                ')'
            ),
            seq(
                '(',
                $._normal_formal_parameters,
                ',',
                $.optional_formal_parameters,
                ')'
            ),
            seq(
                '(',
                $.optional_formal_parameters,
                ')'
            )
        ),

        _normal_formal_parameters: $ => commaSep1($.formal_parameter),
        optional_formal_parameters: $ => choice(
            $._optional_postional_formal_parameters,
            $._named_formal_parameters
        ),

        positional_parameters: $ => seq(
            '[',
            commaSep1(
                $._default_formal_parameter
            ),
            ']'
        ),

        _optional_postional_formal_parameters: $ => seq(
            '[',
            commaSep1TrailingComma(
                $._default_formal_parameter
            ),
            ']'
        ),
        _named_formal_parameters: $ => seq(
            '{',
            commaSep1TrailingComma(
                $._default_named_parameter
            ),
            '}'
        ),

        formal_parameter: $ => $._normal_formal_parameter,

        _default_formal_parameter: $ => seq(
            $.formal_parameter,
            optional(
                seq(
                    '=',
                    $._expression
                )
            )
        ),
        _default_named_parameter: $ => choice(
            seq(
                optional(
                    $._metadata
                ),
                optional(
                    $._required
                ),
                $.formal_parameter,
                optional(
                    seq(
                        '=',
                        $._expression
                    )
                )
            ),
            seq(
                optional(
                    $._metadata
                ),
                optional(
                    $._required
                ),
                $.formal_parameter,
                optional(
                    seq(
                        ':',
                        $._expression
                    )
                )
            )
        ),

        _normal_formal_parameter: $ => seq(
            optional(
                $._metadata
            ),
            choice(
                $._function_formal_parameter,
                $._simple_formal_parameter,
                $.constructor_param,
                $.super_formal_parameter
            )
        ),

        _function_formal_parameter: $ => seq(
            optional(
                $._covariant
            ),
            optional(
                $._type
            ),
            $.identifier,
            $._formal_parameter_part,
            optional($.nullable_type)
        ),

        _simple_formal_parameter: $ => choice(
            $._declared_identifier,
            seq(
                optional(
                    $._covariant
                ),
                choice(
                    $.identifier,
                    alias($._get, $.identifier),
                    alias($._set, $.identifier),
                )
            )
        ),

        // see https://github.com/dart-lang/language/blob/31f3d2bd6fd83b2e5f5019adb276c23fd2900941/working/1855%20-%20super%20parameters/proposal.md
        super_formal_parameter: $ => seq(
            optional($._final_const_var_or_type),
            $.super,
            '.',
            $.identifier,
            optional($._formal_parameter_part)
        ),

        //constructor param = field formal parameter
        constructor_param: $ => seq(
            optional($._final_const_var_or_type),
            $.this,
            '.',
            $.identifier,
            optional($._formal_parameter_part)
        ),

        local_variable_declaration: $ => choice(
            seq(
                optional($._metadata),
                $.initialized_variable_definition,
                $._semicolon
            ),
            seq(
                optional($._metadata),
                $.pattern_variable_declaration,
                $._semicolon
            )
        ),

        script_tag: $ => seq('#!', /.+/, '\n'),

        // Dart 2.19+ allows the unnamed library directive `library;` to
        // attach a doc comment / annotation to a library file without a name.
        library_name: $ => seq(optional($._metadata), 'library', optional($.dotted_identifier_list), $._semicolon),

        dotted_identifier_list: $ => sep1($.identifier, '.'),

        _identifier_or_new: $ => choice($.identifier, $._new_builtin),

        qualified: $ => choice(
            seq($._type_name, '.', $._identifier_or_new),
            seq($._type_name, '.', $._type_name, '.', $._identifier_or_new),
        ),

        // Built in identifier tokens: These should be tokenized.
        //assert,break,case,
        // catch,
        // class,
        // const,
        // continue,
        // default,
        // do,
        // else,
        // enum,
        // extends,
        // false,
        // final,
        // finally,
        // for,
        // if,
        // in,
        // is,
        // new,
        // null,
        // rethrow,
        // return,
        // super,
        // switch,
        // this,
        // throw,
        // true,
        // try,
        // var,
        // void,
        // while,
        // with

        _as: $ => prec(
            DART_PREC.BUILTIN,
            'as',
        ),
        break_builtin: $ => token('break'),
        assert_builtin: $ => token('assert'),
        case_builtin: $ => token('case'),
        rethrow_builtin: $ => token('rethrow'),
        part_of_builtin: $ => token('part of'),
        _covariant: $ => prec(
            DART_PREC.BUILTIN,
            'covariant',
        ),
        _deferred: $ => prec(
            DART_PREC.BUILTIN,
            'deferred',
        ),
        _dynamic: $ => prec(
            DART_PREC.BUILTIN,
            'dynamic',
        ),
        _export: $ => prec(
            DART_PREC.BUILTIN,
            'export',
        ),
        _external: $ => $._external_builtin,
        _factory: $ => prec(
            DART_PREC.BUILTIN,
            'factory',
        ),
        _function_builtin_identifier: $ => prec(
            DART_PREC.BUILTIN,
            'Function',
        ),
        _get: $ => prec(
            DART_PREC.BUILTIN,
            'get',
        ),
        _native: $ => seq(
            'native', optional($.string_literal)
        ),
        _implements: $ => prec(
            DART_PREC.BUILTIN,
            'implements',
        ),
        _import: $ => prec(
            DART_PREC.BUILTIN,
            'import',
        ),
        interface: $ => prec(
            DART_PREC.BUILTIN,
            'interface',
        ),
        base: $ => prec(
            DART_PREC.BUILTIN,
            'base',
        ),
        abstract: $ => prec(
            DART_PREC.BUILTIN,
            'abstract',
        ),
        sealed: $ => prec(
            DART_PREC.BUILTIN,
            'sealed',
        ),
        _library: $ => prec(
            DART_PREC.BUILTIN,
            'library',
        ),
        _operator: $ => prec(
            DART_PREC.BUILTIN,
            'operator',
        ),
        mixin: $ => prec(
            DART_PREC.BUILTIN,
            'mixin',
        ),
        _part: $ => prec(
            DART_PREC.BUILTIN,
            'part',
        ),
        _required: $ => prec(
            DART_PREC.BUILTIN,
            'required',
        ),
        _set: $ => prec(
            DART_PREC.BUILTIN,
            'set',
        ),
        _static: $ => prec(
            DART_PREC.BUILTIN,
            'static',
        ),
        _typedef: $ => prec(
            DART_PREC.BUILTIN,
            'typedef',
        ),
        _new_builtin: $ => prec(
            DART_PREC.BUILTIN,
            'new',
        ),
        logical_and_operator: $ => prec(
            DART_PREC.BUILTIN,
            '&&',
        ),
        logical_or_operator: $ => prec(
            DART_PREC.BUILTIN,
            '||'
        ),
        const_builtin: $ => token('const'),
        final_builtin: $ => token('final'),
        _late_builtin: $ => prec(
            DART_PREC.BUILTIN,
            'late',
        ),

        _external_builtin: $ => prec(
            DART_PREC.BUILTIN,
            'external',
        ),

        this: $ => prec(
            DART_PREC.BUILTIN,
            'this',
        ),

        super: $ => prec(
            DART_PREC.BUILTIN,
            'super',
        ),

        // Per Dart spec, built-in identifiers may appear as labels
        // (e.g. as the key of a named argument: `f(get: x, set: y)`).
        label: $ => seq(
            choice(
                $.identifier,
                alias($._get, $.identifier),
                alias($._set, $.identifier),
                alias($._function_builtin_identifier, $.identifier),
            ),
            ':'
        ),

        _semicolon: $ => token(';'),

        identifier: $ => /[a-zA-Z_$][\w$]*/,
        identifier_dollar_escaped: $ => /([a-zA-Z_]|(\\\$))([\w]|(\\\$))*/,
        //TODO: add support for triple-slash comments as a special category.
        // Trying to add support for nested multiline comments.
        // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890

        // _line_comment: $ => token(seq(
        //     '//', /[^\/].*/
        //   )),
        // _documentation_line_comment: $ => token(seq('///', /.*/)),

        comment: $ => choice(
            $._block_comment,
            seq('//', /([^/\n].*)?/),
            seq(
                '/*',
                /[^*]*\*+([^/*][^*]*\*+)*/,
                '/'
            )
        ),
        //added nesting comments.
        documentation_comment: $ =>
            choice(
                $._documentation_block_comment,
                seq('///', /.*/),
            )
        ,
    }
});

function sep1(rule, separator) {
    return seq(rule, repeat(seq(separator, rule)));
}

function sep2(rule, separator) {
    return seq(rule, repeat1(seq(separator, rule)));
}

function commaSep1(rule) {
    return seq(rule, repeat(seq(',', rule)))
}

function commaSep(rule) {
    return optional(commaSep1(rule))
}

function commaSep2TrailingComma(rule) {
    return seq(rule, repeat1(seq(',', rule)), optional(','))
}

function commaSep1TrailingComma(rule) {
    return seq(rule, repeat(seq(',', rule)), optional(','))
}

function commaSepTrailingComma(rule) {
    return optional(commaSep1TrailingComma(rule))
}

function pureBinaryRun(rule, separator, precedence) {
    return prec.left(
        precedence,
        choice(
            sep2(
                rule,
                separator
            )))
}

function binaryRunLeft(rule, separator, superItem, precedence) {
    return prec.left( //left
        precedence,
        choice(
            sep2(
                // $.bitwise_xor_expression,
                rule,
                separator
            ),
            seq(
                superItem,
                repeat1(
                    seq(
                        separator,
                        rule,
                        // $.bitwise_xor_expression
                    )
                )
            )
        )
    )
}
