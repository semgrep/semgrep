/**
 * @file C# grammar for tree-sitter
 * @author Max Brunsfeld <maxbrunsfeld@gmail.com>
 * @author Damien Guard <damieng@gmail.com>
 * @author Amaan Qureshi <amaanq12@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = {
  GENERIC: 19,
  DOT: 18,
  INVOCATION: 18,
  POSTFIX: 18,
  PREFIX: 17,
  UNARY: 17,
  CAST: 17,
  RANGE: 16,
  SWITCH: 15,
  WITH: 14,
  MULT: 13,
  ADD: 12,
  SHIFT: 11,
  REL: 10,
  EQUAL: 9,
  AND: 8,
  XOR: 7,
  OR: 6,
  LOGICAL_AND: 5,
  LOGICAL_OR: 4,
  COALESCING: 3,
  CONDITIONAL: 2,
  ASSIGN: 1,
  SELECT: 0,
};

const decimalDigitSequence = /([0-9][0-9_]*[0-9]|[0-9])/;

const stringEncoding = /(u|U)8/;

export default grammar({
  name: 'c_sharp',

  conflicts: $ => [
    [$._simple_name, $.generic_name],
    [$._simple_name, $.type_parameter],
    [$._simple_name, $.subpattern],

    [$.tuple_element, $.using_variable_declarator],
    [$.tuple_element, $.declaration_expression],

    [$.tuple_pattern, $.parameter],
    [$.tuple_pattern, $._simple_name],

    [$.lvalue_expression, $._name],
    [$.parameter, $.lvalue_expression],
    // C# 14 null-conditional assignment: `obj?.x = v` requires
    // `conditional_access_expression` to appear in both lvalue and
    // non-lvalue contexts. Tree-sitter needs GLR for the decision.
    [$.lvalue_expression, $.non_lvalue_expression],

    [$.type, $.attribute],
    [$.type, $.nullable_type],
    [$.type, $.nullable_type, $.array_creation_expression],
    [$.type, $._array_base_type],
    [$.type, $._array_base_type, $.array_creation_expression],
    [$.type, $.array_creation_expression],
    [$.type, $._pointer_base_type],

    [$.qualified_name, $.member_access_expression],
    [$.qualified_name, $.explicit_interface_specifier],

    [$._array_base_type, $.stackalloc_expression],

    [$.constant_pattern, $.non_lvalue_expression],
    [$.constant_pattern, $._expression_statement_expression],
    [$.constant_pattern, $.lvalue_expression],
    [$.constant_pattern, $._name],
    [$.constant_pattern, $.lvalue_expression, $._name],

    [$.type, $._name_invocation_pattern, $.recursive_pattern],
    [$.attribute, $.type, $._name_invocation_pattern, $.recursive_pattern],

    [$.parenthesized_pattern, $._parenthesized_pattern_with_designation],

    [$.expression_element, $.argument],
    [$.spread_element, $.range_expression],
    [$._list_pattern_body, $.collection_expression],

    [$._reserved_identifier, $.modifier],
    [$.preproc_if_in_expression, $.preproc_if_in_initializer],
    [$.preproc_if_in_expression, $.preproc_if_in_collection],
    [$._reserved_identifier, $.scoped_type],
    [$._reserved_identifier, $.implicit_type],
    [$._reserved_identifier, $.from_clause],
    [$._reserved_identifier, $.implicit_type, $.var_pattern],
    [$._reserved_identifier, $.type_parameter_constraint],
    [$._reserved_identifier, $.parameter, $.scoped_type],
    [$._reserved_identifier, $.parameter],
    [$._simple_name, $.parameter],
    [$.tuple_element, $.parameter, $.declaration_expression],
    [$.parameter, $.tuple_element],

    [$.event_declaration, $.variable_declarator],

    [$.base_list],
    [$.using_directive, $.modifier],
    [$.using_directive],

    [$._constructor_declaration_initializer, $._simple_name],

    // For C# 14 extension declarations: `extension(scoped X x)` —
    // `scoped` can be a parameter modifier, a scoped_type, or a
    // reserved identifier (when there's no further type). Keep all
    // three alive until later tokens disambiguate.
    [$.receiver_parameter, $.scoped_type, $._reserved_identifier],
  ],

  externals: $ => [
    $._optional_semi,
    $.interpolation_regular_start,
    $.interpolation_verbatim_start,
    $.interpolation_raw_start,
    $.interpolation_start_quote,
    $.interpolation_end_quote,
    $.interpolation_open_brace,
    $.interpolation_close_brace,
    $.interpolation_string_content,
    $.raw_string_start,
    $.raw_string_end,
    $.raw_string_content,
    // C# 14: emitted by the scanner at '(' when forward-scanning shows a
    // simple-lambda parameter list (at least one element has a parameter
    // modifier) closed by ')=>'.
    $._lambda_paren_open,
    // `#if` immediately followed (after the condition line) by `else`.
    $._preproc_else_if_start,
  ],

  extras: $ => [
    /[\s\u00A0\uFEFF\u3000]+/,
    $.comment,
    $.preproc_region,
    $.preproc_endregion,
    $.preproc_line,
    $.preproc_pragma,
    $.preproc_nullable,
    $.preproc_error,
    $.preproc_warning,
    $.preproc_define,
    $.preproc_undef,
    $.ignored_directive,
  ],

  inline: $ => [
    $._namespace_member_declaration,
    $._object_creation_type,
    $._nullable_base_type,
    $._parameter_type_with_modifiers,
    $._top_level_item_no_statement,
  ],

  precedences: $ => [
    [$._anonymous_object_member_declarator, $._simple_name],
    [$.block, $.initializer_expression],
  ],

  supertypes: $ => [
    $.declaration,
    $.expression,
    $.non_lvalue_expression,
    $.lvalue_expression,
    $.literal,
    $.statement,
    $.type,
    $.type_declaration,
    $.pattern,
  ],

  word: $ => $._identifier_token,

  rules: {
    compilation_unit: $ => seq(
      optional($.shebang_directive),
      repeat($._top_level_item),
    ),

    _top_level_item: $ => prec(2, choice(
      $._top_level_item_no_statement,
      $.global_statement,
    )),

    _top_level_item_no_statement: $ => choice(
      $.extern_alias_directive,
      $.using_directive,
      $.global_attribute,
      alias($.preproc_if_in_top_level, $.preproc_if),
      $._namespace_member_declaration,
      $.file_scoped_namespace_declaration,
    ),

    global_statement: $ => prec(1, $.statement),

    extern_alias_directive: $ => seq('extern', 'alias', field('name', $.identifier), ';'),

    using_directive: $ => seq(
      optional('global'),
      'using',
      choice(
        seq(
          optional('unsafe'),
          field('name', $.identifier),
          '=',
          $.type,
        ),
        seq(
          repeat(choice('static', 'unsafe')),
          $._name,
        ),
      ),
      ';',
    ),

    global_attribute: $ => seq(
      '[',
      choice('assembly', 'module'),
      ':',
      commaSep1($.attribute),
      optional(','),
      ']',
    ),

    attribute: $ => seq(
      field('name', $._name),
      optional($.attribute_argument_list),
    ),

    attribute_argument_list: $ => prec(-1, seq(
      '(',
      commaSep($.attribute_argument),
      ')',
    )),

    attribute_argument: $ => prec(-1, seq(
      optional(prec(1, seq(field('name', $.identifier), choice(':', '=')))),
      $.expression,
    )),

    attribute_list: $ => seq(
      '[',
      optional($.attribute_target_specifier),
      commaSep1($.attribute),
      optional(','),
      ']',
    ),

    _attribute_list: $ => choice($.attribute_list, $.preproc_if_in_attribute_list),

    // Higher precedence than `_reserved_identifier`: when both interpretations
    // are valid (e.g. `[field :` could be `_reserved_identifier` followed by
    // bogus `:`, or `attribute_target_specifier`), prefer the target-specifier
    // reading. `_reserved_identifier` still wins when no `:` follows, so
    // `[field]` continues to parse as collection_expression with identifier.
    attribute_target_specifier: _ => prec(1, seq(
      choice('field', 'event', 'method', 'param', 'property', 'return', 'type', 'typevar'),
      ':',
    )),

    _namespace_member_declaration: $ => choice(
      $.namespace_declaration,
      $.type_declaration,
    ),

    namespace_declaration: $ => seq(
      'namespace',
      field('name', $._name),
      field('body', $.declaration_list),
      $._optional_semi,
    ),

    file_scoped_namespace_declaration: $ => seq(
      'namespace',
      field('name', $._name),
      ';',
    ),

    type_declaration: $ => choice(
      $.class_declaration,
      $.struct_declaration,
      $.enum_declaration,
      $.interface_declaration,
      $.delegate_declaration,
      $.record_declaration,
    ),

    class_declaration: $ => seq(
      $._class_declaration_initializer,
      $._declaration_list_body,
    ),

    _class_declaration_initializer: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      'class',
      field('name', $.identifier),
      repeat(choice($.type_parameter_list, $.parameter_list, $.base_list)),
      repeat($.type_parameter_constraints_clause),
    ),

    struct_declaration: $ => seq(
      $._struct_declaration_initializer,
      $._declaration_list_body,
    ),

    _struct_declaration_initializer: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      // C# requires `partial` to be the LAST modifier before the type keyword,
      // so the legal order is `ref partial struct` — `ref` cannot live inside
      // the modifier repeat without swallowing ref types elsewhere.
      optional(seq('ref', optional('partial'))),
      'struct',
      field('name', $.identifier),
      repeat(choice($.type_parameter_list, $.parameter_list, $.base_list)),
      repeat($.type_parameter_constraints_clause),
    ),

    enum_declaration: $ => seq(
      $._enum_declaration_initializer,
      choice(
        seq(field('body', $.enum_member_declaration_list), $._optional_semi),
        ';',
      ),
    ),

    _enum_declaration_initializer: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      'enum',
      field('name', $.identifier),
      optional($.base_list),
    ),

    enum_member_declaration_list: $ => seq(
      '{',
      commaSep(choice(
        $.enum_member_declaration,
        alias($.preproc_if_in_enum_member_declaration, $.preproc_if),
      )),
      optional(','),
      '}',
    ),

    enum_member_declaration: $ => seq(
      repeat($._attribute_list),
      field('name', $.identifier),
      optional(seq('=', field('value', $.expression))),
    ),

    interface_declaration: $ => seq(
      $._interface_declaration_initializer,
      $._declaration_list_body,
    ),

    _interface_declaration_initializer: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      'interface',
      field('name', $.identifier),
      field('type_parameters', optional($.type_parameter_list)),
      optional($.base_list),
      repeat($.type_parameter_constraints_clause),
    ),

    delegate_declaration: $ => seq(
      $._delegate_declaration_initializer,
      repeat($.type_parameter_constraints_clause),
      ';',
    ),

    _delegate_declaration_initializer: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      'delegate',
      field('type', $.type),
      field('name', $.identifier),
      field('type_parameters', optional($.type_parameter_list)),
      field('parameters', $.parameter_list),
    ),

    record_declaration: $ => seq(
      $._record_declaration_initializer,
      $._declaration_list_body,
    ),

    _record_declaration_initializer: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      'record',
      optional(choice('class', 'struct')),
      field('name', $.identifier),
      repeat(choice($.type_parameter_list, $.parameter_list)),
      optional(alias($.record_base, $.base_list)),
      repeat($.type_parameter_constraints_clause),
    ),

    record_base: $ => choice(
      seq(':', commaSep1($._name)),
      seq(':', $.primary_constructor_base_type, optional(seq(',', commaSep1($._name)))),
    ),

    _declaration_list_body: $ => choice(
      seq(field('body', $.declaration_list), $._optional_semi),
      ';',
    ),

    primary_constructor_base_type: $ => seq(
      field('type', $._name),
      $.argument_list,
    ),

    modifier: _ => prec.right(choice(
      'abstract',
      'async',
      'const',
      'extern',
      'file',
      'fixed',
      'internal',
      'new',
      'override',
      'partial',
      'private',
      'protected',
      'public',
      'readonly',
      'required',
      // 'ref',     // `ref` as a modifier can only be used on struct declarations. Other than that it's a ref type or a ref parameter in a declaration.
      // 'scoped',  // `scoped` is either part of a scoped type or a scoped parameter. Both of which are handled outside of `modifier`.
      'sealed',
      'static',
      'unsafe',
      'virtual',
      'volatile',

    )),

    type_parameter_list: $ => seq('<', commaSep1($.type_parameter), '>'),

    type_parameter: $ => seq(
      repeat($._attribute_list),
      optional(choice('in', 'out')),
      field('name', $.identifier),
    ),

    base_list: $ => seq(':', commaSep1(seq($.type, optional($.argument_list)))),

    type_parameter_constraints_clause: $ => seq(
      'where',
      $.identifier,
      ':',
      commaSep1($.type_parameter_constraint),
    ),

    type_parameter_constraint: $ => choice(
      seq('class', optional('?')),
      'struct',
      'notnull',
      'unmanaged',
      $.constructor_constraint,
      field('type', $.type),
    ),

    constructor_constraint: _ => seq('new', '(', ')'),

    operator_declaration: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      field('type', $.type),
      optional($.explicit_interface_specifier),
      'operator',
      optional('checked'),
      field('operator', choice(
        '!',
        '~',
        '++',
        '--',
        'true',
        'false',
        '+', '-',
        '*', '/',
        '%', '^',
        '|', '&',
        '<<', '>>', '>>>',
        '==', '!=',
        '>', '<',
        '>=', '<=',
        // C# 14: user-defined compound assignment operators.
        // https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-14#user-defined-compound-assignment-operators
        '+=', '-=',
        '*=', '/=',
        '%=', '^=',
        '|=', '&=',
        '<<=', '>>=', '>>>=',
      )),
      field('parameters', $.parameter_list),
      $._function_body,
    ),

    conversion_operator_declaration: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      choice(
        'implicit',
        'explicit',
      ),
      repeat1(choice( // Intentionally structured this way for grammar size
        $.explicit_interface_specifier,
        'operator',
        'checked',
      )),
      field('type', $.type),
      field('parameters', $.parameter_list),
      $._function_body,
    ),

    declaration_list: $ => seq(
      '{',
      repeat($.declaration),
      '}',
    ),

    declaration: $ => choice(
      $.class_declaration,
      $.struct_declaration,
      $.enum_declaration,
      $.delegate_declaration,
      $.field_declaration,
      $.method_declaration,
      $.event_declaration,
      $.event_field_declaration,
      $.record_declaration,
      $.constructor_declaration,
      $.destructor_declaration,
      $.indexer_declaration,
      $.interface_declaration,
      $.namespace_declaration,
      $.operator_declaration,
      $.conversion_operator_declaration,
      $.property_declaration,
      $.using_directive,
      $.preproc_if,
      $.extension_declaration,
    ),

    // C# 14: extension declarations introduce extension methods, properties,
    // and operators with a shared receiver inside a non-generic static class.
    // https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/proposals/csharp-14.0/extensions
    //
    //   extension_declaration:
    //     'extension' type_parameter_list? '(' receiver_parameter ')'
    //         type_parameter_constraints_clause* extension_body
    //   extension_body: '{' extension_member_declaration* '}' ';'?
    //   extension_member_declaration:
    //     method_declaration | property_declaration | operator_declaration
    //   receiver_parameter: attributes? parameter_modifiers? type identifier?
    //
    // `extension` is contextual; it is recognized as the start of an
    // extension declaration when followed by `<` or `(` in declaration
    // position. Use prec.dynamic so an identifier named `extension` in
    // other positions still parses.
    extension_declaration: $ => prec.dynamic(1, seq(
      repeat($._attribute_list),
      'extension',
      optional($.type_parameter_list),
      '(',
      $.receiver_parameter,
      ')',
      repeat($.type_parameter_constraints_clause),
      $.extension_body,
    )),

    receiver_parameter: $ => seq(
      repeat($._attribute_list),
      $._parameter_type_with_modifiers,
      optional(field('name', $.identifier)),
    ),

    extension_body: $ => seq(
      '{',
      repeat($._extension_member_declaration),
      '}',
      optional(';'),
    ),

    _extension_member_declaration: $ => choice(
      $.method_declaration,
      $.property_declaration,
      $.operator_declaration,
    ),

    field_declaration: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      $.variable_declaration,
      ';',
    ),

    constructor_declaration: $ => seq(
      $._constructor_declaration_initializer,
      $._function_body,
    ),

    _constructor_declaration_initializer: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      field('name', $.identifier),
      field('parameters', $.parameter_list),
      optional($.constructor_initializer),
    ),

    destructor_declaration: $ => seq(
      repeat($._attribute_list),
      optional('extern'),
      '~',
      field('name', $.identifier),
      field('parameters', $.parameter_list),
      $._function_body,
    ),

    method_declaration: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      field('returns', $.type),
      optional($.explicit_interface_specifier),
      field('name', $.identifier),
      field('type_parameters', optional($.type_parameter_list)),
      field('parameters', $.parameter_list),
      repeat($.type_parameter_constraints_clause),
      $._function_body,
    ),

    event_declaration: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      'event',
      field('type', $.type),
      optional($.explicit_interface_specifier),
      field('name', $.identifier),
      choice(
        field('accessors', $.accessor_list),
        ';',
      ),
    ),

    event_field_declaration: $ => prec.dynamic(1, seq(
      repeat($._attribute_list),
      repeat($.modifier),
      'event',
      $.variable_declaration,
      ';',
    )),

    accessor_list: $ => seq(
      '{',
      repeat($.accessor_declaration),
      '}',
    ),

    accessor_declaration: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      field('name', choice('get', 'set', 'add', 'remove', 'init', $.identifier)),
      $._function_body,
    ),

    indexer_declaration: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      field('type', $.type),
      optional($.explicit_interface_specifier),
      'this',
      field('parameters', $.bracketed_parameter_list),
      choice(
        field('accessors', $.accessor_list),
        seq(field('value', $.arrow_expression_clause), ';'),
      ),
    ),

    bracketed_parameter_list: $ => seq(
      '[',
      sep(choice($.parameter, $._parameter_array), ','),
      ']',
    ),

    property_declaration: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      field('type', $.type),
      optional($.explicit_interface_specifier),
      field('name', $.identifier),
      choice(
        seq(
          field('accessors', $.accessor_list),
          optional(seq('=', field('value', $.expression), ';')),
        ),
        seq(
          field('value', $.arrow_expression_clause),
          ';',
        ),
      ),
    ),

    explicit_interface_specifier: $ => prec(PREC.DOT, seq(
      $._name,
      '.',
    )),

    parameter_list: $ => seq(
      '(',
      sep(choice($.parameter, $._parameter_array), ','),
      ')',
    ),

    _parameter_type_with_modifiers: $ => seq(
      repeat(choice(
        prec.left(alias(choice('this', 'scoped', 'ref', 'readonly'), $.modifier)),
        // `in`/`out` also start an `argument` (prec 1), which won the shift/reduce
        // against this reduction and sent `(in T x) => ...` to ERROR.
        prec.left(1, alias(choice('out', 'in'), $.modifier)),
      )),
      field('type', $.type),
    ),

    parameter: $ => seq(
      repeat($._attribute_list),
      optional($._parameter_type_with_modifiers),
      field('name', $.identifier),
      optional(seq('=', $.expression)),
    ),

    _parameter_array: $ => seq(
      repeat($._attribute_list),
      'params',
      field('type', $.type),
      field('name', $.identifier),
    ),

    constructor_initializer: $ => seq(
      ':',
      choice('base', 'this'),
      $.argument_list,
    ),

    argument_list: $ => seq('(', commaSep($.argument), ')'),

    tuple_pattern: $ => seq(
      '(',
      commaSep1(choice(
        field('name', $.identifier),
        $.discard,
        $.tuple_pattern,
      )),
      ')',
    ),

    argument: $ => prec(1, seq(
      optional(seq(field('name', $.identifier), ':')),
      optional(choice('ref', 'out', 'in')),
      choice(
        $.expression,
        $.declaration_expression,
      ),
    )),

    block: $ => seq('{', repeat($.statement), '}'),

    arrow_expression_clause: $ => seq('=>', $.expression),

    _function_body: $ => choice(
      $._plain_function_body,
      // `void M() #if A { } #else => x; #endif`: the body differs per branch.
      alias($.preproc_if_in_function_body, $.preproc_if),
    ),

    _plain_function_body: $ => choice(
      field('body', $.block),
      seq(field('body', $.arrow_expression_clause), ';'),
      ';',
    ),

    variable_declaration: $ => seq(
      field('type', $.type),
      commaSep1($.variable_declarator),
    ),

    using_variable_declaration: $ => seq(
      field('type', $.type),
      commaSep1(alias($.using_variable_declarator, $.variable_declarator)),
    ),

    variable_declarator: $ => seq(
      choice(field('name', $.identifier), $.tuple_pattern),
      optional($.bracketed_argument_list),
      optional(seq('=', $.expression)),
    ),

    using_variable_declarator: $ => seq(
      field('name', $.identifier),
      optional(seq('=', $.expression)),
    ),

    bracketed_argument_list: $ => seq(
      '[',
      commaSep1($.argument),
      optional(','),
      ']',
    ),

    qualified_identifier: $ => sep1($.identifier, '.'),

    _name: $ => choice(
      $.alias_qualified_name,
      $.qualified_name,
      $._simple_name,
    ),

    alias_qualified_name: $ => seq(
      field('alias', $.identifier),
      '::',
      field('name', $._simple_name),
    ),

    _simple_name: $ => choice(
      $.identifier,
      $.generic_name,
    ),

    qualified_name: $ => prec(PREC.DOT, seq(
      field('qualifier', $._name),
      '.',
      field('name', $._simple_name),
    )),

    generic_name: $ => seq($.identifier, $.type_argument_list),

    type_argument_list: $ => seq(
      '<',
      choice(
        repeat(','),
        commaSep1($.type),
      ),
      '>',
    ),

    type: $ => choice(
      $.implicit_type,
      $.array_type,
      $._name,
      $.nullable_type,
      $.pointer_type,
      $.function_pointer_type,
      $.predefined_type,
      $.tuple_type,
      $.ref_type,
      $.scoped_type,
    ),

    implicit_type: _ => prec.dynamic(1, 'var'),

    array_type: $ => seq(
      field('type', $._array_base_type),
      field('rank', $.array_rank_specifier),
    ),

    _array_base_type: $ => choice(
      $.array_type,
      $._name,
      $.nullable_type,
      $.pointer_type,
      $.function_pointer_type,
      $.predefined_type,
      $.tuple_type,
    ),

    array_rank_specifier: $ => seq(
      '[',
      commaSep(optional($.expression)),
      ']',
    ),

    nullable_type: $ => seq(field('type', $._nullable_base_type), '?'),

    _nullable_base_type: $ => choice(
      $.array_type,
      $._name,
      $.predefined_type,
      $.tuple_type,
    ),

    pointer_type: $ => seq(field('type', $._pointer_base_type), '*'),

    _pointer_base_type: $ => choice(
      $._name,
      $.nullable_type,
      $.pointer_type,
      $.function_pointer_type,
      $.predefined_type,
      $.tuple_type,
    ),

    function_pointer_type: $ => seq(
      'delegate',
      '*',
      optional($.calling_convention),
      '<',
      repeat(seq($.function_pointer_parameter, ',')),
      field('returns', $.type),
      '>',
    ),

    calling_convention: $ => choice(
      'managed',
      seq(
        'unmanaged',
        optional(seq(
          '[',
          commaSep1(choice(
            'Cdecl',
            'Stdcall',
            'Thiscall',
            'Fastcall',
            $.identifier,
          )),
          ']',
        )),
      ),
    ),

    function_pointer_parameter: $ => seq(
      optional(choice('ref', 'out', 'in')),
      field('type', $._ref_base_type),
    ),

    predefined_type: _ => token(choice(
      'bool',
      'byte',
      'char',
      'decimal',
      'double',
      'float',
      'int',
      'long',
      'object',
      'sbyte',
      'short',
      'string',
      'uint',
      'ulong',
      'ushort',
      'nint',
      'nuint',
      'void',
    )),

    ref_type: $ => seq(
      'ref',
      optional('readonly'),
      field('type', $.type),
    ),

    _ref_base_type: $ => choice(
      $.implicit_type,
      $._name,
      $.nullable_type,
      $.array_type,
      $.pointer_type,
      $.function_pointer_type,
      $.predefined_type,
      $.tuple_type,
    ),

    scoped_type: $ => seq(
      'scoped',
      field('type', $._scoped_base_type),
    ),

    _scoped_base_type: $ => choice(
      $._name,
      $.ref_type,
    ),

    tuple_type: $ => seq(
      '(',
      commaSep2($.tuple_element),
      ')',
    ),

    tuple_element: $ => seq(
      field('type', $.type),
      field('name', optional($.identifier)),
    ),

    statement: $ => prec(1, choice(
      $.block,
      $.break_statement,
      $.checked_statement,
      $.continue_statement,
      $.do_statement,
      $.empty_statement,
      $.expression_statement,
      $.fixed_statement,
      $.for_statement,
      $.return_statement,
      $.lock_statement,
      $.yield_statement,
      $.switch_statement,
      $.throw_statement,
      $.try_statement,
      $.unsafe_statement,
      $.using_statement,
      $.foreach_statement,
      $.goto_statement,
      $.labeled_statement,
      $.if_statement,
      $.while_statement,
      $.local_declaration_statement,
      $.local_function_statement,
      alias($.preproc_if_in_top_level, $.preproc_if),
    )),

    break_statement: _ => seq('break', ';'),

    checked_statement: $ => seq(choice('checked', 'unchecked'), $.block),

    continue_statement: _ => seq('continue', ';'),

    do_statement: $ => seq(
      'do',
      field('body', $.statement),
      'while',
      '(',
      field('condition', $.expression),
      ')',
      ';',
    ),

    empty_statement: _ => ';',

    expression_statement: $ => seq($._expression_statement_expression, ';'),

    fixed_statement: $ => seq('fixed', '(', $.variable_declaration, ')', $.statement),

    for_statement: $ => seq(
      'for',
      $._for_statement_conditions,
      field('body', $.statement),
    ),

    _for_statement_conditions: $ => seq(
      '(',
      field('initializer', optional(
        choice($.variable_declaration, commaSep1($.expression)),
      )),
      ';',
      field('condition', optional($.expression)),
      ';',
      field('update', optional(commaSep1($.expression))),
      ')',
    ),

    return_statement: $ => seq(
      'return',
      choice(
        seq(optional($.expression), ';'),
        // `return c ? a : #if X b; #else d; #endif`: the `;` is inside each branch.
        alias($._conditional_ending_in_preproc, $.conditional_expression),
      ),
    ),

    _conditional_ending_in_preproc: $ => seq(
      field('condition', $.expression),
      '?',
      field('consequence', $.expression),
      ':',
      field('alternative', alias($.preproc_if_in_statement_tail, $.preproc_if)),
    ),

    lock_statement: $ => seq('lock', '(', $.expression, ')', $.statement),

    yield_statement: $ => seq(
      'yield',
      choice(
        seq('return', $.expression),
        'break',
      ),
      ';',
    ),

    switch_statement: $ => seq(
      'switch',
      choice(
        seq(
          '(',
          field('value', $.expression),
          ')',
        ),
        field('value', $.tuple_expression),
      ),
      field('body', $.switch_body),
    ),

    switch_body: $ => seq('{', repeat($.switch_section), '}'),

    switch_section: $ => prec.left(seq(
      choice(
        seq(
          'case',
          choice(
            $.expression,
            seq($.pattern, optional($.when_clause)),
          ),
        ),
        'default',
      ),
      ':',
      repeat($.statement),
    )),

    throw_statement: $ => seq('throw', optional($.expression), ';'),

    try_statement: $ => seq(
      'try',
      field('body', $.block),
      repeat($.catch_clause),
      optional($.finally_clause),
    ),

    catch_clause: $ => seq(
      'catch',
      repeat(choice($.catch_declaration, $.catch_filter_clause)),
      field('body', $.block),
    ),

    catch_declaration: $ => seq(
      '(',
      field('type', $.type),
      optional(field('name', $.identifier)),
      ')',
    ),

    catch_filter_clause: $ => seq('when', '(', $.expression, ')'),

    finally_clause: $ => seq('finally', $.block),

    unsafe_statement: $ => seq('unsafe', $.block),

    using_statement: $ => seq(
      optional('await'),
      'using',
      '(',
      choice(
        alias($.using_variable_declaration, $.variable_declaration),
        $.expression,
      ),
      ')',
      field('body', $.statement),
    ),

    foreach_statement: $ => seq(
      $._foreach_statement_initializer,
      field('body', $.statement),
    ),

    _foreach_statement_initializer: $ => seq(
      optional('await'),
      'foreach',
      '(',
      choice(
        seq(
          field('type', $.type),
          field('left', choice($.identifier, $.tuple_pattern)),
        ),
        field('left', $.expression),
      ),
      'in',
      field('right', $.expression),
      ')',
    ),

    goto_statement: $ => seq(
      'goto',
      optional(choice('case', 'default')),
      optional($.expression),
      ';',
    ),

    labeled_statement: $ => seq(
      $.identifier,
      ':',
      $.statement,
    ),

    if_statement: $ => prec.right(seq(
      'if',
      '(',
      field('condition', $.expression),
      ')',
      field('consequence', $.statement),
      optional(choice(
        seq(
          'else',
          field('alternative', $.statement),
        ),
        // `if (a) { } #if X else if (b) { } #endif`: the `else` lives in the block.
        // A plain `else` may still follow `#endif` and continue the chain.
        seq(
          alias($.preproc_if_in_else, $.preproc_if),
          optional(seq(
            'else',
            field('alternative', $.statement),
          )),
        ),
      )),
    )),

    while_statement: $ => seq(
      'while',
      '(',
      field('condition', $.expression),
      ')',
      field('body', $.statement),
    ),

    local_declaration_statement: $ => seq(
      optional('await'),
      optional('using'),
      repeat($.modifier),
      $.variable_declaration,
      ';',
    ),

    local_function_statement: $ => seq(
      $._local_function_declaration,
      repeat($.type_parameter_constraints_clause),
      $._function_body,
    ),

    _local_function_declaration: $ => seq(
      repeat($._attribute_list),
      repeat($.modifier),
      field('type', $.type),
      field('name', $.identifier),
      field('type_parameters', optional($.type_parameter_list)),
      field('parameters', $.parameter_list),
    ),

    pattern: $ => choice(
      $.constant_pattern,
      $.declaration_pattern,
      $.discard,
      $.recursive_pattern,
      $.var_pattern,
      $.negated_pattern,
      // This must come before plain parenthesized_pattern to create GLR conflict
      prec.dynamic(1, alias($._parenthesized_pattern_with_designation, $.recursive_pattern)),
      $.parenthesized_pattern,
      $.relational_pattern,
      $.or_pattern,
      $.and_pattern,
      $.list_pattern,
      $.type_pattern,
    ),

    // Uses '(' pattern ')' to create direct conflict with parenthesized_pattern
    _parenthesized_pattern_with_designation: $ => seq(
      '(',
      $.pattern,
      ')',
      $._variable_designation,
    ),

    constant_pattern: $ => choice(
      $.binary_expression,
      $.checked_expression,
      $.default_expression,
      $.interpolated_string_expression,
      $.parenthesized_expression,
      $.postfix_unary_expression,
      $.prefix_unary_expression,
      $.sizeof_expression,
      $.tuple_expression,
      $.typeof_expression,
      $.member_access_expression,
      alias($._name_invocation_pattern, $.invocation_expression),
      alias($._complex_invocation_expression, $.invocation_expression),
      $.cast_expression,
      $._simple_name,
      $.literal,
    ),

    // Invocation with name - creates conflict with recursive_pattern's Name(positional_pattern_clause)
    _name_invocation_pattern: $ => seq(
      field('function', $._name),
      field('arguments', $.argument_list),
    ),

    // Invocation where function is not a simple name
    _complex_invocation_expression: $ => prec(PREC.INVOCATION, seq(
      field('function', choice(
        $.member_access_expression,
        $.element_access_expression,
        $.invocation_expression,
        $.parenthesized_expression,
        $.conditional_access_expression,
        $.cast_expression,
      )),
      field('arguments', $.argument_list),
    )),

    discard: _ => '_',

    parenthesized_pattern: $ => seq('(', $.pattern, ')'),

    var_pattern: $ => seq('var', $._variable_designation),

    // Precedence above _variable_designation so `case int when x:` reduces
    // to a type pattern before `when` instead of using `when` as a name.
    type_pattern: $ => prec.right(2, field('type', $.type)),

    // The designation-less form outranks the one with a designation so that, on
    // `when`, the parser reduces here and lets `when_clause` take it instead of
    // reading `when` as the designation: `[var a] when Is(a) => ...`.
    list_pattern: $ => choice(
      prec.right(2, $._list_pattern_body),
      prec.right(1, seq($._list_pattern_body, $._variable_designation)),
    ),

    _list_pattern_body: $ => seq(
      '[',
      optional(seq(
        commaSep1(choice($.pattern, $.slice_pattern)),
        optional(','),
      )),
      ']',
    ),

    // C# 11 slice pattern: `..` optionally followed by a subpattern that
    // binds the captured slice. `[a, .. var rest, z]`, `[..]` (bare),
    // `[.. List<int> rest]` (declaration_pattern), etc.
    slice_pattern: $ => choice(
      prec(PREC.RANGE + 1, '..'),
      prec.right(seq('..', $.pattern)),
    ),

    recursive_pattern: $ => prec.left(choice(
      // name followed by positional pattern WITH variable designation
      prec.dynamic(1, seq(
        field('type', $._name),
        $.positional_pattern_clause,
        optional($.property_pattern_clause),
        $._variable_designation,
      )),
      // name followed by positional pattern WITHOUT variable designation
      prec.dynamic(-1, seq(
        field('type', $._name),
        $.positional_pattern_clause,
        optional($.property_pattern_clause),
      )),
      // positional pattern with variable designation (no type prefix)
      prec.dynamic(1, seq(
        $.positional_pattern_clause,
        $._variable_designation,
      )),
      // positional pattern without variable designation (no type prefix)
      $.positional_pattern_clause,
      // other type followed by pattern clauses (type is required here to avoid ambiguity)
      seq(
        field('type', $.type),
        choice(
          seq(
            $.positional_pattern_clause,
            optional($.property_pattern_clause),
          ),
          $.property_pattern_clause,
        ),
        optional($._variable_designation),
      ),
      // no type, just pattern clauses (no variable designation to avoid conflict with above)
      seq(
        choice(
          seq(
            $.positional_pattern_clause,
            $.property_pattern_clause,
          ),
          $.property_pattern_clause,
        ),
        optional($._variable_designation),
      ),
    )),

    positional_pattern_clause: $ => prec(1, seq(
      '(',
      optional(commaSep($.subpattern)),
      ')',
    )),

    property_pattern_clause: $ => prec(1, seq(
      '{',
      commaSep($.subpattern),
      optional(','),
      '}',
    )),

    subpattern: $ => prec.right(seq(
      optional(
        choice(
          seq($.expression, ':'),
          seq($.identifier, ':'),
        ),
      ),
      $.pattern,
    )),

    relational_pattern: $ => choice(
      seq('<', $.expression),
      seq('<=', $.expression),
      seq('>', $.expression),
      seq('>=', $.expression),
    ),

    negated_pattern: $ => seq('not', $.pattern),

    and_pattern: $ => prec.left(PREC.AND, seq(
      field('left', $.pattern),
      field('operator', 'and'),
      field('right', $.pattern),
    )),

    or_pattern: $ => prec.left(PREC.OR, seq(
      field('left', $.pattern),
      field('operator', 'or'),
      field('right', $.pattern),
    )),

    declaration_pattern: $ => seq(
      field('type', $.type),
      $._variable_designation,
    ),

    _variable_designation: $ => prec(1, choice(
      $.discard,
      $.parenthesized_variable_designation,
      field('name', $.identifier),
    )),

    parenthesized_variable_designation: $ => seq(
      '(',
      commaSep($._variable_designation),
      ')',
    ),

    expression: $ => choice(
      $.non_lvalue_expression,
      $.lvalue_expression,
    ),

    non_lvalue_expression: $ => choice(
      'base',
      $.binary_expression,
      $.interpolated_string_expression,
      $.conditional_expression,
      $.conditional_access_expression,
      $.literal,
      $._expression_statement_expression,
      $.is_expression,
      $.is_pattern_expression,
      $.as_expression,
      $.cast_expression,
      $.checked_expression,
      $.collection_expression,
      $.switch_expression,
      $.throw_expression,
      $.default_expression,
      $.lambda_expression,
      $.with_expression,
      $.sizeof_expression,
      $.typeof_expression,
      $.makeref_expression,
      $.ref_expression,
      // Address-of (see `_address_of_expression` below): aliased to
      // `prefix_unary_expression` to preserve the public AST shape.
      // Parallels `_pointer_indirection_expression` (the `*` operator),
      // which is similarly aliased back into `lvalue_expression`.
      alias($._address_of_expression, $.prefix_unary_expression),
      $.reftype_expression,
      $.refvalue_expression,
      $.stackalloc_expression,
      $.range_expression,
      $.array_creation_expression,
      $.anonymous_method_expression,
      $.anonymous_object_creation_expression,
      $.implicit_array_creation_expression,
      $.implicit_object_creation_expression,
      $.implicit_stackalloc_expression,
      $.initializer_expression,
      $.query_expression,
      alias($.preproc_if_in_expression, $.preproc_if),
    ),

    lvalue_expression: $ => choice(
      'this',
      $.member_access_expression,
      $.tuple_expression,
      $._simple_name,
      $.element_access_expression,
      alias($.bracketed_argument_list, $.element_binding_expression),
      alias($._pointer_indirection_expression, $.prefix_unary_expression),
      alias($._parenthesized_lvalue_expression, $.parenthesized_expression),
      // C# 14 null-conditional assignment: `obj?.x = v`, `obj?[i] = v`.
      // https://learn.microsoft.com/en-us/dotnet/csharp/whats-new/csharp-14#null-conditional-assignment
      $.conditional_access_expression,
    ),

    // Covers error CS0201: Only assignment, call, increment, decrement, await, and new object expressions can be used as a statement
    _expression_statement_expression: $ => choice(
      $.assignment_expression,
      $.invocation_expression,
      $.postfix_unary_expression,
      $.prefix_unary_expression,
      $.await_expression,
      $.object_creation_expression,
      $.parenthesized_expression,
    ),

    assignment_expression: $ => seq(
      field('left', $.lvalue_expression),
      field('operator',
        choice(
          '=',
          '+=',
          '-=',
          '*=',
          '/=',
          '%=',
          '&=',
          '^=',
          '|=',
          '<<=',
          '>>=',
          '>>>=',
          '??=',
        ),
      ),
      field('right', $.expression),
    ),

    binary_expression: $ => choice(
      ...[
        ['&&', PREC.LOGICAL_AND],
        ['||', PREC.LOGICAL_OR],
        ['>>', PREC.SHIFT],
        ['>>>', PREC.SHIFT],
        ['<<', PREC.SHIFT],
        ['&', PREC.AND],
        ['^', PREC.XOR],
        ['|', PREC.OR],
        ['+', PREC.ADD],
        ['-', PREC.ADD],
        ['*', PREC.MULT],
        ['/', PREC.MULT],
        ['%', PREC.MULT],
        ['<', PREC.REL],
        ['<=', PREC.REL],
        ['==', PREC.EQUAL],
        ['!=', PREC.EQUAL],
        ['>=', PREC.REL],
        ['>', PREC.REL],
      ].map(([operator, precedence]) =>
        prec.left(precedence, seq(
          field('left', $.expression),
          // @ts-ignore
          field('operator', operator),
          field('right', $.expression),
        )),
      ),
      prec.right(PREC.COALESCING, seq(
        field('left', $.expression),
        field('operator', '??'),
        field('right', $.expression),
      )),
    ),

    postfix_unary_expression: $ => prec(PREC.POSTFIX, seq(
      $.expression,
      choice('++', '--', '!'),
    )),

    prefix_unary_expression: $ => prec(PREC.UNARY, seq(
      // `&` and `*` are intentionally NOT in this choice list:
      //   * `&` → `_address_of_expression` (operand restricted to lvalue
      //           — see comment on that rule for the #413 rationale)
      //   * `*` → `_pointer_indirection_expression` (same shape,
      //           restricted to lvalue, surfaced in `lvalue_expression`)
      // Both are aliased back to `prefix_unary_expression` so the
      // public AST is unaffected.
      choice('++', '--', '+', '-', '!', '~', '^'),
      $.expression,
    )),

    _pointer_indirection_expression: $ => prec.right(PREC.UNARY, seq(
      '*',
      choice(
        $.lvalue_expression,
        // `*(p + 1)` and `*(T*)p`. The operand is not an lvalue, so the #413
        // restriction above shut them out. They stay legal here, but with a
        // negative dynamic precedence so `(a) * (b + c)` still reads as a
        // multiplication rather than a cast of a dereference.
        prec.dynamic(-2, $.parenthesized_expression),
        prec.dynamic(-2, alias($._pointer_cast_expression, $.cast_expression)),
      ),
    )),

    _pointer_cast_expression: $ => prec(PREC.CAST, seq(
      '(',
      field('type', $.pointer_type),
      ')',
      // Not a full `expression`: `*(T*)p = 1` must stay an assignment to the
      // dereference, not a cast of `p = 1`.
      field('value', choice(
        $.lvalue_expression,
        $.parenthesized_expression,
        alias($._address_of_expression, $.prefix_unary_expression),
      )),
    )),

    // Address-of is split out from `prefix_unary_expression` so that
    // `&` can't act as a fallback when the lexer would otherwise
    // emit `&&`. Its operand is restricted to an lvalue (same shape
    // as the spec's `address-of-expression`). This is the fix for
    // tree-sitter#413: `(a) && (b > 0)` was misparsed as
    // `cast(a, &(&(b > 0)))` because the cast state accepted `&` as
    // a unary start and the lexer split `&&` into two `&` tokens.
    // With this split, `&` is only valid before an lvalue, which
    // `(b > 0)` is not — so the cast interpretation can no longer
    // consume the trailing parenthesized expression and `&&` must
    // be emitted as a single token for the binary path to succeed.
    _address_of_expression: $ => prec.right(PREC.UNARY, seq(
      '&',
      $.lvalue_expression,
    )),

    query_expression: $ => seq($.from_clause, $._query_body),

    from_clause: $ => seq(
      'from',
      optional(field('type', $.type)),
      field('name', $.identifier),
      'in',
      $.expression,
    ),

    _query_body: $ => prec.right(sep1(
      seq(
        repeat($._query_clause),
        $._select_or_group_clause,
      ),
      seq('into', $.identifier),
    )),

    _query_clause: $ => choice(
      $.from_clause,
      $.join_clause,
      $.let_clause,
      $.order_by_clause,
      $.where_clause,
    ),

    join_clause: $ => seq(
      'join',
      $._join_header,
      $._join_body,
      optional($.join_into_clause),
    ),

    _join_header: $ => seq(optional(field('type', $.type)), $.identifier, 'in', $.expression),

    _join_body: $ => seq('on', $.expression, 'equals', $.expression),

    join_into_clause: $ => seq('into', $.identifier),

    let_clause: $ => seq(
      'let',
      $.identifier,
      '=',
      $.expression,
    ),

    order_by_clause: $ => seq(
      'orderby',
      commaSep1($._ordering),
    ),

    _ordering: $ => seq(
      $.expression,
      optional(choice('ascending', 'descending')),
    ),

    where_clause: $ => seq('where', $.expression),

    _select_or_group_clause: $ => choice(
      $.group_clause,
      $.select_clause,
    ),

    group_clause: $ => seq('group', $.expression, 'by', $.expression),

    select_clause: $ => seq('select', $.expression),

    conditional_expression: $ => prec.right(PREC.CONDITIONAL, seq(
      field('condition', $.expression),
      '?',
      field('consequence', $.expression),
      ':',
      field('alternative', $.expression),
    )),

    conditional_access_expression: $ => prec.right(PREC.CONDITIONAL, seq(
      field('condition', $.expression),
      '?',
      choice(
        $.member_binding_expression,
        alias($.bracketed_argument_list, $.element_binding_expression),
      ),
    )),

    as_expression: $ => prec(PREC.REL, seq(
      field('left', $.expression),
      field('operator', 'as'),
      field('right', $.type),
    )),

    is_expression: $ => prec(PREC.REL, seq(
      field('left', $.expression),
      field('operator', 'is'),
      field('right', $.type),
    )),

    is_pattern_expression: $ => prec(PREC.REL, seq(
      field('expression', $.expression),
      'is',
      field('pattern', $.pattern),
    )),

    cast_expression: $ => prec(PREC.CAST, prec.dynamic(1, seq( // higher than invocation, lower than binary
      '(',
      field('type', $.type),
      ')',
      field('value', $.expression),
    ))),

    checked_expression: $ => seq(
      choice('checked', 'unchecked'),
      '(',
      $.expression,
      ')',
    ),

    invocation_expression: $ => prec(PREC.INVOCATION, seq(
      field('function', $.expression),
      field('arguments', $.argument_list),
    )),

    switch_expression: $ => prec(PREC.SWITCH, seq(
      $.expression,
      'switch',
      $._switch_expression_body,
    )),
    _switch_expression_body: $ => seq(
      '{',
      commaSep($.switch_expression_arm),
      optional(','),
      '}',
    ),


    switch_expression_arm: $ => seq(
      $.pattern,
      optional($.when_clause),
      '=>',
      $.expression,
    ),

    when_clause: $ => seq('when', $.expression),

    await_expression: $ => prec.right(PREC.UNARY, seq(
      'await',
      $.expression,
    )),

    throw_expression: $ => seq('throw', $.expression),

    element_access_expression: $ => prec(PREC.POSTFIX, seq(
      field('expression', $.expression),
      field('subscript', $.bracketed_argument_list),
    )),

    interpolated_string_expression: $ => choice(
      seq(
        alias($.interpolation_regular_start, $.interpolation_start),
        alias($.interpolation_start_quote, '"'),
        repeat($._interpolated_string_content),
        alias($.interpolation_end_quote, '"'),
      ),
      seq(
        alias($.interpolation_verbatim_start, $.interpolation_start),
        alias($.interpolation_start_quote, '"'),
        repeat($._interpolated_verbatim_string_content),
        alias($.interpolation_end_quote, '"'),
      ),
      seq(
        alias($.interpolation_raw_start, $.interpolation_start),
        alias($.interpolation_start_quote, $.interpolation_quote),
        repeat($._interpolated_raw_string_content),
        alias($.interpolation_end_quote, $.interpolation_quote),
      ),
    ),

    _interpolated_string_content: $ => choice(
      alias($.interpolation_string_content, $.string_content),
      $.escape_sequence,
      $.interpolation,
    ),

    _interpolated_verbatim_string_content: $ => choice(
      alias($.interpolation_string_content, $.string_content),
      $.interpolation,
    ),

    _interpolated_raw_string_content: $ => choice(
      alias($.interpolation_string_content, $.string_content),
      $.interpolation,
    ),

    interpolation: $ => seq(
      alias($.interpolation_open_brace, $.interpolation_brace),
      $.expression,
      optional($.interpolation_alignment_clause),
      optional($.interpolation_format_clause),
      alias($.interpolation_close_brace, $.interpolation_brace),
    ),

    interpolation_alignment_clause: $ => seq(',', $.expression),

    interpolation_format_clause: _ => seq(':', /[^}"]+/),

    member_access_expression: $ => prec(PREC.DOT, seq(
      field('expression', choice($.expression, $.predefined_type, $._name)),
      choice('.', '->'),
      field('name', $._simple_name),
    )),

    member_binding_expression: $ => seq(
      '.',
      field('name', $._simple_name),
    ),

    object_creation_expression: $ => prec.right(seq(
      'new',
      field('type', $.type),
      field('arguments', optional($.argument_list)),
      field('initializer', optional($.initializer_expression)),
    )),

    // inline
    _object_creation_type: $ => choice(
      $._name,
      $.nullable_type,
      $.predefined_type,
    ),

    parenthesized_expression: $ => seq(
      '(',
      $.non_lvalue_expression,
      ')',
    ),

    _parenthesized_lvalue_expression: $ => seq('(', $.lvalue_expression, ')'),

    lambda_expression: $ => prec(-1, seq(
      $._lambda_expression_init,
      '=>',
      field('body', choice($.block, $.expression)),
    )),

    _lambda_expression_init: $ => prec(-1, seq(
      repeat($._attribute_list),
      repeat(prec(-1, alias(choice('static', 'async'), $.modifier))),
      optional(field('type', $.type)),
      field('parameters', $._lambda_parameters),
    ),
    ),

    _lambda_parameters: $ => prec(-1, choice(
      $.parameter_list,
      alias($.identifier, $.implicit_parameter),
      // C# 14: `(ref x) => x`, `(out y) => ...`, `(text, out result) => ...`
      // The opening '(' is recognized via the external _lambda_paren_open
      // token, which the scanner only emits when it can confirm a closing
      // ')=>' follows a parameter list containing at least one modifier.
      $._implicit_parameter_list_with_modifiers,
    )),

    // C# 14 simple-lambda parameter list with modifiers. Each element is
    // either a bare identifier or one or more modifiers (scoped/ref/out/in/
    // readonly) followed by an identifier. The opening token is the
    // external _lambda_paren_open; the closing ')' is the regular literal.
    _implicit_parameter_list_with_modifiers: $ => seq(
      $._lambda_paren_open,
      commaSep1(choice(
        seq(
          repeat1(alias(
            choice('scoped', 'ref', 'out', 'in', 'readonly'),
            $.modifier,
          )),
          alias($.identifier, $.implicit_parameter),
        ),
        alias($.identifier, $.implicit_parameter),
      )),
      ')',
    ),

    array_creation_expression: $ => prec.dynamic(PREC.UNARY, seq(
      'new',
      field('type', $.array_type),
      optional($.initializer_expression),
    )),

    anonymous_method_expression: $ => seq(
      repeat(prec(-1, alias(choice('static', 'async'), $.modifier))),
      'delegate',
      optional(field('parameters', $.parameter_list)),
      $.block,
    ),

    anonymous_object_creation_expression: $ => seq(
      'new',
      '{',
      commaSep($._anonymous_object_member_declarator),
      optional(','),
      '}',
    ),

    _anonymous_object_member_declarator: $ => choice(
      seq($.identifier, '=', $.expression),
      $.expression,
    ),

    implicit_array_creation_expression: $ => seq(
      'new',
      '[',
      repeat(','),
      ']',
      $.initializer_expression,
    ),

    implicit_object_creation_expression: $ => prec.right(seq(
      'new',
      $.argument_list,
      optional($.initializer_expression),
    )),

    implicit_stackalloc_expression: $ => seq(
      'stackalloc',
      '[',
      ']',
      $.initializer_expression,
    ),

    collection_expression: $ => seq(
      '[',
      optional($._collection_items),
      ']',
    ),

    // A list of elements in which `#if` blocks may hold whole comma-terminated
    // elements: `[ 1, #if X 2, 3, #endif ]`. Roslyn treats a directive as
    // trivia, so the comma inside the block is legal.
    _collection_items: $ => choice(
      seq(
        repeat1(choice(
          seq($.collection_element, ','),
          alias($.preproc_if_in_collection, $.preproc_if),
        )),
        optional($.collection_element),
      ),
      $.collection_element,
    ),

    collection_element: $ => choice(
      $.expression_element,
      $.spread_element,
    ),

    expression_element: $ => prec(1, $.expression),

    spread_element: $ => prec.dynamic(1, prec(PREC.RANGE, seq('..', $.expression))),

    initializer_expression: $ => seq(
      '{',
      optional($._initializer_items),
      '}',
    ),

    // Same shape as `_collection_items`: `{ a, #if X b, #endif }`.
    _initializer_items: $ => choice(
      seq(
        repeat1(choice(
          seq($.expression, ','),
          alias($.preproc_if_in_initializer, $.preproc_if),
        )),
        optional($.expression),
      ),
      $.expression,
    ),

    declaration_expression: $ => prec.dynamic(1, seq(
      field('type', $.type),
      field('name', $.identifier),
    )),

    default_expression: $ => prec.right(seq(
      'default',
      optional(seq(
        '(',
        field('type', $.type),
        ')',
      )),
    )),

    with_expression: $ => prec.left(PREC.WITH, seq(
      $.expression,
      'with',
      $._with_body,
    )),
    _with_body: $ => seq(
      '{',
      commaSep($.with_initializer),
      optional(','),
      '}',
    ),

    with_initializer: $ => seq($.identifier, '=', $.expression),

    sizeof_expression: $ => seq(
      'sizeof',
      '(',
      field('type', $.type),
      ')',
    ),

    typeof_expression: $ => seq(
      'typeof',
      '(',
      field('type', $.type),
      ')',
    ),

    makeref_expression: $ => seq(
      '__makeref',
      '(',
      $.expression,
      ')',
    ),

    ref_expression: $ => seq('ref', $.expression),

    reftype_expression: $ => seq(
      '__reftype',
      '(',
      $.expression,
      ')',
    ),

    refvalue_expression: $ => seq(
      '__refvalue',
      '(',
      field('value', $.expression),
      ',',
      field('type', $.type),
      ')',
    ),

    stackalloc_expression: $ => prec.left(seq(
      'stackalloc',
      field('type', $.array_type),
      optional($.initializer_expression),
    )),

    range_expression: $ => prec.right(PREC.RANGE, seq(
      optional($.expression),
      '..',
      optional($.expression),
    )),

    tuple_expression: $ => seq(
      '(',
      commaSep2($.argument),
      ')',
    ),

    literal: $ => choice(
      $.null_literal,
      $.character_literal,
      $.integer_literal,
      $.real_literal,
      $.boolean_literal,
      $.string_literal,
      $.verbatim_string_literal,
      $.raw_string_literal,
    ),

    null_literal: _ => 'null',

    character_literal: $ => seq(
      '\'',
      choice($.character_literal_content, $.escape_sequence),
      '\'',
    ),

    character_literal_content: $ => token.immediate(/[^'\\]/),

    integer_literal: _ => token(seq(
      choice(
        decimalDigitSequence, // Decimal
        (/0[xX][0-9a-fA-F_]*[0-9a-fA-F]+/), // Hex
        (/0[bB][01_]*[01]+/), // Binary
      ),
      optional(/([uU][lL]?|[lL][uU]?)/),
    )),

    real_literal: _ => {
      const suffix = /[fFdDmM]/;
      const exponent = /[eE][+-]?[0-9][0-9_]*/;
      return token(choice(
        seq(
          decimalDigitSequence,
          '.',
          decimalDigitSequence,
          optional(exponent),
          optional(suffix),
        ),
        seq(
          '.',
          decimalDigitSequence,
          optional(exponent),
          optional(suffix),
        ),
        seq(
          decimalDigitSequence,
          exponent,
          optional(suffix),
        ),
        seq(
          decimalDigitSequence,
          suffix,
        ),
      ));
    },

    string_literal: $ => seq(
      '"',
      repeat(choice(
        $.string_literal_content,
        $.escape_sequence,
      )),
      '"',
      optional($.string_literal_encoding),
    ),

    string_literal_content: _ => token.immediate(prec(1, /[^"\\\n]+/)),

    escape_sequence: _ => token(choice(
      /\\x[0-9a-fA-F]{1,4}/,
      /\\u[0-9a-fA-F]{4}/,
      /\\U[0-9a-fA-F]{8}/,
      /\\[abefnrtv'\"\\\?0]/,
    )),

    string_literal_encoding: _ => token.immediate(stringEncoding),

    verbatim_string_literal: _ => token(seq(
      '@"',
      repeat(choice(
        /[^"]/,
        '""',
      )),
      '"',
      optional(stringEncoding),
    )),

    raw_string_literal: $ => seq(
      $.raw_string_start,
      $.raw_string_content,
      $.raw_string_end,
      optional(stringEncoding),
    ),

    boolean_literal: _ => choice('true', 'false'),

    _identifier_token: _ => token(seq(optional('@'), /(\p{XID_Start}|_|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})(\p{XID_Continue}|\\u[0-9A-Fa-f]{4}|\\U[0-9A-Fa-f]{8})*/)),
    identifier: $ => choice(
      $._identifier_token,
      $._reserved_identifier,
    ),

    _reserved_identifier: _ => choice(
      'alias',
      'ascending',
      'by',
      'descending',
      'equals',
      // attribute_target_specifier keywords — contextual only when followed
      // by `:` in `[target: Attr]` position. Anywhere else they're
      // identifiers. Without listing them here the LR table preferred the
      // literal-keyword interpretation, breaking `[type]`, `[field]`, etc.
      // as collection_expression elements. Excludes `'event'` and `'return'`
      // from the attribute_target_specifier choice — those are real C#
      // keywords and must not be accepted as identifiers anywhere else.
      'field',
      'file',
      'from',
      'global',
      'group',
      'into',
      'join',
      'let',
      'method',
      'notnull',
      'on',
      'orderby',
      'param',
      'property',
      'scoped',
      'select',
      'type',
      'typevar',
      'unmanaged',
      'var',
      'when',
      'where',
      'yield',
    ),

    // Preprocessor

    ...preprocIf('', $ => $.declaration),
    ...preprocIf('_in_top_level', $ => choice($._top_level_item_no_statement, $.statement)),
    ...preprocIf('_in_expression', $ => $.expression, -2, false),
    ...preprocIf('_in_statement_tail', $ => seq($.expression, ';'), 0, false),
    ...preprocIf('_in_function_body', $ => seq(repeat($.type_parameter_constraints_clause), $._plain_function_body), 0, false),
    // The block opens with an external `#if` that the scanner only emits when the
    // first thing inside is `else`, so a plain `#if` after an if-statement is
    // still a statement-level block.
    ...preprocIf('_in_else', $ => seq('else', $.statement), 0, false, $ => $._preproc_else_if_start),
    ...preprocIf('_in_collection', $ => $._collection_items, -2, false),
    ...preprocIf('_in_initializer', $ => $._initializer_items, -2, false),
    ...preprocIf('_in_enum_member_declaration', $ => $.enum_member_declaration, 0, false),
    ...preprocIf('_in_attribute_list', $ => $.attribute_list, -1, false),

    preproc_arg: _ => token(prec(-1, /\S([^/\n]|\/[^*]|\\\r?\n)*/)),
    preproc_directive: _ => /#[ \t]*[a-zA-Z0-9]\w*/,

    _preproc_expression: $ => choice(
      $.identifier,
      $.boolean_literal,
      $.integer_literal,
      $.character_literal,
      alias($.preproc_unary_expression, $.unary_expression),
      alias($.preproc_binary_expression, $.binary_expression),
      alias($.preproc_parenthesized_expression, $.parenthesized_expression),
    ),

    preproc_parenthesized_expression: $ => seq(
      '(',
      $._preproc_expression,
      ')',
    ),

    preproc_unary_expression: $ => prec.left(PREC.UNARY, seq(
      field('operator', '!'),
      field('argument', $._preproc_expression),
    )),

    preproc_binary_expression: $ => {
      const table = [
        ['||', PREC.LOGICAL_OR],
        ['&&', PREC.LOGICAL_AND],
        ['==', PREC.EQUAL],
        ['!=', PREC.EQUAL],
      ];

      return choice(...table.map(([operator, precedence]) => {
        return prec.left(precedence, seq(
          field('left', $._preproc_expression),
          // @ts-ignore
          field('operator', operator),
          field('right', $._preproc_expression),
        ));
      }));
    },

    preproc_region: $ => seq(
      preprocessor('region'),
      optional(field('content', $.preproc_arg)),
      /\n/,
    ),

    preproc_endregion: $ => seq(
      preprocessor('endregion'),
      optional(field('content', $.preproc_arg)),
      /\n/,
    ),

    preproc_line: $ => seq(
      preprocessor('line'),
      choice(
        'default',
        'hidden',
        seq($.integer_literal, optional($.string_literal)),
        seq(
          '(', $.integer_literal, ',', $.integer_literal, ')',
          '-',
          '(', $.integer_literal, ',', $.integer_literal, ')',
          optional($.integer_literal),
          $.string_literal,
        ),
      ),
      /\n/,
    ),

    preproc_pragma: $ => seq(
      preprocessor('pragma'),
      choice(
        seq('warning',
          choice('disable', 'restore'),
          commaSep(
            choice(
              $.identifier,
              $.integer_literal,
            ))),
        seq('checksum', $.string_literal, $.string_literal, $.string_literal),
      ),
      /\n/,
    ),

    preproc_nullable: _ => seq(
      preprocessor('nullable'),
      choice('enable', 'disable', 'restore'),
      optional(choice('annotations', 'warnings')),
      /\n/,
    ),

    preproc_error: $ => seq(
      preprocessor('error'),
      $.preproc_arg,
      /\n/,
    ),

    preproc_warning: $ => seq(
      preprocessor('warning'),
      $.preproc_arg,
      /\n/,
    ),

    preproc_define: $ => seq(
      preprocessor('define'),
      $.preproc_arg,
      /\n/,
    ),

    preproc_undef: $ => seq(
      preprocessor('undef'),
      $.preproc_arg,
      /\n/,
    ),

    shebang_directive: _ => token(seq('#!', /.*/)),

    // C# 14 / .NET 10 file-based apps directives. The compiler ignores them;
    // the .NET SDK reads them. Structure follows the SDK spec:
    // https://github.com/dotnet/sdk/blob/main/documentation/general/dotnet-run-file.md
    //
    //   #:package Name@Version     #:sdk Name@Version     #:property Name=Value
    //   #:project path             #:ref path             #:include glob    ...
    //
    // `package`, `sdk` and `property` split into name, separator and value.
    // Any other kind (including unknown ones) takes the rest of the line as
    // its value. Kinds are immediate tokens only valid right after `#:`, so
    // they never reserve these words elsewhere.
    ignored_directive: $ => seq(
      '#:',
      optional(choice(
        seq(
          field('kind', choice(
            alias(token.immediate('package'), $.directive_kind),
            alias(token.immediate('sdk'), $.directive_kind),
            alias(token.immediate('property'), $.directive_kind),
          )),
          optional(seq(
            field('name', $.directive_name),
            optional(field('separator', $.directive_separator)),
            optional(field('value', $.directive_value)),
          )),
        ),
        seq(
          field('kind', alias(token.immediate(/[A-Za-z_][A-Za-z0-9_-]*/), $.directive_kind)),
          optional(field('value', $.directive_value)),
        ),
        // Not a well-formed directive (e.g. `#: package X`): still ignored by
        // the compiler, so keep the rest of the line as an unstructured value.
        field('value', alias(token.immediate(/[ \t]+[^\s]([^\r\n]*[^\s])?/), $.directive_value)),
      )),
      /\n/,
    ),

    directive_name: _ => token(choice(
      /[^\s@="]+/,
      /"([^"\\\n]|\\.)*"/,
    )),

    // `@` for package/sdk versions, `=` for property values. A dedicated
    // token (rather than reusing the C# `@`/`=` literals) keeps these
    // directives from perturbing GLR tie-breaks elsewhere in the grammar.
    directive_separator: _ => token(/[@=]/),

    directive_value: _ => token(/[^\s@=]([^\r\n]*[^\s])?/),

    comment: _ => token(choice(
      seq('//', /[^\n\r]*/),
      seq(
        '/*',
        /[^*]*\*+([^/*][^*]*\*+)*/,
        '/',
      ),
    )),
  },
});

/**
 * Creates a preprocessor regex rule
 *
 * @param {RegExp | Rule | string} command
 *
 * @returns {AliasRule}
 */
function preprocessor(command) {
  return alias(new RegExp('#[ \t]*' + command), '#' + command);
}

/**
 *
 * @param {string} suffix
 *
 * @param {RuleBuilder<string>} content
 *
 * @param {number} precedence
 *
 * @param {boolean} rep
 *
 * @returns {RuleBuilders<string, string>}
 */
function preprocIf(suffix, content, precedence = 0, rep = true, ifToken = null) {
  /**
   *
   * @param {GrammarSymbols<string>} $
   *
   * @returns {ChoiceRule}
   */
  function alternativeBlock($) {
    return choice(
      suffix ? alias($['preproc_else' + suffix], $.preproc_else) : $.preproc_else,
      suffix ? alias($['preproc_elif' + suffix], $.preproc_elif) : $.preproc_elif,
    );
  }

  return {
    ['preproc_if' + suffix]: $ => prec(precedence, seq(
      ifToken ? alias(ifToken($), '#if') : preprocessor('if'),
      field('condition', $._preproc_expression),
      /\n/,
      rep ? repeat(content($)) : optional(content($)),
      field('alternative', optional(alternativeBlock($))),
      preprocessor('endif'),
    )),

    ['preproc_else' + suffix]: $ => prec(precedence, seq(
      preprocessor('else'),
      rep ? repeat(content($)) : optional(content($)),
    )),

    ['preproc_elif' + suffix]: $ => prec(precedence, seq(
      preprocessor('elif'),
      field('condition', $._preproc_expression),
      /\n/,
      rep ? repeat(content($)) : optional(content($)),
      field('alternative', optional(alternativeBlock($))),
    )),
  };
}

/**
 * Creates a rule to match one or more of the rules separated by a comma
 *
 * @param {Rule} rule
 *
 * @returns {SeqRule}
 */
function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}

/**
 * Creates a rule to match two or more of the rules separated by a comma
 *
 * @param {Rule} rule
 *
 * @returns {SeqRule}
 */
function commaSep2(rule) {
  return seq(rule, repeat1(seq(',', rule)));
}

/**
 * Creates a rule to optionally match one or more of the rules separated by a comma
 *
 * @param {Rule} rule
 *
 * @returns {ChoiceRule}
 */
function commaSep(rule) {
  return optional(commaSep1(rule));
}

/**
 * Creates a rule to match one or more of the rules separated by `separator`
 *
 * @param {RuleOrLiteral} rule
 *
 * @param {RuleOrLiteral} separator
 *
 * @returns {SeqRule}
 */
function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}

/**
 * Creates a rule to optionally match one or more of the rules separated by `separator`
 *
 * @param {RuleOrLiteral} rule
 *
 * @param {RuleOrLiteral} separator
 *
 * @returns {ChoiceRule}
 */
function sep(rule, separator) {
  return optional(sep1(rule, separator));
}
