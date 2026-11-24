/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const OP_CHAR = /[!$%&*+\-./:<=>?@^|~]/;
const HASH_OP_CHAR = /[#!$%&*+\-./:<=>?@^|~]/;
const NUMBER = token(choice(
  /[0-9][0-9_]*(\.[0-9_]*)?([eE][+\-]?[0-9][0-9_]*)?[g-zG-Z]?/,
  /0[xX][0-9A-Fa-f][0-9A-Fa-f_]*(\.[0-9A-Fa-f_]*)?([pP][+\-]?[0-9][0-9_]*)?[g-zG-Z]?/,
  /0[oO][0-7][0-7_]*[g-zG-Z]?/,
  /0[bB][01][01_]*[g-zG-Z]?/,
));

module.exports = grammar({
  name: 'ocaml',

  extras: $ => [
    /\s/,
    $.comment,
    $.line_number_directive,
    $.attribute,
  ],

  inline: $ => [
    $._parameter,
    $._argument,
    $._extension,
    $._item_extension,
    $._value_pattern,
    $._label_name,
    $._field_name,
    $._class_name,
    $._class_type_name,
    $._method_name,
    $._type_constructor,
    $._module_name,
    $._module_type_name,
    $._argument_type,
    $._label,
    $._tuple_label,
  ],

  conflicts: $ => [
    [$._proper_tuple_type, $.labeled_tuple_element_type],
  ],

  precedences: $ => [
    [
      $.constructed_type,
      $.hash_type,
      $.parenthesized_type,
      $.function_type,
      $.aliased_type,
      $._type,
    ],
    [
      'prefix',
      'dot',
      'hash',
      'method',
      'app',
      'sign',
      'pow',
      'mult',
      'add',
      'cons',
      'concat',
      'rel',
      'and',
      'or',
      'tuple',
      'assign',
      'set',
      'if',
      'seq',
      $._expression,
      $._sequence_expression,
    ],
    [
      'range_pattern',
      'lazy_pattern',
      'constructor_pattern',
      'cons_pattern',
      'tuple_pattern',
      'or_pattern',
      'alias_pattern',
      'exception_pattern',
      $._pattern,
      $._simple_binding_pattern,
      $._binding_pattern,
    ],
    [$.module_path, $._constructor_name],
  ],

  word: $ => $._lowercase_identifier,

  supertypes: $ => [
    $._structure_item,
    $._signature_item,
    $._parameter,
    $._module_type,
    $._simple_module_expression,
    $._module_expression,
    $._simple_class_type,
    $._class_type,
    $._class_field_specification,
    $._simple_class_expression,
    $._class_expression,
    $._class_field,
    $._polymorphic_type,
    $._simple_type,
    $._type,
    $._simple_expression,
    $._expression,
    $._sequence_expression,
    $._simple_pattern,
    $._effect_pattern,
    $._pattern,
    $._simple_binding_pattern,
    $._binding_pattern,
    $._constant,
    $._signed_constant,
    $._infix_operator,
  ],

  rules: {
    compilation_unit: $ => seq(
      optional($.shebang),
      optional($._structure),
    ),

    shebang: $ => /#!.*/,

    _structure: $ => choice(
      repeat1(';;'),
      seq(
        repeat(';;'),
        choice($._structure_item, $.toplevel_directive, $.expression_item),
        repeat(choice(
          seq(repeat(';;'), choice($._structure_item, $.toplevel_directive)),
          seq(repeat1(';;'), $.expression_item),
        )),
        repeat(';;'),
      ),
    ),

    expression_item: $ => seq(
      $._sequence_expression,
      repeat($.item_attribute),
    ),

    _signature: $ => choice(
      repeat1(';;'),
      seq(repeat1(seq(repeat(';;'), $._signature_item)), repeat(';;')),
    ),

    // Toplevel

    toplevel_directive: $ => seq(
      $.directive,
      optional(choice(
        $._constant,
        $.value_path,
        $.module_path,
      )),
    ),

    // Module implementation

    _structure_item: $ => choice(
      $.value_definition,
      $.external,
      $.type_definition,
      $.exception_definition,
      $.module_definition,
      $.module_type_definition,
      $.open_module,
      $.include_module,
      $.class_definition,
      $.class_type_definition,
      $.floating_attribute,
      $._item_extension,
    ),

    value_definition: $ => seq(
      choice(seq('let', optional($._attribute), optional('rec')), $.let_operator),
      sep1(choice('and', $.let_and_operator), $.let_binding),
    ),

    let_binding: $ => seq(
      field('pattern', $._binding_pattern),
      optional(seq(
        repeat($._parameter),
        optional($._polymorphic_typed),
        optional(seq(':>', field('coercion', $._type))),
        '=',
        field('body', $._sequence_expression),
      )),
      repeat($.item_attribute),
    ),

    _parameter: $ => choice(
      $.parameter,
      alias($._parenthesized_abstract_type, $.abstract_type),
    ),

    parameter: $ => choice(
      field('pattern', $._simple_pattern),
      seq(
        choice('~', '?'),
        field('pattern', $._simple_value_pattern),
      ),
      seq(
        $._label,
        token.immediate(':'),
        field('pattern', $._simple_pattern),
      ),
      seq(
        choice('~', '?'),
        '(',
        field('pattern', $._simple_value_pattern),
        optional($._typed),
        optional(seq('=', field('default', $._sequence_expression))),
        ')',
      ),
      seq(
        $._label,
        token.immediate(':'),
        '(',
        field('pattern', $._pattern),
        optional($._typed),
        seq('=', field('default', $._sequence_expression)),
        ')',
      ),
    ),

    external: $ => seq(
      'external',
      optional($._attribute),
      $._value_name,
      $._polymorphic_typed,
      '=',
      repeat1($.string),
      repeat($.item_attribute),
    ),

    type_definition: $ => seq(
      'type',
      optional($._attribute),
      optional('nonrec'),
      sep1('and', $.type_binding),
    ),

    type_binding: $ => seq(
      optional($._type_params),
      choice(
        seq(
          field('name', $._type_constructor),
          optional($._type_equation),
          optional(seq(
            '=',
            optional('private'),
            field('body', choice($.variant_declaration, $.record_declaration, '..')),
          )),
          repeat($.type_constraint),
        ),
        seq(
          field('name', $.type_constructor_path),
          seq(
            '+=',
            optional('private'),
            field('body', $.variant_declaration),
          ),
        ),
      ),
      repeat($.item_attribute),
    ),

    _type_params: $ => choice(
      $._type_param,
      parenthesize(sep1(',', $._type_param)),
    ),

    _type_param: $ => seq(
      optional(choice(
        seq('+', optional('!')),
        seq('-', optional('!')),
        seq('!', optional(choice('+', '-'))),
      )),
      choice($.type_variable, alias('_', $.type_variable)),
    ),

    _type_equation: $ => seq(
      choice('=', ':='),
      optional('private'),
      field('equation', $._type),
    ),

    variant_declaration: $ => choice(
      seq('|', sep('|', $.constructor_declaration)),
      sep1('|', $.constructor_declaration),
    ),

    constructor_declaration: $ => seq(
      choice(
        $._constructor_name,
        alias(choice(seq('[', ']'), seq('(', ')'), 'true', 'false'), $.constructor_name),
      ),
      optional(choice(
        seq('of', $._constructor_argument),
        seq(
          ':',
          optional(seq(repeat1($.type_variable), '.')),
          optional(seq($._constructor_argument, '->')),
          $._simple_type,
        ),
        seq('=', $.constructor_path),
      )),
    ),

    _constructor_argument: $ => choice(
      sep1('*', $._simple_type),
      $.record_declaration,
    ),

    record_declaration: $ => seq(
      '{',
      sep1(';', $.field_declaration),
      optional(';'),
      '}',
    ),

    field_declaration: $ => seq(
      optional('mutable'),
      $._field_name,
      $._polymorphic_typed,
    ),

    type_constraint: $ => seq(
      'constraint',
      field('type', $._type),
      '=',
      field('constraint', $._type),
    ),

    exception_definition: $ => seq(
      'exception',
      optional($._attribute),
      $.constructor_declaration,
      repeat($.item_attribute),
    ),

    module_definition: $ => seq(
      'module', optional($._attribute), optional('rec'),
      sep1('and', $.module_binding),
    ),

    module_binding: $ => seq(
      choice($._module_name, alias('_', $.module_name)),
      repeat($.module_parameter),
      optional($._module_typed),
      optional(seq(choice('=', ':='), field('body', $._module_expression))),
      repeat($.item_attribute),
    ),

    module_parameter: $ => parenthesize(optional(seq(
      choice($._module_name, alias('_', $.module_name)),
      $._module_typed,
    ))),

    module_type_definition: $ => seq(
      'module', 'type',
      optional($._attribute),
      $._module_type_name,
      optional(seq(choice('=', ':='), field('body', $._module_type))),
      repeat($.item_attribute),
    ),

    open_module: $ => seq(
      'open',
      optional('!'),
      optional($._attribute),
      field('module', $._module_expression),
      repeat($.item_attribute),
    ),

    include_module: $ => seq(
      'include',
      optional($._attribute),
      field('module', $._module_expression),
      repeat($.item_attribute),
    ),

    class_definition: $ => seq(
      'class', optional($._attribute),
      sep1('and', $.class_binding),
    ),

    class_binding: $ => seq(
      optional('virtual'),
      optional(seq(
        '[',
        sep1(',', $._type_param),
        ']',
      )),
      $._class_name,
      repeat($._parameter),
      optional($._class_typed),
      optional(seq('=', field('body', $._class_expression))),
      repeat($.item_attribute),
    ),

    class_type_definition: $ => seq(
      'class', 'type', optional($._attribute),
      sep1('and', $.class_type_binding),
    ),

    class_type_binding: $ => seq(
      optional('virtual'),
      optional(seq(
        '[',
        sep1(',', $._type_param),
        ']',
      )),
      $._class_type_name,
      '=',
      field('body', $._simple_class_type),
      repeat($.item_attribute),
    ),

    // Module signature

    _signature_item: $ => choice(
      $.value_specification,
      $.external,
      $.type_definition,
      $.exception_definition,
      $.module_definition,
      $.module_type_definition,
      $.open_module,
      $.include_module_type,
      $.class_definition,
      $.class_type_definition,
      $.floating_attribute,
      $._item_extension,
    ),

    value_specification: $ => seq(
      'val',
      optional($._attribute),
      $._value_name,
      $._polymorphic_typed,
      repeat($.item_attribute),
    ),

    include_module_type: $ => seq(
      'include',
      optional($._attribute),
      field('module_type', $._module_type),
      repeat($.item_attribute),
    ),

    // Module types

    _module_typed: $ => seq(':', field('module_type', $._module_type)),

    _module_type: $ => choice(
      $.module_type_path,
      $.signature,
      $.module_type_constraint,
      $.module_type_of,
      $.functor_type,
      $.parenthesized_module_type,
      $._extension,
    ),

    signature: $ => seq(
      'sig',
      optional($._signature),
      'end',
    ),

    module_type_constraint: $ => prec.right(seq(
      field('module_type', $._module_type),
      'with',
      sep1('and', choice(
        $.constrain_type,
        $.constrain_module,
        $.constrain_module_type,
      )),
    )),

    constrain_type: $ => seq(
      'type',
      optional($._type_params),
      $.type_constructor_path,
      $._type_equation,
      repeat($.type_constraint),
    ),

    constrain_module: $ => seq(
      'module',
      $.module_path,
      choice('=', ':='),
      field('constraint', $.extended_module_path),
    ),

    constrain_module_type: $ => prec.left(seq(
      'module', 'type',
      $.module_type_path,
      choice('=', ':='),
      field('constraint', $._module_type),
    )),

    module_type_of: $ => seq(
      'module', 'type', 'of',
      field('module', $._module_expression),
    ),

    functor_type: $ => prec.right(seq(
      choice(
        seq(optional('functor'), repeat($.module_parameter)),
        field('domain', $._module_type),
      ),
      '->',
      field('codomain', $._module_type),
    )),

    parenthesized_module_type: $ => parenthesize($._module_type),

    // Module expressions

    _simple_module_expression: $ => choice(
      $.typed_module_expression,
      $.parenthesized_module_expression,
      $.packed_module,
      $._extension,
    ),

    _module_expression: $ => choice(
      $._simple_module_expression,
      $.module_path,
      $.structure,
      $.functor,
      $.module_application,
    ),

    structure: $ => seq(
      'struct',
      optional($._structure),
      'end',
    ),

    functor: $ => prec.right(seq(
      'functor',
      repeat1($.module_parameter),
      '->',
      field('body', $._module_expression),
    )),

    module_application: $ => seq(
      field('functor', $._module_expression),
      choice(
        field('argument', $._simple_module_expression),
        seq('(', ')'),
      ),
    ),

    typed_module_expression: $ => parenthesize(seq(
      field('module', $._module_expression),
      $._module_typed,
    )),

    packed_module: $ => parenthesize(seq(
      'val',
      field('value', $._expression),
      optional($._module_typed),
      optional(seq(':>', field('coercion', $._module_type))),
    )),

    parenthesized_module_expression: $ => parenthesize($._module_expression),

    // Class types

    _class_typed: $ => seq(':', field('class_type', $._class_type)),

    _simple_class_type: $ => choice(
      $.class_type_path,
      $.instantiated_class_type,
      $.class_body_type,
      $.let_open_class_type,
      $._extension,
    ),

    _class_type: $ => choice(
      $._simple_class_type,
      $.class_function_type,
    ),

    instantiated_class_type: $ => seq(
      '[',
      sep1(',', $._type),
      ']',
      $.class_type_path,
    ),

    class_body_type: $ => seq(
      'object',
      optional(parenthesize(field('self_type', $._type))),
      repeat(choice(
        $._class_field_specification,
        $.floating_attribute,
      )),
      'end',
    ),

    _class_field_specification: $ => choice(
      $.inheritance_specification,
      $.instance_variable_specification,
      $.method_specification,
      $.type_parameter_constraint,
      $._item_extension,
    ),

    inheritance_specification: $ => seq(
      'inherit',
      field('class_type', $._simple_class_type),
      repeat($.item_attribute),
    ),

    instance_variable_specification: $ => seq(
      'val',
      repeat(choice('mutable', 'virtual')),
      $._instance_variable_name,
      $._typed,
      repeat($.item_attribute),
    ),

    method_specification: $ => seq(
      'method',
      repeat(choice('private', 'virtual')),
      $._method_name,
      $._polymorphic_typed,
      repeat($.item_attribute),
    ),

    type_parameter_constraint: $ => seq(
      'constraint',
      field('type', $._type),
      '=',
      field('constraint', $._type),
      repeat($.item_attribute),
    ),

    let_open_class_type: $ => seq(
      'let',
      $.open_module,
      'in',
      field('body', $._simple_class_type),
    ),

    class_function_type: $ => seq(
      field('domain', $._argument_type),
      '->',
      field('codomain', $._class_type),
    ),

    // Class expressions

    _simple_class_expression: $ => choice(
      $.class_path,
      $.instantiated_class,
      $.object_expression,
      $.typed_class_expression,
      $.parenthesized_class_expression,
      $._extension,
    ),

    _class_expression: $ => choice(
      $._simple_class_expression,
      $.class_function,
      $.class_application,
      $.let_class_expression,
      $.let_open_class_expression,
    ),

    instantiated_class: $ => seq(
      '[',
      sep1(',', $._type),
      ']',
      $.class_path,
    ),

    typed_class_expression: $ => parenthesize(seq(
      field('class', $._class_expression),
      $._class_typed,
    )),

    class_function: $ => seq(
      'fun',
      repeat1($._parameter),
      '->',
      field('body', $._class_expression),
    ),

    class_application: $ => seq(
      field('class', $._simple_class_expression),
      repeat1(field('argument', $._argument)),
    ),

    let_class_expression: $ => seq(
      $.value_definition,
      'in',
      field('body', $._class_expression),
    ),

    _class_field: $ => choice(
      $.inheritance_definition,
      $.instance_variable_definition,
      $.method_definition,
      $.type_parameter_constraint,
      $.class_initializer,
      $._item_extension,
    ),

    inheritance_definition: $ => seq(
      'inherit',
      optional('!'),
      field('class', $._class_expression),
      optional(seq('as', field('alias', $._value_pattern))),
      repeat($.item_attribute),
    ),

    instance_variable_definition: $ => seq(
      'val',
      optional('!'),
      repeat(choice('mutable', 'virtual')),
      $._instance_variable_name,
      optional($._typed),
      optional(seq(':>', field('coercion', $._type))),
      optional(seq('=', field('body', $._sequence_expression))),
      repeat($.item_attribute),
    ),

    method_definition: $ => seq(
      'method',
      optional('!'),
      repeat(choice('private', 'virtual')),
      $._method_name,
      repeat($._parameter),
      optional($._polymorphic_typed),
      optional(seq('=', field('body', $._sequence_expression))),
      repeat($.item_attribute),
    ),

    class_initializer: $ => seq(
      'initializer',
      field('initializer', $._sequence_expression),
      repeat($.item_attribute),
    ),

    let_open_class_expression: $ => seq(
      'let',
      $.open_module,
      'in',
      field('body', $._class_expression),
    ),

    parenthesized_class_expression: $ => parenthesize($._class_expression),

    // Types

    _typed: $ => seq(':', field('type', $._type)),

    _simple_typed: $ => seq(':', field('type', $._simple_type)),

    _polymorphic_typed: $ => seq(':', field('type', $._polymorphic_type)),

    _polymorphic_type: $ => choice(
      $.polymorphic_type,
      $._type,
    ),

    polymorphic_type: $ => seq(
      choice(
        repeat1($.type_variable),
        alias($._abstract_type, $.abstract_type),
      ),
      '.',
      field('type', $._type),
    ),

    _abstract_type: $ => seq(
      'type',
      repeat1($._type_constructor),
    ),

    _parenthesized_abstract_type: $ => parenthesize($._abstract_type),

    _simple_type: $ => choice(
      $.type_variable,
      $.type_constructor_path,
      $.constructed_type,
      $.local_open_type,
      $.polymorphic_variant_type,
      $.package_type,
      $.hash_type,
      $.object_type,
      $.parenthesized_type,
      $._extension,
    ),

    _type: $ => choice(
      $._simple_type,
      alias($._proper_tuple_type, $.tuple_type),
      alias($._labeled_tuple_type, $.tuple_type),
      $.function_type,
      $.aliased_type,
    ),

    function_type: $ => seq(
      field('domain', $._argument_type),
      '->',
      field('codomain', $._type),
    ),

    _argument_type: $ => choice(
      $._simple_type,
      alias($._proper_tuple_type, $.tuple_type),
      $.labeled_argument_type,
    ),

    labeled_argument_type: $ => seq(
      optional('?'),
      $._label_name,
      ':',
      field('type', $._argument_type),
    ),

    _proper_tuple_type: $ => seq(
      $._simple_type,
      '*',
      $._tuple_type_rhs,
    ),

    _labeled_tuple_type: $ => seq(
      $.labeled_tuple_element_type,
      '*',
      $._tuple_type_rhs,
    ),

    labeled_tuple_element_type: $ => seq(
      $._label_name,
      ':',
      field('type', $._simple_type),
    ),

    _tuple_type_rhs: $ => seq(
      choice($._simple_type, $.labeled_tuple_element_type),
      optional(seq(
        '*',
        choice($._tuple_type_rhs),
      )),
    ),

    constructed_type: $ => seq(
      choice(
        $._simple_type,
        parenthesize(sep1(',', $._type)),
      ),
      $.type_constructor_path,
    ),

    aliased_type: $ => seq(
      field('type', $._type),
      'as',
      field('alias', $.type_variable),
    ),

    local_open_type: $ => seq(
      $.extended_module_path,
      '.',
      choice(
        parenthesize(field('type', $._type)),
        field('type', $.package_type),
        field('type', $.polymorphic_variant_type),
      ),
    ),

    polymorphic_variant_type: $ => choice(
      seq('[', $.tag_specification, ']'),
      seq('[', optional($._tag_spec), '|', sep1('|', $._tag_spec), ']'),
      seq('[>', optional('|'), sep('|', $._tag_spec), ']'),
      seq('[<', optional('|'), sep1('|', $._tag_spec), optional(seq('>', repeat1($.tag))), ']'),
    ),

    _tag_spec: $ => choice(
      $._type,
      $.tag_specification,
    ),

    tag_specification: $ => seq(
      $.tag,
      optional(seq(
        'of',
        optional('&'),
        sep1('&', $._type),
      )),
    ),

    package_type: $ => parenthesize(seq(
      'module',
      optional($._attribute),
      $._module_type,
    )),

    object_type: $ => seq(
      '<',
      optional(choice(
        seq(
          sep1(';', choice(
            $.method_type,
            $._simple_type,
          )),
          optional(seq(';', optional('..'))),
        ),
        '..',
      )),
      '>',
    ),

    method_type: $ => seq(
      $._method_name,
      $._polymorphic_typed,
    ),

    hash_type: $ => seq(
      optional(choice(
        $._simple_type,
        parenthesize(sep1(',', $._type)),
      )),
      '#',
      $.class_type_path,
    ),

    parenthesized_type: $ => parenthesize($._type),

    // Expressions

    _simple_expression: $ => choice(
      $.value_path,
      $._constant,
      $.typed_expression,
      $.constructor_path,
      $.tag,
      $.list_expression,
      $.array_expression,
      $.record_expression,
      $.prefix_expression,
      $.hash_expression,
      $.field_get_expression,
      $.array_get_expression,
      $.string_get_expression,
      $.bigarray_get_expression,
      $.coercion_expression,
      $.local_open_expression,
      $.package_expression,
      $.new_expression,
      $.object_copy_expression,
      $.method_invocation,
      $.object_expression,
      $.parenthesized_expression,
      $.ocamlyacc_value,
      $._extension,
    ),

    _expression: $ => choice(
      $._simple_expression,
      alias($._tuple_expression, $.tuple_expression),
      $.cons_expression,
      $.application_expression,
      $.infix_expression,
      $.sign_expression,
      $.set_expression,
      $.if_expression,
      $.while_expression,
      $.for_expression,
      $.match_expression,
      $.function_expression,
      $.fun_expression,
      $.try_expression,
      $.let_expression,
      $.assert_expression,
      $.lazy_expression,
      $.let_module_expression,
      $.let_open_expression,
      $.let_exception_expression,
    ),

    _sequence_expression: $ => choice(
      $._expression,
      alias($._sequence_expression_anonymous, $.sequence_expression),
    ),

    typed_expression: $ => parenthesize(seq(
      field('expression', $._sequence_expression),
      $._typed,
    )),

    labeled_tuple_element: $ => choice(
      $._tuple_label,
      seq(
        $._tuple_label,
        token.immediate(':'),
        field('expression', $._simple_expression)
      ),
      seq('~', '(', $._label_name, $._typed, ')'),
    ),

    _tuple_expression: $ => prec.right('tuple', seq(
      choice($._expression, $.labeled_tuple_element),
      ',',
      choice($._expression, $.labeled_tuple_element, $._tuple_expression),
    )),

    cons_expression: $ => prec.right('cons', seq(
      field('left', $._expression),
      '::',
      field('right', $._expression),
    )),

    list_expression: $ => seq(
      '[',
      optional(seq(
        sep1(';', $._expression),
        optional(';'),
      )),
      ']',
    ),

    array_expression: $ => seq(
      '[|',
      optional(seq(
        sep1(';', $._expression),
        optional(';'),
      )),
      '|]',
    ),

    record_expression: $ => seq(
      '{',
      optional(seq(
        field('record', $._simple_expression),
        'with',
      )),
      sep1(';', $.field_expression),
      optional(';'),
      '}',
    ),

    field_expression: $ => seq(
      $.field_path,
      optional($._typed),
      optional(seq('=', field('body', $._expression))),
    ),

    application_expression: $ => prec('app', seq(
      field('function', $._simple_expression),
      repeat1(field('argument', $._argument)),
    )),

    _argument: $ => choice(
      $._simple_expression,
      $.labeled_argument,
    ),

    labeled_argument: $ => choice(
      $._label,
      seq(
        $._label,
        token.immediate(':'),
        field('expression', $._simple_expression),
      ),
      seq(
        choice('~', '?'),
        '(',
        $._label_name,
        $._typed,
        ')',
      ),
    ),

    prefix_expression: $ => prec('prefix', seq(
      field('operator', $.prefix_operator),
      field('expression', $._simple_expression),
    )),

    sign_expression: $ => prec('sign', seq(
      field('operator', $.sign_operator),
      field('expression', $._expression),
    )),

    hash_expression: $ => prec.left('hash', seq(
      field('left', $._simple_expression),
      field('operator', $.hash_operator),
      field('right', $._simple_expression),
    )),

    infix_expression: $ => {
      const table = [
        {
          operator: $.pow_operator,
          precedence: 'pow',
          associativity: 'right',
        },
        {
          operator: $.mult_operator,
          precedence: 'mult',
          associativity: 'left',
        },
        {
          operator: $.add_operator,
          precedence: 'add',
          associativity: 'left',
        },
        {
          operator: $.concat_operator,
          precedence: 'concat',
          associativity: 'right',
        },
        {
          operator: $.rel_operator,
          precedence: 'rel',
          associativity: 'left',
        },
        {
          operator: $.and_operator,
          precedence: 'and',
          associativity: 'right',
        },
        {
          operator: $.or_operator,
          precedence: 'or',
          associativity: 'right',
        },
        {
          operator: $.assign_operator,
          precedence: 'assign',
          associativity: 'right',
        },
      ];

      return choice(...table.map(({operator, precedence, associativity}) =>
        prec[associativity](precedence, seq(
          field('left', $._expression),
          field('operator', operator),
          field('right', $._expression),
        )),
      ));
    },

    field_get_expression: $ => prec.left('dot', seq(
      field('record', $._simple_expression),
      '.',
      field('field', $.field_path),
    )),

    array_get_expression: $ => prec('dot', seq(
      field('array', $._simple_expression),
      '.',
      optional(field('operator', $.indexing_operator_path)),
      '(',
      field('index', $._sequence_expression),
      ')',
    )),

    string_get_expression: $ => prec('dot', seq(
      field('string', $._simple_expression),
      '.',
      optional(field('operator', $.indexing_operator_path)),
      '[',
      field('index', $._sequence_expression),
      ']',
    )),

    bigarray_get_expression: $ => prec('dot', seq(
      field('array', $._simple_expression),
      '.',
      optional(field('operator', $.indexing_operator_path)),
      '{',
      field('index', $._sequence_expression),
      '}',
    )),

    set_expression: $ => prec('set', seq(
      choice(
        $.field_get_expression,
        $.array_get_expression,
        $.string_get_expression,
        $.bigarray_get_expression,
        $._instance_variable_name,
      ),
      '<-',
      field('body', $._expression),
    )),

    if_expression: $ => prec.right(seq(
      'if',
      optional($._attribute),
      field('condition', $._sequence_expression),
      $.then_clause,
      optional($.else_clause),
    )),

    then_clause: $ => prec('if', seq(
      'then',
      field('expression', $._expression),
    )),

    else_clause: $ => prec('if', seq(
      'else',
      field('expression', $._expression),
    )),

    while_expression: $ => seq(
      'while',
      optional($._attribute),
      field('condition', $._sequence_expression),
      $.do_clause,
    ),

    do_clause: $ => seq(
      'do',
      optional($._sequence_expression),
      'done',
    ),

    for_expression: $ => seq(
      'for',
      optional($._attribute),
      field('name', $._value_pattern),
      '=',
      field('from', $._sequence_expression),
      choice('to', 'downto'),
      field('to', $._sequence_expression),
      $.do_clause,
    ),

    _sequence_expression_anonymous: $ => prec.right('seq', seq(
      $._expression,
      ';',
      optional(seq(
        optional($._attribute),
        choice($._expression, $._sequence_expression_anonymous),
      )),
    )),

    match_expression: $ => seq(
      choice(
        seq('match', optional($._attribute)),
        $.match_operator,
      ),
      field('expression', $._sequence_expression),
      'with',
      $._match_cases,
    ),

    _match_cases: $ => prec.right(seq(
      optional('|'),
      sep1('|', $.match_case),
    )),

    match_case: $ => seq(
      field('pattern', $._pattern),
      optional($.guard),
      '->',
      field('body', choice($._sequence_expression, $.refutation_case)),
    ),

    guard: $ => seq(
      'when',
      field('expression', $._sequence_expression),
    ),

    refutation_case: $ => '.',

    function_expression: $ => seq(
      'function',
      optional($._attribute),
      $._match_cases,
    ),

    fun_expression: $ => seq(
      'fun',
      optional($._attribute),
      repeat1($._parameter),
      optional($._simple_typed),
      '->',
      field('body', $._sequence_expression),
    ),

    try_expression: $ => seq(
      'try',
      optional($._attribute),
      field('expression', $._sequence_expression),
      'with',
      $._match_cases,
    ),

    let_expression: $ => seq(
      $.value_definition,
      'in',
      field('body', $._sequence_expression),
    ),

    coercion_expression: $ => parenthesize(seq(
      field('expression', $._sequence_expression),
      optional($._typed),
      ':>',
      field('coercion', $._type),
    )),

    assert_expression: $ => seq(
      'assert',
      optional($._attribute),
      field('expression', $._simple_expression),
    ),

    lazy_expression: $ => seq(
      'lazy',
      optional($._attribute),
      field('expression', $._simple_expression),
    ),

    let_module_expression: $ => seq(
      'let',
      $.module_definition,
      'in',
      field('body', $._sequence_expression),
    ),

    let_open_expression: $ => seq(
      'let',
      $.open_module,
      'in',
      field('body', $._sequence_expression),
    ),

    local_open_expression: $ => seq(
      $.module_path,
      '.',
      choice(
        parenthesize(optional(field('expression', $._sequence_expression))),
        field('expression', $.list_expression),
        field('expression', $.array_expression),
        field('expression', $.record_expression),
        field('expression', $.object_copy_expression),
        field('expression', $.package_expression),
      ),
    ),

    package_expression: $ => parenthesize(seq(
      'module',
      optional($._attribute),
      field('module', $._module_expression),
      optional($._module_typed),
    )),

    let_exception_expression: $ => seq(
      'let',
      $.exception_definition,
      'in',
      field('body', $._sequence_expression),
    ),

    new_expression: $ => seq(
      'new',
      optional($._attribute),
      $.class_path,
    ),

    object_copy_expression: $ => seq(
      '{<',
      sep(';', $.instance_variable_expression),
      optional(';'),
      '>}',
    ),

    instance_variable_expression: $ => seq(
      $._instance_variable_name,
      optional(seq('=', field('expression', $._expression))),
    ),

    method_invocation: $ => prec('method', seq(
      field('object', $._simple_expression),
      '#',
      field('method', $._method_name),
    )),

    object_expression: $ => seq(
      'object',
      optional($._attribute),
      optional(parenthesize(seq(
        field('self', $._pattern),
        optional($._typed),
      ))),
      repeat(choice(
        $._class_field,
        $.floating_attribute,
      )),
      'end',
    ),

    parenthesized_expression: $ => choice(
      seq(
        'begin',
        optional($._attribute),
        field('expression', $._sequence_expression),
        'end',
      ),
      parenthesize(field('expression', $._sequence_expression)),
    ),

    ocamlyacc_value: $ => /\$[0-9]+/,

    // Patterns

    _simple_pattern: $ => choice(
      $._value_pattern,
      $._signed_constant,
      $.typed_pattern,
      $.constructor_path,
      $.tag,
      $.polymorphic_variant_pattern,
      $.record_pattern,
      $.list_pattern,
      $.array_pattern,
      $.local_open_pattern,
      $.package_pattern,
      $.parenthesized_pattern,
      $._extension,
    ),

    _effect_pattern: $ => choice(
      $._simple_pattern,
      $.constructor_pattern,
      $.tag_pattern,
      $.lazy_pattern,
    ),

    _pattern: $ => choice(
      $._effect_pattern,
      $.alias_pattern,
      alias($._or_pattern_anonymous, $.or_pattern),
      alias($._tuple_pattern, $.tuple_pattern),
      $.cons_pattern,
      $.range_pattern,
      $.exception_pattern,
      $.effect_pattern,
    ),

    _simple_binding_pattern: $ => choice(
      $._value_name,
      $._signed_constant,
      alias($.typed_binding_pattern, $.typed_pattern),
      $.constructor_path,
      $.tag,
      $.polymorphic_variant_pattern,
      alias($.record_binding_pattern, $.record_pattern),
      alias($.list_binding_pattern, $.list_pattern),
      alias($.array_binding_pattern, $.array_pattern),
      alias($.local_open_binding_pattern, $.local_open_pattern),
      $.package_pattern,
      alias($.parenthesized_binding_pattern, $.parenthesized_pattern),
      $._extension,
    ),

    _binding_pattern: $ => choice(
      $._simple_binding_pattern,
      alias($.alias_binding_pattern, $.alias_pattern),
      alias($._or_binding_pattern_anonymous, $.or_pattern),
      alias($.constructor_binding_pattern, $.constructor_pattern),
      alias($.tag_binding_pattern, $.tag_pattern),
      alias($._tuple_binding_pattern, $.tuple_pattern),
      alias($.cons_binding_pattern, $.cons_pattern),
      $.range_pattern,
      alias($.lazy_binding_pattern, $.lazy_pattern),
    ),

    alias_pattern: $ => prec('alias_pattern', seq(
      field('pattern', $._pattern),
      'as',
      field('alias', $._value_pattern),
    )),

    alias_binding_pattern: $ => prec('alias_pattern', seq(
      field('pattern', $._binding_pattern),
      'as',
      field('alias', $._value_name),
    )),

    typed_pattern: $ => parenthesize(seq(
      field('pattern', $._pattern),
      $._typed,
    )),

    typed_binding_pattern: $ => parenthesize(seq(
      field('pattern', $._binding_pattern),
      $._typed,
    )),

    _or_pattern_anonymous: $ => prec.right('or_pattern', seq(
      $._pattern,
      '|',
      choice($._pattern, $._or_pattern_anonymous),
    )),

    _or_binding_pattern_anonymous: $ => prec.right('or_pattern', seq(
      $._binding_pattern,
      '|',
      choice($._binding_pattern, $._or_binding_pattern_anonymous),
    )),

    constructor_pattern: $ => prec('constructor_pattern', seq(
      $.constructor_path,
      optional(alias($._parenthesized_abstract_type, $.abstract_type)),
      field('pattern', $._pattern),
    )),

    constructor_binding_pattern: $ => prec('constructor_pattern', seq(
      $.constructor_path,
      optional(alias($._parenthesized_abstract_type, $.abstract_type)),
      field('pattern', $._binding_pattern),
    )),

    tag_pattern: $ => prec('constructor_pattern', seq(
      $.tag,
      field('pattern', $._pattern),
    )),

    tag_binding_pattern: $ => prec('constructor_pattern', seq(
      $.tag,
      field('pattern', $._binding_pattern),
    )),

    polymorphic_variant_pattern: $ => seq(
      '#',
      $.type_constructor_path,
    ),

    labeled_tuple_element_pattern: $ => choice(
      $._tuple_label,
      seq(
        $._tuple_label,
        token.immediate(':'),
        field('pattern', $._simple_pattern),
      ),
      seq('~', '(', $._label_name, $._typed, ')'),
    ),

    _tuple_pattern: $ => prec.right('tuple_pattern', seq(
      choice($._pattern, $.labeled_tuple_element_pattern),
      ',',
      choice($._pattern, $.labeled_tuple_element_pattern, $._tuple_pattern),
    )),

    labeled_tuple_element_binding_pattern: $ => choice(
      $._tuple_label,
      seq(
        $._tuple_label,
        token.immediate(':'),
        field('pattern', $._simple_binding_pattern),
      ),
      seq('~', '(', $._label_name, $._typed, ')'),
    ),

    _tuple_binding_pattern: $ => prec.right('tuple_pattern', seq(
      choice($._binding_pattern, $.labeled_tuple_element_binding_pattern),
      ',',
      choice($._binding_pattern, $.labeled_tuple_element_binding_pattern, $._tuple_binding_pattern),
    )),

    record_pattern: $ => seq(
      '{',
      sep1(';', $.field_pattern),
      optional(seq(';', '_')),
      optional(';'),
      '}',
    ),

    field_pattern: $ => seq(
      $.field_path,
      optional($._typed),
      optional(seq('=', field('pattern', $._pattern))),
    ),

    record_binding_pattern: $ => seq(
      '{',
      sep1(';', alias($.field_binding_pattern, $.field_pattern)),
      optional(seq(';', '_')),
      optional(';'),
      '}',
    ),

    field_binding_pattern: $ => seq(
      $.field_path,
      optional($._typed),
      optional(seq('=', field('pattern', $._binding_pattern))),
    ),

    list_pattern: $ => seq(
      '[',
      optional(seq(
        sep1(';', $._pattern),
        optional(';'),
      )),
      ']',
    ),

    list_binding_pattern: $ => seq(
      '[',
      optional(seq(
        sep1(';', $._binding_pattern),
        optional(';'),
      )),
      ']',
    ),

    cons_pattern: $ => prec.right('cons_pattern', seq(
      field('left', $._pattern),
      '::',
      field('right', $._pattern),
    )),

    cons_binding_pattern: $ => prec.right('cons_pattern', seq(
      field('left', $._binding_pattern),
      '::',
      field('right', $._binding_pattern),
    )),

    array_pattern: $ => seq(
      '[|',
      optional(seq(
        sep1(';', $._pattern),
        optional(';'),
      )),
      '|]',
    ),

    array_binding_pattern: $ => seq(
      '[|',
      optional(seq(
        sep1(';', $._binding_pattern),
        optional(';'),
      )),
      '|]',
    ),

    range_pattern: $ => prec('range_pattern', seq(
      field('left', $._signed_constant),
      '..',
      field('right', $._signed_constant),
    )),

    lazy_pattern: $ => prec('lazy_pattern', seq(
      'lazy',
      optional($._attribute),
      field('pattern', $._pattern),
    )),

    lazy_binding_pattern: $ => prec('lazy_pattern', seq(
      'lazy',
      optional($._attribute),
      field('pattern', $._binding_pattern),
    )),

    local_open_pattern: $ => seq(
      $.module_path,
      '.',
      choice(
        parenthesize(optional(field('pattern', $._pattern))),
        field('pattern', $.list_pattern),
        field('pattern', $.array_pattern),
        field('pattern', $.record_pattern),
      ),
    ),

    local_open_binding_pattern: $ => seq(
      $.module_path,
      '.',
      choice(
        parenthesize(optional(field('pattern', $._binding_pattern))),
        field('pattern', $.list_binding_pattern),
        field('pattern', $.array_binding_pattern),
        field('pattern', $.record_binding_pattern),
      ),
    ),

    package_pattern: $ => parenthesize(seq(
      'module',
      optional($._attribute),
      choice($._module_name, alias('_', $.module_name)),
      optional($._module_typed),
    )),

    parenthesized_pattern: $ => parenthesize($._pattern),

    parenthesized_binding_pattern: $ => parenthesize($._binding_pattern),

    exception_pattern: $ => prec('exception_pattern', seq(
      'exception',
      optional($._attribute),
      field('pattern', $._pattern),
    )),

    effect_pattern: $ => seq(
      'effect',
      field('effect', $._effect_pattern),
      ',',
      field('continuation', $._simple_pattern),
    ),

    // Attributes and extensions

    attribute: $ => seq(
      alias(/\[@/, '[@'),
      $.attribute_id,
      optional($.attribute_payload),
      ']',
    ),

    item_attribute: $ => seq(
      '[@@',
      $.attribute_id,
      optional($.attribute_payload),
      ']',
    ),

    floating_attribute: $ => seq(
      '[@@@',
      $.attribute_id,
      optional($.attribute_payload),
      ']',
    ),

    attribute_payload: $ => choice(
      $._structure,
      seq(':', optional(choice($._type, $._signature))),
      seq(
        '?',
        $._pattern,
        optional($.guard),
      ),
    ),

    _extension: $ => choice(
      $.extension,
      $.quoted_extension,
    ),

    extension: $ => seq(
      '[%',
      $.attribute_id,
      optional($.attribute_payload),
      ']',
    ),

    quoted_extension: $ => seq(
      '{%',
      $.attribute_id,
      optional(/\s+/),
      $._quoted_string,
      '}',
    ),

    _item_extension: $ => choice(
      $.item_extension,
      $.quoted_item_extension,
    ),

    item_extension: $ => seq(
      '[%%',
      $.attribute_id,
      optional($.attribute_payload),
      ']',
      repeat($.item_attribute),
    ),

    quoted_item_extension: $ => seq(
      '{%%',
      $.attribute_id,
      optional(/\s+/),
      $._quoted_string,
      '}',
      repeat($.item_attribute),
    ),

    _attribute: $ => seq('%', $.attribute_id),

    // Constants

    _constant: $ => choice(
      $.number,
      $.character,
      $.string,
      $.quoted_string,
      $.boolean,
      $.unit,
    ),

    _signed_constant: $ => choice(
      $._constant,
      $.signed_number,
    ),

    number: $ => NUMBER,

    signed_number: $ => seq(/[+-]/, NUMBER),

    character: $ => seq('\'', $.character_content, token.immediate('\'')),

    character_content: $ => choice(
      token.immediate(/\r*\n/),
      token.immediate(/[^\\'\r\n]/),
      $._null,
      $.escape_sequence,
    ),

    string: $ => seq('"', optional($.string_content), '"'),

    string_content: $ => repeat1(choice(
      token.immediate(/\s/),
      token.immediate(/\[@/),
      /[^\\"%@]+|%|@/,
      $._null,
      $.escape_sequence,
      alias(/\\u\{[0-9A-Fa-f]+\}/, $.escape_sequence),
      alias(/\\\r*\n[\t ]*/, $.escape_sequence),
      $.conversion_specification,
      $.pretty_printing_indication,
    )),

    quoted_string: $ => seq('{', $._quoted_string, '}'),

    _quoted_string: $ => seq(
      $._left_quoted_string_delimiter,
      optional($.quoted_string_content),
      $._right_quoted_string_delimiter,
    ),

    quoted_string_content: $ => repeat1(choice(
      token.immediate(/\s/),
      token.immediate(/\[@/),
      /[^%@|]+|%|@|\|/,
      $._null,
      $.conversion_specification,
      $.pretty_printing_indication,
    )),

    escape_sequence: $ => choice(
      /\\[\\"'ntbr ]/,
      /\\[0-9][0-9][0-9]/,
      /\\x[0-9A-Fa-f][0-9A-Fa-f]/,
      /\\o[0-3][0-7][0-7]/,
    ),

    conversion_specification: $ => token(seq(
      '%',
      optional(/[\-0+ #]/),
      optional(/[1-9][0-9]*|\*/),
      optional(/\.([0-9]*|\*)/),
      choice(
        /[diunlLNxXosScCfFeEgGhHbBat!%@,]/,
        /[lnL][diuxXo]/,
      ),
    )),

    pretty_printing_indication: $ => /@([\[\], ;.{}?]|\\n|<[0-9]+>)/,

    boolean: $ => choice('true', 'false'),

    unit: $ => choice(
      seq('(', ')'),
      seq('begin', 'end'),
    ),

    // Operators

    prefix_operator: $ => token(choice(
      seq('!', choice(optional(/[#!$%&*+\-./:<>?@^|~]/), repeat2(HASH_OP_CHAR))),
      seq(/[~?]/, repeat1(HASH_OP_CHAR)),
    )),

    sign_operator: $ => choice(/[+-]/, /[+-]\./),

    _infix_operator: $ => choice(
      $.pow_operator,
      $.mult_operator,
      $.add_operator,
      $.concat_operator,
      $.rel_operator,
      $.and_operator,
      $.or_operator,
      $.assign_operator,
    ),

    hash_operator: $ => token(seq('#', repeat1(HASH_OP_CHAR))),

    pow_operator: $ => token(choice(
      'lsl', 'lsr', 'asr',
      seq('**', repeat(OP_CHAR)),
    )),

    mult_operator: $ => token(choice(
      'mod', 'land', 'lor', 'lxor',
      seq(/[*/%]/, repeat(OP_CHAR)),
    )),

    add_operator: $ => choice(
      /[+-]/, /[+-]\./,
      token(choice(
        seq('+', repeat1(OP_CHAR)),
        seq('-', choice(repeat1(/[!$%&*+\-./:<=?@^|~]/), repeat2(OP_CHAR))),
      )),
    ),

    concat_operator: $ => token(
      seq(/[@^]/, repeat(OP_CHAR)),
    ),

    rel_operator: $ => token(choice(
      seq(/[=>$]/, repeat(OP_CHAR)),
      seq('<', choice(optional(/[!$%&*+./:<=>?@^|~]/), repeat2(OP_CHAR))),
      seq('&', choice(/[!$%*+\-./:<=>?@^|~]/, repeat2(OP_CHAR))),
      seq('|', choice(/[!$%&*+\-./:<=>?@^~]/, repeat2(OP_CHAR))),
      '!=',
    )),

    and_operator: $ => token(choice('&', '&&')),

    or_operator: $ => token(choice('or', '||')),

    assign_operator: $ => /:=/,

    indexing_operator: $ => token(
      seq(/[!$%&*+\-/:=>?@^|]/, repeat(OP_CHAR)),
    ),

    indexing_operator_path: $ => path($.module_path, $.indexing_operator),

    let_operator: $ => token(
      seq('let', /[$&*+\-/<=>@^|]/, repeat(OP_CHAR)),
    ),

    let_and_operator: $ => token(
      seq('and', /[$&*+\-/<=>@^|]/, repeat(OP_CHAR)),
    ),

    match_operator: $ => token(
      seq('match', /[$&*+\-/<=>@^|]/, repeat(OP_CHAR)),
    ),

    // Names

    _value_name: $ => choice(
      alias($._lowercase_identifier, $.value_name),
      $.parenthesized_operator,
    ),

    _simple_value_pattern: $ => alias($._lowercase_identifier, $.value_pattern),

    _value_pattern: $ => choice(
      $._simple_value_pattern,
      $.parenthesized_operator,
    ),

    parenthesized_operator: $ => parenthesize(choice(
      $.prefix_operator,
      $._infix_operator,
      $.hash_operator,
      seq(
        '.',
        $.indexing_operator,
        choice(
          seq('(', optional(seq(';', '..')), ')'),
          seq('[', optional(seq(';', '..')), ']'),
          seq('{', optional(seq(';', '..')), '}'),
        ),
        optional('<-'),
      ),
      $.let_operator,
      $.let_and_operator,
      $.match_operator,
    )),

    value_path: $ => path($.module_path, $._value_name),

    module_path: $ => path($.module_path, $._module_name),

    extended_module_path: $ => choice(
      path($.extended_module_path, $._module_name),
      seq(
        $.extended_module_path,
        parenthesize($.extended_module_path),
      ),
    ),

    module_type_path: $ => path($.extended_module_path, $._module_type_name),

    field_path: $ => path($.module_path, $._field_name),

    constructor_path: $ => path($.module_path, $._constructor_name),

    type_constructor_path: $ => path($.extended_module_path, $._type_constructor),

    class_path: $ => path($.module_path, $._class_name),

    class_type_path: $ => path($.extended_module_path, $._class_type_name),

    _label_name: $ => alias($._lowercase_identifier, $.label_name),
    _field_name: $ => alias($._lowercase_identifier, $.field_name),
    _class_name: $ => alias($._lowercase_identifier, $.class_name),
    _class_type_name: $ => alias($._lowercase_identifier, $.class_type_name),
    _method_name: $ => alias($._lowercase_identifier, $.method_name),
    _type_constructor: $ => alias($._lowercase_identifier, $.type_constructor),
    _instance_variable_name: $ => alias($._lowercase_identifier, $.instance_variable_name),

    _module_name: $ => alias($._uppercase_identifier, $.module_name),
    _module_type_name: $ => alias(choice($._uppercase_identifier, $._lowercase_identifier), $.module_type_name),
    _constructor_name: $ => choice(
      alias($._uppercase_identifier, $.constructor_name),
      parenthesize(alias('::', $.constructor_name)),
    ),

    _lowercase_identifier: $ => /(\\#)?[\p{Ll}_][\p{XID_Continue}']*/,
    _uppercase_identifier: $ => /[\p{Lu}][\p{XID_Continue}']*/,

    _label: $ => seq(choice('~', '?'), $._label_name),
    _tuple_label: $ => seq('~', $._label_name),
    directive: $ => seq(/#/, choice($._lowercase_identifier, $._uppercase_identifier)),
    type_variable: $ => seq(/'/, choice($._lowercase_identifier, $._uppercase_identifier)),
    tag: $ => seq(/`/, choice($._lowercase_identifier, $._uppercase_identifier)),
    attribute_id: $ => sep1(/\./, choice($._lowercase_identifier, $._uppercase_identifier)),
  },

  externals: $ => [
    $.comment,
    $._left_quoted_string_delimiter,
    $._right_quoted_string_delimiter,
    '"',
    $.line_number_directive,
    $._null,
    $._error_sentinel,
  ],
});

/**
 * Creates a rule that matches zero or more
 * occurrences of `rule` separated by `delimiter`
 *
 * @param {RuleOrLiteral} delimiter
 * @param {RuleOrLiteral} rule
 * @returns {ChoiceRule}
 */
function sep(delimiter, rule) {
  return optional(sep1(delimiter, rule));
}

/**
 * Creates a rule that matches one or more
 * occurrences of `rule` separated by `delimiter`
 *
 * @param {RuleOrLiteral} delimiter
 * @param {RuleOrLiteral} rule
 * @returns {SeqRule}
 */
function sep1(delimiter, rule) {
  return seq(rule, repeat(seq(delimiter, rule)));
}

/**
 * Creates a rule that matches two or more occurrences of `rule`
 *
 * @param {RuleOrLiteral} rule
 * @returns {SeqRule}
 */
function repeat2(rule) {
  return seq(rule, repeat1(rule));
}

/**
 * Creates a rule that matches `rule` surrounded by parentheses
 *
 * @param {RuleOrLiteral} rule
 * @returns {SeqRule}
 */
function parenthesize(rule) {
  return seq('(', rule, ')');
}

/**
 * Creates a rule that matches `final` prefixed by
 * zero or more occurrences of `prefix` separated by a dot
 *
 * @param {RuleOrLiteral} prefix
 * @param {RuleOrLiteral} final
 * @returns {ChoiceRule}
 */
function path(prefix, final) {
  return choice(final, seq(prefix, '.', final));
}
