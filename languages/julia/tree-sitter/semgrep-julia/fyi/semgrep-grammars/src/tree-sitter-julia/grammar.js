/**
 * @file Julia grammar for tree-sitter
 * @author Max Brunsfeld <maxbrunsfeld@gmail.com>
 * @author Sergio A. Vargas <savargasqu+git@unal.edu.co>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const PREC = [
  'afunc',
  'pair',
  'conditional',
  'arrow',
  'lazy_or',
  'lazy_and',
  'where', // FIXME
  'comparison',
  'pipe_left',
  'pipe_right',
  'colon',
  'plus',
  'times',
  'rational',
  'bitshift',
  'prefix',
  'power',
  'decl',
  'dot',
].reduce((result, name, index) => {
  result[name] = index + 10;
  return result;
}, {});

PREC.array = -1;
PREC.tuple = -1;
PREC.assign = -2;
PREC.stmt = -3;
PREC.macro_arg = -4;

const OPERATORS = {
  assignment: `
    += -= *= /= //= \\= ^= %= <<= >>= >>>= |= &=
    −= ÷= ⊻= ≔ ⩴ ≕
  `,

  arrow: `
    <-- --> <-->
    ← → ↔ ↚ ↛ ↞ ↠ ↢ ↣ ↦ ↤ ↮ ⇎ ⇍ ⇏ ⇐ ⇒ ⇔ ⇴ ⇶ ⇷ ⇸ ⇹ ⇺ ⇻ ⇼ ⇽ ⇾ ⇿ ⟵ ⟶ ⟷ ⟹ ⟺ ⟻ ⟼ ⟽ ⟾ ⟿
    ⤀ ⤁ ⤂ ⤃ ⤄ ⤅ ⤆ ⤇ ⤌ ⤍ ⤎ ⤏ ⤐ ⤑ ⤔ ⤕ ⤖ ⤗ ⤘ ⤝ ⤞ ⤟ ⤠ ⥄ ⥅ ⥆ ⥇ ⥈ ⥊ ⥋ ⥎ ⥐ ⥒ ⥓ ⥖ ⥗ ⥚ ⥛ ⥞
    ⥟ ⥢ ⥤ ⥦ ⥧ ⥨ ⥩ ⥪ ⥫ ⥬ ⥭ ⥰ ⧴ ⬱ ⬰ ⬲ ⬳ ⬴ ⬵ ⬶ ⬷ ⬸ ⬹ ⬺ ⬻ ⬼ ⬽ ⬾ ⬿ ⭀ ⭁ ⭂ ⭃ ⥷ ⭄ ⥺ ⭇ ⭈ ⭉
    ⭊ ⭋ ⭌ ￩ ￫ ⇜ ⇝ ↜ ↝ ↩ ↪ ↫ ↬ ↼ ↽ ⇀ ⇁ ⇄ ⇆ ⇇ ⇉ ⇋ ⇌ ⇚ ⇛ ⇠ ⇢ ↷ ↶ ↺ ↻
  `,

  comparison: `
    > < >= <= == === != !==
    ≥ ≤ ≡ ≠ ≢ ∈ ∉ ∋ ∌ ⊆ ⊈ ⊂ ⊄ ⊊ ∝ ∊ ∍ ∥ ∦ ∷ ∺ ∻ ∽ ∾ ≁ ≃ ≂ ≄ ≅ ≆ ≇ ≈ ≉ ≊ ≋ ≌ ≍ ≎ ≐
    ≑ ≒ ≓ ≖ ≗ ≘ ≙ ≚ ≛ ≜ ≝ ≞ ≟ ≣ ≦ ≧ ≨ ≩ ≪ ≫ ≬ ≭ ≮ ≯ ≰ ≱ ≲ ≳ ≴ ≵ ≶ ≷ ≸ ≹ ≺ ≻ ≼ ≽ ≾
    ≿ ⊀ ⊁ ⊃ ⊅ ⊇ ⊉ ⊋ ⊏ ⊐ ⊑ ⊒ ⊜ ⊩ ⊬ ⊮ ⊰ ⊱ ⊲ ⊳ ⊴ ⊵ ⊶ ⊷ ⋍ ⋐ ⋑ ⋕ ⋖ ⋗ ⋘ ⋙ ⋚ ⋛ ⋜ ⋝ ⋞ ⋟ ⋠
    ⋡ ⋢ ⋣ ⋤ ⋥ ⋦ ⋧ ⋨ ⋩ ⋪ ⋫ ⋬ ⋭ ⋲ ⋳ ⋴ ⋵ ⋶ ⋷ ⋸ ⋹ ⋺ ⋻ ⋼ ⋽ ⋾ ⋿ ⟈ ⟉ ⟒ ⦷ ⧀ ⧁ ⧡ ⧣ ⧤ ⧥ ⩦ ⩧
    ⩪ ⩫ ⩬ ⩭ ⩮ ⩯ ⩰ ⩱ ⩲ ⩳ ⩵ ⩶ ⩷ ⩸ ⩹ ⩺ ⩻ ⩼ ⩽ ⩾ ⩿ ⪀ ⪁ ⪂ ⪃ ⪄ ⪅ ⪆ ⪇ ⪈ ⪉ ⪊ ⪋ ⪌ ⪍ ⪎ ⪏ ⪐ ⪑
    ⪒ ⪓ ⪔ ⪕ ⪖ ⪗ ⪘ ⪙ ⪚ ⪛ ⪜ ⪝ ⪞ ⪟ ⪠ ⪡ ⪢ ⪣ ⪤ ⪥ ⪦ ⪧ ⪨ ⪩ ⪪ ⪫ ⪬ ⪭ ⪮ ⪯ ⪰ ⪱ ⪲ ⪳ ⪴ ⪵ ⪶ ⪷ ⪸
    ⪹ ⪺ ⪻ ⪼ ⪽ ⪾ ⪿ ⫀ ⫁ ⫂ ⫃ ⫄ ⫅ ⫆ ⫇ ⫈ ⫉ ⫊ ⫋ ⫌ ⫍ ⫎ ⫏ ⫐ ⫑ ⫒ ⫓ ⫔ ⫕ ⫖ ⫗ ⫘ ⫙ ⫷ ⫸ ⫹ ⫺ ⊢ ⊣
    ⟂ ⫪ ⫫
  `,

  ellipsis: '… ⁝ ⋮ ⋱ ⋰ ⋯',

  plus: `
    ++ |
    − ¦ ⊕ ⊖ ⊞ ⊟ ∪ ∨ ⊔ ∔ ∸ ≏ ⊎ ⊻ ⊽ ⋎ ⋓ ⟇ ⧺ ⧻ ⨈ ⨢ ⨣ ⨤ ⨥ ⨦ ⨧ ⨨ ⨩ ⨪ ⨫ ⨬ ⨭ ⨮ ⨹ ⨺ ⩁ ⩂ ⩅
    ⩊ ⩌ ⩏ ⩐ ⩒ ⩔ ⩖ ⩗ ⩛ ⩝ ⩡ ⩢ ⩣
  `,

  times: `
    * / % & \\
    ⌿ ÷ · · ⋅ ∘ × ∩ ∧ ⊗ ⊘ ⊙ ⊚ ⊛ ⊠ ⊡ ⊓ ∗ ∙ ∤ ⅋ ≀ ⊼ ⋄ ⋆ ⋇ ⋉ ⋊ ⋋ ⋌ ⋏ ⋒ ⟑ ⦸ ⦼ ⦾ ⦿ ⧶ ⧷
    ⨇ ⨰ ⨱ ⨲ ⨳ ⨴ ⨵ ⨶ ⨷ ⨸ ⨻ ⨼ ⨽ ⩀ ⩃ ⩄ ⩋ ⩍ ⩎ ⩑ ⩓ ⩕ ⩘ ⩚ ⩜ ⩞ ⩟ ⩠ ⫛ ⊍ ▷ ⨝ ⟕ ⟖ ⟗ ⨟
  `,

  bitshift: '<< >> >>>',

  power: `
    ^
    ↑ ↓ ⇵ ⟰ ⟱ ⤈ ⤉ ⤊ ⤋ ⤒ ⤓ ⥉ ⥌ ⥍ ⥏ ⥑ ⥔ ⥕ ⥘ ⥙ ⥜ ⥝ ⥠ ⥡ ⥣ ⥥ ⥮ ⥯ ￪ ￬
  `,

  unary: '! ¬ √ ∛ ∜',

  unary_plus: '+ - ± ∓',
};

const ESCAPE_SEQUENCE = token(seq(
  '\\',
  choice(
    /[^uUx0-7]/,
    /[uU][0-9a-fA-F]{1,6}/, // unicode codepoints
    /[0-7]{1,3}/,
    /x[0-9a-fA-F]{2}/,
  ),
));

// Keywords that can be quoted. Some still fail depending on the context.
const KEYWORDS = choice(
  'baremodule',
  'module',
  'abstract',
  'primitive',
  'mutable',
  'struct',
  'quote',
  'let',
  'if',
  'else',
  'elseif',
  'try',
  'catch',
  'finally',
  'for',
  'while',
  'break',
  'continue',
  'using',
  'import',
  'const',
  'global',
  'local',
  'end',
);

module.exports = grammar({
  name: 'julia',

  word: $ => $._word_identifier,

  inline: $ => [
    $._block_form,
    $._terminator,
    $._definition,
    $._statement,
    $._operation,
  ],

  externals: $ => [
    $._block_comment_rest,
    $._immediate_paren,
    $._immediate_bracket,
    $._immediate_brace,
    $._immediate_string_start,
    $._immediate_command_start,
    $._content_cmd_1,
    $._content_cmd_1_raw,
    $._content_cmd_3,
    $._content_cmd_3_raw,
    $._content_str_1,
    $._content_str_1_raw,
    $._content_str_3,
    $._content_str_3_raw,
    $._end_cmd,
    $._end_str,
  ],

  conflicts: $ => [
    [$.juxtaposition_expression, $._primary_expression], // adjoint
    [$.juxtaposition_expression, $._expression],
    [$.matrix_row, $.comprehension_expression], // Comprehensions with newlines
    [$.parenthesized_expression, $.tuple_expression],
  ],

  supertypes: $ => [
    $._expression,
    $._statement,
    $._definition,
  ],

  extras: $ => [
    /\s/,
    $.line_comment,
    $.block_comment,
  ],

  rules: {
    source_file: $ => optional(seq(
      sep1($._terminator, $._block_form),
      optional($._terminator)
    )),

    block: $ => seq(
      sep1($._terminator, $._block_form),
      optional($._terminator)
    ),

    _block_form: $ => choice(
      $._expression,
      $.assignment,
      $.open_tuple,
    ),

    _bracket_form: $ => choice(
      $._expression,
      alias($._closed_assignment, $.assignment),
    ),

    open_tuple: $ => prec(PREC.tuple, seq(
      $._expression,
      repeat1(seq(',', $._expression))
    )),

    // assignments inside blocks
    assignment: $ => prec.right(PREC.assign, seq(
      choice(
        $._primary_expression,
        $.open_tuple,
        $._operation,
        $.operator,
      ),
      alias('=', $.operator),
      $._block_form,
    )),

    // assignments inside brackets
    _closed_assignment: $ => prec.right(PREC.assign, seq(
      choice(
        $._primary_expression,
        $._operation,
        $.operator,
      ),
      alias('=', $.operator),
      $._bracket_form,
    )),

    _expression: $ => choice(
      $._definition,
      $._statement,
      $._primary_expression,
      $._operation,
      $.compound_assignment_expression,
      $.macrocall_expression,
      $.arrow_function_expression,
      $.juxtaposition_expression,
      $.ternary_expression,
      $.operator,
      $.integer_literal,
      $.float_literal,
      prec(-1, alias('begin', $.identifier)),
    ),

    // Definitions

    _definition: $ => choice(
      $.module_definition,
      $.abstract_definition,
      $.primitive_definition,
      $.struct_definition,
      $.function_definition,
      $.macro_definition,
    ),

    module_definition: $ => seq(
      choice('module', 'baremodule'),
      field('name', choice($.identifier, $.interpolation_expression)),
      optional($._terminator),
      optional($.block),
      'end',
    ),

    // TODO: Rename
    type_head: $ => prec(PREC.stmt, choice(
      $._primary_expression,
      $.binary_expression,
    )),

    abstract_definition: $ => seq(
      'abstract',
      'type',
      $.type_head,
      'end',
    ),

    primitive_definition: $ => seq(
      'primitive',
      'type',
      $.type_head,
      $.integer_literal,
      'end',
    ),

    struct_definition: $ => seq(
      optional('mutable'),
      'struct',
      $.type_head,
      optional($._terminator),
      optional($.block),
      'end',
    ),

    signature: $ => prec(PREC.stmt, choice(
      $.identifier, // zero-method definition
      $.call_expression,
      alias($.tuple_expression, $.argument_list), // anonymous function
      $.typed_expression,
      $.where_expression,
    )),

    function_definition: $ => seq(
      'function',
      $.signature,
      optional($._terminator),
      optional($.block),
      'end',
    ),

    macro_definition: $ => seq(
      'macro',
      $.signature,
      optional($._terminator),
      optional($.block),
      'end',
    ),


    // Statements

    _statement: $ => choice(
      // block statements:
      $.compound_statement,
      $.quote_statement,
      $.let_statement,
      $.if_statement,
      $.try_statement,
      $.for_statement,
      $.while_statement,
      // simple statements:
      $.break_statement,
      $.continue_statement,
      $.return_statement,
      $.const_statement,
      $.global_statement,
      $.local_statement,
      $.export_statement,
      $.import_statement,
      $.public_statement,
      $.using_statement,
    ),

    compound_statement: $ => seq('begin', optional($._terminator), optional($.block), 'end'),

    quote_statement: $ => seq('quote', optional($._terminator), optional($.block), 'end'),

    let_statement: $ => seq(
      'let',
      sep(',', $._bracket_form),
      $._terminator,
      optional($.block),
      'end',
    ),

    if_statement: $ => seq(
      'if',
      field('condition', $._expression),
      optional($._terminator),
      optional($.block),
      field('alternative', repeat($.elseif_clause)),
      field('alternative', optional($.else_clause)),
      'end',
    ),

    elseif_clause: $ => seq(
      'elseif',
      field('condition', $._expression),
      optional($._terminator),
      optional($.block),
    ),

    else_clause: $ => seq(
      'else',
      optional($._terminator),
      optional($.block),
    ),

    try_statement: $ => seq(
      'try',
      optional($._terminator),
      optional($.block),
      choice(
        seq(
          $.catch_clause,
          optional($.else_clause),
          optional($.finally_clause),
        ),
        seq(
          $.finally_clause,
          optional($.catch_clause),
          // `else` is not valid here.
        ),
      ),
      'end',
    ),

    catch_clause: $ => prec(1, seq(
      'catch',
      optional($.identifier),
      optional($._terminator),
      optional($.block),
    )),

    finally_clause: $ => seq(
      'finally',
      optional($._terminator),
      optional($.block),
    ),

    for_statement: $ => seq(
      'for',
      sep1(',', $.for_binding),
      optional($._terminator),
      optional($.block),
      'end',
    ),

    while_statement: $ => seq(
      'while',
      field('condition', $._expression),
      optional($._terminator),
      optional($.block),
      'end',
    ),

    break_statement: _ => 'break',

    continue_statement: _ => 'continue',

    return_statement: $ => prec.right(PREC.stmt, seq(
      'return',
      optional($._block_form),
    )),

    const_statement: $ => prec.right(PREC.stmt, seq(
      'const',
      $._block_form,
    )),

    global_statement: $ => prec.right(PREC.stmt, seq(
      'global',
      $._block_form,
    )),

    local_statement: $ => prec.right(PREC.stmt, seq(
      'local',
      $._block_form,
    )),

    import_alias: $ => seq($._importable, 'as', $._exportable),

    import_path: $ => seq(
      token(repeat1('.')),
      choice(
        $.identifier,
        $._scoped_identifier,
      ),
    ),

    _exportable: $ => choice(
      $.identifier,
      $.macro_identifier,
      $.operator,
      $.interpolation_expression,
      parenthesize($._exportable),
    ),

    _importable: $ => choice(
      $._exportable,
      alias($._scoped_identifier, $.import_path),
      $.import_path,
    ),

    _import_list: $ => prec.right(sep1(',', choice(
      $._importable,
      $.import_alias,
    ))),

    selected_import: $ => seq(
      $._importable,
      token.immediate(':'),
      $._import_list,
    ),

    export_statement: $ => seq('export', prec.right(sep1(',', $._exportable))),

    public_statement: $ => seq('public', prec.right(sep1(',', $._exportable))),

    import_statement: $ => seq(
      'import',
      choice(
        $._import_list,
        $.selected_import,
      ),
    ),

    using_statement: $ => seq(
      'using',
      choice(
        $._import_list,
        $.selected_import,
      ),
    ),

    // Primary expressions can be called, indexed, accessed, and type parametrized.
    _primary_expression: $ => choice(
      $.identifier,
      $.boolean_literal,
      $.curly_expression, // Only valid in macros
      $.parenthesized_expression,
      $.tuple_expression,
      $._array,
      $._string,
      $.adjoint_expression,
      $.broadcast_call_expression,
      $.call_expression,
      alias($._closed_macrocall_expression, $.macrocall_expression),
      $.parametrized_type_expression,
      $.field_expression,
      $.index_expression,
      $.interpolation_expression,
      $.quote_expression,
    ),

    _array: $ => choice(
      $.comprehension_expression,
      $.matrix_expression,
      $.vector_expression,
    ),

    comprehension_expression: $ => prec(PREC.array, seq(
      '[',
      $._bracket_form,
      optional($._terminator),
      $.for_clause,
      repeat(choice(
        $.for_clause,
        $.if_clause,
      )),
      ']',
    )),

    generator: $ => seq(
      $._bracket_form,
      $.for_clause,
      repeat(choice(
        $.for_clause,
        $.if_clause,
      )),
    ),

    if_clause: $ => seq(
      'if',
      $._expression,
    ),

    for_clause: $ => prec.right(seq(
      'for',
      sep1(',', $.for_binding),
    )),

    for_binding: $ => prec(1, seq(
      optional('outer'),
      choice(
        $.identifier,
        $.tuple_expression,
        $.typed_expression,
        $.interpolation_expression,
      ),
      alias(choice('in', '=', '∈'), $.operator),
      $._expression,
    )),

    matrix_expression: $ => prec(PREC.array, seq(
      '[',
      $.matrix_row,
      repeat(seq($._terminator, $.matrix_row)),
      optional($._terminator),
      ']',
    )),

    matrix_row: $ => repeat1(prec(PREC.array, $._bracket_form)),

    vector_expression: $ => seq(
      '[',
      sep(',', $._bracket_form),
      optional(','),
      ']',
    ),

    parenthesized_expression: $ => prec.dynamic(1, parenthesize(
      sep1($._semicolon, choice(
        $._bracket_form,
        $.generator,
      )),
      optional($._semicolon),
    )),

    tuple_expression: $ => parenthesize(
      optional($._semicolon),
      sep(choice(',', $._semicolon), choice(
        $._bracket_form,
        $.generator,
      )),
      optional(','),
    ),

    curly_expression: $ => seq(
      '{',
      sep(',', $._bracket_form),
      optional(','),
      '}',
    ),

    adjoint_expression: $ => seq(
      $._primary_expression,
      token.immediate('\''),
    ),

    field_expression: $ => prec(PREC.dot, seq(
      field('value', $._primary_expression),
      token.immediate('.'),
      choice(
        $.identifier,
        $.interpolation_expression,
        $.quote_expression,
        $._string,
      ),
    )),

    index_expression: $ => seq(
      $._primary_expression,
      $._immediate_bracket,
      $._array,
    ),

    parametrized_type_expression: $ => seq(
      $._primary_expression,
      $._immediate_brace,
      $.curly_expression,
    ),

    call_expression: $ => seq(
      choice($._primary_expression, $.operator),
      $._immediate_paren,
      alias($.tuple_expression, $.argument_list),
      optional($.do_clause),
    ),

    broadcast_call_expression: $ => seq(
      $._primary_expression,
      token.immediate('.'),
      $._immediate_paren,
      alias($.tuple_expression, $.argument_list),
      optional($.do_clause),
    ),

    _qualified_macro_identifier: $ => seq(
      $._primary_expression,
      token.immediate('.'),
      $.macro_identifier,
    ),

    _macro_head: $ => choice(
      alias($._qualified_macro_identifier, $.field_expression),
      $.macro_identifier,
    ),

    _closed_macrocall_expression: $ => seq(
      $._macro_head,
      choice(
        seq($._immediate_brace, $.curly_expression),
        seq($._immediate_bracket, $._array),
        seq(
          $._immediate_paren,
          alias($.tuple_expression, $.argument_list),
          optional($.do_clause)
        ),
      ),
    ),

    macrocall_expression: $ => prec.right(seq($._macro_head, optional($.macro_argument_list))),

    macro_argument_list: $ => prec.left(repeat1(prec(PREC.macro_arg, $._block_form))),

    do_clause: $ => seq(
      'do',
      sep(',', $._bracket_form),
      $._terminator,
      optional($.block),
      'end',
    ),

    interpolation_expression: $ => prec.right(PREC.prefix, seq(
      '$',
      choice(
        $.integer_literal,
        $.float_literal,
        $.identifier,
        $.curly_expression,
        $.parenthesized_expression,
        $.tuple_expression,
        $._array,
        $._string,
      ),
    )),

    quote_expression: $ => prec.right(PREC.prefix, seq(
      ':',
      choice(
        $.integer_literal,
        $.float_literal,
        $._string,
        $.identifier,
        $.operator,
        seq($._immediate_brace, $.curly_expression),
        seq($._immediate_bracket, $._array),
        seq($._immediate_paren, choice(
          $.parenthesized_expression,
          $.tuple_expression,
          // Syntactic operators in parentheses
          parenthesize(
            alias(
              choice(
                '::', ':=', '.=', '=',
                $._assignment_operator,
                $._lazy_or_operator,
                $._lazy_and_operator,
                $._syntactic_operator,
              ),
              $.operator,
            ),
          ),
        )),
        // Syntactic operators without parentheses
        alias(
          choice(
            $._assignment_operator,
            $._lazy_or_operator,
            $._lazy_and_operator,
            $._syntactic_operator,
          ),
          $.operator,
        ),
        alias(token.immediate(KEYWORDS), $.identifier),
      ),
    )),


    // Operations

    _operation: $ => choice(
      $.unary_expression,
      $.binary_expression,
      $.range_expression,
      $.splat_expression,
      $.typed_expression,
      $.unary_typed_expression,
      $.where_expression,
    ),

    binary_expression: $ => {
      const table = [
        [prec.right, PREC.pair, $._pair_operator],
        [prec.right, PREC.arrow, $._arrow_operator],
        [prec.left, PREC.lazy_or, $._lazy_or_operator],
        [prec.left, PREC.lazy_and, $._lazy_and_operator],
        [prec.left, PREC.comparison, choice('in', 'isa', $._comparison_operator, $._type_order_operator)],
        [prec.right, PREC.pipe_left, $._pipe_left_operator],
        [prec.left, PREC.pipe_right, $._pipe_right_operator],
        [prec.left, PREC.colon, $._ellipsis_operator],
        [prec.left, PREC.plus, choice($._unary_plus_operator, $._plus_operator)],
        [prec.left, PREC.times, $._times_operator],
        [prec.left, PREC.rational, $._rational_operator],
        [prec.left, PREC.bitshift, $._bitshift_operator],
        [prec.left, PREC.power, $._power_operator],
      ];

      return choice(...table.map(([fn, prec, op]) => fn(prec, seq(
        $._expression,
        alias(op, $.operator),
        $._expression,
      ))));
    },

    unary_expression: $ => prec.right(PREC.prefix, seq(
      alias(choice(
        $._tilde_operator,
        $._type_order_operator,
        $._unary_operator,
        $._unary_plus_operator,
      ), $.operator),
      $._expression,
    )),

    range_expression: $ => prec.left(PREC.colon, seq(
      $._expression,
      token.immediate(':'),
      $._expression,
    )),

    splat_expression: $ => prec(PREC.colon, seq($._expression, '...')),

    ternary_expression: $ => prec.right(PREC.conditional, seq(
      $._expression,
      '?',
      $._bracket_form,
      ':',
      $._bracket_form,
    )),

    typed_expression: $ => prec(PREC.decl, seq(
      $._expression,
      '::',
      $._primary_expression,
    )),

    unary_typed_expression: $ => prec.right(PREC.prefix, seq(
      '::',
      $._primary_expression,
    )),

    arrow_function_expression: $ => prec.right(PREC.afunc, seq(
      choice(
        $.identifier,
        alias($.tuple_expression, $.argument_list),
        $.typed_expression,
      ),
      '->',
      $._bracket_form,
    )),

    juxtaposition_expression: $ => prec.left(seq(
      choice(
        $.integer_literal,
        $.float_literal,
        $.adjoint_expression,
      ),
      $._primary_expression,
    )),

    compound_assignment_expression: $ => prec.right(PREC.assign, seq(
      $._primary_expression,
      alias(choice($._assignment_operator, $._tilde_operator), $.operator),
      $._expression,
    )),

    where_expression: $ => prec.left(PREC.where, seq(
      $._expression,
      'where',
      $._expression,
    )),


    // Tokens

    macro_identifier: $ => seq('@', choice(
      $.identifier,
      $.operator,
      alias($._syntactic_operator, $.operator),
      alias($._scoped_identifier, $.field_expression),
    )),

    _scoped_identifier: $ => seq(
      choice($.identifier, $.interpolation_expression),
      repeat1(
        seq(
          token.immediate('.'),
          choice($.identifier, $.interpolation_expression),
        ),
      ),
    ),

    _word_identifier: _ => {
      const nonIdentifierCharacters = [
        '#',
        '$',
        ',',
        ':',
        ';',
        '@',
        '~',
        '(', ')',
        '{', '}',
        ...Object.values(OPERATORS),
      ].join(' ')
        .trim()
        .replace(/!/g, '')
        .replace(/-/g, '')
        .replace(/\\/g, '\\\\')
        .replace(/\s+/g, '');

      // Some symbols in Sm and So unicode categories that are identifiers
      const validMathSymbols = '°∀-∇∎-∑∫-∳';

      // Emojis are valid Julia identifiers but unsupported due to exploding parser size
      // todo(clason): check if regex can be optimized
      const start = `[_\\p{XID_Start}${validMathSymbols}&&[^0-9#*]]`;
      const rest = `[^"'\`\\s\\.\\-\\[\\]${nonIdentifierCharacters}]*`;
      return new RegExp(start + rest);
    },

    identifier: $ => $._word_identifier,

    // Literals

    boolean_literal: _ => choice('true', 'false'),

    integer_literal: _ => choice(
      token(seq('0b', numeral('01'))),
      token(seq('0o', numeral('0-7'))),
      token(seq('0x', numeral('0-9a-fA-F'))),
      numeral('0-9'),
    ),

    float_literal: _ => {
      const dec = numeral('0-9');
      const hex = numeral('0-9a-fA-F');
      const exponent = /[eEf][+-]?\d+/;
      const hex_exponent = /p[+-]?\d+/;

      const leading_period = token(seq(
        '.',
        dec,
        optional(exponent),
      ));

      // This has to be split into two tokens to avoid conflicts with ellipsis
      const trailing_period = seq(
        dec,
        token.immediate(seq(
          '.',
          optional(dec),
          optional(exponent),
        )),
      );

      const just_exponent = token(seq(dec, exponent));

      const hex_float = token(seq(
        choice(
          seq('0x', hex, optional('.'), optional(hex)),
          seq('0x.', hex),
        ),
        hex_exponent,
      ));

      return choice(leading_period, trailing_period, just_exponent, hex_float);
    },

    _string: $ => choice(
      $.character_literal,
      $.string_literal,
      $.command_literal,
      $.prefixed_string_literal,
      $.prefixed_command_literal,
    ),

    escape_sequence: _ => ESCAPE_SEQUENCE,

    character_literal: _ => token(seq(
      '\'',
      choice(
        /[^'\\]/,
        ESCAPE_SEQUENCE,
      ),
      '\'',
    )),

    _delimiter_str_1: _ => '"',
    _delimiter_str_3: _ => '"""',
    _delimiter_cmd_1: _ => '`',
    _delimiter_cmd_3: _ => '```',

    string_literal: $ => choice(
      seq(
        $._delimiter_str_1,
        repeat(choice(alias($._content_str_1, $.content), $.string_interpolation, $.escape_sequence)),
        $._end_str,
      ),
      seq(
        $._delimiter_str_3,
        repeat(choice(alias($._content_str_3, $.content), $.string_interpolation, $.escape_sequence)),
        $._end_str,
      ),
    ),

    command_literal: $ => choice(
      seq(
        $._delimiter_cmd_1,
        repeat(choice(alias($._content_cmd_1, $.content), $.string_interpolation, $.escape_sequence)),
        $._end_cmd,
      ),
      seq(
        $._delimiter_cmd_3,
        repeat(choice(alias($._content_cmd_3, $.content), $.string_interpolation, $.escape_sequence)),
        $._end_cmd,
      ),
    ),

    prefixed_string_literal: $ => prec.left(seq(
      field('prefix', $.identifier),
      $._immediate_string_start,
      choice(
        seq(
          $._delimiter_str_1,
          repeat(choice(alias($._content_str_1_raw, $.content), $.escape_sequence)),
          $._end_str,
        ),
        seq(
          $._delimiter_str_3,
          repeat(choice(alias($._content_str_3_raw, $.content), $.escape_sequence)),
          $._end_str,
        ),
      ),
      optional(field('suffix', $.identifier)),
    )),

    prefixed_command_literal: $ => prec.left(seq(
      field('prefix', $.identifier),
      $._immediate_command_start,
      choice(
        seq(
          $._delimiter_cmd_1,
          repeat(choice(alias($._content_cmd_1_raw, $.content), $.escape_sequence)),
          $._end_cmd,
        ),
        seq(
          $._delimiter_cmd_3,
          repeat(choice(alias($._content_cmd_3_raw, $.content), $.escape_sequence)),
          $._end_cmd,
        ),
      ),
      optional(field('suffix', $.identifier)),
    )),

    string_interpolation: $ => seq(
      '$',
      choice(
        $.identifier,
        seq($._immediate_paren, parenthesize($._bracket_form)),
      ),
    ),

    operator: $ => choice(
      // NOTE: Syntactic operators (&&, +=, etc) cannot be used as identifiers.
      $._pair_operator,
      $._arrow_operator,
      $._comparison_operator,
      $._pipe_left_operator,
      $._pipe_right_operator,
      $._ellipsis_operator,
      ':',
      $._plus_operator,
      $._times_operator,
      $._rational_operator,
      $._bitshift_operator,
      $._power_operator,
      $._tilde_operator,
      $._type_order_operator,
      $._unary_operator,
      $._unary_plus_operator,
    ),

    _assignment_operator: _ => choice(':=', '$=', '.=', addDot(OPERATORS.assignment)),

    _pair_operator: _ => addDot('=>'),

    _arrow_operator: _ => addDot(OPERATORS.arrow),

    _lazy_or_operator: _ => addDot('||'),

    _lazy_and_operator: _ => addDot('&&'),

    _comparison_operator: _ => addDot(OPERATORS.comparison),

    _pipe_right_operator: _ => addDot('|>'),

    _pipe_left_operator: _ => addDot('<|'),

    _ellipsis_operator: _ => token(choice('..', addDot(OPERATORS.ellipsis))),

    _plus_operator: _ => addDot(OPERATORS.plus),

    _times_operator: _ => addDot(OPERATORS.times),

    _rational_operator: _ => addDot('//'),

    _bitshift_operator: _ => addDot(OPERATORS.bitshift),

    _power_operator: _ => addDot(OPERATORS.power),


    _tilde_operator: _ => addDot('~'), // unary or assignment

    _type_order_operator: _ => addDot('<: >:'), // unary or comparison

    _unary_operator: _ => addDot(OPERATORS.unary),

    _unary_plus_operator: _ => addDot(OPERATORS.unary_plus),


    _syntactic_operator: _ => choice('$', '.', '...', '->', '?'),


    _semicolon: _ => seq(';', repeat(token.immediate(';'))),

    _terminator: $ => choice(/\r?\n/, $._semicolon),

    block_comment: $ => seq(/#=/, $._block_comment_rest),

    // FIXME: This is currently a seq to avoid conflicts with block_comment
    line_comment: _ => seq(/#/, /.*/),
  },
});

/**
 *
 * @param {RuleOrLiteral} separator
 * @param {RuleOrLiteral} rule
 */
function sep(separator, rule) {
  return optional(sep1(separator, rule));
}

/**
 *
 * @param {RuleOrLiteral} separator
 * @param {RuleOrLiteral} rule
 */
function sep1(separator, rule) {
  return seq(rule, repeat(seq(separator, rule)));
}

/**
 *
 * @param {string} operatorString
 */
function addDot(operatorString) {
  const operators = operatorString.trim().split(/\s+/);
  return token(seq(optional('.'), operators.length > 1 ? choice(...operators) : operators[0]));
}

/**
 * @param {string} range
 */
function numeral(range) {
  return RegExp(`[${range}]([${range}]|_[${range}])*`);
}

/**
 *
 * @param {...any} rules
 */
function parenthesize(...rules) {
  return seq('(', ...rules, ')');
}
