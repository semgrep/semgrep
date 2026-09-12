/*
  semgrep-move-on-aptos

  Extends the standard move-on-aptos grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-move-on-aptos/grammar');

const FIELD_PREC = 14;
const UNARY_PREC = 13;

module.exports = grammar(base_grammar, {
  name: 'move_on_aptos',

  conflicts: ($, previous) => previous.concat([
    [$.typed_metavariable, $.name_access_chain],
    [$.term, $.declaration],
    [$._module_path, $.spec_block_target]
  ]),

  precedences: ($, previous) => previous.concat([
    [$._sequence_item, $.declaration],
    [$._script_use_decl, $._script_constant_decl, $._script_func_decl, $._script_spec_block],
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     if they're not already part of the base grammar.
  */
  rules: {
    // Semgrep components, source: semgrep-rust
    ellipsis: $ => '...',
    deep_ellipsis: $ => seq('<...', $._expr, '...>'),

    // Typed metavariable (an expression, not a parameter)
    // This is grammatically indistinguishable from `$.type_hint_expr: $ => seq('(', $._expr, ':', $.type, ')')`.
    // This will be handled by the semgrep converter by checking the metavariable name (`$`).
    typed_metavariable: $ => seq('(', $.identifier, ':', $.type, ')'),

    // Alternate "entry point". Allows parsing a standalone expression.
    semgrep_expression: $ => choice(
      $._expr,
      $.let_expr,
    ),

    // Alternate "entry point". Allows parsing a standalone list of sequence items (statements).
    semgrep_statement: $ => repeat1(choice(
      $._sequence_item,
      $.declaration,
    )),

    // Alternate "entry point". Allows parsing partial declarations (signatures).
    semgrep_partial: $ => seq(
      optional($.attributes),
      repeat($.module_member_modifier),
      choice(
        $._function_signature,
        $._struct_signature,
        $._enum_signature,
      )
    ),

    // Extend the source_file rule to allow semgrep constructs
    source_file: ($, previous) => choice(
      previous,
      $.semgrep_expression,
      $.semgrep_statement,
      $.semgrep_partial,
    ),

    // Module declaration
    declaration: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // Script members
    // We cannot mimic the `declaration` trick here, as the script members are strictly ordered.
    _script_use_decl: ($, previous) => choice(previous, $.ellipsis),
    _script_constant_decl: ($, previous) => choice(previous, $.ellipsis),
    _script_func_decl: ($, previous) => choice(previous, $.ellipsis),
    _script_spec_block: ($, previous) => choice(previous, $.ellipsis),

    // Address block members
    _address_member: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // Spec block members
    _spec_block_member: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // statement (sequence item)
    _sequence_item: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // struct field annotations
    field_annot: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // struct field bindings
    // (e.g. `let T { field_1, ... } = 0;`)
    bind_field: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // struct binding
    // (e.g. `let T { field_1, var: ..., } = obj;`)
    // (e.g. `let ... = obj;`)
    _bind: ($, previous) => choice(
      ...previous.members,
      $.ellipsis,
    ),

    // attribute
    // (e.g. `#[..., attr(...)]`)
    attribute: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // attribute value
    // (e.g. `#[attr(key = ...)]`)
    _attribute_val: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // use member
    // (e.g. `use module_ident::...;`, `use module_ident::{..., item_ident}`)
    _use_member: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // term
    term: ($, previous) => choice(
      ...previous.members,
      $.ellipsis,
      $.deep_ellipsis,
    ),

    // expression 
    _expr: ($, previous) => choice(
      ...previous.members,
      $.ellipsis,
      $.deep_ellipsis,
    ),

    // unary expression
    _unary_expr: ($, previous) => choice(
      ...previous.members,
      prec(UNARY_PREC, $.ellipsis),
      prec(UNARY_PREC, $.deep_ellipsis),
      prec(UNARY_PREC, $.field_access_ellipsis_expr),
      $.typed_metavariable,
    ),

    _dot_or_index_chain: ($, previous) => choice(
      ...previous.members,
      $.field_access_ellipsis_expr,
    ),

    // function parameter
    // (e.g. `call( ..., arg, ...)`)
    parameter: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // for loop ellipsis
    // (e.g. `for (...)`)
    for_loop_expr: ($, previous) => choice(
      previous,
      seq('for', '(', $.ellipsis, ')', field('body', $.block)),
    ),

    // abilities
    // (e.g. struct XXX has ..., YYY)
    ability: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // type parameter
    // (e.g. `Type<..., T>`)
    type_param: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // type
    type: ($, previous) => choice(
      ...previous.members,
      $.ellipsis,
    ),

    // pack field
    // (e.g. `Pack { ..., field }`)
    expr_field: ($, previous) => choice(
      ...previous.members,
      $.ellipsis,
    ),

    // trailing field access
    // (e.g. `foo.bar().baz(). ...`)
    field_access_ellipsis_expr: $ => prec.left(FIELD_PREC, seq(
      field('element', $._dot_or_index_chain), '.', $.ellipsis,
    )),

    // enum variant
    // (e.g. `enum Foo { ..., Bar }`)
    _variant: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // match arm
    // (e.g. `match foo { ..., bar => baz }`)
    match_arm: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // identifier, extended to support metavariables
    identifier: ($, previous) => token(choice(
      previous,
      // Metavariables
      alias(choice(
        /\$[A-Z_][A-Z_0-9]*/,
        /\$\.\.\.[A-Z_][A-Z_0-9]*/,
      ), $.meta_var),
    )),
  }
});
