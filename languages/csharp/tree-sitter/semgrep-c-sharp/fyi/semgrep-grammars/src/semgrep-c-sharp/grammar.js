/*
 * semgrep-c-sharp
 *
 * Extend the original tree-sitter C# grammar with semgrep-specific constructs
 * used to represent semgrep patterns.
 *
 */

// tree-sitter-c-sharp 0.23.6+ switched grammar.js to ESM (`export default`);
// older versions used `module.exports`. Handle both shapes.
const _imported = require('tree-sitter-c-sharp/grammar');
const standard_grammar = _imported.default || _imported;

module.exports = grammar(standard_grammar, {
  /*
    We can't rename the grammar to 'csharp' because it relies on a custom
    lexer ('scanner.c') which comes with the inherited grammar and provides
    a function 'tree_sitter_c_sharp_external_scanner_create'.
    The code generator ('tree-sitter generate') however expects this function
    name to match the grammar name, so if we rename it here to 'csharp',
    it will generate glue code that calls
    'tree_sitter_csharp_external_scanner_create'
    while only 'tree_sitter_c_sharp_external_scanner_create' is available.
  */
  name: 'c_sharp',

  conflicts: ($, previous) => [
    ...previous,
    [$.expression, $.parameter],
    [$.argument, $.collection_element],
    [$._list_pattern_body, $.expression],
    [$._reserved_identifier, $.extension_declaration],
  ],

  // The upstream grammar skips the C# 14 file-based app directives
  // (`#:package Name@1.0`, `#:sdk ...`) as extras, so they never reach the
  // CST and rules cannot match them. Parse them as top-level items instead
  // (see `_top_level_item` below).
  extras: ($, previous) =>
    previous.filter(e => !(e.type === 'SYMBOL' && e.name === 'ignored_directive')),

  rules: {

    // Entry point
    compilation_unit: ($, previous) => {
      return choice(
        previous,
        $.semgrep_expression,
        // A lone `case P: ...` pattern, matched against any switch section
        // (as for TypeScript).
        $.switch_section,
      );
    },

    // File-based app directives. The SDK only accepts them at the start of
    // the file; as top-level items they are accepted wherever a top-level
    // item is, which is enough for scanning.
    // Also accept an extension block at the top level, so that a pattern
    // can be just `extension($T $X) { ... }` (real code nests it in a
    // static class). Its dynamic precedence beats the reading as a call
    // `extension(...)` followed by a block.
    // `extension` stays usable as an identifier (e.g. a top-level call
    // `extension(x);`) where an extension declaration is also allowed.
    _reserved_identifier: ($, previous) => choice(
      ...previous.members,
      'extension',
    ),

    _top_level_item: ($, previous) => choice(
      previous,
      $.ignored_directive,
      $.extension_declaration,
    ),

    // Alternate "entry point". Allows parsing a standalone expression.
    semgrep_expression: $ => seq('__SEMGREP_EXPRESSION', $.expression),

    // Metavariables
    identifier: ($, previous) => {
      return choice(
        previous,
        $._semgrep_metavariable
      );
    },

    _semgrep_metavariable: $ => token(/\$[A-Z_][A-Z_0-9]*/),
    semgrep_variadic_metavariable: ($) => /\$\.\.\.[A-Z_][A-Z_0-9]*/,

    parameter: ($, previous) => {
      return choice(
        previous,
        $.ellipsis
      );
    },

    // alt: we could also extend 'identifier' above and allow 'semgrep_variadic_metavariable'
    // there to be more general
    argument: ($, previous) => {
      return choice(
        previous,
        $.semgrep_variadic_metavariable
      );
    },

    declaration: ($, previous) => {
      return choice(
        ...previous.members,
        $.ellipsis,
        // A misplaced file-based app directive inside a type (see
        // `_top_level_item`); upstream skips it anywhere as an extra.
        $.ignored_directive,
      );
    },

    // We want ellipses to be interchangeable with namespace member declarations, so
    // we need to add them in to `type_declaration` here.
    type_declaration: ($, previous) => {
      return choice(
        ...previous.members,
        $.ellipsis
      )
    },

    // (Older versions of this extension wrapped `file_scoped_namespace_declaration`
    // to allow a prefix of statements/declarations before it. In the post-C#-14
    // grammar `compilation_unit` already accepts arbitrary `_top_level_item`s
    // — including `global_statement` and `_namespace_member_declaration` —
    // before `file_scoped_namespace_declaration`, so the wrapper is redundant
    // and now conflicts with `preproc_if_in_top_level`. Removed.)

    enum_member_declaration: ($, previous) => choice(
          previous,
	  $.ellipsis,
    ),

    // Statement ellipsis: '...' not followed by ';'
    expression_statement: ($, previous) => {
      return choice(
        previous,
        prec.right(100, seq($.ellipsis, ';')),  // expression ellipsis
        prec.right(100, $.ellipsis),  // statement ellipsis
        seq($.deep_ellipsis, ';'),
        seq($.member_access_ellipsis_expression, ';'),
        seq($._semgrep_metavariable, ';'),
        seq($.typed_metavariable, ';')
      );
    },

    // Expression ellipsis
    expression: ($, previous) => {
      return choice(
        ...previous.members,
        $.ellipsis,
        $.deep_ellipsis,
        $.member_access_ellipsis_expression,
        $.typed_metavariable
      );
    },

    // TODO: how to use PREC.DOT from original grammar instead of 18 below?
    member_access_ellipsis_expression : $ => prec(18, seq(
      field('expression', choice($.expression, $.predefined_type, $._name)),
      choice('.', '->'),
      $.ellipsis
     )),

    // Same shape as cast_expression but with a metavariable in place of the
    // cast value. We need prec.dynamic > cast_expression's (which is 1) so the
    // typed_metavariable interpretation wins over an ambiguous parse like
    // `(List<T> $X)` which can otherwise be read as `parenthesized_expression`
    // containing the binary chain `List < T > $X`.
    //TODO: use PREC.CAST from original grammar instead of 17 below
    typed_metavariable: $ => prec.dynamic(2, prec.right(17, seq(
      '(',
      field('type', $.type),
      field('metavar', $._semgrep_metavariable),
      ')',
    ))),

    // Real-world C# code occasionally contains technically-invalid
    // escape sequences inside string literals (`"\\share\images"` where
    // `\i` is not one of the spec's recognized escapes). Roslyn rejects
    // these, but the upstream tree-sitter grammar follows Roslyn and
    // produces an ERROR node, which then fails semgrep's parse-error
    // gate. Semgrep is a static analyzer that wants to scan as much
    // code as it can, so we broaden `escape_sequence` here to accept
    // any `\<single-char>` after the spec-recognized escapes have had a
    // chance to match.
    escape_sequence: ($, previous) => choice(previous, token(/\\./)),

    // `[1, $...REST]`: a variadic metavariable among collection elements.
    // (`...` already works there, since it is an expression.)
    collection_element: ($, previous) => choice(
      ...previous.members,
      prec.dynamic(10, $.semgrep_variadic_metavariable),
    ),

    // `case [1, ..., 9]:`: ellipsis in a list pattern. The dynamic
    // precedence makes the list-pattern reading win over a constant pattern
    // holding a collection expression with an ellipsis element.
    _list_pattern_body: ($, previous) =>
      addToChoicesWith(previous, 'slice_pattern', prec.dynamic(10, $.ellipsis)),

    // `extension($T $X) { ... }`: ellipsis among extension members.
    // (Right-associative so that a trailing `;` stays in the body rather
    // than becoming an empty statement when the block is at the top level.)
    extension_body: ($, previous) => prec.right(seq(
      '{',
      repeat(choice($._extension_member_declaration, $.ellipsis)),
      '}',
      optional(';'),
    )),

    // Enum members guarded by `#if` may carry their own commas, before or
    // after the member (`A,\n#if X\n  B,\n#endif`, `A\n#if X\n  , B\n#endif`),
    // which upstream only accepts outside the `#if`. Accept any sequence
    // of members, commas and (nested) `#if` blocks: commas carry no
    // meaning for semgrep, and the compiler checks their placement.
    enum_member_declaration_list: $ => seq(
      '{',
      optional($._enum_member_items),
      '}',
    ),

    _enum_member_items: $ => repeat1(choice(
      $.enum_member_declaration,
      ',',
      alias($.preproc_if_in_enum_member_declaration, $.preproc_if),
    )),

    preproc_if_in_enum_member_declaration: ($, previous) =>
      withBranchContent(previous, 3, 'CHOICE', _ => optional($._enum_member_items)),
    preproc_elif_in_enum_member_declaration: ($, previous) =>
      withBranchContent(previous, 3, 'CHOICE', _ => optional($._enum_member_items)),
    preproc_else_in_enum_member_declaration: ($, previous) =>
      withBranchContent(previous, 1, 'CHOICE', _ => optional($._enum_member_items)),

    // `#:` directives inside a top-level `#if` (e.g. `#if DEBUG` around
    // `#:package ...`).
    preproc_if_in_top_level: ($, previous) =>
      withBranchContent(previous, 3, 'REPEAT',
        old => repeat(choice(old.content, $.ignored_directive))),
    preproc_elif_in_top_level: ($, previous) =>
      withBranchContent(previous, 3, 'REPEAT',
        old => repeat(choice(old.content, $.ignored_directive))),
    preproc_else_in_top_level: ($, previous) =>
      withBranchContent(previous, 1, 'REPEAT',
        old => repeat(choice(old.content, $.ignored_directive))),

    deep_ellipsis: $ => seq(
      '<...', $.expression, '...>'
    ),

    ellipsis: $ => '...',
  }
});

function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}

// `previous` is a `prec(n, seq(...))` rule generated by upstream's
// `preprocIf`; replace the `index`-th member of its sequence (the branch
// content, expected to be a rule of type `type`) with `make(old)`. Fail
// loudly rather than silently edit the wrong member if upstream reshapes
// `preprocIf`.
function withBranchContent(previous, index, type, make) {
  const seqRule = previous.type === 'PREC' ? previous.content : null;
  const old = seqRule && seqRule.type === 'SEQ' ? seqRule.members[index] : null;
  if (!old || old.type !== type) {
    throw new Error(
      `withBranchContent: expected a ${type} at index ${index} of a ` +
      `prec(seq(...)) rule, got ${old ? old.type : JSON.stringify(previous).slice(0, 80)}`);
  }
  const members = seqRule.members.slice();
  members[index] = make(old);
  return prec(previous.value, seq(...members));
}

// Return a copy of the upstream rule `rule` where `extra` is added to every
// CHOICE that offers the symbol `name`, looking through SEQ/REPEAT/CHOICE.
// Fail loudly if there is none, rather than silently not extending the
// rule after an upstream change.
function addToChoicesWith(rule, name, extra) {
  let count = 0;
  const walk = r => {
    switch (r.type) {
      case 'SEQ': return seq(...r.members.map(walk));
      case 'REPEAT': return repeat(walk(r.content));
      case 'REPEAT1': return repeat1(walk(r.content));
      case 'CHOICE': {
        const members = r.members.map(walk);
        if (r.members.some(m => m.type === 'SYMBOL' && m.name === name)) {
          count++;
          return choice(...members, extra);
        }
        return choice(...members);
      }
      default: return r;
    }
  };
  const result = walk(rule);
  if (count === 0) {
    throw new Error(`addToChoicesWith: no choice offering ${name} in the rule`);
  }
  return result;
}
