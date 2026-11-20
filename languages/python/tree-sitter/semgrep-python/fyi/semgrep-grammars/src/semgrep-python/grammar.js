/*
  semgrep-python

  Extends the standard python grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-python/grammar');

module.exports = grammar(base_grammar, {
  name: 'python',

  conflicts: ($, previous) => previous.concat([
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     if they're not already part of the base grammar.
  */
  rules: {
  /*
    semgrep_ellipsis: $ => '...',

    _expression: ($, previous) => choice(
      $.semgrep_ellipsis,
      ...previous.members
    ),
  */
    // Metavariables

   // Rather than creating a separate metavariable term
   // and adding it to identifiers, this instead overrides the
   // regex that is defined in the original tree-sitter grammar.
   // this is needed since currently in the original tree-sitter grammar,
   // identifier is a terminal, and thus can't do
   // the usual choice/previous shadowing definition.

    identifier: $ => /\$?[_\p{XID_Start}][_\p{XID_Continue}]*/,

    // Allow '...' in the attribute position of a dot-access expression,
    // so that patterns like `a. ... .d` work for matching call chains.
    // This mirrors the Java grammar's field_access override.
    // PREC.call = 22 in the base Python grammar.
    attribute: $ => prec(22, seq(
      field('object', $.primary_expression),
      '.',
      field('attribute', choice($.identifier, '...')),
    )),

    // Upstream's except_clause only accepts a single comma (the Python 2
    // `except E, e:` alias form), so `except A, B, C:` (valid since Python
    // 3.14, PEP 758) fails to parse and semgrep reports a PartialParsing
    // error for the rest of the clause. Accept further comma-separated
    // exception expressions after the first one; the `as` alias form and
    // the two-name `except A, B:` shape are unchanged, so the inherited
    // upstream corpus keeps passing. Upstream 0.25.0 instead rewrote this
    // rule around commaSep1 (tree-sitter-python a4c106f3), but that changes
    // the tree of the two-name form, which the pinned upstream corpus
    // still expects as value plus alias.
    except_clause: $ => seq(
      'except',
      optional(seq(
        field('value', $.expression),
        optional(seq(
          choice('as', ','),
          field('alias', $.expression),
          // Further comma-separated exception expressions (PEP 758).
          // The repeat lives inside this group, after the first comma,
          // so `, B` can only be the alias and `, C` can only be an
          // extra; splitting them into sibling groups is ambiguous.
          repeat(seq(
            ',',
            field('extra', $.expression),
          )),
        )),
      )),
      ':',
      $._suite,
    ),

  }
});
