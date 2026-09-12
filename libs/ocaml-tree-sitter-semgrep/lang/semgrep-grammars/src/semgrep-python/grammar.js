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

  }
});
