/*
  THIS IS NOW EMPTY SINCE WE USE THE TYPESCRIPT GRAMMAR TO
  PARSE JAVASCRIPT IN SEMGREP.

  semgrep-javascript

  Extends the standard javascript grammar with semgrep pattern constructs.
*/

const javascript_grammar = require('tree-sitter-javascript/grammar');

module.exports = grammar(javascript_grammar, {
  name: 'javascript',
  rules: {
    // Allow a bare '>' among JSX element children. The upstream external
    // scanner ends a jsx_text token at '>', but a literal '>' between
    // tags is valid JSX.
    _jsx_child: ($, previous) => choice(previous, '>'),
  }
});

// copy-pasted from the original grammar
function commaSep1(rule) {
  return seq(rule, repeat(seq(',', rule)));
}

// copy-pasted from the original grammar
function commaSep(rule) {
  return optional(commaSep1(rule));
}
