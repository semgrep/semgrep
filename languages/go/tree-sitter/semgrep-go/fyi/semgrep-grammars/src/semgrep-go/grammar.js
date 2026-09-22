/*
  semgrep-go

  Accepts Go source snippets for Semgrep.
*/

const base_grammar = require('tree-sitter-go/grammar');

module.exports = grammar(base_grammar, {
  name: 'go',

  rules: {
    // Snippets may end in a statement without a newline. Avoid synthetic EOF
    // tokens, which have no source text for typed-CST recovery.
    source_file: $ => seq(
      repeat(seq(
        choice($._statement, $._top_level_declaration),
        choice(/\n/, ';'),
      )),
      optional(choice($._statement, $._top_level_declaration)),
    ),
  }
});
