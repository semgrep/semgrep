/*
  semgrep-typescript

  Extends the standard typescript grammar with semgrep pattern constructs.
*/

const typescript_grammar =
      require('tree-sitter-typescript/typescript/grammar');

const semgrepExt = require('../common/semgrep-ext');

module.exports = grammar(typescript_grammar, {
  name: 'typescript',
  ...semgrepExt,
});
