/*
  semgrep-tsx

  Extends the standard tsx grammar with semgrep pattern constructs.
*/

const tsx_grammar =
      require('tree-sitter-typescript/tsx/grammar');

const semgrepExt = require('../common/semgrep-ext');

module.exports = grammar(tsx_grammar, {
  name: 'tsx',
  ...semgrepExt,
});
