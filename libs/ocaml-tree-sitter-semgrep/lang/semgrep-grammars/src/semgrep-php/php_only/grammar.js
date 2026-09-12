/*
  semgrep-php_only

  Extends the standard php_only grammar with semgrep pattern constructs.
*/

const php_only_grammar = require('tree-sitter-php/php_only/grammar');

const semgrepExt = require('../common/semgrep-ext');

module.exports = grammar(php_only_grammar, {
  name: 'php_only',
  ...semgrepExt,
});
