/*
  semgrep-php

  Extends the standard php grammar with semgrep pattern constructs.
*/

const php_grammar = require('tree-sitter-php/php/grammar');

const semgrepExt = require('../common/semgrep-ext');

module.exports = grammar(php_grammar, {
  name: 'php',
  ...semgrepExt,
});
