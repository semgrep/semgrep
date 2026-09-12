/*
 * Tests emoji parsing (only made available in tree-sitter v0.20).
 */
module.exports = grammar({
  name: 'emoji',
  extras: $ => [
    '\n',
  ],
  rules: {
    program: $ => /[\p{Emoji}]/
  }
});
