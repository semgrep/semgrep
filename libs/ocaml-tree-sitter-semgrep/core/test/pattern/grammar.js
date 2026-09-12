/*
  Check that inline patterns are factored out into their own rule.
*/
module.exports = grammar({
  name: 'pattern',
  rules: {
    program: $ => choice(
      /a/,
      'b',
    ),
  }
});
