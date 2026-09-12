/*
  Test inlining/deinlining issues.
*/
module.exports = grammar({
  name: 'inline',
  rules: {
    program: $ => seq(
      'a',
      /b+/  // should stay inline
    )
  }
});
