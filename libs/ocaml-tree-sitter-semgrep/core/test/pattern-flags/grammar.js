/*
  Check that pattern flags (e.g. case-insensitive) are handled correctly.
*/
module.exports = grammar({
  name: 'pattern_flags',
  rules: {
    program: $ => repeat($.keyword),
    keyword: $ => /hello/i,
  }
});
