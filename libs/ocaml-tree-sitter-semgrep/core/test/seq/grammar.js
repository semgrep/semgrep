module.exports = grammar({
  name: "seq",
  rules: {
    program: $ => seq(
      $.variable,
      $.number,
      $.number
    ),
    variable: $ => /[a-z]+/,
    number: $ => /[0-9]+/
  }
});
