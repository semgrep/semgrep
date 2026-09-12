module.exports = grammar({
  name: "choice",
  rules: {
    program: $ => choice(
      $.number,
      $.variable
    ),
    variable: $ => /[a-z]+/,
    number: $ => /[0-9]+/
  }
});
