module.exports = grammar({
  name: "choice_seq",
  rules: {
    program: $ => seq(
      $.variable,
      choice(
        $.number,
        $.variable
      ),
      $.number
    ),
    variable: $ => /[a-z]+/,
    number: $ => /\d+/
  }
});
