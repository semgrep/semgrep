module.exports = grammar({
  name: "repeat_choice",
  rules: {
    program: $ => repeat(
      choice(
        $.variable,
        $.number
      )
    ),
    variable: $ => /[a-z]+/,
    number: $ => /\d+/
  }
});
