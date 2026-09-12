module.exports = grammar({
  name: "ignore_error",
  rules: {
    program: $ => choice(
      seq('(', repeat($.program), ')'),
      seq('[', $.program, $.program, ']'),
      $.variable,
    ),
    variable: $ => /[a-z]+/
  }
});
