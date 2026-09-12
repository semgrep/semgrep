module.exports = grammar({
  name: "anon_pat",
  rules: {
    program: $ => seq(
      /[a-z]+/,
      /[a-z]+/,
      /[0-9]+/,
      token(/x+/),
      token.immediate(/y+/),
      token(prec.dynamic(1, 'stays_inline')),
      'end'
    )
  }
});
