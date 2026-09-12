module.exports = grammar({
  name: "token",
  rules: {
    program: $ => seq(
      '<',
      token(prec(1, '>')) // should output the same as just '>'
    )
  }
});
