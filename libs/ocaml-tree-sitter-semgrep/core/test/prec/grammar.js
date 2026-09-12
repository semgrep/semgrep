module.exports = grammar({
  name: "prec",
  rules: {
    exp: $ => choice(
      'x',
      prec(1, seq('+', $.exp)),
      prec.left(seq($.exp, '+', $.exp))
    )
  }
});
