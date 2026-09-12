module.exports = grammar({
  name: "opt_repeat1",
  rules: {
    program: $ => optional(
      $.numbers
    ),
    numbers: $ => repeat1($.number),
    number: $ => /[0-9]+/
  }
});
