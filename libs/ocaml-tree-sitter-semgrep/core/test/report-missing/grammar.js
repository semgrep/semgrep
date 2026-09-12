module.exports = grammar({
  name: "report_missing",
  rules: {
    program: $ => repeat($.tuple),

    tuple: $ => choice(
      seq(
        'a', $.terminator
      ),
      seq(
        'b', ';'
      ),
      seq(
        'c', /;/
      ),
    ),
    terminator: $ => /[;?.]/
  }
});
