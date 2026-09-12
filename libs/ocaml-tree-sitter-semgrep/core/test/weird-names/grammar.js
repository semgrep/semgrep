module.exports = grammar({
  name: "weird_names",
  rules: {
    program: $ => seq(
      $.VARIABLE,
      $.variable,
      $._variable,
      $.variable_
    ),

    VARIABLE: $ => '1',
    variable: $ => '2',
    _variable: $ => '3',
    variable_: $ => '4',
  }
});
