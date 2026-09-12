module.exports = grammar({
  name: "recurse",
  /*
     We test the generation of type definitions, which inlines tuples,
     doesn't run into infinite loops.
  */
  rules: {
    tuple: $ => seq(
      $.number,
      choice(
          $.tuple,
          $.variable
      )
    ),
    number: $ => /[0-9]+/,
    variable: $ => /[a-z]+/
  }
});
