/*
  Test the inference of good names for simple patterns.
*/
module.exports = grammar({
  name: 'pattern_name',
  rules: {
    program: $ => repeat($.element),
    element: $ => choice(
      /lowercase/,
      /UPPERCASE/,
      /[mmm][I][xX][Ee]d/,
      /underscore_separator/,
      /dash-separator/,
      /42/,
      /[lL][aA][sS][tT][__][99][00][__][dD][aA][yY][sS]/,
      /X/,
      /x/, // expect different name than for /X/
      /_/,
      /-/, // expect different name than for /_/
      /#/,
      /01abfc7/, // intentional collision with the hash for /#/
      seq(
        'thing',
        /lowercase/, // duplicate, should share a name with the other copy
      ),
    )
  }
});
