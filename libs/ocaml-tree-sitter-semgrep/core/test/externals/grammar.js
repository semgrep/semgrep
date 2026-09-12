module.exports = grammar({
  name: "externals",

  /**
   * We want to test our ability to deal with externals.
   * There are three allowed kinds, demonstrated here.
   * Strings, patterns, and identifiers.
   */
  externals: $ => [
    "foo",
    /\n/,
    $.bar
  ],

  rules: {
    program: ($) => repeat(choice("foo", /\n/, $.bar))
  },
});
