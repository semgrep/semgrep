module.exports = grammar({
  name: "explicit_extra",

  /*
    A token declared as an 'extra' can occur anywhere.
    It can also occur explicitly in a rule but this used to confuse
    ocaml-tree-sitter when it tried to recover the typed CST.

    This example demonstrates the workaround consisting in giving a different
    name to an extra used explicitly.
   */
  extras: ($) => [/\s+/, $.extra],

  rules: {
    // used to be bad:
    // program: ($) => seq('a', $.extra),

    // good:
    program: ($) => seq('a', $.explicit_extra),

    explicit_extra: ($) => 'X',
    extra: ($) => 'X',
  },
});
