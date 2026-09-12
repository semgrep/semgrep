module.exports = grammar({
  name: "rectypes",
  /*
     This results in a cyclic type abbreviation, which requires passing the
     '-rectypes' option to the ocaml compilers.
     For example, launch 'utop -rectypes' and enter:

       type tuple = int * tuple option;;
  */
  rules: {
    tuple: $ => seq(
        $.number,
        optional($.tuple)
    ),
    number: $ => /[0-9]+/
  }
});
