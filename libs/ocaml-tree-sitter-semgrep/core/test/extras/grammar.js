module.exports = grammar({
  name: "extras",
  rules: {
    program: $ => repeat(
      $.number
    ),
    number: $ => /[0-9]+/,
    letter: $ => /[a-zA-Z]/,
    comment: $ => /#.*/,
    complex_extra: $ => seq('(', repeat(choice($.number, $.letter)), ')'),
    extra: $ => 'extra',    // a rule named 'extra' (!)
    extras: $ => 'extras',  // a rule named 'extras' (!)
  },
  extras: $ => [
    $.comment,       // appears in the CST
    $.complex_extra, // same
    $.extra,
    $.extras,
    "IGNOREME",  // doesn't appear in the CST because it doesn't have a name
    /\s|\\\n/    // same
  ]
})
