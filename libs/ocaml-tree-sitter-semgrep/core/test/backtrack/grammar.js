/*
  Test matching of a regular expression that needs backtracking:

    (a?a)?a

  should match all of the following:

    a         -> None, Some a
    aa        -> Some (None, a), a
    aaa       -> Some (Some a, a), a

  For more complete backtracking tests, use unit tests since they're faster
  and do just that.
*/
module.exports = grammar({
  name: "backtrack",
  rules: {
    program: $ => seq(
        optional(
            seq(
                optional($.alpha),
                $.alpha
            )
        ),
        $.alpha
    ),
    alpha: $ => /[a-z]/
  }
});
