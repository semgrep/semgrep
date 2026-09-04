/*
  semgrep-swift

  Extends the standard swift grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-swift/grammar');

module.exports = grammar(base_grammar, {
  name: 'swift',

  conflicts: ($, previous) => previous.concat([
    [$._three_dot_operator, $.semgrep_expression_ellipsis],
    [$._three_dot_operator, $.semgrep_expression_ellipsis, $.semgrep_ellipsis],
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     if they're not already part of the base grammar.
  */
  rules: {
    // Swift has an unbounded range operator which is also three dots. When we
    // are parsing a pattern, we will convert that into an ellipsis after it's
    // parsed, but let's use low precedence here so that normally we only
    // produce this node where the unbounded range operator cannot exist.
    semgrep_expression_ellipsis: $ => prec.dynamic(-1337, "..."),

    // Use "..." here and in semgrep_expression_ellipsis above, rather than
    // `$._three_dot_operator`, so that the conflicts list is easier to
    // understand.
    semgrep_ellipsis: $ => "...",

    semgrep_ellipsis_metavar : $ => /\$\.\.\.[a-zA-Z_][a-zA-Z_0-9]*/,

    /*
    * Unfortunately, `...>` is a valid custom operator in Swift, and it can
    * occur as part of an expression. So, for the pattern `<... 5 ...>`, it gets
    * lexed as `<...`, followed by the expression `5`, followed by a
    * `custom_operator` token. This doesn't fit the deep ellipsis rule, so it
    * results in a parse error.
    *
    * A proper fix would be to extend the scanner so that `...>` is its own
    * token, and then handle it separately in the grammar. This would be
    * complicated, and would likely present significant maintainability
    * challenges because the way custom scanners are structured does not invite
    * extensibility. We would probably have to maintain a fork of the scanner.
    *
    * Instead, we just replaced `...>` with `$.custom_operator` in this rule.
    * Unfortunately, it will prevent people from writing patterns like `<... 5
    * .+. 1 ...>` because the custom operator in the middle will get mistaken
    * for the end of the deep ellipsis. However, this can easily be worked
    * around with parentheses, and it's probably going to come up rarely, if
    * ever.
    */
    semgrep_deep_ellipsis: $ => seq("<...", $._expression, $.custom_operator),

    _expression: ($, previous) => choice(
      previous,
      $.semgrep_expression_ellipsis,
      $.semgrep_ellipsis_metavar,
      $.semgrep_deep_ellipsis,
    ),

    _type: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
    ),

    _type_level_declaration: ($, previous) => choice (
      previous,
      $.semgrep_ellipsis,
    ),

    type_parameter: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
    ),

    navigation_suffix: ($, previous) => choice(
      previous,
      seq(
        $._dot,
        field("suffix", $.semgrep_ellipsis),
      ),
    ),

    parameter: ($, previous) => choice (
      previous,
      $.semgrep_ellipsis,
      $.semgrep_ellipsis_metavar
    )
  }
});
