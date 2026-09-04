/*
  semgrep-promql

  Extends the standard promql grammar with semgrep pattern constructs.
*/

const base_grammar = require("tree-sitter-promql/grammar");

module.exports = grammar(base_grammar, {
  name: "promql",

  conflicts: ($, prev) =>
    prev.concat([[$.metric_name, $.string_literal, $.float_literal]]),

  rules: {
    _semgrep_metavariable: (_) => token(/\$[A-Z_][A-Z_0-9]*/),
    semgrep_ellipsis: (_) => "...",

    // Metavariables
    metric_name: ($, prev) => choice($._semgrep_metavariable, prev),
    function_name: ($, prev) => choice($._semgrep_metavariable, prev),
    label_name: ($, prev) => choice($._semgrep_metavariable, prev),
    label_value: ($, prev) => choice($._semgrep_metavariable, prev),
    string_literal: ($, prev) => choice($._semgrep_metavariable, prev),
    float_literal: ($, prev) => choice($._semgrep_metavariable, prev),
    _duration: ($, prev) => choice($._semgrep_metavariable, prev),

    // Ellipsis
    _query_expression: ($, prev) => choice($.semgrep_ellipsis, prev),

    label_selectors: ($, _) =>
      seq("{", commaSep(choice($.semgrep_ellipsis, $.label_matcher)), "}"),

    grouping: ($) =>
      seq(
        choice(caseInsensitive("by"), caseInsensitive("without")),
        "(",
        commaSep(choice($.semgrep_ellipsis, $.label_name)),
        ")",
      ),
  },
});

function commaSep(rule) {
  return optional(commaSep1(rule));
}

function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)), optional(","));
}

// taken from https://github.com/stadelmanma/tree-sitter-fortran/blob/a9c79b20a84075467d705ebe714c90f275dd5835/grammar.js#L1125C1-L1133C2
function caseInsensitive(keyword) {
  let result = new RegExp(
    keyword
      .split("")
      .map((l) => (l !== l.toUpperCase() ? `[${l}${l.toUpperCase()}]` : l))
      .join(""),
  );
  return result;
}
