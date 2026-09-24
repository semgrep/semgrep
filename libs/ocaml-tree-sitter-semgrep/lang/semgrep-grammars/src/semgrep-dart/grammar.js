/*
  semgrep-dart

  Extends the standard dart grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-dart/grammar');

module.exports = grammar(base_grammar, {
  name: 'dart',

  conflicts: ($, previous) => previous.concat([
    [$._expression, $.formal_parameter],
    [$.spread_element, $.semgrep_ellipsis],
    [$._expression, $.expression_statement],
    // `...` at top level can be reached two ways now that
    // expression_statement is permitted at the top level: directly as a
    // `_top_level_definition` (via the rule below) or wrapped in an
    // expression_statement. Declaring this conflict lets tree-sitter
    // use GLR to keep both alternatives alive until disambiguation.
    [$._top_level_definition, $.expression_statement],
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     No need for special extensions for metavariables because Dart
     already accepts $ as part of an identifier.
  */
  rules: {
    // entry point
    program: ($, previous) =>
      choice(previous, $.semgrep_expression),

    semgrep_ellipsis: $ => prec.left(1, '...'),
    semgrep_named_ellipsis: $ => /\$\.\.\.[A-Z_][A-Z_0-9]*/,
    deep_ellipsis: $ => seq(
            '<...', $._expression, '...>'
    ),

    // Allow expression statements at the top level so that semgrep
    // patterns like
    //   $V = get();
    //   ...
    //   eval($V);
    // parse without being misinterpreted as a function signature.
    // Real Dart forbids bare assignments at the top level, but in
    // pattern mode this is the only way to express a sequence of
    // statements without a containing function body.
    _top_level_definition: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
      $.expression_statement,
    ),

    semgrep_metavariable: $ => /\$[A-Z_][A-Z_0-9]*/,

    // Alternate "entry point". Allows parsing a standalone expression.
    semgrep_expression: ($) => seq("__SEMGREP_EXPRESSION", $._semgrep_pattern),

    // Hidden (leading underscore): the choice node is inlined into
    // `semgrep_expression`, so the parse tree exposes the inner
    // expression/statement/statement-list directly.
    _semgrep_pattern: $ => choice(
      $._expression,
      $._statement,
      $.semgrep_statement_list,
    ),

    // Multi-statement pattern body. Lets a polyglot like
    //   $V = get();
    //   ...
    //   eval($V);
    // parse cleanly via the `__SEMGREP_EXPRESSION` entry point as a
    // sequence of statements, sidestepping the top-level
    // function-declaration ambiguity that would otherwise greedily eat
    // `$V = get();` as a function signature. Requires at least two
    // statements so the single-statement case keeps using `$._statement`
    // above (avoiding a parse-tree ambiguity between the two arms).
    semgrep_statement_list: $ => seq($._statement, repeat1($._statement)),

    _expression: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
      $.semgrep_named_ellipsis,
      $.deep_ellipsis,
    ),
    expression_statement: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
    ),
    formal_parameter: ($, previous) => choice(
       $.semgrep_ellipsis,
       previous
    ),

    // Allow `...` inside class bodies so that the polyglot pattern
    // `class $X { ... }` parses. Without this, the base grammar's
    // `_class_member_definition` accepts only declarations and method
    // signatures and rejects `...`, forcing a Dart-specific .sgrep
    // workaround like `class $X { }`.
    _class_member_definition: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
    ),

    // Allow `. ...` in a method/property-access chain so the polyglot
    // `dots_method_chaining` pattern
    //   $X = $O.foo(). ... .bar(). ...
    // parses. The base grammar's `selector` rule only accepts
    // `_assignable_selector | argument_part | type_arguments | !`, none
    // of which can match a bare `...` between chain segments. We add a
    // new `semgrep_dot_ellipsis_selector` that fills the same slot —
    // `seq('.', $.semgrep_ellipsis)` — and graft it into `selector` so
    // it interleaves naturally between real `.foo()` / `.bar()` calls.
    semgrep_dot_ellipsis_selector: $ => seq('.', $.semgrep_ellipsis),

    selector: ($, previous) => choice(
      previous,
      $.semgrep_dot_ellipsis_selector,
    ),
}
});
