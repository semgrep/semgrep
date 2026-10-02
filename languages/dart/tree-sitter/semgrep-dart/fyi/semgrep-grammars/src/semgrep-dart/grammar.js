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

    // Allow a bare `...` as a top-level "definition" so a polyglot
    // pattern like `class $X { ... } ... eval($V);` can use ellipsis
    // between top-level declarations, not just inside them.
    //
    // This rule used to also admit a bare expression_statement, to let
    // `$V = get(); ...; eval($V);`-style multi-statement patterns parse
    // without a containing function body (real Dart forbids executable
    // statements at the top level). That's now handled properly by
    // semgrep_statement_list below, reached only through the isolated
    // `__SEMGREP_EXPRESSION` entry point -- so it no longer needs to
    // leak into whole-program parsing here. Widening _top_level_definition
    // globally affected *any* Dart file, not just pattern fragments: a
    // real file with a bare top-level statement (invalid Dart, but
    // something tree-sitter's error recovery has its own opinion about)
    // would parse differently through this wrapper than through the
    // unmodified base grammar, which is what caused semgrep-dart's
    // inherited copy of tree-sitter-dart's own test corpus to diverge
    // from its pinned expectations (see e.g. "comment selector 1" in
    // test/corpus/inherited/big_tests.txt).
    _top_level_definition: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
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
