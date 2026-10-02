/*
  semgrep-objc

  Extends the standard Objective-C grammar (tree-sitter-objc) with semgrep
  pattern constructs: the ellipsis operator ('...'), deep ellipsis
  ('<... ...>'), metavariables ('$FOO'), and typed metavariables ('(int $X)').

  This overlay is modeled on semgrep's cpp overlay
  (fyi/semgrep-grammars/src/semgrep-cpp/grammar.js in the semgrep-cpp package),
  because Objective-C is part of the C family and shares its node names
  (`expression`/`statement` supertypes, `argument_list`, `parameter_list`,
  `compound_statement`, `_block_item`, `_top_level_statement`, ...).

  NOTE ON THE C BASE GRAMMAR (inherits: "c")
  ------------------------------------------
  The base Objective-C grammar declares `inherits: "c"` (see the top of the
  vendored tree-sitter-objc grammar.json: `"name":"objc","inherits":"c"`). The
  codegen step MUST therefore be able to resolve the C base grammar, exactly as
  semgrep's cpp overlay resolves tree-sitter-c one layer below tree-sitter-cpp.
  In the *generated* grammar.json the inherited C rules are already fully
  inlined, but `require('tree-sitter-objc/grammar')` below still needs both
  tree-sitter-objc's grammar.js AND tree-sitter-c reachable on NODE_PATH so that
  `tree-sitter generate` can expand the require chain. See BOOTSTRAP.md.

  Base-grammar rule names used below were taken from
  tree-sitter-grammars/tree-sitter-objc v3.0.2 (src/grammar.json).
*/

const base_grammar = require('tree-sitter-objc/grammar');

module.exports = grammar(base_grammar, {
  name: 'objc',

  conflicts: ($, previous) => previous.concat([
    // Reported by `tree-sitter generate` for the semgrep ellipsis in
    // expression and message-send keyword positions.
    [$.range_expression, $.semgrep_ellipsis],
    [$.range_expression],
    [$.subscript_range_designator, $.range_expression, $.semgrep_ellipsis],
    // The next three are flagged "unnecessary" when generating from this
    // grammar directly, but are REQUIRED when ocaml-tree-sitter's
    // `simplify` pass rewrites the grammar before the definitive
    // `tree-sitter generate` (see scripts/ocaml-tree-sitter-gen-c).
    [$.keyword_declarator, $.method_declaration],
    [$.method_declaration],
    [$.type_qualifier, $.method_parameter],
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     if they're not already part of the base grammar.
  */
  rules: {

    // --- Alternate entry point -------------------------------------------
    // The base grammar's start symbol is `translation_unit` (a list of
    // top-level items), so a bare expression pattern has nowhere to attach.
    // Mirror semgrep-cpp: add a sentinel-prefixed entry point that lets
    // semgrep force the parser into "parse a standalone expression" mode.
    translation_unit: ($, previous) => choice(
      previous,
      $.semgrep_expression
    ),

    semgrep_expression: $ => seq('__SEMGREP_EXPRESSION', $.expression),

    // --- Metavariables ----------------------------------------------------
    // IMPORTANT: unlike tree-sitter-c / tree-sitter-cpp, the tree-sitter-objc
    // `identifier` token pattern ALREADY admits '$' as a start/continue char:
    //   (\$|\p{XID_Start}|_|\\u....|\\U........)(\$|\p{XID_Continue}|...)*
    // so plain metavariables like `$FOO` already lex as `identifier` in the
    // base grammar. We therefore deliberately do NOT redefine `identifier`
    // here: it would be a no-op, and `identifier` is the grammar's `word:`
    // token, which cannot be turned into a non-terminal `choice(...)`. Semgrep
    // recognizes the '$'-prefixed identifiers as metavariables in a later
    // stage. (This is the wrinkle the cpp overlay had to work around with a
    // `\$?`-prefixed regex; ObjC needs no such workaround.)

    // Uppercase metavariable token, used only for typed metavariables.
    semgrep_metavar: $ => /\$[A-Z_][A-Z_0-9]*/,

    semgrep_typed_metavar: $ => seq(
      $.type_descriptor,
      $.semgrep_metavar
    ),

    // Allow `(int $X)` typed-metavariable patterns.
    parenthesized_expression: ($, previous) => choice(
      previous,
      seq('(', $.semgrep_typed_metavar, ')')
    ),

    // --- Ellipsis ---------------------------------------------------------
    // `expression` is a supertype in tree-sitter-objc (a choice of
    // `_expression_not_binary` and `binary_expression`). Adding named-symbol
    // members keeps it a valid supertype (a choice of symbols).
    expression: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
      $.deep_ellipsis,
      $.semgrep_named_ellipsis
    ),

    // Allow `...` as a standalone item inside a `{ ... }` block body and at
    // the top level. Slight precedence bump so we reduce to the statement
    // position rather than to a bare expression statement.
    _block_item: ($, previous) => choice(
      previous,
      prec(1, $.semgrep_ellipsis)
    ),

    _top_level_statement: ($, previous) => choice(
      previous,
      prec(1, $.semgrep_ellipsis)
    ),

    // Field / method chaining: `foo. ... .bar`. In the base grammar
    // `_field_identifier` is an alias of `identifier`.
    _field_identifier: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis
    ),

    // --- Ellipsis terminals ----------------------------------------------
    semgrep_ellipsis: $ => '...',
    deep_ellipsis: $ => seq('<...', $.expression, '...>'),
    semgrep_named_ellipsis: $ => /\$\.\.\.[A-Z_][A-Z_0-9]*/,

    // --- Objective-C-specific ellipsis: message sends ---------------------
    // Allow `...` in *keyword position* of a message send, covering both
    //   * trailing: `[recv sel:$X ...]`   ("and any further keyword args")
    //   * mid:      `[recv sel:$X ... sel2:$Y]`
    //   * whole:    `[recv ...]`
    // This is the base grammar's message_expression with the repeat1 items
    // generalized to choice(keyword-item, semgrep_ellipsis). The converter
    // maps keyword-position ellipsis to a G.Arg (G.Ellipsis) argument (see
    // Parse_objc_tree_sitter.map_message_expression).
    message_expression: $ => prec(15, seq(
      '[',
      field('receiver', choice($.expression, $.generic_specifier)),
      repeat1(choice(
        seq(
          field('method', $.identifier),
          repeat(seq(':', seq($.expression, repeat(seq(',', $.expression)))))
        ),
        // Covers ellipsis in leading/whole positions ([recv ...],
        // [recv ... sel:$X]). In MID position (`[obj sel:$X ... sel2:$Y]`)
        // the C range_expression still wins the GLR race and swallows
        // `$X ... sel2`; the converter canonicalizes that shape back into
        // an argument-position ellipsis (see group_vals in
        // Parse_objc_tree_sitter.map_message_expression).
        prec.dynamic(10, $.semgrep_ellipsis)
      )),
      ']'
    )),

  }
});
