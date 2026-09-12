/*
  semgrep-julia

  Extends the standard julia grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-julia/grammar');

module.exports = grammar(base_grammar, {
  name: 'julia',

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     if they're not already part of the base grammar.
  */
  rules: {
    semgrep_ellipsis: _ => '...',

    catch_clause: $ => prec(1, seq(
      'catch',
      optional(choice($.identifier, alias($.semgrep_ellipsis, $.catch_ellipsis))),
      optional($._terminator),
      optional($._block),
    )),

    deep_expression: $ => seq("<...", $._expression, "...>"),

    // Note that this is actually slightly more permissive than a Semgrep
    // metavariable usually is. This is fine, because having a slightly more
    // permissive grammar is OK, we will just dispatch in the Generic
    // translation.
    semgrep_extended_metavariable: _ =>
      /\$[A-Z_][a-zA-Z_0-9]*/,

    // Metavariables
    // We allow an identifier to be a simple metavariable regex, so that
    // we properly support metavariables.
    // Low precedence, so that we take less priority than parsing as
    // an interpolation expression, if possible. See the comments below.
    identifier: ($, previous) =>
      prec(1, choice(
        previous,
        $.semgrep_extended_metavariable
      )),

    // ...But we also allow an interpolation expression to be a metavariable.
    // `interpolation_expression` is a superset of identifiers, so this doesn't
    // produce any extra parsing power, but we play with the precedence so that
    // anytime we see an interpolation_expression that is a metavariable, we
    // should prefer to parse it as an `interpolation_expression` rather than an
    // `identifier`.
    //
    // This ensures that in the produced parse tree, even if we are parsing a
    // Julia target rather than pattern, we can retain enough information to
    // parse interpolation expressions normally, and only opt-in to parsing them
    // as identifiers.
    //
    // Notably, we can't say that `interpolation_expression` should just be
    // `prec(999, previous)`, because the token stream is different. We should
    // be able to handle the same token lexed by the metavariable regex, so we
    // duplicate the regex here.
    interpolation_expression: ($, previous) => choice(
      previous,
      // We alias the included metavariable to an $.identifier so that the parse
      // tree stays the same. Otherwise, we will fail `tree-sitter` tests.
      alias(prec(999,
        $.semgrep_extended_metavariable
      ), $.identifier)
    ),

    _expression: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
      $.deep_expression,
    ),
  }
});
