/*
  semgrep-elixir

  Extends the standard elixir grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-elixir/grammar');

module.exports = grammar(base_grammar, {
  name: 'elixir',

  conflicts: ($, previous) => previous.concat([
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     if they're not already part of the base grammar.
  */
  rules: {
    // No need for _SEMGREP_EXPRESSION hack here, because Elixir allows
    // toplevel expressions.

    // Metavariables
    identifier: ($, previous) => {
      return choice(
        previous,
        $._semgrep_metavariable
      );
    },

    _keyword: ($, previous) => {
      return choice(
        ...previous.members,
        $.metavariable_keyword
      );
    },

    metavariable_keyword: $ => seq($._semgrep_metavariable, /:\s/),

    _atom: ($, previous) => {
      return choice(
        ...previous.members,
        $.metavariable_atom
      );
    },

    metavariable_atom: $ => seq(":", $._semgrep_metavariable),

    _semgrep_metavariable: $ => token(/\$[A-Z_][A-Z_0-9]*/),

    // Ellipsis
    // No need for extensions to _expressions for ellipsis because
    // Elixir already uses "..." as valid identifiers
    //
    // However, we do need to override the "pair" rule which is used
    // for keyword parameters, arguments, and map items. Otherwise,
    // ellipsis won't work in places where Elixir expects
    // keyword/pairs, such as
    //   foo(some_arg: 0, ...)
    //   %{some_item: 0, ...}
    pair: ($, previous) => {
      return choice(
        previous,
        $.semgrep_ellipsis,
      );
    },

    // Note that because of the pair rule above, now there is
    // ambiguity whether the ellipsis in foo(...) is an
    // ellipsis for positional or keyword arguments. If ... matches
    // with the identity rule, it will be considered a positional
    // argument. If ... matches with semgrep_ellipsis, it will be
    // part of parsing the pair rule, which means its a keyword
    // argument.  In practice, this doesn't matter because either
    // way, they will become ParamEllipsis in the Generic AST
    // anyway.
    semgrep_ellipsis: $ => prec(-1, '...'),

    _expression: ($, previous) => choice(
      ...previous.members,
      $.deep_ellipsis,
    ),

    // The actual ellipsis rules
    deep_ellipsis: $ => seq(
            '<...', $._expression, '...>'
    ),
  }
});
