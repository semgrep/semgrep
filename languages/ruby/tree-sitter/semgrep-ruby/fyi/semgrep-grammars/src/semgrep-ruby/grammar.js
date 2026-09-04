/*
  semgrep-ruby

  Extends the standard ruby grammar with semgrep pattern constructs.
*/

/*

(2023-02-25) A warning from Emma: I spent about a week trying to make
pattern parsing work before I gave up. I spent so much time because the
existing dyp parser has a long startup cost to create the parser (see `pp`
in the library). However, I ran into too many problems. Some of these may
be my own misunderstanding, some of these are inherent to the existing
grammar:

1. It is hard to distinguish a range (`a...b`) from ellipsis. In particular,
the example I worked with was:

```
$V = get_data user
...
eval $V
```

With my naive modification of the grammar, this parses as `user ...` being
a range with null at the other end. It's not clear to me why that's possible,
which makes it hard to figure out how to avoid it.

2. This parser without modifications appears to interpret regex strings
oddly. The `regexp_string.sgrep` in the ruby pattern tests ends up being
parsed into a ConcatString.

3. There was some strange behavior with underscores. If you look at my
modification of `call`, I use `$._primary` where the original used
`$.primary`. When I didn't, I got an error `Undefined symbol 'primary'`.
I don't know why I got that error or the _ fixed it. I also had to modify
`call` by making a rule for `_call`. I did that because otherwise my change
had no effect. An interesting note is that a rule for `call_` exists in the
grammar.

This is particularly interesting to me because I tried a similar modification
for `command_call_with_block`, which doesn't have a corresponding rule for
`command_call_with_block_`. When I made a rule for `command_call_with_block`,
the generated grammar didn't modify the original `command_call_with_block` but
made a new rule `command_call_with_block_`. When I made a rule for
`_command_call_with_block`, I failed one of the test cases because it parsed
something that should have been a `call` as a `chained_command_call`.

Other grammars seem to make changes as I would have expected, so I'm particularly
baffled.

I started this as a side project, so I don't have the time to investigate
these in depth. I'm recording them for whoever tries this after me. They
may or may not still be present issues.

*/

const standard_grammar = require('tree-sitter-ruby/grammar');

module.exports = grammar(standard_grammar, {
  name: 'ruby',
  // word: $ => $.old_identifier,

  externals: ($, previous) => previous.concat([
    // See the "sgrep-ext" in scanner.cc for more on how this
    // works. Just assume that this token is correctly put in
    // for all instances of a real Semgrep ellipsis, followed
    // by a newline.
    $.semgrep_ellipsis_followed_by_newline
  ]),

  conflicts: ($, previous) => previous.concat([
    /* This is just to solve the fact that all of these things
       can potentially be an ellipsis.
       We don't actually really care which one it is, though. It
       shouldn't matter in the Generic translation.

       The keyword_pattern and pair cases look like:
       { x => y, ..., z => w }

       The primary case is jsutu sage of an ellipsis as an expression.
    */
    [$.keyword_pattern, $.pair],
    [$._primary, $.pair],
    [$._primary, $.keyword_pattern]
  ]),


  /* There are a few cases for Semgrep ellipses that we would like to
     be able to parse.

     I identify the following situations as pertinent to us:
     1) singular statements of Semgrep ellipses, e.g.
          foo()
          ...
          bar()
     2) ellipses in function arguments or function parameters, e.g.
          foo(..., 1)
          or
          def method (x, ...)
          end
     3) ellipses in dot access chains
     4) (less important) ellipses for standalone expressions
     5) several other miscellaneous locations, such as inside of hashes {...}

     The former is hard because of conflating it with range expressions,
     which look like e1 ... e2. See scanner.cc and our accompanying sgrep-ext
     for how the $.semgrep_ellipsis_followed_by_newline solves this.

     For the second, Ruby has a notion of "forward parameters", which happen
     to look identical to this use case. So we can piggy-back off of that.

     For the third, expressions in Ruby can be of the form <e>.<id>, meaning
     that we can just permit identifiers to be ellipses, and this will be fine.
     This actually solves the fourth as well.
   */


  rules: {
    /* Extend identifier to accept metavariable names
       The precedence on the token is -1 because global variables
       in Ruby also start with $. To satisfy tests, global_variable
       needs to have higher precedence than identifier */
    identifier: ($, previous) =>
      token(prec(-1,
        // The token is outside of the choice here, because of the word token
        // restriction. $.identifier is the word token, meaning that it cannot
        // be a nonterminal like a choice, so the choice must be inside a larger
        // token.
        choice(
          previous,
          token(prec(1000, /\$[A-Z_][A-Z_0-9]*/)),
          // Need a high precedence here, so that "$...X" is not parsed
          // as a range expression from $ to X.
          token(prec(1000, /\$\.\.\.[A-Z_][A-Z_0-9]*/)),
          // Same here, so it works within dot accesses.
          alias(token(prec(1000, "...")), $.semgrep_ellipsis)
      ))
      ),

    // Need high precedence so this is preferred over global_variable.
    semgrep_metavariable: $ => token(prec(1000, /\$[A-Z_][A-Z_0-9]*/)),

    deep_ellipsis: $ => seq('<...', $._expression, '...>'),

    semgrep_ellipsis: $ => '...',

    _primary: ($, previous) => {
      return choice(
        previous,
        // This allows us to do interpolations and just
        // generally have ... wherever an expression is
        // needed.
        // High precedence to disambiguate it from a pair.
        $.semgrep_ellipsis,
        $.semgrep_ellipsis_followed_by_newline,
        $.deep_ellipsis
      );
    },

    // Slightly more precedence, as we would like to prefer to parse a
    // forward_argument to a semgrep ellipsis.
    forward_argument: ($, previous) => prec(1, previous),

    // Constants can appear in some places that identifiers cannot. This
    // includes the names of classes.
    // Needed for "Class with ellipses"
    constant: ($, previous) =>
      choice(previous,
        $.semgrep_metavariable,
      ),

    // needed for "Hash Pattern Ellipsis"
    keyword_pattern: ($, previous) =>
        choice(previous,
          $.semgrep_ellipsis
        ),

    // needed for "Hash Ellipsis"
    pair: ($, previous) =>
        choice(previous,
          $.semgrep_ellipsis
        ),

    // This is so that when we see something like
    // foo ..., 3
    // the parser is able to correctly identify that the
    // foo ...
    // is not a one-sided range expression. Otherwise, it would
    // start to try and reduce to a range, and get confused.
    command_argument_list: ($, previous) =>
      choice(
        seq(
          // This lets us effectively fake another token of context.
          // When parsing, it seem tree-sitter will only allow itself to look
          // ahead by a single token, when choosing to reduce or shift.
          // When we see something like
          // foo ... ,
          // we see an identifier, and our lookahead token is ..., and we don't
          // see farther than that. That means that the following two things are
          // indistinguishable:
          // 1) a range expression
          // foo ... bar
          // 2) a command argument list with an ellipsis in it
          // foo ..., 1, 2, ...
          // We need  to make the correct choice,
          // so what we will do is fake an additional token of context by folding
          // the comma into the token of the ellipsis.

          // WARNING: This will cause one particular strange case of the translation,
          // which is the interpretation of "x ..." as the application of x to a
          // semgrep ellipsis.
          alias(token(prec(1000, seq("...", ","))), $.semgrep_ellipsis_and_comma),
          previous
        ),
        previous
      )
    ,

    // Both of these changes are necessary so that we parse do-blocks in the presence of
    // ellipses correctly.
    // Otherwise, something like "foo ... do ... end" is confusing, because "foo ..."
    // looks like the start of a range expression.
    // We de-prioritize range expressions to prevent further conflicts that arise from the
    // former change.
    command_call_with_block: ($, previous) => choice(
      previous,
      seq(
        $._arg,
        '...',
        $.do_block
      ),
      // Dynamic precedence will solve a conflict between
      // arg '...' hash
      // arg '..' block
      prec.dynamic(1,seq(
        $._arg,
        '...',
        $.block
      ))
    ),
    range: ($,previous) => prec(-1, previous),

    /* ellipsis: $ =>  '...',

    deep_ellipsis: $ => seq('<...', $._expression, '...>'),

    _expression: ($, previous) => {
      return choice(
        previous,
        $.ellipsis,
        $.deep_ellipsis,
      );
    },

    _call: ($, previous) => prec.left(
      56,
      seq(
        field(
          "receiver",
          $._primary
        ),
        choice(
          ".",
          "&."
        ),
        field(
          "method",
          choice(
            $.identifier,
            $.operator,
            $.constant,
            $.argument_list,
            $.ellipsis
          )
        )
      )
    ), */

  }
});
