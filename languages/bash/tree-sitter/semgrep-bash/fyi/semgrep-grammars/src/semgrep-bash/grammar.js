/*
  semgrep-bash

  Extends the standard bash grammar with semgrep pattern constructs.

  This adds special treatment for semgrep metavariables.
  Semgrep ellipses are covered by the 'word' rule, except that a '...' token
  is also accepted as a word where needed (see '_terminated_statement').
*/

const base_grammar = require('tree-sitter-bash/grammar');

module.exports = grammar(base_grammar, {
  name: 'bash',

  conflicts: ($, previous) => previous.concat([
  ]),

  rules: {
    // Upstream makes every arithmetic binary operator left-associative, so
    // `2 ** 3 ** 2` parses as `(2 ** 3) ** 2` and `a = b = 1` as
    // `(a = b) = 1`; it also ranks '+=' etc. below '='. In bash, '**' and
    // assignments are right-associative, and all assignments share one
    // precedence. Otherwise the same table and values (upstream's PREC).
    _arithmetic_binary_expression: $ => {
      const table = [
        [choice('+=', '-=', '*=', '/=', '%=', '**=', '<<=', '>>=', '&=', '^=', '|='), 1, 'right'],
        [choice('=', '=~'), 1, 'right'],
        ['||', 3, 'left'],
        ['&&', 4, 'left'],
        ['|', 5, 'left'],
        ['^', 6, 'left'],
        ['&', 7, 'left'],
        [choice('==', '!='), 8, 'left'],
        [choice('<', '>', '<=', '>='), 9, 'left'],
        [choice('<<', '>>'), 12, 'left'],
        [choice('+', '-'), 13, 'left'],
        [choice('*', '/', '%'), 14, 'left'],
        ['**', 15, 'right'],
      ];
      return choice(...table.map(([operator, precedence, assoc]) =>
        prec[assoc](precedence, seq(
          field('left', $._arithmetic_expression),
          field('operator', operator),
          field('right', $._arithmetic_expression),
        ))));
    },

    // Upstream's ternary is left-associative: `x ? y : z ? 1 : 0` parses as
    // `(x ? y : z) ? 1 : 0`. In bash, as in C, it's `x ? y : (z ? 1 : 0)`.
    // Upstream's PREC.TERNARY (2).
    _arithmetic_ternary_expression: $ => prec.right(2, seq(
      field('condition', $._arithmetic_expression),
      '?',
      field('consequence', $._arithmetic_expression),
      ':',
      field('alternative', $._arithmetic_expression),
    )),

    // Upstream gives unary - + ~ ! in arithmetic a lower precedence than
    // binary operators, so `-a + b` parses as `-(a + b)`. In bash they bind
    // tighter than all binary operators, including '**' (`-2 ** 2` is 4).
    // Use upstream's PREC.PREFIX (17) and PREC.NEGATE (16) values.
    _arithmetic_unary_expression: $ => choice(
      prec(17, seq(
        field('operator', choice(token(prec(1, '++')), token(prec(1, '--')))),
        $._arithmetic_expression,
      )),
      prec(16, seq(
        field('operator', choice(
          token(prec(1, '-')), token(prec(1, '+')), token(prec(1, '~')),
        )),
        $._arithmetic_expression,
      )),
      prec.right(16, seq(
        field('operator', '!'),
        $._arithmetic_expression,
      )),
    ),

    // Upstream's flat `pipeline` lets a redirected *pipeline* be a stage, so
    // `a | b | c` NEWLINE `d > f` merges `d` into `c` (tree-sitter-bash #347).
    // A redirect binds to the last command of a pipeline anyway, so exclude
    // pipelines from the body. List _statement's members (minus pipeline)
    // rather than use _statement_not_pipeline: its prec(1) breaks e.g.
    // `done 3<<<"$X"`.
    redirected_statement: $ => prec.dynamic(-1, prec.right(-1, choice(
      seq(
        field('body', choice(
          $.redirected_statement, $.variable_assignment, $.variable_assignments,
          $.command, $.declaration_command, $.unset_command, $.test_command,
          $.negated_command, $.for_statement, $.c_style_for_statement,
          $.while_statement, $.if_statement, $.case_statement, $.list,
          $.compound_statement, $.function_definition, $.subshell,
        )),
        field('redirect', repeat1(choice($.file_redirect, $.heredoc_redirect))),
      ),
      seq(
        field('body', choice($.if_statement, $.while_statement)),
        $.herestring_redirect,
      ),
      field('redirect', repeat1($._redirect)),
      $.herestring_redirect,
    ))),


    // Allow '...' as the last statement of a block without a terminator, as
    // in 'while ...; do ... done', 'if ...; then ... fi' and '{ ...; ... }'.
    // Upstream requires a terminator before the closing keyword. Only '...' is
    // allowed to skip it: for any command, the keyword 'done' would be lexed as
    // closing the block and break 'for i in a; do echo done; done'.
    // Tree-sitter prefers the string '...' to the 'word' pattern on a tie, so
    // the parser sees a dedicated token; accept it wherever a word is. Do not
    // give it a lexical precedence: it would win against the external newline
    // token, and 'echo a\n...' would read '...' as an argument.
    _terminated_statement: ($, previous) => choice(
      previous,
      seq(optional(previous), alias($._ellipsis_command, $.command)),
    ),

    _ellipsis_command: $ => field('name', alias($._ellipsis_name, $.command_name)),
    _ellipsis_name: $ => alias('...', $.word),

    _orig_simple_variable_name: $ => alias(/\w+/, $.variable_name),

    // A variable name *not* immediately followed by '=' or '+='.
    _simple_variable_name: ($, previous) => choice(
      $.semgrep_metavariable,
      previous
    ),

    _primary_expression: ($, previous) => choice(
      $.semgrep_deep_expression,
      alias('...', $.word),
      previous
    ),

    semgrep_deep_expression: $ => seq(
      '<...',
      $._literal,  // includes concatenations and unquoted expansions
      '...>'
    ),

    // Named ellipses like '$...HELLO'.
    // Regular ellipses '...' are parsed as 'word' (or as the '...' token
    // aliased to 'word', see '_terminated_statement').
    semgrep_named_ellipsis: $ => /\$\.\.\.[A-Z_][A-Z_0-9]*/,

    simple_expansion: $ => choice(
      seq(
        // This should parse the same input as the original. It should not
        // parse '$$X' as "expand metavariable $X".
        // TODO: use token.immediate(...) to only accept variables that stick
        //       to the '$'.
        //       This should be fixed in the original grammar as well.
        '$',
        choice(
          $._orig_simple_variable_name,  // no metavariable allowed here
          $._special_variable_name,
          alias('!', $.special_variable_name),
          alias('#', $.special_variable_name)
        )
      ),
      $.semgrep_named_ellipsis
    ),

    // Override variable assignments to treat '$X=42' as an assignment rather
    // than an ordinary literal.
    variable_assignment: ($, previous) => choice(
      seq(
        // $FOO= or $FOO+=
        choice(
          $.semgrep_metavar_eq,
          $.semgrep_metavar_pluseq
        ),
        field('value', choice(
          $._literal,
          $.array,
          $._empty_value,
          alias($._comment_word, $.word)
        ))
      ),
      previous
    ),

    // '$X=0' in arithmetic contexts such as 'for (( $X=0; ...))'.
    _c_variable_assignment: ($, previous) => choice(
      seq($.semgrep_metavar_eq, $._c_expression),
      previous
    ),

    // Copy of the original with $._simple_variable_name ->
    // $._orig_simple_variable_name: '$X' in arithmetic must remain a
    // 'simple_expansion' (as in the original grammar) rather than lex as a
    // bare 'semgrep_metavariable' name. Patterns such as '$(( $X + 1 ))'
    // still get a metavariable via 'simple_expansion'.
    // Also accepts the ellipsis '...' and deep expressions '<... $X ...>'.
    _arithmetic_literal: $ => prec(1, choice(
      alias('...', $.word),
      $.semgrep_deep_expression,
      $.number,
      $.subscript,
      $.simple_expansion,
      $.expansion,
      $._orig_simple_variable_name,
      $.variable_name,
      $.string,
      $.raw_string,
    )),

    // Ellipsis in C-style for headers: 'for (( ...; ...; ... ))'.
    _c_expression_not_assignment: ($, previous) => choice(
      alias('...', $.word),
      previous
    ),

    // We only want to extend simple variable names, not fragments of
    // command-line arguments. See 'literal'.
    _extended_word: $ => choice(
      $.semgrep_metavariable,
      $.word
    ),

    // Fragile. Copy of the original with $.word -> $._extended_word
    function_definition: $ => prec.right(seq(
      choice(
        seq(
          'function',
          field('name', $._extended_word),
          optional(seq('(', ')'))
        ),
        seq(
          field('name', choice($.word, alias('...', $.word))),
          '(', ')'
        )
      ),
      field(
        'body',
        choice(
          $.compound_statement,
          $.subshell,
          $.test_command,
          $.if_statement)
      ),
      field('redirect', optional($._redirect))
    )),

    semgrep_metavariable: $ => /\$[A-Z_][A-Z_0-9]*/,
    semgrep_metavar_eq: $ => /\$[A-Z_][A-Z_0-9]*=/,
    semgrep_metavar_pluseq: $ => /\$[A-Z_][A-Z_0-9]*\+=/,
  }
});
