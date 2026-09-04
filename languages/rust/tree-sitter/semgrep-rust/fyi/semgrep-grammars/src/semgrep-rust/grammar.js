/*
 * semgrep-rust
 *
 * Extend the original tree-sitter Rust grammar with semgrep-specific constructs
 * used to represent semgrep patterns.
 */

const standard_grammar = require('tree-sitter-rust/grammar');

module.exports = grammar(standard_grammar, {
  name: 'rust',

  conflicts: ($, previous) => previous.concat([
    [$._non_delim_token, $.ellipsis],
    [$._token_pattern, $.ellipsis],
  ]),

  rules: {

    // Entry point
    source_file: ($, previous) => {
      return choice(
        previous,
        $.semgrep_expression,
        $.semgrep_statement,
      );
    },

    // Alternate "entry point". Allows parsing a standalone expression.
    semgrep_expression: $ => seq('__SEMGREP_EXPRESSION', $._expression),

    // Alternate "entry point". Allows parsing a standalone list of statements.
    semgrep_statement: $ => seq('__SEMGREP_STATEMENT', repeat1($._statement)),

    semgrep_typed_metavar: $ =>
      seq(
        $.identifier,
        ':',
        $._type,
      ),

    // Metavariables
    identifier: ($, previous) => {
      return token(
        choice(
          previous,
          /\$[A-Z_][A-Z_0-9]*/,
          /\$\.\.\.[A-Z_][A-Z_0-9]*/,
        )
      );
    },

    // Statement ellipsis: '...' not followed by ';'
    expression_statement: ($, previous) => {
      return choice(
        previous,
        prec.right(100, seq($.ellipsis, ';')),  // expression ellipsis
        prec.right(100, $.ellipsis),  // statement ellipsis
      );
    },

    _declaration_statement: ($, previous) => {
      return choice(
        previous,
        $.ellipsis
      );
    },

    field_declaration: ($, previous) => {
      return choice(
        previous,
        $.ellipsis
      );
    },

    // Expression ellipsis
    _expression: ($, previous) => {
      return choice(
        ...previous.members,
        $.ellipsis,
        $.deep_ellipsis,
        $.member_access_ellipsis_expression,
      );
    },

    parenthesized_expression: ($, previous) => seq(
      '(',
      choice($._expression, $.semgrep_typed_metavar),
      ')'
    ),

    _non_special_token: ($, previous) => choice(
      previous,
      $.ellipsis,
    ),

    // TODO: have to use 13 instead of PREC.field because the Rust grammar
    // doesn't export precedences. Should we use something like
    // https://github.com/jhnns/rewire to access this directly?
    member_access_ellipsis_expression: $ => prec(13, seq(
      field('expression', $._expression),
      '.',
      $.ellipsis
    )),

    deep_ellipsis: $ => seq(
      '<...', $._expression, '...>'
    ),

    ellipsis: $ => '...',
  }
});


function sepBy1(sep, rule) {
  return seq(rule, repeat(seq(sep, rule)))
}

function sepBy(sep, rule) {
  return optional(sepBy1(sep, rule))
}
