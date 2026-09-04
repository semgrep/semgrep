/*
  semgrep-bash

  Extends the standard bash grammar with semgrep pattern constructs.

  This adds special treatment for semgrep metavariables.
  Semgrep ellipses are covered by the 'word' rule.
*/

const base_grammar = require('tree-sitter-bash/grammar');

module.exports = grammar(base_grammar, {
  name: 'bash',

  conflicts: ($, previous) => previous.concat([
  ]),

  rules: {

    _orig_simple_variable_name: $ => alias(/\w+/, $.variable_name),

    // A variable name *not* immediately followed by '=' or '+='.
    _simple_variable_name: ($, previous) => choice(
      $.semgrep_metavariable,
      previous
    ),

    _primary_expression: ($, previous) => choice(
      $.semgrep_deep_expression,
      previous
    ),

    semgrep_deep_expression: $ => seq(
      '<...',
      $._literal,  // includes concatenations and unquoted expansions
      '...>'
    ),

    // Named ellipses like '$...HELLO'.
    // Regular ellipses '...' are already parsed as 'word'.
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
          $._empty_value
        ))
      ),
      previous
    ),

    // We only want to extend simple variable names, not fragments of
    // command-line arguments. See 'literal'.
    _extended_word: $ => choice(
      $.semgrep_metavariable,
      $.word
    ),

    // Fragile. Copy of the original with $.word -> $._extended_word
    function_definition: $ => seq(
      choice(
        seq(
          'function',
          field('name', $._extended_word),
          optional(seq('(', ')'))
        ),
        seq(
          field('name', $.word),
          '(', ')'
        )
      ),
      field(
        'body',
        choice(
          $.compound_statement,
          $.subshell,
          $.test_command)
      )
    ),

    semgrep_metavariable: $ => /\$[A-Z_][A-Z_0-9]*/,
    semgrep_metavar_eq: $ => /\$[A-Z_][A-Z_0-9]*=/,
    semgrep_metavar_pluseq: $ => /\$[A-Z_][A-Z_0-9]*\+=/,
  }
});
