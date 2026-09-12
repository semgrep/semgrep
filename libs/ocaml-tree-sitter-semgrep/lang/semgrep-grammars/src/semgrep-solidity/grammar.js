/*
  semgrep-solidity

  Extends the standard solidity grammar with semgrep pattern constructs.
*/

const base_grammar = require('tree-sitter-solidity/grammar');

module.exports = grammar(base_grammar, {
  name: 'solidity',

  conflicts: ($, previous) => previous.concat([
  ]),

  /*
     Support for semgrep ellipsis ('...') and metavariables ('$FOO'),
     if they're not already part of the base grammar.
  */
  rules: {

      // Entry point. No need for the __SEMGREP_EXPRESSION hack here because
      // Solidity restricts what can appear at the toplevel, so no
      // ambiguity for the semgrep extensions.
        source_file: ($, previous) => {
          return choice(
            previous,
            repeat1($.statement),
            $.expression,
            $.constructor_definition,
            $.modifier_definition,
          );
        },

        // Metavariables. No need to patch the identifier rule because
        // Solidity already accepts '$' as part of an identifier

        // Metavariables for Solidity versions
        _pragma_version_constraint: ($, previous) => {
            return choice(
                previous,
                seq(
                    optional(
                        $.solidity_version_comparison_operator
                    ),
                    $.identifier
                ),
            )
        },

      // Ellipsis
        expression: ($, previous) => {
            return choice(
                previous,
                $.ellipsis,
                $.deep_ellipsis,
                $.member_ellipsis_expression
            );
        },

        // Statement ellipsis: src: semgrep-csharp trick
       expression_statement: ($, previous) => {
          return choice(
                previous,
                prec.right(100, seq($.ellipsis, ';')),  // expression ellipsis
                prec.right(100, $.ellipsis),  // statement ellipsis
          );
        },

       // There's no PREC.MEMBER to reuse: the upstream PREC table is a
       // module-local `const` (not exported) and has no MEMBER entry. The base
       // grammar's `member_expression` uses `prec.dynamic(1, ...)`, but dynamic
       // precedence doesn't resolve the static LR conflict this rule creates,
       // so we need a static `prec` here instead.
       member_ellipsis_expression : $ => prec(1, seq(
            field('object', choice(
                $.expression,
                $.identifier,
            )),
            '.',
            $.ellipsis
       )),

        _contract_member: ($, previous) => {
            return choice(
               previous,
               $.ellipsis
            );
        },
        struct_member: ($, previous) => {
            return choice(
               previous,
               $.ellipsis
            );
        },

        parameter: ($, previous) => {
            return choice(
               previous,
               $.ellipsis
            );
        },

        event_parameter: ($, previous) => {
            return choice(
               previous,
               $.ellipsis
            );
        },

        for_statement: ($, previous) => {
            return choice(
               previous,
               seq('for', '(', $.ellipsis, ')', $.statement)
            );
        },

        inheritance_specifier: ($, previous) => {
            return choice(
                previous,
                $.ellipsis
            );
        },

      // Allow ellipsis among the enum values by extending the base
      // grammar's enum_body rule.
      enum_body: ($, previous) => seq(
            '{',
            commaSep(choice(
                alias($.identifier, $.enum_value),
                $.ellipsis
            )),
            '}',
      ),

      // The actual ellipsis rules
        deep_ellipsis: $ => seq(
            '<...', $.expression, '...>'
        ),

        ellipsis: $ => '...',
  }
});

// copy-pasted from the original grammar, used by our enum_body override
function commaSep1(rule) {
    return seq(
        rule,
        repeat(
            seq(
                ',',
                rule
            )
        ),
        optional(','),
    );
}

function commaSep(rule) {
    return optional(commaSep1(rule));
}
