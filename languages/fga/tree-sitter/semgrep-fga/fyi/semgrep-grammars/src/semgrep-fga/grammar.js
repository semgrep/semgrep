const base_grammar = require('../tree-sitter-fga/grammar');

module.exports = grammar(base_grammar, {
  name: 'fga',

  conflicts: ($, previous) => previous.concat([
    [$.relations]
  ]),

  rules: {
    semgrep_ellipsis: $ => '...',

    semgrep_metavariable: $ => /\$[A-Z_][A-Z_0-9]*/,

    type_declaration: ($, previous) => seq(
      optional('extend'),
      'type',
      choice($.identifier, $.semgrep_metavariable),
      '\n',
      optional($.relations)
    ),

    definition: ($, previous) => seq(
      'define',
      field('relation', choice($.identifier, $.semgrep_metavariable)),
      ':',
      $.relation_def
    ),

    _module_file: ($, previous) => seq(
      choice($.model, $.module),
      repeat(choice(
        $.type_declaration,
        $.condition_declaration,
        $.semgrep_ellipsis
      ))
    ),

    relations: ($, previous) => seq(
      'relations',
      repeat(choice(
        $.definition,
        $.semgrep_ellipsis
      ))
    ),

    _expression: ($, previous) => choice(
      ...previous.members,
      $.semgrep_ellipsis,
      $.semgrep_metavariable,
    ),

    param: ($, previous) => seq(
      choice($.identifier, $.semgrep_metavariable),  // Allow $VAR
      ':',
      choice($.type_identifier, $.semgrep_metavariable)  // Allow $TYPE
    ),

    direct_relationship: ($, previous) => seq(
      '[',
      choice(
        seq(
          seq(
            choice($.identifier, $.relation_ref, $.all, $.semgrep_metavariable),
            optional($.conditional)
          ),
          optional(repeat(seq(
            ',',
            seq(
              choice($.identifier, $.relation_ref, $.all, $.semgrep_metavariable),
              optional($.conditional)
            )
          )))
        ),
        $.semgrep_ellipsis
      ),
      ']'
    ),
  }
});
