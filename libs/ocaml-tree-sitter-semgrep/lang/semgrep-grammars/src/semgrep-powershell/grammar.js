/*
  semgrep-powershell

  Extends the standard powershell grammar with semgrep pattern constructs.

  Metavariables ($FOO) are already handled by the base grammar's variable
  rule, which matches $[A-Z_][A-Z_0-9]* naturally.
  We add support for ellipsis ('...') in statements and expressions.
*/

// tree-sitter-powershell uses ESM ('export default grammar(...)') which can't
// be loaded via require(). The ./prep script creates a CJS copy as a workaround.
// See the prep script header for details.
const base_grammar = require('./grammar-base');

module.exports = grammar(base_grammar, {
  name: 'powershell',

  conflicts: ($, previous) => previous.concat([
    [$._statement, $._expression],
  ]),

  rules: {
    semgrep_ellipsis: $ => '...',

    semgrep_deep_ellipsis: $ => seq('<...', $._expression, '...>'),

    // Allow ellipsis as a statement
    _statement: ($, previous) => prec.right(choice(
      previous,
      $.semgrep_ellipsis,
    )),

    // Allow ellipsis and deep ellipsis in expressions
    _expression: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
      $.semgrep_deep_ellipsis,
    ),

    // Allow ellipsis in command arguments
    _command_element: ($, previous) => prec.right(choice(
      previous,
      $.semgrep_ellipsis,
    )),

    // Allow ellipsis in method invocation argument lists
    // e.g. $OBJ.GetMethods(...)
    argument_expression: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
    ),
  }
});
