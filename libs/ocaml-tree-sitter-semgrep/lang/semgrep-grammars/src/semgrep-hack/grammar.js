/*
  semgrep-hack

  Extends the standard hack grammar with semgrep pattern constructs.
*/

// The npm package is 'tree-sitter-hacklang', not 'tree-sitter-hack',
// because npm doesn't like the word 'hack'. Here, we don't use npm
// and point to the local folder named tree-sitter-hack.
//
const base_grammar = require('tree-sitter-hack/grammar');

// BEGIN: Helper functions copied from tree-sitter-hack.

/**
 * Comma separated rules. A ',' as the last argument indicates an optional trailing comma.
 */
function com(...rules) {
  if (rules[rules.length - 1] == ',') {
    rules.splice(-1, 1);
    return seq(...rules, repeat(seq(',', ...rules)), optional(','));
  } else {
    return seq(...rules, repeat(seq(',', ...rules)));
  }
}

[seq, choice, alias, com].forEach((func) => {
  func.opt = (...args) => optional(func(...args));
  func.rep = (...args) => repeat(func(...args));
  func.rep1 = (...args) => repeat1(func(...args));
});

const opt = optional;
const rep = repeat;

// END: Helper functions copied from tree-sitter-hack.

module.exports = grammar(base_grammar, {
  name: 'hack',

  conflicts: ($, previous) => previous.concat([]),

  rules: {
    /*
      Support for semgrep ellipsis ('...')
    */

    // For now, we give precedence to native ellipsis (aka variadic modifiers).
    ellipsis: ($) => prec(-1, '...'),
    deep_ellipsis: ($) => prec(-1, seq('<...', $._expression, '...>')),

    // By using an empty statement, we leverage a statement type that is strictly only used within
    // $._statement and nowhere else.
    empty_statement: ($, previous) =>
      choice(
        previous,
        // For conflict with `ellipsis;`
        // we want it to be read as an expression with a semicolon
        // instead of a statement
        prec(-1, $.ellipsis) // statement ellipsis
      ),

    _expression: ($, previous) => choice(previous, $.ellipsis, $.deep_ellipsis),

    member_declarations: ($) =>
      seq(
        '{',
        choice.rep(
          // Copied from existing grammar
          alias($._class_const_declaration, $.const_declaration),
          $.method_declaration,
          $.property_declaration,
          $.type_const_declaration,
          $.trait_use_clause,
          $.require_implements_clause,
          $.require_extends_clause,
          $.xhp_attribute_declaration,
          $.xhp_children_declaration,
          $.xhp_category_declaration,

          // Additions
          $.ellipsis
        ),
        '}'
      ),

    parameter: ($, previous) => choice(previous, $.ellipsis),

    /*
      Support for semgrep metavariables ('$FOO')
    */

    semgrep_identifier: ($) => /\$[A-Z_][A-Z_0-9]*/,
    _semgrep_variadic_identifier: ($) => /\$\.\.\.[A-Z_][A-Z_0-9]*/,
    _semgrep_extended_identifier: ($) =>
      choice($.semgrep_identifier, $.identifier),

    // Normally, we would hope to extend $.identifier to support
    // the $ prefix. However, this causes conflicts with Hack
    // variable syntax. So, we manually add the semgrep semgrep_identifier
    // where want to support it.
    // TODO: Continue adding language constructs where we want to support
    // metavariable overrides (like in XHP). Or get global $.identifier
    // extension working.

    qualified_identifier: ($, previous) =>
      choice(previous, $.semgrep_identifier),

    // Alternative reach-in edit strategy:
    /*
    _function_declaration_header: ($, previous) => {
      // Overriding name field
      previous.members[2]['content'] = choice(
        $.identifier,
        $._semgrep_identifier
      );
      return previous;
    },
    */

    alias_declaration: ($) =>
      seq(
        opt($.attribute_modifier),
        choice('type', 'newtype'),
        $._semgrep_extended_identifier, // Overridden
        opt($.type_parameters),
        field('as', seq.opt('as', $._type)),
        '=',
        $._type,
        ';'
      ),

    _function_declaration_header: ($) =>
      seq(
        opt($.async_modifier),
        'function',
        field('name', $._semgrep_extended_identifier), // Overridden
        opt($.type_parameters),
        $.parameters,
        seq.opt(':', opt($.attribute_modifier), field('return_type', $._type)),
        opt($.where_clause)
      ),

    trait_declaration: ($) =>
      seq(
        opt($.attribute_modifier),
        'trait',
        field('name', $._semgrep_extended_identifier), // Overridden
        opt($.type_parameters),
        opt($.implements_clause),
        opt($.where_clause),
        field('body', $.member_declarations)
      ),

    interface_declaration: ($) =>
      seq(
        opt($.attribute_modifier),
        'interface',
        field('name', $._semgrep_extended_identifier), // Overridden
        opt($.type_parameters),
        opt($.extends_clause),
        opt($.where_clause),
        field('body', $.member_declarations)
      ),

    class_declaration: ($) =>
      seq(
        opt($.attribute_modifier),
        opt($._class_modifier),
        opt($._class_modifier),
        opt($.xhp_modifier),
        'class',
        field(
          'name',
          choice($._semgrep_extended_identifier, $._xhp_identifier) // Overridden
        ),
        opt($.type_parameters),
        opt($.extends_clause),
        opt($.implements_clause),
        opt($.where_clause),
        field('body', $.member_declarations)
      ),

    _class_const_declarator: ($) =>
      seq(
        field(
          'name',
          choice(
            $._semgrep_extended_identifier, // Overridden
            alias($._keyword, $.identifier)
          )
        ),
        field(
          'value',
          // The only reason we need a separate const declarator for classes is that
          // the assignment expression is optional.
          seq.opt('=', $._expression)
        )
      ),

    type_const_declaration: ($) =>
      seq(
        opt($.attribute_modifier),
        rep($._member_modifier),
        'const',
        'type',
        field('name', $._semgrep_extended_identifier), // Overridden
        opt($.type_parameters),
        field('as', seq.opt('as', $._type)),
        field('type', seq.opt('=', $._type)),
        ';'
      ),

    const_declarator: ($) =>
      seq(
        field(
          'name',
          choice(
            $._semgrep_extended_identifier, // Overridden
            alias($._keyword, $.identifier)
          )
        ),
        field('value', seq('=', $._expression))
      ),

    enum_declaration: ($) =>
      seq(
        opt($.attribute_modifier),
        'enum',
        field('name', $._semgrep_extended_identifier), // Overridden
        ':',
        field('type', $._type),
        field('as', seq.opt('as', $._type)),
        '{',
        rep($.enumerator),
        '}'
      ),

    enumerator: ($) =>
      seq($._semgrep_extended_identifier, '=', $._expression, ';'), // Overridden

    /*
      Support for semgrep variadic metavariables ('$...ARGS')
    */

    argument: ($, previous) => choice(previous, $._semgrep_variadic_identifier),
  },
});
