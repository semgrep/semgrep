/*
 * A partial tree-sitter grammar object containing the semgrep extensions to the
 * typescript grammar. This way, these extensions can be reused in both the TSX
 * and the TS grammars.
 *
 * Even if we only use one of these parsers to parse patterns, it's simpler to
 * keep the CSTs the same and there's little downside in doing so.
 */
module.exports = {
  conflicts: ($, previous) => previous.concat([
    [$.semgrep_expression_ellipsis, $.spread_element],
    [$.semgrep_expression_ellipsis, $.rest_pattern],
    [$.semgrep_expression_ellipsis, $.rest_type],
    [$.semgrep_expression_ellipsis, $.rest_type, $.spread_element, $.rest_pattern],
    [$.semgrep_expression_ellipsis, $.spread_element, $.rest_pattern],
    [$.semgrep_expression_ellipsis, $.semgrep_ellipsis],
    [$.class_body, $.public_field_definition],
    [$.pair, $.pair_pattern],
    // Crazy amount of conflicts here, from allowing `method_pattern` to exist.
    // Seems that it's highly conflicting with the possibility of a standalone expression.
    // Bright side, patterns need to be parsed but once and are usually very small.
    // Conflicts shouldn't be so expensive.
    [$.primary_expression, $.method_definition, $.method_signature],
    [$.primary_expression, $.method_definition, $.method_signature, $.index_signature],
    [$.primary_expression, $.index_signature],
    [$.public_field_definition],
    // Conflict for `pattern` having ellipses
    [$.pattern, $._formal_parameter],
    // Conflict for allowing `...` to be a statement
    [$.spread_element, $.rest_pattern, $.semgrep_ellipsis],
    [$.spread_element, $.rest_pattern, $.semgrep_ellipsis, $.semgrep_expression_ellipsis],
    [$.statement, $.pair, $.pair_pattern],
  ]),


  rules: {
    program: ($, previous) => choice(
      previous,
      // Used as a semgrep pattern
      $.switch_case,
      $.semgrep_expression,
    ),

    // Alternate "entry point". Allows parsing a standalone expression.
    semgrep_expression: $ => seq('__SEMGREP_EXPRESSION', $.semgrep_pattern),

    deep_ellipsis: $ => seq(
      '<...', $.expression, '...>'
    ),

    statement: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
    ),

    // For `var $X = $FUNC($REQ, $RES, ...) {...}`.
    // Unfortunately, it seems we used to allow this pattern, which really should
    // have a `function` keyword.
    // It normally fails to parse because we insert a semicolon
    // var $X = $FUNC($REQ, $RES, ...) {...}
    //                                ^ here
    // and don't know how to interpret it properly.
    assign_lambda: $ => seq(
      'var',
      field('name', choice($.identifier, $._destructuring_pattern)),
      '=',
      // Notably, we cannot use `function_expression` here, which would expect
      // a `function` keyword!
      // $.function_expression,
      field('name', optional($.identifier)),
      $._call_signature,
      field('body', $.statement_block),
    ),

    semgrep_pattern: $ => choice(
      $.expression,
      seq($.pair, optional($._semicolon)),
      $.method_pattern,
      $.function_declaration_pattern,
      $.finally_clause,
      $.catch_clause,
      $.assign_lambda,
    ),

    method_pattern: $ => choice(
      // we extracted this from the below `choice`, which contained the definitions of
      // class body elements, to allow us to match those patterns
      // however, `public_field_definition` is kinda funky and is potentially ambiguous
      // with property patterns, as something of the form `x : ty` could either be a
      // class property or a pair in a record
      // so we will allow public field definitions, but only in the presence of decorators
      seq(
        repeat1(field('decorator', $.decorator)),
        $.public_field_definition,
      ),
      seq(
        // We also had to factor out decorators here, not faithful to the original grammar,
        // so that we could have decorators in front of method signatures too.
        repeat(field('decorator', $.decorator)),
        choice(
          $.abstract_method_signature,
          $.index_signature,
          $.method_signature,
          seq(
            $.method_definition,
            optional($._semicolon),
          )
        )
      )
    ),

    function_declaration_pattern: $ => prec.right('declaration', seq(
      optional('async'),
      'function',
      choice(field('name', $.identifier), $.semgrep_ellipsis),
      $._call_signature,
      field('body', $.statement_block),
      optional($._automatic_semicolon),
    )),

    // inlined from `tree-sitter-javascript`
    // This allows us to write `import foo from $X;`
    _from_clause: $ => seq(
      'from', choice(
        field('source', $.string),
        $.semgrep_metavariable,
      )
    ),

    // This allows us to put `...` in the condition of a for loop.
    for_statement: $ => seq(
      'for',
      '(',
      choice(
        $.semgrep_ellipsis,
        seq(
          choice(
            field('initializer', choice($.lexical_declaration, $.variable_declaration)),
            seq(field('initializer', $._expressions), ';'),
            field('initializer', $.empty_statement),
          ),
          field('condition', choice(
            seq($._expressions, ';'),
            $.empty_statement,
          )),
          field('increment', optional($._expressions)),
        ),
      ),
      ')',
      field('body', $.statement),
    ),
    semgrep_metavariable: $ => /\$[A-Z_][A-Z_0-9]*/,

    _jsx_child: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
      $.semgrep_metavariable,
    ),

    // To permit {...}, for `...` in objects.
    pair: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
    ),

    // To permit `(...) => 1`, for `...` in parameter patterns.
    pair_pattern: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
    ),

    pattern: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis
    ),

    /*
      semgrep metavariables are already valid javascript/typescript
      identifiers so we do nothing for them.
    */

    semgrep_metavar_ellipsis: $ => /\$\.\.\.[A-Z_][A-Z_0-9]*/,
    semgrep_ellipsis: $ => '...',

    /* ellipsis in function parameters
     * e.g. function foo(..., x, ...)
     */
    _formal_parameter: $ => choice(
      $.semgrep_ellipsis,
      $.required_parameter,
      $.optional_parameter,
    ),

    /* This part is copied from the original grammar to add `$.semgrep_ellipsis`
     * as one of the choices. Make sure it is in sync with the original grammar
     * and update it if necessary.
     * TODO: update the original grammar to include an intermediate `class_element`
     * rule so that we can avoid copying and pasting the original rule here.
     */
    class_body: $ => seq(
      '{',
      repeat(choice(
        $.semgrep_ellipsis,
        $.decorator,
        seq($.method_definition, optional($._semicolon)),
        // As it happens for functions, the semicolon insertion should not
        // happen if a block follows the closing paren, because then it's a
        // *definition*, not a declaration. Example:
        //     public foo()
        //     { <--- this brace made the method signature become a definition
        //     }
        // The same rule applies for functions and that's why we use
        // "_function_signature_automatic_semicolon".
        seq($.method_signature, choice($._function_signature_automatic_semicolon, ',')),
        seq(
          choice(
            $.abstract_method_signature,
            $.index_signature,
            $.method_signature,
            $.public_field_definition
          ),
          choice($._semicolon, ',')
        )
      )),
      '}'
    ),

    member_expression: $ => prec('member', seq(
      field('object', choice($.expression, $.primary_expression, $.import)),
      choice('.', field('optional_chain', $.optional_chain)),
      field('property', choice(
        $.private_property_identifier,
        alias($.identifier, $.property_identifier),
        $.semgrep_ellipsis
      ),
    ))),

    // Allows `...` as in `import { ... } from 'foo'`
    import_specifier: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
    ),

    // Allows `...` as in `<div ... href={foo}></div>`
    _jsx_attribute: ($, previous) => choice(previous, $.semgrep_ellipsis),

    // Allows metavariables in `<div foo=$FOO></div>`
    _jsx_attribute_value: ($, previous) => choice(
      previous,
      $.semgrep_metavariable,
    ),

/* TODO: restore this when the changes are made in semgrep.
   Remove the XXXXXXX when uncommenting.

   You also need to restore the test file:
     lang/semgrep-grammars/src/semgrep-typescript/tsx/corpus/semgrep-ext.txt
   See the original PR:
     https://github.com/semgrep/ocaml-tree-sitter-semgrep/pull/488

    semgrep_metavar_ellipsis: $ => /\$\.\.\.[A-Z_][A-Z_0-9]*XXXXXXX/,
/*
    /* In the expression context, there are LR(1) conflicts with spread and
     * rest. I (nmote) don't think that these are true ambiguities, but just in
     * case we'll declare conflicts and set this to low dynamic precedence so as
     * to avoid incorrectly parsing target programs. */
    semgrep_expression_ellipsis: $ => prec.dynamic(-1337, '...'),

    primary_expression: ($, previous) => choice(
      previous,
      $.semgrep_expression_ellipsis,
      $.deep_ellipsis,
      $.semgrep_metavar_ellipsis,
    ),

/* TODO: restore this when the changes are made in semgrep.
    _jsx_attribute: ($, previous) => choice(
      previous,
      $.semgrep_ellipsis,
      $.semgrep_metavar_ellipsis
    ),
*/
    // TODO Remove this when we update tree-sitter-typescript past
    // https://github.com/tree-sitter/tree-sitter-typescript/pull/239. I (nmote)
    // ran into unrelated issues updating it, documented in
    // https://linear.app/r2c/issue/PA-2572/address-error-when-updating-tree-sitter-typescript.
    comment: ($, previous) => token(choice(
      previous,
      // https://tc39.es/ecma262/#sec-html-like-comments
      seq('<!--', /.*/),
      // This allows code to exist before this token on the same line.
      //
      // Technically, --> is supposed to have nothing before it on the same line
      // except for comments and whitespace, but that is difficult to express,
      // and in general tree sitter grammars tend to prefer to be overly
      // permissive anyway.
      //
      // This approach does not appear to cause problems in practice.
      seq('-->', /.*/)
   )),
  }
}
