// https://openfga.dev/docs/configuration-language
const DIGIT = /[0-9]/;
const HEX_DIGIT = /[0-9a-fA-F]/;
const EXPONENT = /[eE][+-]?[0-9]+/;

const PREC = {
  primary: 7,
  unary: 6,
  multiplicative: 5,
  additive: 4,
  comparative: 3,
  and: 2,
  or: 1,
};
const types = [
  'string',
  'int',
  'map',
  'uint',
  'list',
  'timestamp',
  'bool',
  'duration',
  'double',
  'ipaddress',
];
const multiplicative_operators = ['*', '/', '%', '<<', '>>', '&', '&^'];
const additive_operators = ['+', '-', '|', '^'];
const comparative_operators = ['==', '!=', '<', '<=', '>', '>='];

module.exports = grammar({
  name: 'fga',

  extras: ($) => [$.comment, /\s/],

  word: ($) => $.identifier,

  supertypes: ($) => [$._expression],

  rules: {
    source_file: ($) => choice($._project_file, $._module_file),

    _project_file: ($) => seq(alias($.quoted_schema, $.schema), $.contents),
    _module_file: ($) =>
      seq(
        choice($.model, $.module),
        repeat(choice($.type_declaration, $.condition_declaration)),
      ),

    quoted_schema: ($) => seq('schema:', $._quoted_version),

    contents: ($) => seq('contents:', repeat(seq('-', $.file))),

    file: ($) => /.+\..+/,

    schema: ($) => seq('schema', $.version),

    version: ($) => /[0-9]+\.[0-9]+/,
    _quoted_version: ($) => seq('\'', $.version, '\''),

    model: ($) => seq('model', '\n', $.schema),
    module: ($) => seq('module', $.identifier),

    type_declaration: ($) =>
      seq(
        optional('extend'),
        'type',
        $.identifier,
        '\n',
        optional($.relations),
      ),

    relations: ($) => seq('relations', repeat($.definition)),

    definition: ($) =>
      seq('define', field('relation', $.identifier), ':', $.relation_def),

    relation_def: ($) =>
      choice(
        $.direct_relationship,
        seq(
          optional(seq($.direct_relationship, $.operator)),
          list(choice($.identifier, $.indirect_relation), $.operator),
        ),
      ),

    operator: ($) => choice('or', 'and', 'but not'),

    direct_relationship: ($) =>
      seq(
        '[',
        list(
          seq(
            choice($.identifier, $.relation_ref, $.all),
            optional($.conditional),
          ),
          ',',
        ),
        ']',
      ),

    conditional: ($) => seq('with', $.identifier),

    indirect_relation: ($) => seq($.identifier, 'from', $.identifier),

    relation_ref: ($) =>
      seq($.identifier, token.immediate(prec(1, '#')), $.identifier),

    all: ($) => seq($.identifier, token.immediate(':*')),

    condition_declaration: ($) =>
      seq(
        'condition',
        field('name', $.identifier),
        '(',
        optional(list($.param, ',')),
        ')',
        field('body', $.condition_body),
      ),

    param: ($) => seq($.identifier, ':', $.type_identifier),

    type_identifier: ($) => choice(...types),

    _comparative_operator: ($) => choice(...comparative_operators),

    condition_body: ($) => seq('{', $._expression, '}'),

    binary_expression: ($) =>
      choice(
        prec.left(
          PREC.multiplicative,
          seq(
            field('left', $._expression),
            field('operator', choice(...multiplicative_operators)),
            field('right', $._expression),
          ),
        ),
        prec.left(
          PREC.additive,
          seq(
            field('left', $._expression),
            field('operator', choice(...additive_operators)),
            field('right', $._expression),
          ),
        ),
        prec.left(
          PREC.comparative,
          seq(
            field('left', $._expression),
            field('operator', choice(...comparative_operators)),
            field('right', $._expression),
          ),
        ),
        prec.left(
          PREC.and,
          seq(
            field('left', $._expression),
            field('operator', '&&'),
            field('right', $._expression),
          ),
        ),
        prec.left(
          PREC.or,
          seq(
            field('left', $._expression),
            field('operator', '||'),
            field('right', $._expression),
          ),
        ),
      ),

    unary_expression: ($) =>
      prec(
        PREC.unary,
        seq(
          field('operator', choice('+', '-', '!')),
          field('operand', $._expression),
        ),
      ),

    call_expression: ($) =>
      prec(
        PREC.primary,
        seq(
          field('function', choice($.selector_expression, $.identifier)),
          field('arguments', $.argument_list),
        ),
      ),

    selector_expression: ($) =>
      prec(
        PREC.primary,
        seq(field('operand', $.identifier), '.', field('field', $.identifier)),
      ),

    argument_list: ($) => seq('(', optional(list($._expression, ',')), ')'),

    _expression: ($) =>
      choice(
        $.number_literal,
        $.boolean_literal,
        $.string_literal,
        $.null_literal,
        // $.unary_expression,
        $.binary_expression,
        $.selector_expression,
        $.call_expression,
        $.identifier,
        // slices?
      ),

    number_literal: $ => choice(
      $.float_literal,
      $.int_literal,
      $.uint_literal
    ),

    float_literal: $ => token(choice(
      seq(repeat1(DIGIT), '.', repeat1(DIGIT), optional(EXPONENT)),
      seq(repeat1(DIGIT), EXPONENT),
      seq('.', repeat1(DIGIT), optional(EXPONENT))
    )),

    int_literal: $ => token(choice(
      repeat1(DIGIT),
      seq('0x', repeat1(HEX_DIGIT))
    )),

    uint_literal: $ => token(choice(
      seq(repeat1(DIGIT), choice('u', 'U')),
      seq('0x', repeat1(HEX_DIGIT), choice('u', 'U'))
    )),

    boolean_literal: $ => choice('true', 'false'),

    null_literal: $ => 'null',

    string_literal: $ => token(choice(
      seq('"', repeat(choice(/[^"\\\n\r]/, /\\./)), '"'),
      seq("'", repeat(choice(/[^'\\\n\r]/, /\\./)), "'")
    )),

    identifier: ($) => /[a-zA-Z_-]+/,

    comment: ($) => token(seq('#', /.+/)),
  },
});

/**
 * Creates a rule to match one or more of the rules separated by a separator
 *
 * @param {rule} rule to match against
 * @param {separator} separator of the list of elements
 */
function list(rule, separator) {
  return seq(rule, optional(repeat(seq(separator, rule))));
}
