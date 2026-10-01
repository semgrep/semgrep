// ---------------------------------------------------------------------------------------
// R has an operator precedence table defined here:
// https://github.com/wch/r-source/blob/0a8f53a7ba47463f1c938dd3e2c2acc7a2d3a1c2/src/main/gram.y#L419-L441
//
// Our precedence table defined in `PREC` follows the R precedence order pretty closely.
// However, we sometimes need to adjust the associativity, see `NOTE ON PREC.RIGHT` below.
//
// We also don't need the `ELSE` or `PIPEBIND` precedence specifications from the table.
// We handle `ELSE` within the `if` node using a special external, and `=>` didn't make it
// into a release version of R (it is off by default) and we think it is unlikely to make
// a return.
//
// Note that if a precedence rank is unspecified in a rule, it can be assumed to be 0.
// ---------------------------------------------------------------------------------------

// ---------------------------------------------------------------------------------------
// NOTE ON PREC.RIGHT:
//
// A few things in this table are left associative in R's grammar, but we are forced to
// make them right associative to get the behavior we want. This includes:
// - $, @
// - ::, :::
//
// We are forced to do this because we want these nodes to have an `optional()` RHS.
// While `dplyr::` isn't parsable R code, we want it to be recognized as a `::` node with
// a known package name, as this helps us generate completions.
//
// The trailing `optional()` then means there are two interpretations of `foo::bar`:
// - [foo::][bar] == [namespace][identifier]
// - [foo::bar] == [namespace]
//
// We want to force the latter, which means that the namespace rule has to be right
// associative to "prefer matching a rule that ends later".
//
// In practice we don't think this will matter for the rules we've had to swap the order
// on, since they are typically the only things at their numeric precedence rank.
// ---------------------------------------------------------------------------------------

// ---------------------------------------------------------------------------------------
// NOTE ON OPERATORS:
//
// For `unary_operator`, `binary_operator`, `extract_operator`, and `namespace_operator`,
// the way these have been grouped is based on the semantic definition of each kind of
// operator group. Specifically:
//
// <unary> <expr>
// <expr> <binary> <expr>
// <expr> <extract> <symbol>
// <symbol> <namespace> <symbol>
//
// In theory, we could have gone further to, say, split out comparison and arithmetic
// operators from the binary operator group, or split out the pipe as its own rule.
// However, this is all rather arbitrary, and we decided it was best to stop the grouping
// at semantic meaning, and let end consumers layer on additional behavior as needed by
// creating more granular groups based on the `operator` field.
// ---------------------------------------------------------------------------------------

// ---------------------------------------------------------------------------------------
// NOTE ON NEWLINES BETWEEN PARENTHESES:
//
// Typically, we handle newlines explicitly in the R grammar, as they often do have
// contextual meaning.
//
// However, the external scanner will proactively consume `\n` if we are within a scope
// defined by a `(`, `[`, or `[[`. This is why you don't see `repeat($.newline)` within
// the `parameters` node, or after the `(` of an `if_statement` node - newlines will have
// been consumed already!
// ---------------------------------------------------------------------------------------

const PREC = {
  // #
  // NOTE: If we don't put comments at a negative rank, then `"#"` will treat the `#` as
  // the start of a comment rather than being part of the string.
  COMMENT: { ASSOC: prec, RANK: -1},

  // {, (
  // NOTE: If we understand correctly, brace and parenthesis blocks are given the same
  // precedence as all other general expressions. Note that they are not tagged with an
  // explicit `%prec` in the links below, and the last terminal nodes of `}` and `)` do
  // not have a `%left` or `%right` precedence assigned in the table above (the last
  // terminal node is how Bison assigns precedence to a rule by default), so we are left
  // to assume they have default precedence.
  // https://github.com/wch/r-source/blob/0a8f53a7ba47463f1c938dd3e2c2acc7a2d3a1c2/src/main/gram.y#L467-L468
  // https://www.gnu.org/software/bison/manual/html_node/How-Precedence.html
  BLOCK: { ASSOC: prec, RANK: 0 },

  // ?
  HELP: { ASSOC: prec.left, RANK: 1 },

  // function() {}
  // while() {}
  // for() {}
  // repeat {}
  FUNCTION_OR_LOOP: { ASSOC: prec.left, RANK: 2 },

  // if {}
  IF: { ASSOC: prec.right, RANK: 3 },

  // <-, <<-, :=
  LEFT_ASSIGN: { ASSOC: prec.right, RANK: 4 },

  // =
  EQUALS_ASSIGN: { ASSOC: prec.right, RANK: 5 },

  // ->, ->>
  RIGHT_ASSIGN: { ASSOC: prec.left, RANK: 6 },

  // ~
  TILDE: { ASSOC: prec.left, RANK: 7 },

  // |, ||
  OR: { ASSOC: prec.left, RANK: 8 },

  // &, &&
  AND: { ASSOC: prec.left, RANK: 9 },

  // !
  UNARY_NOT: { ASSOC: prec.left, RANK: 10 },

  // >, >=, <, <=, ==, !=
  // NOTE: These are nonassoc in R's grammar, but we have to specify
  // associativity to generate the grammar, and left seems correct.
  COMPARISON: { ASSOC: prec.left, RANK: 11 },

  // +, -
  PLUS_MINUS: { ASSOC: prec.left, RANK: 12 },

  // *, /
  MULTIPLY_DIVIDE: { ASSOC: prec.left, RANK: 13 },

  // %>%, %<>%, |>
  SPECIAL_OR_PIPE: { ASSOC: prec.left, RANK: 14 },

  // :
  COLON: { ASSOC: prec.left, RANK: 15 },

  // +, -
  UNARY_PLUS_MINUS: { ASSOC: prec.left, RANK: 16 },

  // ^, **
  EXPONENTIATE: { ASSOC: prec.right, RANK: 17 },

  // $, @
  // NOTE: See `NOTE ON PREC.RIGHT` above
  EXTRACT: { ASSOC: prec.right, RANK: 18 },

  // ::, :::
  // NOTE: See `NOTE ON PREC.RIGHT` above
  NAMESPACE: { ASSOC: prec.right, RANK: 19 },

  // match(1, 2), [, [[
  // NOTE: We aren't entirely sure how Bison works for calls and subsets. Practically,
  // we need calls to have high precedence so things like `function(x, y, z) match(x, y)`
  // don't get parsed as a call with function `function(x, y, z) match` and arguments of
  // `(x, y)`. In the Bison grammar, there is no `%prec` specified for these rules, and
  // the last terminal nodes of `]` and `)` don't have an assigned precedence in the
  // table, so in theory they have the same precedence as general R expressions, but that
  // obviously isn't the case. Possibly this has to do with Bison's lookahead that
  // tree-sitter doesn't do.
  // https://github.com/wch/r-source/blob/0a8f53a7ba47463f1c938dd3e2c2acc7a2d3a1c2/src/main/gram.y#L501
  // https://github.com/wch/r-source/blob/0a8f53a7ba47463f1c938dd3e2c2acc7a2d3a1c2/src/main/gram.y#L507-L508
  // https://github.com/wch/r-source/blob/0a8f53a7ba47463f1c938dd3e2c2acc7a2d3a1c2/src/main/gram.y#L441
  CALL: { ASSOC: prec.right, RANK: 20 },
}

module.exports = grammar({
  name: 'r',

  extras: $ => [
    $.comment,
    /\s/
  ],

  // We inline `_identifier` and `_string_or_identifier` for use in
  // `_string_or_identifier` itself and for use in
  // `_argument_name_string_or_identifier_or_null`.
  //
  // In particular, in `_argument_name_string_or_identifier_or_null` we need
  // all choices flattened into a flat array, so that `prec(1)` can be applied
  // to all choices, which doesn't seem to happen when we have nesting.
  //
  // Without this, we get conflicts between `_identifier` and `_expression`
  // because it can't choose between `_argument_unnamed` and `_argument_named`
  // because the `prec(1)` isn't being applied recursively to `_identifier`
  //
  // I imagine it working like this:
  //
  // ```
  // _argument_name_string_or_identifier_or_null: $ => prec(1, choice(
  //   $._string_or_identifier,
  //   $.null
  // )),
  // inlines to ->
  // _argument_name_string_or_identifier_or_null: $ => prec(1, choice(
  //   $.string,
  //   $._identifier,
  //   $.null
  // )),
  // inlines to ->
  // _argument_name_string_or_identifier_or_null: $ => prec(1, choice(
  //   $.string,
  //   $.dots,
  //   $.dot_dot_i,
  //   $.identifier,
  //   $.null
  // )),
  // ```
  inline: $ => [
    $._identifier,
    $._string_or_identifier
  ],

  externals: $ => [
    $._start,
    $._newline,
    $._semicolon,
    $._raw_string_open,
    $._raw_string_content,
    $._raw_string_close,
    // Don't use `_external` variants directly. Instead use their aliased versions.
    $._external_else,
    $._external_open_parenthesis,
    $._external_close_parenthesis,
    $._external_open_brace,
    $._external_close_brace,
    $._external_open_bracket,
    $._external_close_bracket,
    $._external_open_bracket2,
    $._external_close_bracket2,
    // If a syntax error is encountered, the first thing to occur is that our external
    // scanner is called with all symbols marked valid. We really don't want this, so
    // we include a sentinel external to detect this and decline to handle.
    $._error_sentinel
  ],

  word: $ => $.identifier,

  rules: {
    // Top-level rules.
    // The zero width `$._start` ensures that `program` starts at `(0, 0)`.
    program: $ => seq(
        $._start,
        repeat(choice($._expression, $._semicolon, $._newline))
    ),

    // Function definitions.
    function_definition: $ => withPrec(PREC.FUNCTION_OR_LOOP, seq(
      field("name", choice("\\", "function")),
      repeat($._newline),
      field("parameters", $.parameters),
      repeat($._newline),
      field("body", $._expression)
    )),

    // NOTE: We include "(" and ")" as part of the rule here to allow
    // tree-sitter to create a "parameters" node in the AST even when
    // no parameters are declared for a function.
    // NOTE: See `NOTE ON NEWLINES BETWEEN PARENTHESES` above
    parameters: $ => seq(
      field("open", $._open_parenthesis),
      optional(seq(
        field("parameter", $.parameter),
        repeat(seq($.comma, field("parameter", $.parameter)))
      )),
      field("close", $._close_parenthesis)
    ),

    parameter: $ => choice(
      $._parameter_with_default,
      $._parameter_without_default
    ),

    // Note that the default is required if we see an `=`,
    // i.e. `function(x = ) {}` is not valid R code and does
    // not parse (#161).
    _parameter_with_default: $ => seq(
      $._parameter_name,
      "=",
      field("default", $._expression)
    ),

    _parameter_without_default: $ => $._parameter_name,

    _parameter_name: $ => field("name", $._identifier),

    // Control flow.
    // NOTE: See `NOTE ON NEWLINES BETWEEN PARENTHESES` above
    if_statement: $ => withPrec(PREC.IF, seq(
      "if",
      repeat($._newline),
      field("open", $._open_parenthesis),
      field("condition", $._expression),
      field("close", $._close_parenthesis),
      repeat($._newline),
      field("consequence", $._expression),
      // No `repeat($._newline)` here. Specially handled in the scanner instead.
      optional(seq(
        $._else,
        repeat($._newline),
        field("alternative", $._expression)
      ))
    )),

    // NOTE: See `NOTE ON NEWLINES BETWEEN PARENTHESES` above
    for_statement: $ => withPrec(PREC.FUNCTION_OR_LOOP, seq(
      "for",
      repeat($._newline),
      field("open", $._open_parenthesis),
      field("variable", $._identifier),
      "in",
      field("sequence", $._expression),
      field("close", $._close_parenthesis),
      repeat($._newline),
      field("body", $._expression)
    )),

    // NOTE: See `NOTE ON NEWLINES BETWEEN PARENTHESES` above
    while_statement: $ => withPrec(PREC.FUNCTION_OR_LOOP, seq(
      "while",
      repeat($._newline),
      field("open", $._open_parenthesis),
      field("condition", $._expression),
      field("close", $._close_parenthesis),
      repeat($._newline),
      field("body", $._expression)
    )),

    repeat_statement: $ => withPrec(PREC.FUNCTION_OR_LOOP, seq(
      "repeat",
      repeat($._newline),
      field("body", $._expression)
    )),

    // Blocks.
    braced_expression: $ => withPrec(PREC.BLOCK, seq(
      field("open", $._open_brace),
      repeat(field("body", choice($._expression, $._semicolon, $._newline))),
      field("close", $._close_brace)
    )),

    // NOTE: R requires exactly 1 expression within parentheses
    // NOTE: See `NOTE ON NEWLINES BETWEEN PARENTHESES` above
    parenthesized_expression: $ => withPrec(PREC.BLOCK, seq(
      field("open", $._open_parenthesis),
      field("body", $._expression),
      field("close", $._close_parenthesis)
    )),

    // Function calls and subsetting.
    call: $ => withPrec(PREC.CALL, seq(
      field("function", $._expression),
      field("arguments", alias($.call_arguments, $.arguments))
    )),

    subset: $ => withPrec(PREC.CALL, seq(
      field("function", $._expression),
      field("arguments", alias($.subset_arguments, $.arguments))
    )),

    subset2: $ => withPrec(PREC.CALL, seq(
      field("function", $._expression),
      field("arguments", alias($.subset2_arguments, $.arguments))
    )),

    // The actual matching rules for arguments in each of the above.
    //
    // Complicated by the fact that we want to support any number
    // of sequential commas, but two arguments must be separated
    // by at least one comma.
    //
    // We accomplish this by using `delimSep1()`, but we make the
    // actual `rule` itself `optional()`.
    //
    // # Allowed
    // fn()
    // fn(,)
    // fn(,,)
    // fn(a)
    // fn(a, b)
    // fn(,,a,,b,,)
    //
    // # Not allowed
    // fn(a b)
    //
    // NOTE: See `NOTE ON NEWLINES BETWEEN PARENTHESES` above
    call_arguments: $ => seq(
      field("open", $._open_parenthesis),
      delimSep1(optional(field("argument", $.argument)), $.comma),
      field("close", $._close_parenthesis)
    ),
    subset_arguments: $ => seq(
      field("open", $._open_bracket),
      delimSep1(optional(field("argument", $.argument)), $.comma),
      field("close", $._close_bracket)
    ),
    subset2_arguments: $ => seq(
      field("open", $._open_bracket2),
      delimSep1(optional(field("argument", $.argument)), $.comma),
      field("close", $._close_bracket2)
    ),

    // An argument; either named or unnamed.
    argument: $ => choice(
      $._argument_named,
      $._argument_unnamed
    ),

    // Note that the value is optional, i.e. both of these are valid:
    // fn(a = 1)
    // fn(a = )
    _argument_named: $ => seq(
      field("name", $._argument_name_string_or_identifier_or_null),
      "=",
      optional($._argument_value)
    ),

    _argument_unnamed: $ => $._argument_value,

    _argument_value: $ => field("value", $._expression),

    // Operators
    // NOTE: See `NOTE ON OPERATORS` above
    // NOTE: Newlines are allowed after all unary operators
    unary_operator: $ => {
      const table = [
        ["?", PREC.HELP],
        ["~", PREC.TILDE],
        ["!", PREC.UNARY_NOT],
        ["+", PREC.UNARY_PLUS_MINUS],
        ["-", PREC.UNARY_PLUS_MINUS]
      ];

      return choice(...table.map(([operator, prec]) => prec.ASSOC(prec.RANK, seq(
        field("operator", operator),
        repeat($._newline),
        field("rhs", $._expression)
      ))))
    },

    // NOTE: Expressions are allowed on either side of the operator
    binary_operator: $ => {
      const table = [
        ["?", PREC.HELP],

        ["~", PREC.TILDE],

        ["<-", PREC.LEFT_ASSIGN],
        ["<<-", PREC.LEFT_ASSIGN],
        [":=", PREC.LEFT_ASSIGN],

        ["->", PREC.RIGHT_ASSIGN],
        ["->>", PREC.RIGHT_ASSIGN],

        ["=", PREC.EQUALS_ASSIGN],

        ["|", PREC.OR],
        ["&", PREC.AND],

        ["||", PREC.OR],
        ["&&", PREC.AND],

        ["<", PREC.COMPARISON],
        ["<=", PREC.COMPARISON],
        [">", PREC.COMPARISON],
        [">=", PREC.COMPARISON],
        ["==", PREC.COMPARISON],
        ["!=", PREC.COMPARISON],

        ["+", PREC.PLUS_MINUS],
        ["-", PREC.PLUS_MINUS],
        ["*", PREC.MULTIPLY_DIVIDE],
        ["/", PREC.MULTIPLY_DIVIDE],
        ["**", PREC.EXPONENTIATE],
        ["^", PREC.EXPONENTIATE],

        // Special infix operator
        // Regex: Between two `%`, anything but another `%`, `\`, or `\n`.
        // Includes primitives `%%` and `%/%`.
        // Alias is used for targeting in `highlights.scm`.
        // TODO: This could probably be fine tuned to disallow more things.
        [alias(/%[^%\\\n]*%/, "special"), PREC.SPECIAL_OR_PIPE],
        ["|>", PREC.SPECIAL_OR_PIPE],

        [":", PREC.COLON]
      ];

      return choice(...table.map(([operator, prec]) => prec.ASSOC(prec.RANK, seq(
        field("lhs", $._expression),
        field("operator", operator),
        repeat($._newline),
        field("rhs", $._expression)
      ))))
    },

    // NOTE: Expression on LHS, string/identifier on RHS
    extract_operator: $ => {
      const table = [
        ["$", PREC.EXTRACT],
        ["@", PREC.EXTRACT]
      ];

      return choice(...table.map(([operator, prec]) => prec.ASSOC(prec.RANK, seq(
        field("lhs", $._expression),
        field("operator", operator),
        repeat($._newline),
        optional(field("rhs", $._string_or_identifier))
      ))))
    },

    // NOTE: No newlines are allowed. String/identifier on both LHS and RHS.
    namespace_operator: $ => {
      const table = [
        ["::", PREC.NAMESPACE],
        [":::", PREC.NAMESPACE]
      ];

      return choice(...table.map(([operator, prec]) => prec.ASSOC(prec.RANK, seq(
        field("lhs", $._string_or_identifier),
        field("operator", operator),
        optional(field("rhs", $._string_or_identifier))
      ))))
    },

    // Numeric literals.
    integer: $ => seq($._float_literal, "L"),
    complex: $ => seq($._float_literal, "i"),
    float: $ => $._float_literal,

    // NOTE: See `?NumericConstants` for precise details
    //
    // For hexadecimal:
    // - '0'
    // - 'x' or 'X'
    // - Non-empty sequence of '[0-9a-fA-F.]' restricted to at most 1 '.'
    //   - LHS `([0-9a-fA-F]+(\.[0-9a-fA-F]*)?)` handles 0x1, 0x1.2
    //   - RHS `(\.[0-9a-fA-F]*)` handles 0x.1, and 0x. (surprisingly R allows this)
    // - Optional binary exponent of:
    //   - 'p' or 'P'
    //   - Optional '+' or '-'
    //   - Non-empty sequence of '[0-9]'
    _hex_literal: $ => /0[xX](([0-9a-fA-F]+(\.[0-9a-fA-F]*)?)|(\.[0-9a-fA-F]*))([pP][+-]?[0-9]+)?/,
    _number_literal: $ => /(?:(?:\d+(?:\.\d*)?)|(?:\.\d+))(?:[eE][+-]?\d*)?/,
    _float_literal: $ => choice($._hex_literal, $._number_literal),

    // Strings.
    //
    // NOTE: Keep the field and child node structure the same across all 3 variants!
    string: $ => choice(
      $._raw_string,
      $._single_quoted_string,
      $._double_quoted_string
    ),

    _raw_string: $ => seq(
      field("open", alias($._raw_string_open, $.string_open)),
      optional(field("content", alias($._raw_string_content, $.string_content))),
      field("close", alias($._raw_string_close, $.string_close))
    ),

    _single_quoted_string: $ => seq(
      field("open", alias('\'', $.string_open)),
      optional(field("content", alias($._single_quoted_string_content, $.string_content))),
      field("close", alias('\'', $.string_close))
    ),

    _double_quoted_string: $ => seq(
      field("open", alias('"', $.string_open)),
      optional(field("content", alias($._double_quoted_string_content, $.string_content))),
      field("close", alias('"', $.string_close))
    ),

    // Explanation is:
    // - Between two quote characters, allow either:
    //   - Anything except `'` (or `"`) or `\`
    //   - An escape sequence
    _single_quoted_string_content: $ => repeat1(choice(
      /[^'\\]+/,
      $.escape_sequence
    )),

    _double_quoted_string_content: $ => repeat1(choice(
      /[^"\\]+/,
      $.escape_sequence
    )),

    escape_sequence: $ => token.immediate(seq(
      '\\',
      choice(
        /[^0-9xuU]/,
        /[0-7]{1,3}/,
        /x[0-9a-fA-F]{1,2}/,
        /u[0-9a-fA-F]{1,4}/,
        /u\{[0-9a-fA-F]{1,4}\}/,
        /U[0-9a-fA-F]{1,8}/,
        /U\{[0-9a-fA-F]{1,8}\}/
      )
    )),

    // NOTE: The ordering here matters. Since `identifier` would match both
    // `dots` and `dot_dot_i` and everyone has the same precedence (of 0),
    // we must put `dots` and `dot_dot_i` first in the grammar itself to
    // allow them to match first.
    dots: $ => "...",
    dot_dot_i: $ => /[.][.]\d+/,

    // Identifiers.
    //
    // NOTE: R identifiers can start with a letter or a `.`, as long as the `.` is not
    // followed by a digit, like `.1foo`. This has practical implications, as otherwise
    // `.1i` and `.1L` can be confused as identifiers (#190).
    // https://github.com/wch/r-source/blob/f118c37fe39c57f302341a89da75e25045d5f01e/src/library/base/man/Quotes.Rd#L92-L95
    //
    // NOTE: `_` isn't a valid way to start an R identifier, but we are a little
    // lax here and parse it anyways. One reason is because want to support a lone `_` as
    // the pipe placeholder identifier. It could be included as a separate `"_"` choice,
    // but then `_foo` parses as two identifiers: `_` and `foo`, making it impossible to
    // check that `_foo` is an invalid identifier. It seems simpler to parse `_foo` as a
    // single identifier, and then let downstream consumers do further checks on the
    // validity as needed (#71).
    //
    // NOTE: Due to the linked tree-sitter discussion, if `_unquoted_identifier` and
    // `_quoted_identifier` are their own hidden terminal rules, then we can't detect error
    // recovered `identifier`s as missing with `ts_node_is_missing()`. The workaround used
    // here inlines the regexes, and wraps the `choice()` call in a single terminal
    // `token()` so `identifier` can still be used as the `word` rule.
    // https://github.com/tree-sitter/tree-sitter/issues/3332
    //
    // NOTE: Do not use this directly in the grammar, use `_identifier` instead!
    identifier: $ => {
      const _unquoted_identifier = /[\p{XID_Start}_][\p{XID_Continue}.]*/;
      const _unquoted_identifier_with_leading_dot = /\.(?:[\p{XID_Start}._][\p{XID_Continue}.]*)?/;
      const _quoted_identifier = /`((?:\\(.|\n))|[^`\\])*`/;

      return token(
        choice(
          _unquoted_identifier,
          _unquoted_identifier_with_leading_dot,
          _quoted_identifier
        )
      )
    },

    // NOTE: This is the actual "identifier" used throughout the grammar. R
    // treats `...` and `..i` exactly the same as any other `identifier`
    // throughout the R grammar. In theory, we could collapse `dots` and
    // `dot_dot_i` into `identifier`, but downstream consumers sometimes
    // highlight `dots` and `dot_dot_i` differently than other identifiers. This
    // is supported by the fact that `...` and `..i` are "reserved words" in R,
    // see `?Reserved`. We also considered nesting `dots` and `dot_dot_i` as
    // children under `identifier`, but that is complicated by the fact that we
    // use `identifier` as the `word`, so it needs to be a token (#157, #176).
    _identifier: $ => choice(
      $.dots,
      $.dot_dot_i,
      $.identifier
    ),

    _string_or_identifier: $ => choice(
      $.string,
      $._identifier
    ),

    // NOTE: This is only for use in `_argument_named`
    //
    // This is extremely similar to `_string_or_identifier`,
    // but there are two differences outlined below
    //
    // # Usage of `prec(1, )`
    //
    // Since `_argument_unnamed` can be an arbitrary `_expression` (with
    // precedence 0) which includes `string`, `identifier`, `dots`, `dot_dot_i`,
    // and `null`, there is an ambiguity between:
    // - Starting the `value` of an `_argument_unnamed`
    // - Starting the `name` of an `_argument_named`
    //
    // We set a higher precedence here to try and match `_argument_named` first.
    //
    // # Inclusion of `null`
    //
    // Since the beginning of R, `NULL` has surprisingly been allowed in
    // function argument name position, like `fn(NULL = 1)` and `fn(NULL = )`.
    // This is surprising because numeric constants like `NA` and `Inf` are not
    // allowed here, and `NULL` isn't specially allowed anywhere else you might
    // expect it to be special cased, like `x$NULL` or `function(NULL) {}` (#164).
    _argument_name_string_or_identifier_or_null: $ => prec(1, choice(
      $._string_or_identifier,
      $.null
    )),

    // Keywords.
    // We define keywords as those contained in `?Reserved`, i.e. it must be a reserved
    // word in R's parser to be considered here. If a keyword from `?Reserved` is already
    // mentioned within a wider rule (like "if" and "function"), then it is not included
    // again here. Grammar consumers can choose to highlight more words as required (#109, #189).
    next: $ => "next",
    break: $ => "break",
    true: $ => "TRUE",
    false: $ => "FALSE",
    null: $ => "NULL",
    inf: $ => "Inf",
    nan: $ => "NaN",
    na: $ => choice(
      "NA",
      "NA_integer_",
      "NA_real_",
      "NA_complex_",
      "NA_character_"
    ),

    // A general R expression.
    _expression: $ => choice(
      $.function_definition,

      $.if_statement,
      $.for_statement,
      $.while_statement,
      $.repeat_statement,

      $.braced_expression,
      $.parenthesized_expression,

      $.call,
      $.subset,
      $.subset2,

      $.unary_operator,
      $.binary_operator,
      $.extract_operator,
      $.namespace_operator,

      $.integer,
      $.complex,
      $.float,

      $.string,

      $.identifier,
      $.dots,
      $.dot_dot_i,

      $.next,
      $.break,
      $.true,
      $.false,
      $.null,
      $.inf,
      $.nan,
      $.na
    ),

    // Comments.
    comment: $ => token(withPrec(PREC.COMMENT, seq('#', /[^\r\n]*/))),

    // Commas. We include these in the AST so we can figure out the
    // argument call position. This is necessary given how R tolerates
    // missing arguments in function calls.
    comma: $ => ",",

    // Provide aliasing of some key externals.
    // This gives `highlights.scm` something to target for
    // `@punctuation.bracket` and `@keyword`.
    _else: $ => alias($._external_else, "else"),
    _open_parenthesis: $ => alias($._external_open_parenthesis, "("),
    _close_parenthesis: $ => alias($._external_close_parenthesis, ")"),
    _open_brace: $ => alias($._external_open_brace, "{"),
    _close_brace: $ => alias($._external_close_brace, "}"),
    _open_bracket: $ => alias($._external_open_bracket, "["),
    _close_bracket: $ => alias($._external_close_bracket, "]"),
    _open_bracket2: $ => alias($._external_open_bracket2, "[["),
    _close_bracket2: $ => alias($._external_close_bracket2, "]]")
  }
})

function withPrec(prec, rule) {
  return prec.ASSOC(prec.RANK, rule)
}

// Creates a rule to match one or more of the `rule`s separated by a `delim`
//
// Used in argument list generation
function delimSep1(rule, delim) {
  return seq(
    rule,
    repeat(seq(delim, rule))
  )
}
