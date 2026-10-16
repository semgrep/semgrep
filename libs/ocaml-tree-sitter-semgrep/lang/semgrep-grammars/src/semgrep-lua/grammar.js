/*
 * semgrep-lua
 *
 * Extend Lua with metavariables and expression patterns. Reuse native
 * varargs for ellipses, including in statement and parameter patterns.
 */

const standard_grammar = require('tree-sitter-lua/grammar').default;

module.exports = grammar(standard_grammar, {
    name: 'lua',

    // Bare expressions overlap calls and contextual 'global' declarations.
    conflicts: ($, previous) => [
        ...previous,
        [$.statement, $.expression],
        [$._global_function_declaration, $._contextual_keyword],
        [$._global_implicit_variable_declaration, $._contextual_keyword],
        [$._global_variable_declaration, $._contextual_keyword],
        [$._global_variable_declaration, $._global_implicit_variable_declaration, $._contextual_keyword],
    ],

    rules: {
        // Semgrep patterns may be bare expressions, including metavariables
        // and ellipses, both at the top level and inside statement blocks.
        statement: ($, previous) => choice(previous, prec(-1, $.expression)),

        // A leading ellipsis can match parameters before explicit names.
        _parameter_list: ($, previous) => choice(
            previous,
            seq(
                $.vararg_expression,
                repeat1(seq(',', $.identifier)),
                optional(seq(',', $._vararg_parameter))
            ),
            seq($.vararg_expression, ',', $._vararg_parameter)
        ),

        // Upstream hash_bang_line is /#.*/. Lua skips any first line that
        // starts with '#', which also swallows length patterns. Keep '#!'
        // shebangs and comment-style first lines ('# ignored ...'), but not
        // unary length operands: glued '#foo' / '#$X' / '#"..."', or spaced
        // '# "..."' / '# $X' (tree-sitter regexes cannot use look-around).
        // One token pattern keeps the CST shape as a single terminal.
        hash_bang_line: (_) =>
            /#(?:![^\r\n\u2028\u2029]*|[ \t\f\v]+[^\$"'\[{(\.\r\n\u2028\u2029][^\r\n\u2028\u2029]*)/,

        // Lua treats CR, LF, CRLF and LFCR as a single escaped line break.
        escape_sequence: ($, previous) => choice(
            previous,
            token.immediate(seq('\\', choice('\r', '\r\n', '\n\r')))
        ),

        // LuaJIT accepts non-ASCII bytes in names. Keep ASCII punctuation
        // excluded, except for the leading '$' of a Semgrep metavariable.
        identifier: $ => token(choice(
            /[a-zA-Z_\u0080-\u{10ffff}][a-zA-Z0-9_\u0080-\u{10ffff}]*/u,
            /\$[a-zA-Z_][a-zA-Z0-9_]*/
        ))
    }
});
