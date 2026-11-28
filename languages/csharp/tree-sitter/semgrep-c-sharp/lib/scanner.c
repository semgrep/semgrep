#include "tree_sitter/alloc.h"
#include "tree_sitter/array.h"
#include "tree_sitter/parser.h"

#include <string.h>
#include <wctype.h>

enum TokenType {
    OPT_SEMI,
    INTERPOLATION_REGULAR_START,
    INTERPOLATION_VERBATIM_START,
    INTERPOLATION_RAW_START,
    INTERPOLATION_START_QUOTE,
    INTERPOLATION_END_QUOTE,
    INTERPOLATION_OPEN_BRACE,
    INTERPOLATION_CLOSE_BRACE,
    INTERPOLATION_STRING_CONTENT,
    RAW_STRING_START,
    RAW_STRING_END,
    RAW_STRING_CONTENT,
    LAMBDA_PAREN_OPEN,
    PREPROC_ELSE_IF_START,
};

typedef enum {
    REGULAR = 1 << 0,
    VERBATIM = 1 << 1,
    RAW = 1 << 2,
} StringType;

typedef struct {
    uint8_t dollar_count;
    uint8_t open_brace_count;
    uint8_t quote_count;
    StringType string_type;
} Interpolation;

static inline bool is_regular(Interpolation *interpolation) { return interpolation->string_type & REGULAR; }

static inline bool is_verbatim(Interpolation *interpolation) { return interpolation->string_type & VERBATIM; }

static inline bool is_raw(Interpolation *interpolation) { return interpolation->string_type & RAW; }

typedef struct {
    uint8_t quote_count;
    Array(Interpolation) interpolation_stack;
} Scanner;

static inline void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

static inline void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

// ---- Helpers for LAMBDA_PAREN_OPEN scanning ---------------------------

static inline bool is_id_start(int32_t c) {
    return c == '_' || iswalpha(c);
}

static inline bool is_id_continue(int32_t c) {
    return c == '_' || iswalnum(c);
}

// Skip whitespace, line comments (//...), and block comments (/* ... */).
// Does NOT skip preprocessor directives — simple-lambda parameter lists
// cannot legally contain them.
static void skip_ws_and_comments(TSLexer *lexer) {
    for (;;) {
        int32_t c = lexer->lookahead;
        if (c == ' ' || c == '\t' || c == '\r' || c == '\n') {
            advance(lexer);
        } else if (c == '/') {
            advance(lexer);
            if (lexer->lookahead == '/') {
                while (lexer->lookahead != 0 && lexer->lookahead != '\n') {
                    advance(lexer);
                }
            } else if (lexer->lookahead == '*') {
                advance(lexer);
                int32_t prev = 0;
                while (lexer->lookahead != 0 &&
                       !(prev == '*' && lexer->lookahead == '/')) {
                    prev = lexer->lookahead;
                    advance(lexer);
                }
                if (lexer->lookahead == '/') advance(lexer);
            } else {
                // a stray '/' isn't valid in a param list; bail out by
                // making the caller see something unexpected
                return;
            }
        } else {
            return;
        }
    }
}

// Consume an identifier into a fixed-size buffer. Returns the actual
// length consumed (may exceed `max`, in which case `buf` is only filled
// up to `max` bytes — useful for "too long to be a keyword" detection).
static size_t consume_identifier_into(TSLexer *lexer, char *buf, size_t max) {
    size_t n = 0;
    while (is_id_continue(lexer->lookahead)) {
        if (n < max) {
            buf[n] = (char)lexer->lookahead;
        }
        n++;
        advance(lexer);
    }
    return n;
}

// True iff the first `n` bytes of `buf` equal the C string `kw`.
static bool buf_equals(const char *buf, size_t n, const char *kw) {
    size_t klen = strlen(kw);
    return n == klen && memcmp(buf, kw, klen) == 0;
}

void *tree_sitter_c_sharp_external_scanner_create() {
    Scanner *scanner = ts_calloc(1, sizeof(Scanner));
    array_init(&scanner->interpolation_stack);
    return scanner;
}

void tree_sitter_c_sharp_external_scanner_destroy(void *payload) {
    Scanner *scanner = (Scanner *)payload;
    array_delete(&scanner->interpolation_stack);
    ts_free(scanner);
}

unsigned tree_sitter_c_sharp_external_scanner_serialize(void *payload, char *buffer) {
    Scanner *scanner = (Scanner *)payload;

    if (scanner->interpolation_stack.size * 4 + 2 > TREE_SITTER_SERIALIZATION_BUFFER_SIZE) {
        return 0;
    }

    unsigned size = 0;

    buffer[size++] = (char)scanner->quote_count;
    buffer[size++] = (char)scanner->interpolation_stack.size;

    for (unsigned i = 0; i < scanner->interpolation_stack.size; i++) {
        Interpolation interpolation = scanner->interpolation_stack.contents[i];
        buffer[size++] = (char)interpolation.dollar_count;
        buffer[size++] = (char)interpolation.open_brace_count;
        buffer[size++] = (char)interpolation.quote_count;
        buffer[size++] = (char)interpolation.string_type;
    }

    return size;
}

void tree_sitter_c_sharp_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
    Scanner *scanner = (Scanner *)payload;

    scanner->quote_count = 0;
    array_clear(&scanner->interpolation_stack);
    unsigned size = 0;

    if (length > 0) {
        scanner->quote_count = (unsigned char)buffer[size++];
        scanner->interpolation_stack.size = (unsigned char)buffer[size++];
        array_reserve(&scanner->interpolation_stack, scanner->interpolation_stack.size);

        for (unsigned i = 0; i < scanner->interpolation_stack.size; i++) {
            Interpolation interpolation = {0};
            interpolation.dollar_count = buffer[size++];
            interpolation.open_brace_count = buffer[size++];
            interpolation.quote_count = buffer[size++];
            interpolation.string_type = (unsigned char)buffer[size++];
            scanner->interpolation_stack.contents[i] = interpolation;
        }
    }

    assert(size == length);
}

// Outcome of `scan_lambda_paren_open` — distinguishes "didn't start"
// (lexer untouched, wrapper can fall through to other token handlers)
// from "started and failed" (lexer cursor advanced past characters
// the wrapper must not let other handlers consume — they would emit
// tokens with wrong spans, e.g. an `INTERPOLATION_REGULAR_START`
// span covering `(false, $` in `return (false, $"...");`).
typedef enum {
    LAMBDA_SCAN_NO_PAREN,         // not a '(', no advance() done
    LAMBDA_SCAN_FAILED_AFTER_PAREN, // '(' was consumed, dirty state
    LAMBDA_SCAN_SUCCESS,
} LambdaScanResult;

// Try to recognize a C# 14 simple-lambda parameter list at the current
// position. On SUCCESS, `lexer->result_symbol` is LAMBDA_PAREN_OPEN
// and `mark_end` is set on the opening '('. On NO_PAREN, the lexer is
// untouched (only `skip(lexer)` over leading whitespace, which doesn't
// participate in token boundaries). On FAILED_AFTER_PAREN, the lexer
// has advanced past at least the opening '(' and the caller must
// surface this as a false return from the external_scanner_scan
// function so tree-sitter rewinds.
//
// Grammar of the pattern (after the opening '('):
//
//   element (',' element)* ')' '=>'
//
//   element  := modifier+ identifier
//             | identifier
//
//   modifier ∈ { scoped, ref, out, in, readonly }
//
// At least one element must carry a modifier; otherwise we'd over-fire
// on plain `(x, y) => ...` which is already handled by the existing
// `_lambda_parameters` choice.
//
// Implementation note: identifier-tokens are consumed *whole* into a
// small buffer before being classified as modifier-or-name. A
// character-by-character probe against each candidate keyword would be
// unworkable because `ref` is a prefix of `readonly` (they share `re`)
// and TSLexer has no rewind primitive: any probe order leaves one of
// the two keywords unreachable. Buffering the whole identifier
// sidesteps the prefix conflict entirely.
static LambdaScanResult scan_lambda_paren_open(TSLexer *lexer) {
    // External scanners run before whitespace `extras` are skipped, so
    // we need to skip leading whitespace/comments ourselves before
    // checking for the opening '('. `skip(lexer)` consumes the char as
    // an extra (not part of any token), so even on NO_PAREN return the
    // wrapper's fall-through into other handlers is safe — those
    // handlers do their own whitespace skipping.
    while (iswspace(lexer->lookahead)) {
        skip(lexer);
    }
    if (lexer->lookahead != '(') return LAMBDA_SCAN_NO_PAREN;
    advance(lexer);
    lexer->mark_end(lexer);

    // From this point on, the lexer cursor has moved past '('. Any
    // return must be FAILED_AFTER_PAREN unless we reach SUCCESS.
    #define BAIL return LAMBDA_SCAN_FAILED_AFTER_PAREN

    // We require at least one "hard" modifier (ref/out/in/readonly) across
    // the whole list before committing. `scoped` alone is not a valid C#
    // parameter modifier — it must always combine with a hard modifier —
    // and `scoped` is also a legal type name, so accepting `(scoped x) =>`
    // as a modifier+name pair would collide with the existing
    // `parameter_list` path on input that semantically means
    // type+identifier.
    bool saw_hard_modifier = false;
    bool expecting_element = true;

    for (;;) {
        skip_ws_and_comments(lexer);
        int32_t c = lexer->lookahead;

        if (c == 0) BAIL;   // EOF mid-list

        if (c == ')') {
            advance(lexer);
            skip_ws_and_comments(lexer);
            if (!saw_hard_modifier) BAIL;
            if (lexer->lookahead != '=') BAIL;
            advance(lexer);
            if (lexer->lookahead != '>') BAIL;
            lexer->result_symbol = LAMBDA_PAREN_OPEN;
            return LAMBDA_SCAN_SUCCESS;
        }

        if (!expecting_element) {
            if (c != ',') BAIL;
            advance(lexer);
            expecting_element = true;
            continue;
        }

        // Consume identifier-tokens in this element. Each one is either a
        // parameter modifier (`scoped`/`ref`/`out`/`in`/`readonly`) — in which
        // case we continue looking for more — or the parameter name, which
        // ends the element. Consuming the whole token before classifying
        // avoids any need to rewind the lexer on a prefix-conflict (e.g. ref
        // vs readonly).
        bool consumed_name = false;
        while (!consumed_name) {
            skip_ws_and_comments(lexer);
            if (!is_id_start(lexer->lookahead)) BAIL;

            char buf[9];  // "readonly" is 8 chars
            size_t n = consume_identifier_into(lexer, buf, sizeof(buf));

            bool is_hard_modifier = (n <= 8) && (
                buf_equals(buf, n, "ref") ||
                buf_equals(buf, n, "out") ||
                buf_equals(buf, n, "in") ||
                buf_equals(buf, n, "readonly")
            );
            bool is_soft_modifier = (n == 6) && buf_equals(buf, n, "scoped");

            if (is_hard_modifier) {
                saw_hard_modifier = true;
            } else if (is_soft_modifier) {
                // `scoped` is a modifier only when followed by a hard
                // modifier; keep scanning. If the element turns out to
                // end with `scoped <identifier>` and no hard modifier
                // appeared, the final `saw_hard_modifier` check fails
                // and we fall back to the regular parameter_list path.
            } else {
                consumed_name = true;
            }
        }
        expecting_element = false;
    }
}
#undef BAIL


// `#if <condition>` whose first content, after the condition line and any blank
// lines or comments, is the keyword `else`. Only `#if` is consumed (mark_end);
// the rest is read ahead and handed back so the normal tokens lex it. Lets an
// `else` branch live inside a directive without a parser conflict on `#if`.
static bool scan_preproc_else_if_start(TSLexer *lexer) {
    while (iswspace(lexer->lookahead)) {
        skip(lexer);
    }
    if (lexer->lookahead != '#') return false;
    advance(lexer);
    while (lexer->lookahead == ' ' || lexer->lookahead == '\t') advance(lexer);
    if (lexer->lookahead != 'i') return false;
    advance(lexer);
    if (lexer->lookahead != 'f') return false;
    advance(lexer);
    if (is_id_continue(lexer->lookahead)) return false;
    lexer->mark_end(lexer);
    // rest of the condition line
    while (lexer->lookahead != 0 && lexer->lookahead != '\n') advance(lexer);
    skip_ws_and_comments(lexer);
    const char *kw = "else";
    for (int i = 0; kw[i]; i++) {
        if (lexer->lookahead != (int32_t)kw[i]) return false;
        advance(lexer);
    }
    if (is_id_continue(lexer->lookahead)) return false;
    lexer->result_symbol = PREPROC_ELSE_IF_START;
    return true;
}

bool tree_sitter_c_sharp_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    Scanner *scanner = (Scanner *)payload;

    uint8_t brace_advanced = 0;
    uint8_t quote_count = 0;
    bool did_advance = false;

    // Lambda-paren scanning advances forward speculatively past the
    // opening `(`. If it consumes input and then bails, the lexer
    // cursor is mispositioned and must not be reused by other token
    // handlers below — they would emit tokens with spans starting at
    // the original scan position but ending at the dirty cursor (e.g.
    // an INTERPOLATION_REGULAR_START swallowing `(false, $` in
    // `return (false, $"...");`). When the scanner consumed `(` but
    // didn't confirm a lambda, return false here so tree-sitter
    // rewinds and lets the built-in `(` tokenizer match.
    if (valid_symbols[LAMBDA_PAREN_OPEN]) {
        switch (scan_lambda_paren_open(lexer)) {
            case LAMBDA_SCAN_SUCCESS:
                return true;
            case LAMBDA_SCAN_FAILED_AFTER_PAREN:
                return false;
            case LAMBDA_SCAN_NO_PAREN:
                break;  // lexer untouched; fall through
        }
    }

    if (valid_symbols[PREPROC_ELSE_IF_START]) {
        if (scan_preproc_else_if_start(lexer)) return true;
        return false;
    }

    // error recovery, gives better trees this way
    if (valid_symbols[OPT_SEMI] && valid_symbols[INTERPOLATION_REGULAR_START]) {
        return false;
    }

    if (valid_symbols[OPT_SEMI]) {
        lexer->result_symbol = OPT_SEMI;
        if (lexer->lookahead == ';') {
            advance(lexer);
        }
        return true;
    }

    if (valid_symbols[RAW_STRING_START]) {
        while (iswspace(lexer->lookahead)) {
            skip(lexer);
        }

        if (lexer->lookahead == '"') {
            while (lexer->lookahead == '"') {
                advance(lexer);
                quote_count++;
            }

            if (quote_count >= 3) {
                lexer->result_symbol = RAW_STRING_START;
                scanner->quote_count = quote_count;
                return true;
            }
        }
    }

    if (valid_symbols[RAW_STRING_END] && lexer->lookahead == '"') {
        while (lexer->lookahead == '"') {
            advance(lexer);
            quote_count++;
        }

        if (quote_count == scanner->quote_count) {
            lexer->result_symbol = RAW_STRING_END;
            scanner->quote_count = 0;
            return true;
        }

        did_advance = quote_count > 0;
    }

    if (valid_symbols[RAW_STRING_CONTENT]) {
        while (lexer->lookahead) {
            if (lexer->lookahead == '"') {
                lexer->mark_end(lexer);
                quote_count = 0;

                while (lexer->lookahead == '"') {
                    advance(lexer);
                    quote_count++;
                }

                if (quote_count == scanner->quote_count) {
                    lexer->result_symbol = RAW_STRING_CONTENT;
                    return true;
                }
            }
            advance(lexer);
            did_advance = true;
        }
        lexer->mark_end(lexer);
        lexer->result_symbol = RAW_STRING_CONTENT;
        return true;
    }

    if (valid_symbols[INTERPOLATION_REGULAR_START] || valid_symbols[INTERPOLATION_VERBATIM_START] ||
        valid_symbols[INTERPOLATION_RAW_START]) {
        while (iswspace(lexer->lookahead)) {
            skip(lexer);
        }

        uint8_t dollar_advanced = 0;

        bool is_verbatim = false;

        if (lexer->lookahead == '@') {
            is_verbatim = true;
            advance(lexer);
        }

        while (lexer->lookahead == '$' && quote_count == 0) {
            advance(lexer);
            dollar_advanced++;
        }

        if (dollar_advanced > 0 && (lexer->lookahead == '"' || lexer->lookahead == '@')) {
            lexer->result_symbol = INTERPOLATION_REGULAR_START;
            Interpolation interpolation = {
                .dollar_count = dollar_advanced,
                .open_brace_count = 0,
                .quote_count = 0,
                .string_type = 0,
            };

            if (is_verbatim || lexer->lookahead == '@') {
                if (lexer->lookahead == '@') {
                    advance(lexer);
                    is_verbatim = true;
                }
                lexer->result_symbol = INTERPOLATION_VERBATIM_START;
                interpolation.string_type = VERBATIM;
            }

            lexer->mark_end(lexer);
            advance(lexer);

            if (lexer->lookahead == '"' && !is_verbatim) {
                advance(lexer);
                if (lexer->lookahead == '"') {
                    lexer->result_symbol = INTERPOLATION_RAW_START;
                    interpolation.string_type |= RAW;
                    array_push(&scanner->interpolation_stack, interpolation);
                }
                // If we find 1 or 3 quotes, we push an interpolation.
                // If there's only two quotes, that's just an empty string
            } else {
                interpolation.string_type |= REGULAR;
                array_push(&scanner->interpolation_stack, interpolation);
            }

            return true;
        }
    }

    if (valid_symbols[INTERPOLATION_START_QUOTE] && scanner->interpolation_stack.size > 0) {
        Interpolation *current_interpolation = array_back(&scanner->interpolation_stack);

        if (is_verbatim(current_interpolation) || is_regular(current_interpolation)) {
            if (lexer->lookahead == '"') {
                advance(lexer);
                current_interpolation->quote_count++;
            }
        } else {
            while (lexer->lookahead == '"') {
                advance(lexer);
                current_interpolation->quote_count++;
            }
        }

        lexer->result_symbol = INTERPOLATION_START_QUOTE;
        return current_interpolation->quote_count > 0;
    }

    if (valid_symbols[INTERPOLATION_END_QUOTE] && scanner->interpolation_stack.size > 0) {
        Interpolation *current_interpolation = array_back(&scanner->interpolation_stack);

        if (is_raw(current_interpolation)) {
            while (lexer->lookahead == '"') {
                advance(lexer);
                quote_count++;
            }

            if (quote_count == current_interpolation->quote_count) {
                lexer->result_symbol = INTERPOLATION_END_QUOTE;
                array_pop(&scanner->interpolation_stack);
                return true;
            }

            did_advance = quote_count > 0;
        } else if (lexer->lookahead == '"') {
            // A regular or verbatim string ends at ONE quote. Counting the whole run of quotes
            // (as a raw string must) misread a verbatim `""` escape that follows a hole or the
            // start quote: in `$@"a ""{t}"""` the `"""` after `{t}` counted three, matched
            // nothing, and the content then ran past the real end of the string.
            advance(lexer);
            if (!is_verbatim(current_interpolation) || lexer->lookahead != '"') {
                lexer->result_symbol = INTERPOLATION_END_QUOTE;
                array_pop(&scanner->interpolation_stack);
                return true;
            }
            // `""` in a verbatim string is an escaped quote: both quotes are string content.
            advance(lexer);
            did_advance = true;
        }
    }

    // A brace token never starts after quotes taken above: those quotes are string content, and a
    // brace token here would span them (`$@"{a}""{b}"` read `""{` as the hole's brace).
    if (valid_symbols[INTERPOLATION_OPEN_BRACE] && scanner->interpolation_stack.size > 0 && !did_advance) {
        Interpolation *current_interpolation = array_back(&scanner->interpolation_stack);

        while (lexer->lookahead == '{' && brace_advanced < current_interpolation->dollar_count) {
            advance(lexer);
            brace_advanced++;
        }

        if (brace_advanced > 0 && brace_advanced == current_interpolation->dollar_count &&
            (brace_advanced == 0 || lexer->lookahead != '{')) {
            current_interpolation->open_brace_count = brace_advanced;
            lexer->result_symbol = INTERPOLATION_OPEN_BRACE;
            return true;
        }
    }

    if (valid_symbols[INTERPOLATION_CLOSE_BRACE] && scanner->interpolation_stack.size > 0 && !did_advance) {
        uint8_t brace_advanced = 0;
        Interpolation *current_interpolation = array_back(&scanner->interpolation_stack);

        while (iswspace(lexer->lookahead)) {
            advance(lexer);
        }

        while (lexer->lookahead == '}') {
            advance(lexer);
            brace_advanced++;

            if (brace_advanced == current_interpolation->open_brace_count) {
                current_interpolation->open_brace_count = 0;
                lexer->result_symbol = INTERPOLATION_CLOSE_BRACE;
                return true;
            }
        }

        return false;
    }

    if (valid_symbols[INTERPOLATION_STRING_CONTENT] && scanner->interpolation_stack.size > 0) {
        lexer->result_symbol = INTERPOLATION_STRING_CONTENT;
        Interpolation *current_interpolation = array_back(&scanner->interpolation_stack);

        while (lexer->lookahead) {
            // top-down approach, first see if it's raw
            if (is_raw(current_interpolation)) {
                if (lexer->lookahead == '"') {
                    lexer->mark_end(lexer);
                    advance(lexer);
                    if (lexer->lookahead == '"') {
                        advance(lexer);
                        uint8_t quote_advanced = 2;
                        while (lexer->lookahead == '"') {
                            quote_advanced++;
                            advance(lexer);
                        }
                        if (quote_advanced == current_interpolation->quote_count) {
                            return did_advance;
                        }
                    }
                }

                if (lexer->lookahead == '{') {
                    lexer->mark_end(lexer);

                    while (lexer->lookahead == '{' && brace_advanced < current_interpolation->open_brace_count) {
                        advance(lexer);
                        brace_advanced++;
                    }

                    if (brace_advanced == current_interpolation->open_brace_count &&
                        (brace_advanced == 0 || lexer->lookahead != '{')) {
                        return did_advance;
                    }
                }
            }

            // then verbatim, since it could be verbatim + raw, but run the raw branch first
            else if (is_verbatim(current_interpolation)) {
                if (lexer->lookahead == '"') {
                    lexer->mark_end(lexer);
                    advance(lexer);
                    if (lexer->lookahead == '"') {
                        advance(lexer);
                        continue;
                    }
                    return did_advance;
                }

                if (lexer->lookahead == '{') {
                    lexer->mark_end(lexer);

                    while (lexer->lookahead == '{' && brace_advanced < current_interpolation->open_brace_count) {
                        advance(lexer);
                        brace_advanced++;
                    }

                    if (brace_advanced == current_interpolation->open_brace_count &&
                        (brace_advanced == 0 || lexer->lookahead != '{')) {
                        return did_advance;
                    }
                    // `{{` is an escaped brace, and both braces are content. Take the second one
                    // here and start counting again, so a `{` straight after the escape can still
                    // open a hole: in `$"{{{x}}}"` the third `{` starts `{x}`. Without the reset
                    // `brace_advanced` stays at the limit, the third `{` is read as content too,
                    // and the hole's expression becomes string text.
                    if (lexer->lookahead == '{') {
                        advance(lexer);
                        brace_advanced = 0;
                        did_advance = true;
                        continue;
                    }
                }
            }

            // finally regular
            else if (is_regular(current_interpolation)) {
                if (lexer->lookahead == '\\' || lexer->lookahead == '\n' || lexer->lookahead == '"') {
                    lexer->mark_end(lexer);
                    return did_advance;
                }

                if (lexer->lookahead == '{') {
                    lexer->mark_end(lexer);

                    while (lexer->lookahead == '{' && brace_advanced < current_interpolation->open_brace_count) {
                        advance(lexer);
                        brace_advanced++;
                    }

                    if (brace_advanced == current_interpolation->open_brace_count &&
                        (brace_advanced == 0 || lexer->lookahead != '{')) { // if we're in a brace we're not allowed to
                                                                            // collect more than the open_brace_count
                        return did_advance;
                    }
                    // An escaped `{{`: see the verbatim branch above.
                    if (lexer->lookahead == '{') {
                        advance(lexer);
                        brace_advanced = 0;
                        did_advance = true;
                        continue;
                    }
                }
            }

            if (lexer->lookahead != '{') {
                brace_advanced = 0;
            }
            advance(lexer);
            did_advance = true;
        }

        lexer->mark_end(lexer);
        return did_advance;
    }

    return false;
}
