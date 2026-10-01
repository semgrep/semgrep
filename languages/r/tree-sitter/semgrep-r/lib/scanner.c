#include <stdint.h> // uint8_t
#include <string.h> // memcpy()
#include <wctype.h> // iswspace()

#include "tree_sitter/alloc.h"
#include "tree_sitter/parser.h"

// Uncomment if debugging for extra output during parsing. Note that we can't
// use `vprintf()` for print debugging in WASM or on CRAN for the R package!
// #define TREE_SITTER_R_DEBUG

#ifdef TREE_SITTER_R_DEBUG
#include <stdarg.h> // va_list, va_start(), va_end()
#include <stdio.h>  // vprintf()

static inline void debug_print(const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
}
#else
#define debug_print(...)
#endif

enum TokenType {
  START,
  NEWLINE,
  SEMICOLON,
  RAW_STRING_OPEN,
  RAW_STRING_CONTENT,
  RAW_STRING_CLOSE,
  ELSE,
  OPEN_PAREN,
  CLOSE_PAREN,
  OPEN_BRACE,
  CLOSE_BRACE,
  OPEN_BRACKET,
  CLOSE_BRACKET,
  OPEN_BRACKET2,
  CLOSE_BRACKET2,
  ERROR_SENTINEL
};

// ---------------------------------------------------------------------------------------

typedef struct {
  char* pointer;
  unsigned length;
} SerializeBuffer;

typedef struct {
  const char* pointer;
  unsigned length;
} DeserializeBuffer;

// ---------------------------------------------------------------------------------------

// i.e. `}---"`
//       ^ ^ ^
//       | | |- closing_quote = "
//       | |- hyphen_count = 3
//       |- closing_bracket = }
typedef struct {
  char closing_bracket;
  uint8_t hyphen_count;
  char closing_quote;
} RawStringState;

#define RAW_STRING_CLOSING_BRACKET_SIZE sizeof(char)
#define RAW_STRING_HYPHEN_COUNT_SIZE sizeof(uint8_t)
#define RAW_STRING_CLOSING_QUOTE_SIZE sizeof(char)

static void raw_string_state_set(
  RawStringState* raw_string_state,
  char closing_bracket,
  uint8_t hyphen_count,
  char closing_quote
) {
  raw_string_state->closing_bracket = closing_bracket;
  raw_string_state->hyphen_count = hyphen_count;
  raw_string_state->closing_quote = closing_quote;
}

static void raw_string_state_reset(RawStringState* raw_string_state) {
  raw_string_state_set(raw_string_state, '\0', 0, '\0');
}

static bool raw_string_state_init(RawStringState* raw_string_state) {
  raw_string_state_reset(raw_string_state);
  return true;
}

static void raw_string_state_destroy(RawStringState* raw_string_state) {
  // Nothing to free, recognize argument for Windows checks
  (void) raw_string_state;
}

// Serialize fields individually to skip the struct's padding bytes, which
// `sizeof(RawStringState)` would otherwise include
static void raw_string_state_serialize(RawStringState* raw_string_state, SerializeBuffer* buffer) {
  memcpy(buffer->pointer, &raw_string_state->closing_bracket, RAW_STRING_CLOSING_BRACKET_SIZE);
  buffer->pointer += RAW_STRING_CLOSING_BRACKET_SIZE;
  buffer->length += RAW_STRING_CLOSING_BRACKET_SIZE;

  memcpy(buffer->pointer, &raw_string_state->hyphen_count, RAW_STRING_HYPHEN_COUNT_SIZE);
  buffer->pointer += RAW_STRING_HYPHEN_COUNT_SIZE;
  buffer->length += RAW_STRING_HYPHEN_COUNT_SIZE;

  memcpy(buffer->pointer, &raw_string_state->closing_quote, RAW_STRING_CLOSING_QUOTE_SIZE);
  buffer->pointer += RAW_STRING_CLOSING_QUOTE_SIZE;
  buffer->length += RAW_STRING_CLOSING_QUOTE_SIZE;
}

static bool raw_string_state_deserialize(RawStringState* raw_string_state, DeserializeBuffer* buffer) {
  if (buffer->length <
      (RAW_STRING_CLOSING_BRACKET_SIZE + RAW_STRING_HYPHEN_COUNT_SIZE + RAW_STRING_CLOSING_QUOTE_SIZE)) {
    debug_print(
      "`raw_string_state_deserialize()` failed. Can't deserialize raw string state. Buffer length %u.\n",
      buffer->length
    );
    return false;
  }

  memcpy(&raw_string_state->closing_bracket, buffer->pointer, RAW_STRING_CLOSING_BRACKET_SIZE);
  buffer->pointer += RAW_STRING_CLOSING_BRACKET_SIZE;
  buffer->length -= RAW_STRING_CLOSING_BRACKET_SIZE;

  memcpy(&raw_string_state->hyphen_count, buffer->pointer, RAW_STRING_HYPHEN_COUNT_SIZE);
  buffer->pointer += RAW_STRING_HYPHEN_COUNT_SIZE;
  buffer->length -= RAW_STRING_HYPHEN_COUNT_SIZE;

  memcpy(&raw_string_state->closing_quote, buffer->pointer, RAW_STRING_CLOSING_QUOTE_SIZE);
  buffer->pointer += RAW_STRING_CLOSING_QUOTE_SIZE;
  buffer->length -= RAW_STRING_CLOSING_QUOTE_SIZE;

  return true;
}

// ---------------------------------------------------------------------------------------
// Stack structure inspired from tree-sitter-julia

// We purposefully use `char` as the element storage type. An `enum` would be
// simpler, but it doesn't have `char` element storage so you can't fit as many
// of them inside `TREE_SITTER_SERIALIZATION_BUFFER_SIZE`. We expect
// `SCOPE_SIZE` to be 1 everywhere, but we are careful just in case.
typedef char Scope;

const Scope SCOPE_TOP_LEVEL = 0;
const Scope SCOPE_BRACE = 1;
const Scope SCOPE_PAREN = 2;
const Scope SCOPE_BRACKET = 3;
const Scope SCOPE_BRACKET2 = 4;

// A stack data structure for tracking the current `Scope`
//
// `SCOPE_TOP_LEVEL` is never actually pushed onto the stack. It is returned
// from `scopes_peek()` as a base case when `count = 0`. Note that in
// `scopes_pop()` we still check for `count > 0` before peeking to retain the
// invariant that we can't pop without something on the stack.
//
// This actually makes serialization/deserialization very simple. Even if we pushed an
// initial `SCOPE_TOP_LEVEL` in the create hook, there is no guarantee that that will get
// serialized (so the length of the stack won't be remembered) because the serialize hook
// only runs when we accept a token from our external scanner. That would complicate the
// deserialize hook by forcing us to differentiate between the cases of:
// 1) A deserialization call restoring state from a previous serialization (count > 0).
// 2) A deserialization call when there wasn't a previous serialization (count = 0),
//    where we'd have to repush an initial `SCOPE_TOP_LEVEL`.
typedef struct {
  Scope* data;
  unsigned count;
} Scopes;

#define SCOPE_SIZE sizeof(Scope)
#define COUNT_SIZE sizeof(unsigned)

// `MAX_SCOPES_COUNT` is the maximum number of nested scopes that are allowed.
// It is computed as the remaining `buffer` space allowed by
// `TREE_SITTER_SERIALIZATION_BUFFER_SIZE` after removing all other serialized
// elements, so keep this up to date with anything else that gets serialized!
// Ideally we leave as much space for this as possible.
#define MAX_SCOPES_COUNT                                                                                               \
  ((TREE_SITTER_SERIALIZATION_BUFFER_SIZE - RAW_STRING_CLOSING_BRACKET_SIZE - RAW_STRING_HYPHEN_COUNT_SIZE -           \
    RAW_STRING_CLOSING_QUOTE_SIZE - COUNT_SIZE) /                                                                      \
   SCOPE_SIZE)

static void scopes_reset(Scopes* scopes) {
  scopes->count = 0;
}

static bool scopes_init(Scopes* scopes) {
  Scope* data = ts_malloc(MAX_SCOPES_COUNT * SCOPE_SIZE);
  if (data == NULL) {
    debug_print("`scopes_init()` failed. Can't allocate scope array.");
    return false;
  }
  scopes->data = data;

  scopes_reset(scopes);

  return true;
}

static void scopes_destroy(Scopes* scopes) {
  ts_free(scopes->data);
}

static bool scopes_push(Scopes* scopes, Scope scope) {
  if (scopes->count >= MAX_SCOPES_COUNT) {
    // Return `false` so `scan()` can return `false` and refuse to handle the token.
    // Should only ever happen in pathological cases (i.e. >1000 unmatched opening braces).
    debug_print("`scopes_push()` failed. `scopes` is at maximum capacity.\n");
    return false;
  }

  scopes->data[scopes->count] = scope;
  scopes->count++;

  return true;
}

static Scope scopes_peek(Scopes* scopes) {
  if (scopes->count == 0) {
    return SCOPE_TOP_LEVEL;
  } else {
    return scopes->data[scopes->count - 1];
  }
}

static bool scopes_pop(Scopes* scopes, Scope expected) {
  if (scopes->count == 0) {
    // Return `false` so `scan()` can return `false` and refuse to handle the token
    debug_print("`scopes_pop()` failed. `scopes` is empty, nothing to pop.\n");
    return false;
  }

  Scope scope = scopes_peek(scopes);
  scopes->count--;

  if (scope != expected) {
    // Return `false` so `scan()` can return `false` and refuse to handle the token
    debug_print("`scopes_pop()` failed. Actual scope '%c' does not match expected scope '%c'.\n", scope, expected);
    return false;
  }

  return true;
}

static void scopes_serialize(Scopes* scopes, SerializeBuffer* buffer) {
  // Serialize `count` so we know how many `Scope`s to deserialize
  memcpy(buffer->pointer, &scopes->count, COUNT_SIZE);
  buffer->pointer += COUNT_SIZE;
  buffer->length += COUNT_SIZE;

  // Serialize `Scope` array
  if (scopes->count > 0) {
    const unsigned scopes_size = scopes->count * SCOPE_SIZE;
    memcpy(buffer->pointer, scopes->data, scopes_size);
    buffer->pointer += scopes_size;
    buffer->length += scopes_size;
  }
}

static bool scopes_deserialize(Scopes* scopes, DeserializeBuffer* buffer) {
  if (buffer->length < COUNT_SIZE) {
    debug_print("`scopes_deserialize()` failed. Can't deserialize `count`. Buffer length %u.\n", buffer->length);
    return false;
  }

  // Deserialize `count`
  memcpy(&scopes->count, buffer->pointer, COUNT_SIZE);
  buffer->pointer += COUNT_SIZE;
  buffer->length -= COUNT_SIZE;

  // Deserialize `Scope` array
  if (scopes->count > 0) {
    const unsigned scopes_size = scopes->count * SCOPE_SIZE;

    if (buffer->length < scopes_size) {
      debug_print("`scopes_deserialize()` failed. Can't deserialize scopes. Buffer length %u.\n", buffer->length);
      return false;
    }

    memcpy(scopes->data, buffer->pointer, scopes_size);
    buffer->pointer += scopes_size;
    buffer->length -= scopes_size;
  }

  return true;
}

// ---------------------------------------------------------------------------------------

typedef struct {
  RawStringState raw_string_state;
  Scopes scopes;
} Payload;

const unsigned PAYLOAD_SIZE = sizeof(Payload);

static void payload_reset(Payload* payload) {
  raw_string_state_reset(&payload->raw_string_state);
  scopes_reset(&payload->scopes);
}

static Payload* payload_new(void) {
  Payload* payload = ts_malloc(PAYLOAD_SIZE);
  if (payload == NULL) {
    debug_print("`payload_new()` failed. Can't allocate payload.");
    return NULL;
  }

  if (!raw_string_state_init(&payload->raw_string_state)) {
    debug_print("`payload_new()` failed. Can't initialize raw string state.");
    ts_free(payload);
    return NULL;
  }

  if (!scopes_init(&payload->scopes)) {
    debug_print("`payload_new()` failed. Can't initialize scopes.");
    raw_string_state_destroy(&payload->raw_string_state);
    ts_free(payload);
    return NULL;
  }

  return payload;
}

static void payload_destroy(Payload* payload) {
  raw_string_state_destroy(&payload->raw_string_state);
  scopes_destroy(&payload->scopes);
  ts_free(payload);
}

static void payload_serialize(Payload* payload, SerializeBuffer* buffer) {
  raw_string_state_serialize(&payload->raw_string_state, buffer);
  scopes_serialize(&payload->scopes, buffer);
}

static bool payload_deserialize(Payload* payload, DeserializeBuffer* buffer) {
  if (!raw_string_state_deserialize(&payload->raw_string_state, buffer)) {
    return false;
  }
  if (!scopes_deserialize(&payload->scopes, buffer)) {
    return false;
  }
  return true;
}

static inline bool payload_exists(void* payload) {
  return payload != NULL;
}

// ---------------------------------------------------------------------------------------

// Consume all leading whitespace before the next meaningful character
//
// - For whitespace that isn't a newline, we skip it entirely.
//   This includes spaces, tabs, `\r`, etc.
//
// - For newlines inside a `(`, `[`, or `[[` scope, we skip them.
//   In this context, newlines have no syntactic meaning and R's parser
//   simply eats them, so we do the same.
//
// - For newlines inside a "top level" or `{` scope, we return to `scan()`
//   and give our handlers a chance to run. In this context, these newlines
//   have contextual meaning, particularly for `if` statements.
//
// Because our external scanner is called on each character, this helper
// effectively replaces the usage of `/\s/` in `extras`. That said,
// practically the `/\s/` seems to still be needed. It seems like the
// internal scanner re-checks that the whitespace that we advanced over is
// skippable, which is why you see `skip character:' '` twice in the debug logs
// (once in the external scanner, once in the internal scanner). Based on some
// experimentation, this also seems true for Python, so we aren't too worried
// about it.
//
// Resist the urge to "simplify" this by refusing to handle whitespace at all
// in the external scanner. In theory we could return to the internal scanner
// when we see a non-newline whitespace and let the `extras` handling eat it,
// but in practice this does not work. An external scanner MUST skip whitespace.
// https://github.com/tree-sitter/tree-sitter/discussions/884#discussioncomment-302898
// https://github.com/tree-sitter/tree-sitter/issues/2735#issuecomment-1830392298
static inline void consume_whitespace_and_ignored_newlines(TSLexer* lexer, Scopes* scopes) {
  while (iswspace(lexer->lookahead)) {
    if (lexer->lookahead != '\n') {
      // Whitespace that isn't a newline, skip
      lexer->advance(lexer, true);
      continue;
    }

    Scope scope = scopes_peek(scopes);
    if (scope == SCOPE_PAREN || scope == SCOPE_BRACKET || scope == SCOPE_BRACKET2) {
      // Newline in `(`, `[`, or `[[` scope, skip
      lexer->advance(lexer, true);
      continue;
    }

    // Contextual newline, let handlers in `scan()` handle it
    break;
  }
}

// Check for an identifier continuation character
//
// Identifier continuation characters are defined in `identifier` in `grammar.js` as:
// - `XID_Continue` (0-9, a-z, A-Z, `_`, unicode support)
// - `.`
//
// We can't easily test `XID_Continue` from the scanner, but we can get close!
//
// For ASCII values (< 128):
// - `iswalnum()` handles 0-9, a-z, A-Z
// - `_` specially supported
// - `.` specially supported
// For non-ASCII values (>= 128):
// - Assume continuation
//
// The unhandled values in the ASCII range all correspond to non-continuation characters
// (like `>` or `#` or `"`).
//
// Assuming non-ASCII values are continuation characters is a bit of an
// overapproximation. The point is to allow this example to correctly parse as
// `elseμ <- 1`, aligning with R.
//
// ```r
// {
//   if (TRUE) 1
//   elseμ <- 1
// }
// ```
//
// However, we'd also parse the following example as `else· <- 1`, but really
// R's `iswalnum()` in UTF-8 locale would reject this and cause a syntax error
// on the U+00B7 middle dot. Being a bit more lax where R would generate a
// syntax error is okay, especially since it is fairly pathological.
//
// ```r
// {
//   if (TRUE) 1
//   else· <- 1
// }
// ```
//
// https://github.com/wch/r-source/blob/0a30adbff0fcff79ef608276024949881134bd08/src/main/gram.y#L3397-L3433
static inline bool is_identifier_continuation(int x) {
  return iswalnum(x) || x == '_' || x == '.' || x >= 128;
}

static inline bool scan_else(TSLexer* lexer) {
  if (lexer->lookahead != 'e') {
    return false;
  }
  lexer->advance(lexer, false);

  if (lexer->lookahead != 'l') {
    return false;
  }
  lexer->advance(lexer, false);

  if (lexer->lookahead != 's') {
    return false;
  }
  lexer->advance(lexer, false);

  if (lexer->lookahead != 'e') {
    return false;
  }
  lexer->advance(lexer, false);

  // Check that this `else` isn't part of a larger identifier, like `else_idx` (#200)
  if (is_identifier_continuation(lexer->lookahead)) {
    return false;
  }

  // We found `else`, return special `external` for it
  lexer->mark_end(lexer);
  lexer->result_symbol = ELSE;

  return true;
}

static inline bool scan_else_with_leading_newlines(TSLexer* lexer) {
  // Advance to the next non-newline, non-space character,
  // we know we have at least 1 newline because this function was called
  while (iswspace(lexer->lookahead)) {
    if (lexer->lookahead != '\n') {
      lexer->advance(lexer, true);
      continue;
    }

    lexer->advance(lexer, true);
    lexer->mark_end(lexer);
    lexer->result_symbol = NEWLINE;
  }

  // If the next symbol is a comment, we allow the internal scanner to pick it up.
  // Due to `mark_end()`, we've skipped past the newlines that would otherwise interfere
  // with a situation like below, where the rogue newline would make it look like we
  // exited the `if` statement, making a potential `else` node "invalid" in terms of
  // `valid_symbols`. Returning `false` seems to make `lexer->result_symbol = NEWLINE`
  // completely ignored.
  //
  // {
  //   if (cond) {
  //   }
  //   # comment
  //   else {
  //
  //   }
  // }
  if (lexer->lookahead == '#') {
    return false;
  }

  // Give the `ELSE` external scanner a chance to run, otherwise we
  // return a `NEWLINE` external. Either way we return `true` because
  // we have found a token of some kind.
  scan_else(lexer);

  return true;
}

// Scan a raw string literal; see R source code for implementation:
// https://github.com/wch/r-source/blob/52b730f217c12ba3d95dee0cd1f330d1977b5ea3/src/main/gram.y#L3102
static inline bool scan_raw_string_open(TSLexer* lexer, RawStringState* raw_string_state) {
  // Raw string literals can start with either 'r' or 'R'
  char prefix = lexer->lookahead;
  if (prefix != 'r' && prefix != 'R') {
    return false;
  }
  lexer->advance(lexer, false);

  // Check for quote character
  char closing_quote = lexer->lookahead;
  if (closing_quote != '"' && closing_quote != '\'') {
    return false;
  }
  lexer->advance(lexer, false);

  // Start counting '-' characters
  // Bail on pathological case of 256 hyphens
  uint8_t hyphen_count = 0;
  while (lexer->lookahead == '-') {
    if (hyphen_count == UINT8_MAX) {
      return false;
    }
    lexer->advance(lexer, false);
    hyphen_count += 1;
  }

  // Check for an opening bracket, and figure out
  // the corresponding closing bracket
  char opening_bracket = lexer->lookahead;
  char closing_bracket;
  if (opening_bracket == '(') {
    closing_bracket = ')';
    lexer->advance(lexer, false);
  } else if (opening_bracket == '[') {
    closing_bracket = ']';
    lexer->advance(lexer, false);
  } else if (opening_bracket == '{') {
    closing_bracket = '}';
    lexer->advance(lexer, false);
  } else {
    return false;
  }

  // Success!
  lexer->mark_end(lexer);
  lexer->result_symbol = RAW_STRING_OPEN;
  raw_string_state_set(raw_string_state, closing_bracket, hyphen_count, closing_quote);
  return true;
}

// We're in the body of the raw string, start looping until
// we find the matching `closing_bracket -> hyphen_count -> quote` sequence
//
// We purposefully only `advance()` on known non-closing sequence elements at the
// very beginning in the `!= closing_bracket` check (#162).
//
// Consider the following:
//
// r"(())"
//     ^^
//     ||
//     || 2) Which advances us to `)`. But this isn't a `"`, so we should loop around
//     ||    without advancing past the `)`.
//     | 1) This looks like it might be a closing `)`.
//
// If we also called `advance()` in the `!= closing_quote` branch, we'd skip past the
// `)` and we'd fail to recognize the raw string.
//
// Same logic applies to:
//
// r"-())-"
//     ^^
//     ||
//     || 2) Which advances us to `)`. But this isn't a `-`, so we should loop around
//     ||    without advancing past the `)`.
//     | 1) This looks like it might be a closing `)`.
//
// If we also called `advance()` in the `!matched_hyphens` branch, we'd skip past the
// `)` and we'd fail to recognize the raw string.
//
// In the case of `r"()"` when there is no string content, we avoid emitting a
// content node altogether, and instead immediately close out the raw string.
// This has to happen from here because we can't rewind the lexer.
static inline bool scan_raw_string_content_or_close(TSLexer* lexer, RawStringState* raw_string_state) {
  const char closing_bracket = raw_string_state->closing_bracket;
  const uint8_t hyphen_count = raw_string_state->hyphen_count;
  const char closing_quote = raw_string_state->closing_quote;

  bool any_content = false;

  while (!lexer->eof(lexer)) {
    if (lexer->lookahead != closing_bracket) {
      // Consume an arbitrary string part
      lexer->advance(lexer, false);
      any_content = true;
      continue;
    }

    // Assume we've captured all string content, and that we are going to try
    // and match against the closing sequence. If we are right, this marker was
    // the cutoff for string content. If we are wrong, we loop around again and
    // will reset the marker.
    lexer->mark_end(lexer);

    // Consume a closing bracket
    lexer->advance(lexer, false);

    // Try and consume `hyphen_count` hyphens in a row
    // (Start "matched" for the case of 0 hyphens)
    bool matched_hyphens = true;

    for (uint8_t i = 0; i < hyphen_count; ++i) {
      if (lexer->lookahead != '-') {
        matched_hyphens = false;
        break;
      }

      // Consume a hyphen
      lexer->advance(lexer, false);
    }

    if (!matched_hyphens) {
      any_content = true;
      continue;
    }

    if (lexer->lookahead != closing_quote) {
      any_content = true;
      continue;
    }

    // Consume a closing quote
    lexer->advance(lexer, false);

    // Success! We have consumed the closing sequence.
    if (any_content) {
      // If there was content, everything up to `mark_end()` contains it.
      // We will reconsume the closing sequence next in `scan_raw_string_close()`.
      lexer->result_symbol = RAW_STRING_CONTENT;
    } else {
      // If there wasn't content, close the raw string immediately. We don't
      // emit a zero width content node, consistent with single and double
      // quoted strings.
      lexer->mark_end(lexer);
      lexer->result_symbol = RAW_STRING_CLOSE;
    }

    return true;
  }

  // If we get here, this implies we hit eof (and so we have
  // an unclosed raw string)
  return false;
}

// Trust that `scan_raw_string_content_or_close()` validated that the closing sequence
// is coming up next, so we just consume it without checking a second time
static inline bool scan_raw_string_close(TSLexer* lexer, RawStringState* raw_string_state) {
  // Consume closing bracket
  lexer->advance(lexer, false);

  // Consume hyphens
  for (uint8_t i = 0; i < raw_string_state->hyphen_count; ++i) {
    lexer->advance(lexer, false);
  }

  // Consume closing quote
  lexer->advance(lexer, false);

  lexer->mark_end(lexer);
  lexer->result_symbol = RAW_STRING_CLOSE;
  return true;
}

static inline bool scan_semicolon(TSLexer* lexer) {
  lexer->advance(lexer, false);
  lexer->mark_end(lexer);
  lexer->result_symbol = SEMICOLON;
  return true;
}

static inline bool scan_newline(TSLexer* lexer) {
  lexer->advance(lexer, false);
  lexer->mark_end(lexer);
  lexer->result_symbol = NEWLINE;
  return true;
}

static inline bool scan_open_block(TSLexer* lexer, Scopes* scopes, Scope scope, TSSymbol symbol) {
  if (!scopes_push(scopes, scope)) {
    return false;
  }
  lexer->advance(lexer, false);
  lexer->mark_end(lexer);
  lexer->result_symbol = symbol;
  return true;
}

static inline bool scan_close_block(TSLexer* lexer, Scopes* scopes, Scope scope, TSSymbol symbol) {
  if (!scopes_pop(scopes, scope)) {
    return false;
  }
  lexer->advance(lexer, false);
  lexer->mark_end(lexer);
  lexer->result_symbol = symbol;
  return true;
}

static inline bool scan_open_bracket_or_bracket2(TSLexer* lexer, Scopes* scopes, const bool* valid_symbols) {
  // We know lookahead is the first `[`
  lexer->advance(lexer, false);

  // If we see `[[` when it's a valid symbol, greedily accept that
  if (valid_symbols[OPEN_BRACKET2] && lexer->lookahead == '[') {
    if (!scopes_push(scopes, SCOPE_BRACKET2)) {
      return false;
    }
    lexer->advance(lexer, false);
    lexer->mark_end(lexer);
    lexer->result_symbol = OPEN_BRACKET2;
    return true;
  }

  // If we see either `[` followed by something else, or `[[` when `[[` happens to
  // not be a valid symbol, accept the single `[` if it's a valid symbol.
  if (valid_symbols[OPEN_BRACKET]) {
    if (!scopes_push(scopes, SCOPE_BRACKET)) {
      return false;
    }
    lexer->mark_end(lexer);
    lexer->result_symbol = OPEN_BRACKET;
    return true;
  }

  // If we see a `[` that isn't captured by the above cases, we don't know how to
  // handle it
  return false;
}

static inline bool scan_close_bracket2(TSLexer* lexer, Scopes* scopes) {
  // We know the lookahead is the first `]`
  lexer->advance(lexer, false);

  if (lexer->lookahead != ']') {
    // Like `x[[1]` where we instead want an unmatched `]`
    return false;
  }

  return scan_close_block(lexer, scopes, SCOPE_BRACKET2, CLOSE_BRACKET2);
}

static bool scan(TSLexer* lexer, Scopes* scopes, RawStringState* raw_string_state, const bool* valid_symbols) {
  if (valid_symbols[ERROR_SENTINEL]) {
    // Decline to handle when in "error recovery" mode. When a syntax error occurs,
    // tree-sitter calls the external scanner with all `valid_symbols` marked as valid.
    return false;
  }

  if (valid_symbols[START]) {
    // The `START` symbol is only valid at the very beginning of a file before we
    // have seen any tokens. We emit this zero width symbol to force the `program`
    // node to open at position `(0, 0)`, regardless of how much leading whitespace
    // (including both `' '` and `\r`) there may be before our first "real" token.
    // This ensures the AST spans the entire file, which consumers of it rely on (#151).
    lexer->result_symbol = START;
    return true;
  }

  // These cases are only valid after `scan_raw_string_open()` accepts a raw
  // string opening sequence. They must run before whitespace and newlines are
  // consumed, otherwise `r"(  hello)"` won't capture the whitespace in the
  // `string_content`.
  if (valid_symbols[RAW_STRING_CONTENT]) {
    if (!valid_symbols[RAW_STRING_CLOSE]) {
      debug_print("Expected `RAW_STRING_CLOSE` to also be valid.");
    }
    return scan_raw_string_content_or_close(lexer, raw_string_state);
  } else if (valid_symbols[RAW_STRING_CLOSE]) {
    return scan_raw_string_close(lexer, raw_string_state);
  }

  consume_whitespace_and_ignored_newlines(lexer, scopes);

  // Purposefully structured as a series of exclusive if statements to
  // emphasize that we can't check any other condition after entering a branch,
  // because each `scan_*()` function calls `advance()` internally, meaning that
  // `lookahead` will no longer be accurate for checking other branches.

  if (valid_symbols[SEMICOLON] && lexer->lookahead == ';') {
    return scan_semicolon(lexer);
  } else if (valid_symbols[OPEN_PAREN] && lexer->lookahead == '(') {
    return scan_open_block(lexer, scopes, SCOPE_PAREN, OPEN_PAREN);
  } else if (valid_symbols[CLOSE_PAREN] && lexer->lookahead == ')') {
    return scan_close_block(lexer, scopes, SCOPE_PAREN, CLOSE_PAREN);
  } else if (valid_symbols[OPEN_BRACE] && lexer->lookahead == '{') {
    return scan_open_block(lexer, scopes, SCOPE_BRACE, OPEN_BRACE);
  } else if (valid_symbols[CLOSE_BRACE] && lexer->lookahead == '}') {
    return scan_close_block(lexer, scopes, SCOPE_BRACE, CLOSE_BRACE);
  } else if ((valid_symbols[OPEN_BRACKET] || valid_symbols[OPEN_BRACKET2]) && lexer->lookahead == '[') {
    return scan_open_bracket_or_bracket2(lexer, scopes, valid_symbols);
  } else if (valid_symbols[CLOSE_BRACKET] && lexer->lookahead == ']' && scopes_peek(scopes) == SCOPE_BRACKET) {
    // Must check the scope before entering this branch to account for `x[[a[1]]]` where
    // the first `]` occurs when both `]` and `]]` are valid. The scope breaks the tie
    // in favor of this branch.
    return scan_close_block(lexer, scopes, SCOPE_BRACKET, CLOSE_BRACKET);
  } else if (valid_symbols[CLOSE_BRACKET2] && lexer->lookahead == ']' && scopes_peek(scopes) == SCOPE_BRACKET2) {
    // Must check the scope before entering this branch to account for `x[a[[1]]]` where
    // the first `]` occurs when both `]` and `]]` are valid. The scope breaks the tie
    // in favor of this branch.
    return scan_close_bracket2(lexer, scopes);
  } else if (valid_symbols[RAW_STRING_OPEN] && (lexer->lookahead == 'r' || lexer->lookahead == 'R')) {
    return scan_raw_string_open(lexer, raw_string_state);
  } else if (valid_symbols[ELSE] && lexer->lookahead == 'e') {
    return scan_else(lexer);
  } else if (valid_symbols[ELSE] && scopes_peek(scopes) == SCOPE_BRACE && lexer->lookahead == '\n') {
    // If we are inside a `SCOPE_BRACE`, this is an extremely special case where `else`
    // can follow any number of newlines or whitespace and still be valid.
    return scan_else_with_leading_newlines(lexer);
  } else if (valid_symbols[NEWLINE] && lexer->lookahead == '\n') {
    // The above condition with `valid_symbols[ELSE]` must be checked first.
    // Due to `consume_whitespace_and_ignored_newlines()`, expect that we are either in
    // a `SCOPE_TOP_LEVEL` or a `SCOPE_BRACE` if we saw a new line at this point, which
    // is when they have contextual meaning and require their own token.
    return scan_newline(lexer);
  }

  return false;
}

// ---------------------------------------------------------------------------------------

void* tree_sitter_r_external_scanner_create(void) {
  return payload_new();
}

bool tree_sitter_r_external_scanner_scan(void* payload, TSLexer* lexer, const bool* valid_symbols) {
  if (!payload_exists(payload)) {
    return false;
  }
  Payload* payload_typed = (Payload*) payload;
  return scan(lexer, &payload_typed->scopes, &payload_typed->raw_string_state, valid_symbols);
}

// Note that `buffer` is allocated and owned by tree-sitter, not us, to be
// `TREE_SITTER_SERIALIZATION_BUFFER_SIZE`, so we can't write more than this
// into the buffer!
unsigned tree_sitter_r_external_scanner_serialize(void* payload, char* buffer) {
  if (!payload_exists(payload)) {
    return 0;
  }
  SerializeBuffer serialize_buffer = (SerializeBuffer) {.pointer = buffer, .length = 0};
  payload_serialize(payload, &serialize_buffer);
  if (serialize_buffer.length > TREE_SITTER_SERIALIZATION_BUFFER_SIZE) {
    debug_print("`serialize_buffer.length` can't be greater than `TREE_SITTER_SERIALIZATION_BUFFER_SIZE`\n");
  }
  return serialize_buffer.length;
}

void tree_sitter_r_external_scanner_deserialize(void* payload, const char* buffer, unsigned length) {
  if (!payload_exists(payload)) {
    return;
  }
  if (length == 0) {
    // At the start of every parse we are called with length 0 as a "reset" signal
    payload_reset(payload);
    return;
  }
  DeserializeBuffer deserialize_buffer = (DeserializeBuffer) {.pointer = buffer, .length = length};
  if (!payload_deserialize(payload, &deserialize_buffer)) {
    payload_reset(payload);
    debug_print("`payload_deserialize()` failed. Can't deserialize payload.\n");
  }
}

void tree_sitter_r_external_scanner_destroy(void* payload) {
  if (payload_exists(payload)) {
    payload_destroy(payload);
  }
}
