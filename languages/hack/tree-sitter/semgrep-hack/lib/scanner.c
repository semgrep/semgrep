#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <tree_sitter/parser.h>
#include <wctype.h>

/**
 * Debugging helper macros. Example output:
 *
 * > HEREDOC_START
 * scan_start() <-
 * next E
 * next O
 * next F
 * del EOF
 * set HEREDOC_START
 * stop \n
 * next \n
 * scan_delimiter() <-
 * scan_delimiter() -> false
 * scan_start() -> true
 *
 * > HEREDOC_START_NEWLINE HEREDOC_BODY HEREDOC_END_NEWLINE HEREDOC_END
 * scan_body() <-
 * next \n
 * stop $
 * scan_delimiter() <-
 * scan_delimiter() -> false
 * set HEREDOC_START_NEWLINE
 * scan_body() -> true
 */

#define debug 0

#define print(...) \
  if (debug) printf(__VA_ARGS__)

#define peek() lexer->lookahead

#define next()                    \
  {                               \
    endline chr = str(peek());    \
    print("next %s\n", chr.str);  \
    lexer->advance(lexer, false); \
  }

#define skip()                   \
  {                              \
    endline chr = str(peek());   \
    print("skip %s\n", chr.str); \
    lexer->advance(lexer, true); \
  }

#define stop()                   \
  {                              \
    endline chr = str(peek());   \
    print("stop %s\n", chr.str); \
    lexer->mark_end(lexer);      \
  }

#define set(symbol)                        \
  {                                        \
    print("set %s\n", TokenTypes[symbol]); \
    lexer->result_symbol = symbol;         \
  }

#define ret(function, result)                                   \
  print("%s() -> %s\n", function, (result) ? "true" : "false"); \
  return result;

#define MAX(a, b) ((a) > (b) ? (a) : (b))

enum TokenType {
  HEREDOC_START,
  HEREDOC_START_NEWLINE,
  HEREDOC_BODY,
  HEREDOC_END_NEWLINE,
  HEREDOC_END,
  EMBEDDED_OPENING_BRACE,
};

const char *const TokenTypes[] = {
    [HEREDOC_START] = "HEREDOC_START",                    //
    [HEREDOC_START_NEWLINE] = "HEREDOC_START_NEWLINE",    //
    [HEREDOC_BODY] = "HEREDOC_BODY",                      //
    [HEREDOC_END_NEWLINE] = "HEREDOC_END_NEWLINE",        //
    [HEREDOC_END] = "HEREDOC_END",                        //
    [EMBEDDED_OPENING_BRACE] = "EMBEDDED_OPENING_BRACE",  //
};

typedef struct {
  uint32_t cap;
  uint32_t len;
  char *data;
} String;

static String string_new() { return (String){.cap = 16, .len = 0, .data = calloc(1, 17)}; }

static void string_resize(String *string, uint32_t cap) {
  void *tmp = realloc(string->data, (cap + 1));
  assert(tmp != NULL);
  string->data = tmp;
  memset(string->data + string->len, 0, ((cap + 1) - string->len));
  string->cap = cap;
}

static void string_grow(String *string, uint32_t cap) {
  if (string->cap < cap) {
    string_resize(string, cap);
  }
}

static void string_push(String *string, char chr) {
  if (string->cap == string->len) {
    string_resize(string, MAX(16, string->len * 2));
  }
  string->data[string->len++] = chr;
}

static void string_free(String *string) {
  if (string->data != NULL) {
    free(string->data);
  }
}

static void string_clear(String *string) {
  string->len = 0;
  memset(string->data, 0, string->cap);
}

typedef struct {
  String delimiter;
  bool is_nowdoc;
  bool did_start;
  bool did_end;
} Scanner;

typedef struct {
  char str[3];
} endline;

static endline str(int32_t chr) {
  switch (chr) {
    case '\n':
      return (endline){.str = "\\n"};
    case '\r':
      return (endline){.str = "\\r"};
    case '\t':
      return (endline){.str = "\\t"};
    case ' ':
      return (endline){.str = "\\s"};
    case '\0':
      return (endline){.str = "\\0"};
    default:
      if (iswspace(chr)) {
        return (endline){.str = "\\s"};
      }

      return (endline){.str = {(char)chr, '\0', '\0'}};
  }
}

static unsigned serialize(Scanner *scanner, char *buffer) {
  if (scanner->delimiter.len + 2 >= TREE_SITTER_SERIALIZATION_BUFFER_SIZE) {
    return 0;
  }
  buffer[0] = (char)scanner->is_nowdoc;
  buffer[1] = (char)scanner->did_start;
  buffer[2] = (char)scanner->did_end;
  memcpy(&buffer[3], scanner->delimiter.data, scanner->delimiter.len);
  return scanner->delimiter.len + 3;
}

static void deserialize(Scanner *scanner, const char *buffer, unsigned length) {
  if (length == 0) {
    scanner->is_nowdoc = false;
    scanner->did_start = false;
    scanner->did_end = false;
    string_clear(&scanner->delimiter);
  } else {
    scanner->is_nowdoc = buffer[0];
    scanner->did_start = buffer[1];
    scanner->did_end = buffer[2];
    scanner->delimiter.len = length - 3;
    string_grow(&scanner->delimiter, scanner->delimiter.len);
    memcpy(scanner->delimiter.data, &buffer[3], scanner->delimiter.len);
  }
}

// This function returns true if c is a valid starting character of a name/identifier
static inline bool is_identifier_start_char(int32_t chr) {
  return (chr == '_') || ('a' <= chr && chr <= 'z') || ('A' <= chr && chr <= 'Z') ||
         (128 <= chr && chr <= 255);
}

static bool scan_delimiter(Scanner *scanner, TSLexer *lexer) {
  print("scan_delimiter() <-\n");
  for (unsigned long index = 0; index < scanner->delimiter.len; index++) {
    if (scanner->delimiter.data[index] == peek()) {
      next();
    } else {
      ret("scan_delimiter", false);
    }
  }
  ret("scan_delimiter", true);
}

static bool scan_body(Scanner *scanner, TSLexer *lexer) {
  print("scan_body() <-\n");

  bool did_advance = false;

  for (;;) {
    if (peek() == '\0') {
      return false;
    }

    if (peek() == '\\') {
      next();
      next();
      did_advance = true;
      continue;
    }

    if ((peek() == '{' || peek() == '$') && !scanner->is_nowdoc) {
      stop();

      if (peek() == '{') {
        next();

        if (peek() == '$' && !did_advance) {
          stop();
          next();

          if (is_identifier_start_char(peek())) {
            set(EMBEDDED_OPENING_BRACE);
            ret("scan_body", true);
          }
        }
      }

      if (peek() == '$') {
        next();

        if (is_identifier_start_char(peek())) {
          set(HEREDOC_BODY);
          ret("scan_body", did_advance);
        }
      }

      did_advance = true;
      continue;
    }

    if (scanner->did_end || peek() == '\n') {
      if (did_advance) {
        // <<<EOF
        // x     \n
        // EOF;  ^^ able to detect did_end
        stop();
        next();
      } else if (peek() == '\n') {
        if (scanner->did_end) {
          // Detected did_end in a previous HEREDOC_BODY or HEREDOC_START scan. Can skip newline.
          //
          // <<<EOF\n
          // EOF;  ^^ detected did_end during HEREDOC_START scan
          ///
          // <<<EOF
          // x     \n
          // EOF;  ^^ detected did_end during HEREDOC_BODY scan
          skip();
        } else {
          // Did not detect did_end in a previous scan. Newline could be HEREDOC_START_NEWLINE,
          // HEREDOC_BODY, HEREDOC_END_NEWLINE.
          //
          // <<<EOF\n
          // x     ^^ HEREDOC_START_NEWLINE
          // EOF;
          //
          // <<<EOF
          // $variable\n
          // x        ^^ HEREDOC_BODY
          // EOF;
          //
          // <<<EOF
          // $variable\n
          // EOF;     ^^ HEREDOC_END_NEWLINE
          next();
          stop();
        }
      }

      if (scan_delimiter(scanner, lexer)) {
        if (!did_advance && scanner->did_end) stop();

        if (peek() == ';') next();
        if (peek() == '\n') {
          if (did_advance) {
            set(HEREDOC_BODY);
            scanner->did_start = true;
            scanner->did_end = true;
          } else if (scanner->did_end) {
            set(HEREDOC_END);
            string_clear(&scanner->delimiter);
            scanner->is_nowdoc = false;
            scanner->did_start = false;
            scanner->did_end = false;
          } else {
            set(HEREDOC_END_NEWLINE);
            scanner->did_start = true;
            scanner->did_end = true;
          }
          ret("scan_body", true);
        }
      } else if (!scanner->did_start && !did_advance) {
        scanner->did_start = true;
        set(HEREDOC_START_NEWLINE);
        ret("scan_body", true);
      }

      did_advance = true;
      continue;
    }

    next();
    did_advance = true;
  }
}

static bool scan_start(Scanner *scanner, TSLexer *lexer) {
  print("scan_start() <-\n");

  while (iswspace(peek())) skip();

  scanner->is_nowdoc = peek() == '\'';
  string_clear(&scanner->delimiter);

  int32_t quote = 0;
  if (scanner->is_nowdoc || peek() == '"') {
    quote = peek();
    next();
  }

  if (iswalpha(peek()) || peek() == '_') {
    string_push(&scanner->delimiter, peek());
    next();

    while (iswalnum(peek()) || peek() == '_') {
      string_push(&scanner->delimiter, peek());
      next();
    }
  }

  print("del %s\n", scanner->delimiter.data);

  if (peek() == quote) {
    next();
  } else if (quote != 0) {
    // Opening quote exists, but we found no matching closing quote.
    ret("scan_start", false);
  }

  // A valid delimiter must end with a newline with no whitespace in between.
  if (peek() != '\n' || scanner->delimiter.len == 0) {
    return false;
  }

  set(HEREDOC_START);
  stop();
  next();

  if (scan_delimiter(scanner, lexer)) {
    if (peek() == ';') next();
    if (peek() == '\n') {
      // <<<EOF\n
      // EOF;  ^^ able to detect did_end
      scanner->did_end = true;
    }
  }

  ret("scan_start", true);
}

/**
 * Note: if we return false for a scan, variable value changes are overwritten with the values of
 * the last successful scan. https://tree-sitter.github.io/tree-sitter/creating-parsers#serialize
 */
static bool scan(Scanner *scanner, TSLexer *lexer, const bool *expected) {
  print("\n> ");
  if (expected[HEREDOC_START]) {
    print("%s ", TokenTypes[HEREDOC_START]);
  }
  if (expected[HEREDOC_START_NEWLINE]) {
    print("%s ", TokenTypes[HEREDOC_START_NEWLINE]);
  }
  if (expected[HEREDOC_BODY]) {
    print("%s ", TokenTypes[HEREDOC_BODY]);
  }
  if (expected[HEREDOC_END_NEWLINE]) {
    print("%s ", TokenTypes[HEREDOC_END_NEWLINE]);
  }
  if (expected[HEREDOC_END]) {
    print("%s ", TokenTypes[HEREDOC_END]);
  }
  if (expected[EMBEDDED_OPENING_BRACE]) {
    print("%s ", TokenTypes[EMBEDDED_OPENING_BRACE]);
  }
  print("\n");

  if ((expected[HEREDOC_BODY] || expected[HEREDOC_END] || expected[EMBEDDED_OPENING_BRACE]) &&
      scanner->delimiter.len > 0) {
    return scan_body(scanner, lexer);
  }

  if (expected[HEREDOC_START]) {
    return scan_start(scanner, lexer);
  }

  return false;
}

void *tree_sitter_hack_external_scanner_create() {
  Scanner *scanner = calloc(1, sizeof(Scanner));
  scanner->delimiter = string_new();
  return scanner;
}

bool tree_sitter_hack_external_scanner_scan(void *payload, TSLexer *lexer, const bool *expected) {
  Scanner *scanner = (Scanner *)payload;
  return scan(scanner, lexer, expected);
}

unsigned tree_sitter_hack_external_scanner_serialize(void *payload, char *state) {
  Scanner *scanner = (Scanner *)payload;
  return serialize(scanner, state);
}

void tree_sitter_hack_external_scanner_deserialize(
    void *payload, const char *state, unsigned length) {
  Scanner *scanner = (Scanner *)payload;
  deserialize(scanner, state, length);
}

void tree_sitter_hack_external_scanner_destroy(void *payload) {
  Scanner *scanner = (Scanner *)payload;
  string_free(&scanner->delimiter);
  free(scanner);
}
