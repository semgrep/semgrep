#include <stdio.h>
#include <wctype.h>
#include <stdbool.h>

#include "tree_sitter/parser.h"

#ifdef DEBUG
#define log(...) fprintf(stderr, __VA_ARGS__)
#else
#define log(...)
#endif

/// @brief Token types. The order must match the order of the `externals` in the grammar.
enum TokenType {
  BLOCK_DOC_COMMENT_MARKER,
  BLOCK_COMMENT_CONTENT,
  LINE_DOC_CONTENT,

  // Error state sentinel, must be the last one.
  ERROR_SENTINEL,
};

/// Tree-sitter interfaces
/// Source: https://github.com/tree-sitter/tree-sitter-javascript/blob/master/src/scanner.c
/// Ref:    https://tree-sitter.github.io/tree-sitter/creating-parsers#external-scanners
void *tree_sitter_move_on_aptos_external_scanner_create() { return NULL; }

void tree_sitter_move_on_aptos_external_scanner_destroy(void *p) {}

unsigned tree_sitter_move_on_aptos_external_scanner_serialize(void *p, char *buffer) { return 0; }

void tree_sitter_move_on_aptos_external_scanner_deserialize(void *p, const char *b, unsigned n) {}

/// @brief Advance the lexer by one character
/// @param lexer the lexer
inline static void advance(TSLexer *lexer) { lexer->advance(lexer, false); }

/// @brief Skip the char, which will be treated as a whitespace character.
/// @param lexer the lexer
inline static void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

/// @brief Check if the lookahead marks the end of line. EOF is not considered as EOL.
/// @param lexer 
/// @return if it is EOL
inline static bool is_eol(TSLexer *lexer) {
  int ch = lexer->lookahead;
  return ch == '\n' || ch == 0x2028 || ch == 0x2029;
}


inline static bool scan_block_doc_comment_marker(TSLexer *lexer) {
  if (lexer->lookahead != '*') return false;
  // '*' but not '**', '*/'
  advance(lexer);
  // This means finishing matching `block_comment_marker`.
  // This allows matching tokens that require multiple characters of lookahead.
  lexer->mark_end(lexer);
  if (lexer->lookahead == '/' || lexer->lookahead == '*')
    return false;
  lexer->result_symbol = BLOCK_DOC_COMMENT_MARKER;
  return true;
}


inline static bool scan_block_comment_content(TSLexer *lexer) {
  // At this stage, we have already entered a comment block: matched `/*`.
  size_t comment_depth = 1;

  // munch the rest.
  while (lexer->lookahead && comment_depth > 0) {
    switch (lexer->lookahead) {
    case '*':
      // don't match the outmost '*/' (comment_depth = 1): it will be recognised by tree-sitter.
      // `advance` after `mark_end` will be discarded, so the tree-sitter can still recognise the `*/`.
      // `mark_end` can be called multiple times.
      if (comment_depth == 1)
        lexer->mark_end(lexer);
      advance(lexer);
      if (lexer->lookahead == '/') {
        // matched pattern: */
        log("exits a comment layer[%d]\n", lexer->get_column(lexer));
        comment_depth--;
        advance(lexer);
      }
      break;
    case '/':
      advance(lexer);
      if (lexer->lookahead == '*') {
        // matched pattern: /*
        advance(lexer);
        log("entering a nested comment\n");
        comment_depth++;
      }
      break;
    default:
      advance(lexer);
      break;
    }
  }
  log("exit parsing block comment contents, remaining depth: %d, col: %d\n", comment_depth, lexer->get_column(lexer));
  if (comment_depth > 0) // comment does not end, its content is scanned.
    lexer->mark_end(lexer);
  else
    lexer->result_symbol = BLOCK_COMMENT_CONTENT;
  return comment_depth <= 0;
}

/// @brief Scan a document line, basically `/.*/`.
/// @param lexer 
/// @return if matched (always `true`)
static inline bool scan_line_doc_content(TSLexer *lexer) {
  log("parse line doc comment\n");
  lexer->result_symbol = LINE_DOC_CONTENT;
  while (lexer->lookahead) {
    if (is_eol(lexer)) {
      // Include the EOL character. This is friendly to the markdown renderer (although it is not necessary).
      advance(lexer);
      break;
    }
    advance(lexer);
  }
  return true;
  
}


/// @brief Scanner entry, called by tree-sitter.
/// @ref https://github.com/tree-sitter/tree-sitter-rust/blob/master/src/scanner.c
/// @param payload 
/// @param lexer 
/// @param valid_symbols 
/// @return 
bool tree_sitter_move_on_aptos_external_scanner_scan(
  void *payload,
  TSLexer *lexer,
  const bool *valid_symbols
) {
  if (valid_symbols[ERROR_SENTINEL]) {
    log("error state\n");
    return false;
  }

  if (valid_symbols[LINE_DOC_CONTENT]) {
    return scan_line_doc_content(lexer);
  }

  bool ret = false;
  if (valid_symbols[BLOCK_DOC_COMMENT_MARKER])
    ret = scan_block_doc_comment_marker(lexer);
  if (!ret && valid_symbols[BLOCK_COMMENT_CONTENT])
    ret = scan_block_comment_content(lexer);
  log("ret: %s\n", ret ? "true" : "false");
  return ret;
}

