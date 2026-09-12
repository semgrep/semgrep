#include <stdlib.h>

#include "tree_sitter/parser.h"

enum TokenType { F, NEWLINE, B };

/**
 * For this toy example, we don't actually need any state.
 */
void *tree_sitter_externals_external_scanner_create() {
        return NULL;
}

/**
 * Destroy the (nonexistent) state.
 */
void tree_sitter_externals_external_scanner_destroy(void *indents_v) {
        return;
}

unsigned tree_sitter_externals_external_scanner_serialize(void *payload,
                                                          char *buffer) {
        // With no state to store, we write 0 bytes.
        return 0;
}

void tree_sitter_externals_external_scanner_deserialize(void *payload,
                                                        const char *buffer,
                                                        unsigned length) {
        // There is nothing to deserialize.
        return;
}

static void advance(TSLexer *lexer) {
        lexer->advance(lexer, false);
}

/** Really simple lexer. Just lex "f", "\n", and "b" individually.
 */
bool tree_sitter_externals_external_scanner_scan(void *payload, TSLexer *lexer,
                                                 const bool *valid_symbols) {
        if (lexer->lookahead == 'f') {
                advance(lexer);
                lexer->result_symbol = F;
                return true;
        } else if (lexer->lookahead == '\n') {
                advance(lexer);
                lexer->result_symbol = NEWLINE;
                return true;
        } else if (lexer->lookahead == 'b') {
                advance(lexer);
                lexer->result_symbol = B;
                return true;
        }
        return false;
}
