#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 125
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 109
#define ALIAS_COUNT 2
#define TOKEN_COUNT 72
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 10
#define MAX_ALIAS_SEQUENCE_LENGTH 7
#define PRODUCTION_ID_COUNT 10

enum ts_symbol_identifiers {
  sym_identifier = 1,
  anon_sym_schema_COLON = 2,
  anon_sym_contents_COLON = 3,
  anon_sym_DASH = 4,
  sym_file = 5,
  anon_sym_schema = 6,
  sym_version = 7,
  anon_sym_SQUOTE = 8,
  anon_sym_model = 9,
  anon_sym_LF = 10,
  anon_sym_module = 11,
  anon_sym_extend = 12,
  anon_sym_type = 13,
  anon_sym_relations = 14,
  anon_sym_define = 15,
  anon_sym_COLON = 16,
  anon_sym_or = 17,
  anon_sym_and = 18,
  anon_sym_butnot = 19,
  anon_sym_LBRACK = 20,
  anon_sym_COMMA = 21,
  anon_sym_RBRACK = 22,
  anon_sym_with = 23,
  anon_sym_from = 24,
  anon_sym_POUND = 25,
  anon_sym_COLON_STAR = 26,
  anon_sym_condition = 27,
  anon_sym_LPAREN = 28,
  anon_sym_RPAREN = 29,
  anon_sym_string = 30,
  anon_sym_int = 31,
  anon_sym_map = 32,
  anon_sym_uint = 33,
  anon_sym_list = 34,
  anon_sym_timestamp = 35,
  anon_sym_bool = 36,
  anon_sym_duration = 37,
  anon_sym_double = 38,
  anon_sym_ipaddress = 39,
  anon_sym_EQ_EQ = 40,
  anon_sym_BANG_EQ = 41,
  anon_sym_LT = 42,
  anon_sym_LT_EQ = 43,
  anon_sym_GT = 44,
  anon_sym_GT_EQ = 45,
  anon_sym_LBRACE = 46,
  anon_sym_RBRACE = 47,
  anon_sym_STAR = 48,
  anon_sym_SLASH = 49,
  anon_sym_PERCENT = 50,
  anon_sym_LT_LT = 51,
  anon_sym_GT_GT = 52,
  anon_sym_AMP = 53,
  anon_sym_AMP_CARET = 54,
  anon_sym_PLUS = 55,
  anon_sym_PIPE = 56,
  anon_sym_CARET = 57,
  anon_sym_AMP_AMP = 58,
  anon_sym_PIPE_PIPE = 59,
  anon_sym_BANG = 60,
  anon_sym_DOT = 61,
  sym_float_literal = 62,
  sym_int_literal = 63,
  sym_uint_literal = 64,
  anon_sym_true = 65,
  anon_sym_false = 66,
  sym_null_literal = 67,
  sym_string_literal = 68,
  sym_comment = 69,
  sym_semgrep_ellipsis = 70,
  sym_semgrep_metavariable = 71,
  sym_source_file = 72,
  sym_project_file = 73,
  sym_module_file = 74,
  sym_quoted_schema = 75,
  sym_contents = 76,
  sym_schema = 77,
  sym_quoted_version = 78,
  sym_model = 79,
  sym_module = 80,
  sym_type_declaration = 81,
  sym_relations = 82,
  sym_definition = 83,
  sym_relation_def = 84,
  sym_operator = 85,
  sym_direct_relationship = 86,
  sym_conditional = 87,
  sym_indirect_relation = 88,
  sym_relation_ref = 89,
  sym_all = 90,
  sym_condition_declaration = 91,
  sym_param = 92,
  sym_type_identifier = 93,
  sym_condition_body = 94,
  sym_binary_expression = 95,
  sym_call_expression = 96,
  sym_selector_expression = 97,
  sym_argument_list = 98,
  sym_expression = 99,
  sym_number_literal = 100,
  sym_boolean_literal = 101,
  aux_sym_module_file_repeat1 = 102,
  aux_sym_contents_repeat1 = 103,
  aux_sym_relations_repeat1 = 104,
  aux_sym_relation_def_repeat1 = 105,
  aux_sym_direct_relationship_repeat1 = 106,
  aux_sym_condition_declaration_repeat1 = 107,
  aux_sym_argument_list_repeat1 = 108,
  alias_sym_imm_tok_colonstar = 109,
  alias_sym_imm_tok_prec_p1_hash = 110,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [sym_identifier] = "identifier",
  [anon_sym_schema_COLON] = "schema:",
  [anon_sym_contents_COLON] = "contents:",
  [anon_sym_DASH] = "-",
  [sym_file] = "file",
  [anon_sym_schema] = "schema",
  [sym_version] = "version",
  [anon_sym_SQUOTE] = "'",
  [anon_sym_model] = "model",
  [anon_sym_LF] = "\n",
  [anon_sym_module] = "module",
  [anon_sym_extend] = "extend",
  [anon_sym_type] = "type",
  [anon_sym_relations] = "relations",
  [anon_sym_define] = "define",
  [anon_sym_COLON] = ":",
  [anon_sym_or] = "or",
  [anon_sym_and] = "and",
  [anon_sym_butnot] = "but not",
  [anon_sym_LBRACK] = "[",
  [anon_sym_COMMA] = ",",
  [anon_sym_RBRACK] = "]",
  [anon_sym_with] = "with",
  [anon_sym_from] = "from",
  [anon_sym_POUND] = "#",
  [anon_sym_COLON_STAR] = ":*",
  [anon_sym_condition] = "condition",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_string] = "string",
  [anon_sym_int] = "int",
  [anon_sym_map] = "map",
  [anon_sym_uint] = "uint",
  [anon_sym_list] = "list",
  [anon_sym_timestamp] = "timestamp",
  [anon_sym_bool] = "bool",
  [anon_sym_duration] = "duration",
  [anon_sym_double] = "double",
  [anon_sym_ipaddress] = "ipaddress",
  [anon_sym_EQ_EQ] = "==",
  [anon_sym_BANG_EQ] = "!=",
  [anon_sym_LT] = "<",
  [anon_sym_LT_EQ] = "<=",
  [anon_sym_GT] = ">",
  [anon_sym_GT_EQ] = ">=",
  [anon_sym_LBRACE] = "{",
  [anon_sym_RBRACE] = "}",
  [anon_sym_STAR] = "*",
  [anon_sym_SLASH] = "/",
  [anon_sym_PERCENT] = "%",
  [anon_sym_LT_LT] = "<<",
  [anon_sym_GT_GT] = ">>",
  [anon_sym_AMP] = "&",
  [anon_sym_AMP_CARET] = "&^",
  [anon_sym_PLUS] = "+",
  [anon_sym_PIPE] = "|",
  [anon_sym_CARET] = "^",
  [anon_sym_AMP_AMP] = "&&",
  [anon_sym_PIPE_PIPE] = "||",
  [anon_sym_BANG] = "!",
  [anon_sym_DOT] = ".",
  [sym_float_literal] = "float_literal",
  [sym_int_literal] = "int_literal",
  [sym_uint_literal] = "uint_literal",
  [anon_sym_true] = "true",
  [anon_sym_false] = "false",
  [sym_null_literal] = "null_literal",
  [sym_string_literal] = "string_literal",
  [sym_comment] = "comment",
  [sym_semgrep_ellipsis] = "semgrep_ellipsis",
  [sym_semgrep_metavariable] = "semgrep_metavariable",
  [sym_source_file] = "source_file",
  [sym_project_file] = "project_file",
  [sym_module_file] = "module_file",
  [sym_quoted_schema] = "quoted_schema",
  [sym_contents] = "contents",
  [sym_schema] = "schema",
  [sym_quoted_version] = "quoted_version",
  [sym_model] = "model",
  [sym_module] = "module",
  [sym_type_declaration] = "type_declaration",
  [sym_relations] = "relations",
  [sym_definition] = "definition",
  [sym_relation_def] = "relation_def",
  [sym_operator] = "operator",
  [sym_direct_relationship] = "direct_relationship",
  [sym_conditional] = "conditional",
  [sym_indirect_relation] = "indirect_relation",
  [sym_relation_ref] = "relation_ref",
  [sym_all] = "all",
  [sym_condition_declaration] = "condition_declaration",
  [sym_param] = "param",
  [sym_type_identifier] = "type_identifier",
  [sym_condition_body] = "condition_body",
  [sym_binary_expression] = "binary_expression",
  [sym_call_expression] = "call_expression",
  [sym_selector_expression] = "selector_expression",
  [sym_argument_list] = "argument_list",
  [sym_expression] = "expression",
  [sym_number_literal] = "number_literal",
  [sym_boolean_literal] = "boolean_literal",
  [aux_sym_module_file_repeat1] = "module_file_repeat1",
  [aux_sym_contents_repeat1] = "contents_repeat1",
  [aux_sym_relations_repeat1] = "relations_repeat1",
  [aux_sym_relation_def_repeat1] = "relation_def_repeat1",
  [aux_sym_direct_relationship_repeat1] = "direct_relationship_repeat1",
  [aux_sym_condition_declaration_repeat1] = "condition_declaration_repeat1",
  [aux_sym_argument_list_repeat1] = "argument_list_repeat1",
  [alias_sym_imm_tok_colonstar] = "imm_tok_colonstar",
  [alias_sym_imm_tok_prec_p1_hash] = "imm_tok_prec_p1_hash",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [sym_identifier] = sym_identifier,
  [anon_sym_schema_COLON] = anon_sym_schema_COLON,
  [anon_sym_contents_COLON] = anon_sym_contents_COLON,
  [anon_sym_DASH] = anon_sym_DASH,
  [sym_file] = sym_file,
  [anon_sym_schema] = anon_sym_schema,
  [sym_version] = sym_version,
  [anon_sym_SQUOTE] = anon_sym_SQUOTE,
  [anon_sym_model] = anon_sym_model,
  [anon_sym_LF] = anon_sym_LF,
  [anon_sym_module] = anon_sym_module,
  [anon_sym_extend] = anon_sym_extend,
  [anon_sym_type] = anon_sym_type,
  [anon_sym_relations] = anon_sym_relations,
  [anon_sym_define] = anon_sym_define,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_or] = anon_sym_or,
  [anon_sym_and] = anon_sym_and,
  [anon_sym_butnot] = anon_sym_butnot,
  [anon_sym_LBRACK] = anon_sym_LBRACK,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [anon_sym_with] = anon_sym_with,
  [anon_sym_from] = anon_sym_from,
  [anon_sym_POUND] = anon_sym_POUND,
  [anon_sym_COLON_STAR] = anon_sym_COLON_STAR,
  [anon_sym_condition] = anon_sym_condition,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_string] = anon_sym_string,
  [anon_sym_int] = anon_sym_int,
  [anon_sym_map] = anon_sym_map,
  [anon_sym_uint] = anon_sym_uint,
  [anon_sym_list] = anon_sym_list,
  [anon_sym_timestamp] = anon_sym_timestamp,
  [anon_sym_bool] = anon_sym_bool,
  [anon_sym_duration] = anon_sym_duration,
  [anon_sym_double] = anon_sym_double,
  [anon_sym_ipaddress] = anon_sym_ipaddress,
  [anon_sym_EQ_EQ] = anon_sym_EQ_EQ,
  [anon_sym_BANG_EQ] = anon_sym_BANG_EQ,
  [anon_sym_LT] = anon_sym_LT,
  [anon_sym_LT_EQ] = anon_sym_LT_EQ,
  [anon_sym_GT] = anon_sym_GT,
  [anon_sym_GT_EQ] = anon_sym_GT_EQ,
  [anon_sym_LBRACE] = anon_sym_LBRACE,
  [anon_sym_RBRACE] = anon_sym_RBRACE,
  [anon_sym_STAR] = anon_sym_STAR,
  [anon_sym_SLASH] = anon_sym_SLASH,
  [anon_sym_PERCENT] = anon_sym_PERCENT,
  [anon_sym_LT_LT] = anon_sym_LT_LT,
  [anon_sym_GT_GT] = anon_sym_GT_GT,
  [anon_sym_AMP] = anon_sym_AMP,
  [anon_sym_AMP_CARET] = anon_sym_AMP_CARET,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_PIPE] = anon_sym_PIPE,
  [anon_sym_CARET] = anon_sym_CARET,
  [anon_sym_AMP_AMP] = anon_sym_AMP_AMP,
  [anon_sym_PIPE_PIPE] = anon_sym_PIPE_PIPE,
  [anon_sym_BANG] = anon_sym_BANG,
  [anon_sym_DOT] = anon_sym_DOT,
  [sym_float_literal] = sym_float_literal,
  [sym_int_literal] = sym_int_literal,
  [sym_uint_literal] = sym_uint_literal,
  [anon_sym_true] = anon_sym_true,
  [anon_sym_false] = anon_sym_false,
  [sym_null_literal] = sym_null_literal,
  [sym_string_literal] = sym_string_literal,
  [sym_comment] = sym_comment,
  [sym_semgrep_ellipsis] = sym_semgrep_ellipsis,
  [sym_semgrep_metavariable] = sym_semgrep_metavariable,
  [sym_source_file] = sym_source_file,
  [sym_project_file] = sym_project_file,
  [sym_module_file] = sym_module_file,
  [sym_quoted_schema] = sym_quoted_schema,
  [sym_contents] = sym_contents,
  [sym_schema] = sym_schema,
  [sym_quoted_version] = sym_quoted_version,
  [sym_model] = sym_model,
  [sym_module] = sym_module,
  [sym_type_declaration] = sym_type_declaration,
  [sym_relations] = sym_relations,
  [sym_definition] = sym_definition,
  [sym_relation_def] = sym_relation_def,
  [sym_operator] = sym_operator,
  [sym_direct_relationship] = sym_direct_relationship,
  [sym_conditional] = sym_conditional,
  [sym_indirect_relation] = sym_indirect_relation,
  [sym_relation_ref] = sym_relation_ref,
  [sym_all] = sym_all,
  [sym_condition_declaration] = sym_condition_declaration,
  [sym_param] = sym_param,
  [sym_type_identifier] = sym_type_identifier,
  [sym_condition_body] = sym_condition_body,
  [sym_binary_expression] = sym_binary_expression,
  [sym_call_expression] = sym_call_expression,
  [sym_selector_expression] = sym_selector_expression,
  [sym_argument_list] = sym_argument_list,
  [sym_expression] = sym_expression,
  [sym_number_literal] = sym_number_literal,
  [sym_boolean_literal] = sym_boolean_literal,
  [aux_sym_module_file_repeat1] = aux_sym_module_file_repeat1,
  [aux_sym_contents_repeat1] = aux_sym_contents_repeat1,
  [aux_sym_relations_repeat1] = aux_sym_relations_repeat1,
  [aux_sym_relation_def_repeat1] = aux_sym_relation_def_repeat1,
  [aux_sym_direct_relationship_repeat1] = aux_sym_direct_relationship_repeat1,
  [aux_sym_condition_declaration_repeat1] = aux_sym_condition_declaration_repeat1,
  [aux_sym_argument_list_repeat1] = aux_sym_argument_list_repeat1,
  [alias_sym_imm_tok_colonstar] = alias_sym_imm_tok_colonstar,
  [alias_sym_imm_tok_prec_p1_hash] = alias_sym_imm_tok_prec_p1_hash,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [sym_identifier] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_schema_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_contents_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH] = {
    .visible = true,
    .named = false,
  },
  [sym_file] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_schema] = {
    .visible = true,
    .named = false,
  },
  [sym_version] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_SQUOTE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_model] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LF] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_module] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_extend] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_type] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_relations] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_define] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_or] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_and] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_butnot] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_with] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_from] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_POUND] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COLON_STAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_condition] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_string] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_int] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_map] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_uint] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_list] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_timestamp] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_bool] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_duration] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_double] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_ipaddress] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_BANG_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_GT_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SLASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PERCENT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT_LT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_GT_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AMP] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AMP_CARET] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PLUS] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PIPE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_CARET] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AMP_AMP] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PIPE_PIPE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_BANG] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = false,
  },
  [sym_float_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_int_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_uint_literal] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_true] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_false] = {
    .visible = true,
    .named = false,
  },
  [sym_null_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_string_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_comment] = {
    .visible = true,
    .named = true,
  },
  [sym_semgrep_ellipsis] = {
    .visible = true,
    .named = true,
  },
  [sym_semgrep_metavariable] = {
    .visible = true,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym_project_file] = {
    .visible = true,
    .named = true,
  },
  [sym_module_file] = {
    .visible = true,
    .named = true,
  },
  [sym_quoted_schema] = {
    .visible = true,
    .named = true,
  },
  [sym_contents] = {
    .visible = true,
    .named = true,
  },
  [sym_schema] = {
    .visible = true,
    .named = true,
  },
  [sym_quoted_version] = {
    .visible = true,
    .named = true,
  },
  [sym_model] = {
    .visible = true,
    .named = true,
  },
  [sym_module] = {
    .visible = true,
    .named = true,
  },
  [sym_type_declaration] = {
    .visible = true,
    .named = true,
  },
  [sym_relations] = {
    .visible = true,
    .named = true,
  },
  [sym_definition] = {
    .visible = true,
    .named = true,
  },
  [sym_relation_def] = {
    .visible = true,
    .named = true,
  },
  [sym_operator] = {
    .visible = true,
    .named = true,
  },
  [sym_direct_relationship] = {
    .visible = true,
    .named = true,
  },
  [sym_conditional] = {
    .visible = true,
    .named = true,
  },
  [sym_indirect_relation] = {
    .visible = true,
    .named = true,
  },
  [sym_relation_ref] = {
    .visible = true,
    .named = true,
  },
  [sym_all] = {
    .visible = true,
    .named = true,
  },
  [sym_condition_declaration] = {
    .visible = true,
    .named = true,
  },
  [sym_param] = {
    .visible = true,
    .named = true,
  },
  [sym_type_identifier] = {
    .visible = true,
    .named = true,
  },
  [sym_condition_body] = {
    .visible = true,
    .named = true,
  },
  [sym_binary_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_call_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_selector_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_argument_list] = {
    .visible = true,
    .named = true,
  },
  [sym_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_number_literal] = {
    .visible = true,
    .named = true,
  },
  [sym_boolean_literal] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_module_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_contents_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_relations_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_relation_def_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_direct_relationship_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_condition_declaration_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_argument_list_repeat1] = {
    .visible = false,
    .named = false,
  },
  [alias_sym_imm_tok_colonstar] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_prec_p1_hash] = {
    .visible = true,
    .named = true,
  },
};

enum ts_field_identifiers {
  field_arguments = 1,
  field_body = 2,
  field_field = 3,
  field_function = 4,
  field_left = 5,
  field_name = 6,
  field_operand = 7,
  field_operator = 8,
  field_relation = 9,
  field_right = 10,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_arguments] = "arguments",
  [field_body] = "body",
  [field_field] = "field",
  [field_function] = "function",
  [field_left] = "left",
  [field_name] = "name",
  [field_operand] = "operand",
  [field_operator] = "operator",
  [field_relation] = "relation",
  [field_right] = "right",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 2},
  [2] = {.index = 2, .length = 2},
  [3] = {.index = 4, .length = 2},
  [4] = {.index = 6, .length = 2},
  [5] = {.index = 8, .length = 1},
  [6] = {.index = 9, .length = 2},
  [7] = {.index = 11, .length = 3},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_body, 4},
    {field_name, 1},
  [2] =
    {field_body, 5},
    {field_name, 1},
  [4] =
    {field_arguments, 1},
    {field_function, 0},
  [6] =
    {field_body, 6},
    {field_name, 1},
  [8] =
    {field_relation, 1},
  [9] =
    {field_field, 2},
    {field_operand, 0},
  [11] =
    {field_left, 0},
    {field_operator, 1},
    {field_right, 2},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
  [8] = {
    [1] = alias_sym_imm_tok_colonstar,
  },
  [9] = {
    [1] = alias_sym_imm_tok_prec_p1_hash,
  },
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 4,
  [5] = 5,
  [6] = 6,
  [7] = 7,
  [8] = 8,
  [9] = 9,
  [10] = 10,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
  [15] = 15,
  [16] = 16,
  [17] = 17,
  [18] = 18,
  [19] = 19,
  [20] = 20,
  [21] = 21,
  [22] = 22,
  [23] = 23,
  [24] = 24,
  [25] = 25,
  [26] = 26,
  [27] = 27,
  [28] = 28,
  [29] = 29,
  [30] = 30,
  [31] = 31,
  [32] = 32,
  [33] = 33,
  [34] = 34,
  [35] = 35,
  [36] = 36,
  [37] = 37,
  [38] = 38,
  [39] = 39,
  [40] = 40,
  [41] = 41,
  [42] = 42,
  [43] = 43,
  [44] = 44,
  [45] = 45,
  [46] = 46,
  [47] = 47,
  [48] = 48,
  [49] = 49,
  [50] = 50,
  [51] = 51,
  [52] = 52,
  [53] = 53,
  [54] = 54,
  [55] = 55,
  [56] = 56,
  [57] = 57,
  [58] = 58,
  [59] = 59,
  [60] = 60,
  [61] = 61,
  [62] = 62,
  [63] = 63,
  [64] = 64,
  [65] = 65,
  [66] = 66,
  [67] = 67,
  [68] = 68,
  [69] = 69,
  [70] = 70,
  [71] = 71,
  [72] = 72,
  [73] = 73,
  [74] = 74,
  [75] = 75,
  [76] = 76,
  [77] = 77,
  [78] = 78,
  [79] = 79,
  [80] = 80,
  [81] = 81,
  [82] = 82,
  [83] = 83,
  [84] = 84,
  [85] = 85,
  [86] = 86,
  [87] = 87,
  [88] = 88,
  [89] = 89,
  [90] = 90,
  [91] = 91,
  [92] = 92,
  [93] = 93,
  [94] = 94,
  [95] = 95,
  [96] = 96,
  [97] = 97,
  [98] = 98,
  [99] = 99,
  [100] = 100,
  [101] = 101,
  [102] = 102,
  [103] = 103,
  [104] = 104,
  [105] = 105,
  [106] = 106,
  [107] = 107,
  [108] = 108,
  [109] = 109,
  [110] = 110,
  [111] = 111,
  [112] = 112,
  [113] = 113,
  [114] = 114,
  [115] = 115,
  [116] = 116,
  [117] = 117,
  [118] = 118,
  [119] = 119,
  [120] = 120,
  [121] = 121,
  [122] = 122,
  [123] = 123,
  [124] = 124,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(43);
      ADVANCE_MAP(
        '!', 82,
        '#', 58,
        '$', 34,
        '%', 72,
        '&', 75,
        '\'', 50,
        '(', 60,
        ')', 61,
        '*', 70,
        '+', 77,
        ',', 56,
        '-', 47,
        '.', 84,
        '/', 71,
        '0', 87,
        ':', 53,
        '<', 64,
        '=', 18,
        '>', 66,
        '[', 55,
        ']', 57,
        '^', 79,
        'b', 110,
        'c', 105,
        's', 98,
        '{', 68,
        '|', 78,
        '}', 69,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(38);
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(88);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 1:
      if (lookahead == '\n') ADVANCE(51);
      if (lookahead == '#') ADVANCE(35);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(1);
      END_STATE();
    case 2:
      if (lookahead == '\n') SKIP(3);
      if (lookahead == '#') ADVANCE(13);
      if (lookahead == '.') ADVANCE(14);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(2);
      if (lookahead != 0) ADVANCE(15);
      END_STATE();
    case 3:
      if (lookahead == '\n') SKIP(3);
      if (lookahead == '#') ADVANCE(13);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(2);
      if (lookahead != 0) ADVANCE(15);
      END_STATE();
    case 4:
      if (lookahead == '"') ADVANCE(93);
      if (lookahead == '\\') ADVANCE(36);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '\r') ADVANCE(4);
      END_STATE();
    case 5:
      if (lookahead == '#') ADVANCE(58);
      if (lookahead == ',') ADVANCE(56);
      if (lookahead == ':') ADVANCE(8);
      if (lookahead == ']') ADVANCE(57);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(6);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 6:
      if (lookahead == '#') ADVANCE(35);
      if (lookahead == ',') ADVANCE(56);
      if (lookahead == ']') ADVANCE(57);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(6);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 7:
      if (lookahead == '\'') ADVANCE(93);
      if (lookahead == '\\') ADVANCE(37);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '\r') ADVANCE(7);
      END_STATE();
    case 8:
      if (lookahead == '*') ADVANCE(59);
      END_STATE();
    case 9:
      if (lookahead == '.') ADVANCE(12);
      END_STATE();
    case 10:
      if (lookahead == '.') ADVANCE(12);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(85);
      END_STATE();
    case 11:
      if (lookahead == '.') ADVANCE(30);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(11);
      END_STATE();
    case 12:
      if (lookahead == '.') ADVANCE(115);
      END_STATE();
    case 13:
      if (lookahead == '.') ADVANCE(113);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(112);
      END_STATE();
    case 14:
      if (lookahead == '.') ADVANCE(48);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(48);
      END_STATE();
    case 15:
      if (lookahead == '.') ADVANCE(14);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(15);
      END_STATE();
    case 16:
      if (lookahead == ':') ADVANCE(45);
      END_STATE();
    case 17:
      if (lookahead == '=') ADVANCE(63);
      END_STATE();
    case 18:
      if (lookahead == '=') ADVANCE(62);
      END_STATE();
    case 19:
      if (lookahead == 'e') ADVANCE(22);
      END_STATE();
    case 20:
      if (lookahead == 'n') ADVANCE(23);
      END_STATE();
    case 21:
      if (lookahead == 'n') ADVANCE(27);
      END_STATE();
    case 22:
      if (lookahead == 'n') ADVANCE(28);
      END_STATE();
    case 23:
      if (lookahead == 'o') ADVANCE(26);
      END_STATE();
    case 24:
      if (lookahead == 'o') ADVANCE(21);
      END_STATE();
    case 25:
      if (lookahead == 's') ADVANCE(16);
      END_STATE();
    case 26:
      if (lookahead == 't') ADVANCE(54);
      END_STATE();
    case 27:
      if (lookahead == 't') ADVANCE(19);
      END_STATE();
    case 28:
      if (lookahead == 't') ADVANCE(25);
      END_STATE();
    case 29:
      if (lookahead == '+' ||
          lookahead == '-') ADVANCE(32);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(86);
      END_STATE();
    case 30:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(49);
      END_STATE();
    case 31:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(85);
      END_STATE();
    case 32:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(86);
      END_STATE();
    case 33:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(91);
      END_STATE();
    case 34:
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_') ADVANCE(116);
      END_STATE();
    case 35:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(114);
      END_STATE();
    case 36:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(4);
      END_STATE();
    case 37:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(7);
      END_STATE();
    case 38:
      if (eof) ADVANCE(43);
      ADVANCE_MAP(
        '!', 82,
        '#', 35,
        '$', 34,
        '%', 72,
        '&', 75,
        '\'', 50,
        '(', 60,
        ')', 61,
        '*', 70,
        '+', 77,
        ',', 56,
        '-', 47,
        '.', 84,
        '/', 71,
        '0', 87,
        ':', 52,
        '<', 64,
        '=', 18,
        '>', 66,
        '[', 55,
        ']', 57,
        '^', 79,
        'b', 110,
        'c', 105,
        's', 98,
        '{', 68,
        '|', 78,
        '}', 69,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(38);
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(88);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 39:
      if (eof) ADVANCE(43);
      ADVANCE_MAP(
        '!', 17,
        '#', 35,
        '%', 72,
        '&', 75,
        '(', 60,
        ')', 61,
        '*', 70,
        '+', 77,
        ',', 56,
        '-', 46,
        '.', 83,
        '/', 71,
        '<', 64,
        '=', 18,
        '>', 66,
        '^', 79,
        'c', 24,
        '|', 78,
        '}', 69,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(39);
      END_STATE();
    case 40:
      if (eof) ADVANCE(43);
      ADVANCE_MAP(
        '"', 4,
        '#', 35,
        '$', 34,
        '\'', 7,
        ')', 61,
        ',', 56,
        '.', 10,
        '0', 89,
        '[', 55,
        ']', 57,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(40);
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(90);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 41:
      if (eof) ADVANCE(43);
      ADVANCE_MAP(
        '#', 35,
        '\'', 50,
        '(', 60,
        ')', 61,
        ',', 56,
        ':', 52,
        ']', 57,
        's', 98,
        '{', 68,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(41);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(11);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 42:
      if (eof) ADVANCE(43);
      if (lookahead == '#') ADVANCE(35);
      if (lookahead == '.') ADVANCE(9);
      if (lookahead == 'b') ADVANCE(110);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(42);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(anon_sym_schema_COLON);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_contents_COLON);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(anon_sym_DASH);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(sym_file);
      if (lookahead == '.') ADVANCE(48);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(48);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(sym_version);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(49);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(anon_sym_LF);
      if (lookahead == '\n') ADVANCE(51);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(anon_sym_COLON);
      if (lookahead == '*') ADVANCE(59);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(anon_sym_butnot);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(anon_sym_POUND);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(anon_sym_COLON_STAR);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(anon_sym_EQ_EQ);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_BANG_EQ);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_LT);
      if (lookahead == '<') ADVANCE(73);
      if (lookahead == '=') ADVANCE(65);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_LT_EQ);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(anon_sym_GT);
      if (lookahead == '=') ADVANCE(67);
      if (lookahead == '>') ADVANCE(74);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(anon_sym_GT_EQ);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(anon_sym_PERCENT);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(anon_sym_LT_LT);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(anon_sym_GT_GT);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(anon_sym_AMP);
      if (lookahead == '&') ADVANCE(80);
      if (lookahead == '^') ADVANCE(76);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(anon_sym_AMP_CARET);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(anon_sym_PIPE);
      if (lookahead == '|') ADVANCE(81);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(anon_sym_CARET);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(anon_sym_AMP_AMP);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(anon_sym_PIPE_PIPE);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(anon_sym_BANG);
      if (lookahead == '=') ADVANCE(63);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(anon_sym_DOT);
      if (lookahead == '.') ADVANCE(12);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(sym_float_literal);
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(29);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(85);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(sym_float_literal);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(86);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(sym_int_literal);
      if (lookahead == '.') ADVANCE(30);
      if (lookahead == 'x') ADVANCE(33);
      if (lookahead == 'U' ||
          lookahead == 'u') ADVANCE(92);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(88);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(sym_int_literal);
      if (lookahead == '.') ADVANCE(30);
      if (lookahead == 'U' ||
          lookahead == 'u') ADVANCE(92);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(88);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(sym_int_literal);
      if (lookahead == '.') ADVANCE(31);
      if (lookahead == 'x') ADVANCE(33);
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(29);
      if (lookahead == 'U' ||
          lookahead == 'u') ADVANCE(92);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(90);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(sym_int_literal);
      if (lookahead == '.') ADVANCE(31);
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(29);
      if (lookahead == 'U' ||
          lookahead == 'u') ADVANCE(92);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(90);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(sym_int_literal);
      if (lookahead == 'U' ||
          lookahead == 'u') ADVANCE(92);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(91);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(sym_uint_literal);
      END_STATE();
    case 93:
      ACCEPT_TOKEN(sym_string_literal);
      END_STATE();
    case 94:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == ' ') ADVANCE(20);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 95:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == ':') ADVANCE(44);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 96:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == ':') ADVANCE(45);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 97:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(95);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 98:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(101);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 99:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(102);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 100:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(104);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 101:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h') ADVANCE(99);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 102:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'm') ADVANCE(97);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 103:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(109);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 104:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(108);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 105:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(103);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 106:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 's') ADVANCE(96);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 107:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(94);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 108:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(106);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 109:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(100);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 110:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'u') ADVANCE(107);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 111:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '-' ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(111);
      END_STATE();
    case 112:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead == '.') ADVANCE(113);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(112);
      END_STATE();
    case 113:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead == '.') ADVANCE(48);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(48);
      END_STATE();
    case 114:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(114);
      END_STATE();
    case 115:
      ACCEPT_TOKEN(sym_semgrep_ellipsis);
      END_STATE();
    case 116:
      ACCEPT_TOKEN(sym_semgrep_metavariable);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_') ADVANCE(116);
      END_STATE();
    default:
      return false;
  }
}

static bool ts_lex_keywords(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      ADVANCE_MAP(
        'a', 1,
        'b', 2,
        'c', 3,
        'd', 4,
        'e', 5,
        'f', 6,
        'i', 7,
        'l', 8,
        'm', 9,
        'n', 10,
        'o', 11,
        'r', 12,
        's', 13,
        't', 14,
        'u', 15,
        'w', 16,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(0);
      END_STATE();
    case 1:
      if (lookahead == 'n') ADVANCE(17);
      END_STATE();
    case 2:
      if (lookahead == 'o') ADVANCE(18);
      END_STATE();
    case 3:
      if (lookahead == 'o') ADVANCE(19);
      END_STATE();
    case 4:
      if (lookahead == 'e') ADVANCE(20);
      if (lookahead == 'o') ADVANCE(21);
      if (lookahead == 'u') ADVANCE(22);
      END_STATE();
    case 5:
      if (lookahead == 'x') ADVANCE(23);
      END_STATE();
    case 6:
      if (lookahead == 'a') ADVANCE(24);
      if (lookahead == 'r') ADVANCE(25);
      END_STATE();
    case 7:
      if (lookahead == 'n') ADVANCE(26);
      if (lookahead == 'p') ADVANCE(27);
      END_STATE();
    case 8:
      if (lookahead == 'i') ADVANCE(28);
      END_STATE();
    case 9:
      if (lookahead == 'a') ADVANCE(29);
      if (lookahead == 'o') ADVANCE(30);
      END_STATE();
    case 10:
      if (lookahead == 'u') ADVANCE(31);
      END_STATE();
    case 11:
      if (lookahead == 'r') ADVANCE(32);
      END_STATE();
    case 12:
      if (lookahead == 'e') ADVANCE(33);
      END_STATE();
    case 13:
      if (lookahead == 'c') ADVANCE(34);
      if (lookahead == 't') ADVANCE(35);
      END_STATE();
    case 14:
      if (lookahead == 'i') ADVANCE(36);
      if (lookahead == 'r') ADVANCE(37);
      if (lookahead == 'y') ADVANCE(38);
      END_STATE();
    case 15:
      if (lookahead == 'i') ADVANCE(39);
      END_STATE();
    case 16:
      if (lookahead == 'i') ADVANCE(40);
      END_STATE();
    case 17:
      if (lookahead == 'd') ADVANCE(41);
      END_STATE();
    case 18:
      if (lookahead == 'o') ADVANCE(42);
      END_STATE();
    case 19:
      if (lookahead == 'n') ADVANCE(43);
      END_STATE();
    case 20:
      if (lookahead == 'f') ADVANCE(44);
      END_STATE();
    case 21:
      if (lookahead == 'u') ADVANCE(45);
      END_STATE();
    case 22:
      if (lookahead == 'r') ADVANCE(46);
      END_STATE();
    case 23:
      if (lookahead == 't') ADVANCE(47);
      END_STATE();
    case 24:
      if (lookahead == 'l') ADVANCE(48);
      END_STATE();
    case 25:
      if (lookahead == 'o') ADVANCE(49);
      END_STATE();
    case 26:
      if (lookahead == 't') ADVANCE(50);
      END_STATE();
    case 27:
      if (lookahead == 'a') ADVANCE(51);
      END_STATE();
    case 28:
      if (lookahead == 's') ADVANCE(52);
      END_STATE();
    case 29:
      if (lookahead == 'p') ADVANCE(53);
      END_STATE();
    case 30:
      if (lookahead == 'd') ADVANCE(54);
      END_STATE();
    case 31:
      if (lookahead == 'l') ADVANCE(55);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(anon_sym_or);
      END_STATE();
    case 33:
      if (lookahead == 'l') ADVANCE(56);
      END_STATE();
    case 34:
      if (lookahead == 'h') ADVANCE(57);
      END_STATE();
    case 35:
      if (lookahead == 'r') ADVANCE(58);
      END_STATE();
    case 36:
      if (lookahead == 'm') ADVANCE(59);
      END_STATE();
    case 37:
      if (lookahead == 'u') ADVANCE(60);
      END_STATE();
    case 38:
      if (lookahead == 'p') ADVANCE(61);
      END_STATE();
    case 39:
      if (lookahead == 'n') ADVANCE(62);
      END_STATE();
    case 40:
      if (lookahead == 't') ADVANCE(63);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(anon_sym_and);
      END_STATE();
    case 42:
      if (lookahead == 'l') ADVANCE(64);
      END_STATE();
    case 43:
      if (lookahead == 'd') ADVANCE(65);
      END_STATE();
    case 44:
      if (lookahead == 'i') ADVANCE(66);
      END_STATE();
    case 45:
      if (lookahead == 'b') ADVANCE(67);
      END_STATE();
    case 46:
      if (lookahead == 'a') ADVANCE(68);
      END_STATE();
    case 47:
      if (lookahead == 'e') ADVANCE(69);
      END_STATE();
    case 48:
      if (lookahead == 's') ADVANCE(70);
      END_STATE();
    case 49:
      if (lookahead == 'm') ADVANCE(71);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(anon_sym_int);
      END_STATE();
    case 51:
      if (lookahead == 'd') ADVANCE(72);
      END_STATE();
    case 52:
      if (lookahead == 't') ADVANCE(73);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(anon_sym_map);
      END_STATE();
    case 54:
      if (lookahead == 'e') ADVANCE(74);
      if (lookahead == 'u') ADVANCE(75);
      END_STATE();
    case 55:
      if (lookahead == 'l') ADVANCE(76);
      END_STATE();
    case 56:
      if (lookahead == 'a') ADVANCE(77);
      END_STATE();
    case 57:
      if (lookahead == 'e') ADVANCE(78);
      END_STATE();
    case 58:
      if (lookahead == 'i') ADVANCE(79);
      END_STATE();
    case 59:
      if (lookahead == 'e') ADVANCE(80);
      END_STATE();
    case 60:
      if (lookahead == 'e') ADVANCE(81);
      END_STATE();
    case 61:
      if (lookahead == 'e') ADVANCE(82);
      END_STATE();
    case 62:
      if (lookahead == 't') ADVANCE(83);
      END_STATE();
    case 63:
      if (lookahead == 'h') ADVANCE(84);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_bool);
      END_STATE();
    case 65:
      if (lookahead == 'i') ADVANCE(85);
      END_STATE();
    case 66:
      if (lookahead == 'n') ADVANCE(86);
      END_STATE();
    case 67:
      if (lookahead == 'l') ADVANCE(87);
      END_STATE();
    case 68:
      if (lookahead == 't') ADVANCE(88);
      END_STATE();
    case 69:
      if (lookahead == 'n') ADVANCE(89);
      END_STATE();
    case 70:
      if (lookahead == 'e') ADVANCE(90);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(anon_sym_from);
      END_STATE();
    case 72:
      if (lookahead == 'd') ADVANCE(91);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(anon_sym_list);
      END_STATE();
    case 74:
      if (lookahead == 'l') ADVANCE(92);
      END_STATE();
    case 75:
      if (lookahead == 'l') ADVANCE(93);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(sym_null_literal);
      END_STATE();
    case 77:
      if (lookahead == 't') ADVANCE(94);
      END_STATE();
    case 78:
      if (lookahead == 'm') ADVANCE(95);
      END_STATE();
    case 79:
      if (lookahead == 'n') ADVANCE(96);
      END_STATE();
    case 80:
      if (lookahead == 's') ADVANCE(97);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(anon_sym_true);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(anon_sym_type);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(anon_sym_uint);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(anon_sym_with);
      END_STATE();
    case 85:
      if (lookahead == 't') ADVANCE(98);
      END_STATE();
    case 86:
      if (lookahead == 'e') ADVANCE(99);
      END_STATE();
    case 87:
      if (lookahead == 'e') ADVANCE(100);
      END_STATE();
    case 88:
      if (lookahead == 'i') ADVANCE(101);
      END_STATE();
    case 89:
      if (lookahead == 'd') ADVANCE(102);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(anon_sym_false);
      END_STATE();
    case 91:
      if (lookahead == 'r') ADVANCE(103);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(anon_sym_model);
      END_STATE();
    case 93:
      if (lookahead == 'e') ADVANCE(104);
      END_STATE();
    case 94:
      if (lookahead == 'i') ADVANCE(105);
      END_STATE();
    case 95:
      if (lookahead == 'a') ADVANCE(106);
      END_STATE();
    case 96:
      if (lookahead == 'g') ADVANCE(107);
      END_STATE();
    case 97:
      if (lookahead == 't') ADVANCE(108);
      END_STATE();
    case 98:
      if (lookahead == 'i') ADVANCE(109);
      END_STATE();
    case 99:
      ACCEPT_TOKEN(anon_sym_define);
      END_STATE();
    case 100:
      ACCEPT_TOKEN(anon_sym_double);
      END_STATE();
    case 101:
      if (lookahead == 'o') ADVANCE(110);
      END_STATE();
    case 102:
      ACCEPT_TOKEN(anon_sym_extend);
      END_STATE();
    case 103:
      if (lookahead == 'e') ADVANCE(111);
      END_STATE();
    case 104:
      ACCEPT_TOKEN(anon_sym_module);
      END_STATE();
    case 105:
      if (lookahead == 'o') ADVANCE(112);
      END_STATE();
    case 106:
      ACCEPT_TOKEN(anon_sym_schema);
      END_STATE();
    case 107:
      ACCEPT_TOKEN(anon_sym_string);
      END_STATE();
    case 108:
      if (lookahead == 'a') ADVANCE(113);
      END_STATE();
    case 109:
      if (lookahead == 'o') ADVANCE(114);
      END_STATE();
    case 110:
      if (lookahead == 'n') ADVANCE(115);
      END_STATE();
    case 111:
      if (lookahead == 's') ADVANCE(116);
      END_STATE();
    case 112:
      if (lookahead == 'n') ADVANCE(117);
      END_STATE();
    case 113:
      if (lookahead == 'm') ADVANCE(118);
      END_STATE();
    case 114:
      if (lookahead == 'n') ADVANCE(119);
      END_STATE();
    case 115:
      ACCEPT_TOKEN(anon_sym_duration);
      END_STATE();
    case 116:
      if (lookahead == 's') ADVANCE(120);
      END_STATE();
    case 117:
      if (lookahead == 's') ADVANCE(121);
      END_STATE();
    case 118:
      if (lookahead == 'p') ADVANCE(122);
      END_STATE();
    case 119:
      ACCEPT_TOKEN(anon_sym_condition);
      END_STATE();
    case 120:
      ACCEPT_TOKEN(anon_sym_ipaddress);
      END_STATE();
    case 121:
      ACCEPT_TOKEN(anon_sym_relations);
      END_STATE();
    case 122:
      ACCEPT_TOKEN(anon_sym_timestamp);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 41},
  [2] = {.lex_state = 39},
  [3] = {.lex_state = 39},
  [4] = {.lex_state = 39},
  [5] = {.lex_state = 39},
  [6] = {.lex_state = 39},
  [7] = {.lex_state = 39},
  [8] = {.lex_state = 39},
  [9] = {.lex_state = 39},
  [10] = {.lex_state = 39},
  [11] = {.lex_state = 39},
  [12] = {.lex_state = 39},
  [13] = {.lex_state = 39},
  [14] = {.lex_state = 39},
  [15] = {.lex_state = 39},
  [16] = {.lex_state = 39},
  [17] = {.lex_state = 39},
  [18] = {.lex_state = 39},
  [19] = {.lex_state = 39},
  [20] = {.lex_state = 40},
  [21] = {.lex_state = 40},
  [22] = {.lex_state = 40},
  [23] = {.lex_state = 40},
  [24] = {.lex_state = 40},
  [25] = {.lex_state = 40},
  [26] = {.lex_state = 40},
  [27] = {.lex_state = 40},
  [28] = {.lex_state = 42},
  [29] = {.lex_state = 40},
  [30] = {.lex_state = 42},
  [31] = {.lex_state = 42},
  [32] = {.lex_state = 42},
  [33] = {.lex_state = 42},
  [34] = {.lex_state = 42},
  [35] = {.lex_state = 42},
  [36] = {.lex_state = 42},
  [37] = {.lex_state = 42},
  [38] = {.lex_state = 42},
  [39] = {.lex_state = 42},
  [40] = {.lex_state = 42},
  [41] = {.lex_state = 42},
  [42] = {.lex_state = 42},
  [43] = {.lex_state = 40},
  [44] = {.lex_state = 40},
  [45] = {.lex_state = 40},
  [46] = {.lex_state = 40},
  [47] = {.lex_state = 40},
  [48] = {.lex_state = 40},
  [49] = {.lex_state = 40},
  [50] = {.lex_state = 40},
  [51] = {.lex_state = 5},
  [52] = {.lex_state = 40},
  [53] = {.lex_state = 5},
  [54] = {.lex_state = 40},
  [55] = {.lex_state = 40},
  [56] = {.lex_state = 40},
  [57] = {.lex_state = 40},
  [58] = {.lex_state = 40},
  [59] = {.lex_state = 40},
  [60] = {.lex_state = 40},
  [61] = {.lex_state = 40},
  [62] = {.lex_state = 40},
  [63] = {.lex_state = 40},
  [64] = {.lex_state = 40},
  [65] = {.lex_state = 40},
  [66] = {.lex_state = 40},
  [67] = {.lex_state = 40},
  [68] = {.lex_state = 40},
  [69] = {.lex_state = 41},
  [70] = {.lex_state = 39},
  [71] = {.lex_state = 39},
  [72] = {.lex_state = 39},
  [73] = {.lex_state = 41},
  [74] = {.lex_state = 41},
  [75] = {.lex_state = 40},
  [76] = {.lex_state = 41},
  [77] = {.lex_state = 40},
  [78] = {.lex_state = 41},
  [79] = {.lex_state = 41},
  [80] = {.lex_state = 41},
  [81] = {.lex_state = 40},
  [82] = {.lex_state = 41},
  [83] = {.lex_state = 41},
  [84] = {.lex_state = 41},
  [85] = {.lex_state = 39},
  [86] = {.lex_state = 41},
  [87] = {.lex_state = 40},
  [88] = {.lex_state = 40},
  [89] = {.lex_state = 41},
  [90] = {.lex_state = 40},
  [91] = {.lex_state = 40},
  [92] = {.lex_state = 41},
  [93] = {.lex_state = 41},
  [94] = {.lex_state = 41},
  [95] = {.lex_state = 41},
  [96] = {.lex_state = 40},
  [97] = {.lex_state = 39},
  [98] = {.lex_state = 41},
  [99] = {.lex_state = 41},
  [100] = {.lex_state = 40},
  [101] = {.lex_state = 40},
  [102] = {.lex_state = 40},
  [103] = {.lex_state = 1},
  [104] = {.lex_state = 41},
  [105] = {.lex_state = 41},
  [106] = {.lex_state = 41},
  [107] = {.lex_state = 41},
  [108] = {.lex_state = 41},
  [109] = {.lex_state = 39},
  [110] = {.lex_state = 40},
  [111] = {.lex_state = 3},
  [112] = {.lex_state = 39},
  [113] = {.lex_state = 41},
  [114] = {.lex_state = 1},
  [115] = {.lex_state = 40},
  [116] = {.lex_state = 40},
  [117] = {.lex_state = 40},
  [118] = {.lex_state = 41},
  [119] = {.lex_state = 1},
  [120] = {.lex_state = 41},
  [121] = {.lex_state = 40},
  [122] = {.lex_state = 41},
  [123] = {.lex_state = 40},
  [124] = {.lex_state = 41},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [sym_identifier] = ACTIONS(1),
    [anon_sym_schema_COLON] = ACTIONS(1),
    [anon_sym_contents_COLON] = ACTIONS(1),
    [anon_sym_DASH] = ACTIONS(1),
    [anon_sym_schema] = ACTIONS(1),
    [sym_version] = ACTIONS(1),
    [anon_sym_SQUOTE] = ACTIONS(1),
    [anon_sym_model] = ACTIONS(1),
    [anon_sym_module] = ACTIONS(1),
    [anon_sym_extend] = ACTIONS(1),
    [anon_sym_type] = ACTIONS(1),
    [anon_sym_relations] = ACTIONS(1),
    [anon_sym_define] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_or] = ACTIONS(1),
    [anon_sym_and] = ACTIONS(1),
    [anon_sym_butnot] = ACTIONS(1),
    [anon_sym_LBRACK] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_RBRACK] = ACTIONS(1),
    [anon_sym_with] = ACTIONS(1),
    [anon_sym_from] = ACTIONS(1),
    [anon_sym_POUND] = ACTIONS(1),
    [anon_sym_COLON_STAR] = ACTIONS(1),
    [anon_sym_condition] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_string] = ACTIONS(1),
    [anon_sym_int] = ACTIONS(1),
    [anon_sym_map] = ACTIONS(1),
    [anon_sym_uint] = ACTIONS(1),
    [anon_sym_list] = ACTIONS(1),
    [anon_sym_timestamp] = ACTIONS(1),
    [anon_sym_bool] = ACTIONS(1),
    [anon_sym_duration] = ACTIONS(1),
    [anon_sym_double] = ACTIONS(1),
    [anon_sym_ipaddress] = ACTIONS(1),
    [anon_sym_EQ_EQ] = ACTIONS(1),
    [anon_sym_BANG_EQ] = ACTIONS(1),
    [anon_sym_LT] = ACTIONS(1),
    [anon_sym_LT_EQ] = ACTIONS(1),
    [anon_sym_GT] = ACTIONS(1),
    [anon_sym_GT_EQ] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
    [anon_sym_STAR] = ACTIONS(1),
    [anon_sym_SLASH] = ACTIONS(1),
    [anon_sym_PERCENT] = ACTIONS(1),
    [anon_sym_LT_LT] = ACTIONS(1),
    [anon_sym_GT_GT] = ACTIONS(1),
    [anon_sym_AMP] = ACTIONS(1),
    [anon_sym_AMP_CARET] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_PIPE] = ACTIONS(1),
    [anon_sym_CARET] = ACTIONS(1),
    [anon_sym_AMP_AMP] = ACTIONS(1),
    [anon_sym_PIPE_PIPE] = ACTIONS(1),
    [anon_sym_BANG] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [sym_int_literal] = ACTIONS(1),
    [sym_uint_literal] = ACTIONS(1),
    [anon_sym_true] = ACTIONS(1),
    [anon_sym_false] = ACTIONS(1),
    [sym_null_literal] = ACTIONS(1),
    [sym_comment] = ACTIONS(3),
    [sym_semgrep_ellipsis] = ACTIONS(1),
    [sym_semgrep_metavariable] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(120),
    [sym_project_file] = STATE(122),
    [sym_module_file] = STATE(122),
    [sym_quoted_schema] = STATE(85),
    [sym_model] = STATE(47),
    [sym_module] = STATE(47),
    [anon_sym_schema_COLON] = ACTIONS(5),
    [anon_sym_model] = ACTIONS(7),
    [anon_sym_module] = ACTIONS(9),
    [sym_comment] = ACTIONS(11),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 6,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(15), 1,
      anon_sym_LPAREN,
    ACTIONS(19), 1,
      anon_sym_DOT,
    STATE(9), 1,
      sym_argument_list,
    ACTIONS(17), 4,
      anon_sym_LT,
      anon_sym_GT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(13), 18,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
      anon_sym_PLUS,
      anon_sym_CARET,
      anon_sym_AMP_AMP,
      anon_sym_PIPE_PIPE,
  [39] = 5,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(15), 1,
      anon_sym_LPAREN,
    STATE(9), 1,
      sym_argument_list,
    ACTIONS(17), 4,
      anon_sym_LT,
      anon_sym_GT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(13), 18,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
      anon_sym_PLUS,
      anon_sym_CARET,
      anon_sym_AMP_AMP,
      anon_sym_PIPE_PIPE,
  [75] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(23), 4,
      anon_sym_LT,
      anon_sym_GT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(21), 19,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
      anon_sym_PLUS,
      anon_sym_CARET,
      anon_sym_AMP_AMP,
      anon_sym_PIPE_PIPE,
  [106] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(17), 4,
      anon_sym_LT,
      anon_sym_GT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(13), 18,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
      anon_sym_PLUS,
      anon_sym_CARET,
      anon_sym_AMP_AMP,
      anon_sym_PIPE_PIPE,
  [136] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(27), 4,
      anon_sym_LT,
      anon_sym_GT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(25), 18,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
      anon_sym_PLUS,
      anon_sym_CARET,
      anon_sym_AMP_AMP,
      anon_sym_PIPE_PIPE,
  [166] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(31), 4,
      anon_sym_LT,
      anon_sym_GT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(29), 18,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
      anon_sym_PLUS,
      anon_sym_CARET,
      anon_sym_AMP_AMP,
      anon_sym_PIPE_PIPE,
  [196] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(35), 4,
      anon_sym_LT,
      anon_sym_GT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(33), 18,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
      anon_sym_PLUS,
      anon_sym_CARET,
      anon_sym_AMP_AMP,
      anon_sym_PIPE_PIPE,
  [226] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(39), 4,
      anon_sym_LT,
      anon_sym_GT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(37), 18,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
      anon_sym_PLUS,
      anon_sym_CARET,
      anon_sym_AMP_AMP,
      anon_sym_PIPE_PIPE,
  [256] = 12,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(43), 1,
      anon_sym_COMMA,
    ACTIONS(45), 1,
      anon_sym_RPAREN,
    ACTIONS(53), 1,
      anon_sym_AMP,
    ACTIONS(55), 1,
      anon_sym_PIPE,
    ACTIONS(57), 1,
      anon_sym_AMP_AMP,
    ACTIONS(59), 1,
      anon_sym_PIPE_PIPE,
    STATE(79), 1,
      aux_sym_argument_list_repeat1,
    ACTIONS(49), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(41), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_CARET,
    ACTIONS(47), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
    ACTIONS(51), 6,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
  [304] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(63), 4,
      anon_sym_LT,
      anon_sym_GT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(61), 18,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
      anon_sym_PLUS,
      anon_sym_CARET,
      anon_sym_AMP_AMP,
      anon_sym_PIPE_PIPE,
  [334] = 5,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(53), 1,
      anon_sym_AMP,
    ACTIONS(67), 3,
      anon_sym_LT,
      anon_sym_GT,
      anon_sym_PIPE,
    ACTIONS(51), 6,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
    ACTIONS(65), 12,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_RBRACE,
      anon_sym_PLUS,
      anon_sym_CARET,
      anon_sym_AMP_AMP,
      anon_sym_PIPE_PIPE,
  [368] = 7,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(53), 1,
      anon_sym_AMP,
    ACTIONS(55), 1,
      anon_sym_PIPE,
    ACTIONS(67), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(41), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_CARET,
    ACTIONS(51), 6,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
    ACTIONS(65), 9,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_RBRACE,
      anon_sym_AMP_AMP,
      anon_sym_PIPE_PIPE,
  [406] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(67), 4,
      anon_sym_LT,
      anon_sym_GT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(65), 18,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
      anon_sym_PLUS,
      anon_sym_CARET,
      anon_sym_AMP_AMP,
      anon_sym_PIPE_PIPE,
  [436] = 8,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(53), 1,
      anon_sym_AMP,
    ACTIONS(55), 1,
      anon_sym_PIPE,
    ACTIONS(49), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(41), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_CARET,
    ACTIONS(47), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
    ACTIONS(65), 5,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_AMP_AMP,
      anon_sym_PIPE_PIPE,
    ACTIONS(51), 6,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
  [476] = 9,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(53), 1,
      anon_sym_AMP,
    ACTIONS(55), 1,
      anon_sym_PIPE,
    ACTIONS(57), 1,
      anon_sym_AMP_AMP,
    ACTIONS(49), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(41), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_CARET,
    ACTIONS(47), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
    ACTIONS(65), 4,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_PIPE_PIPE,
    ACTIONS(51), 6,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
  [518] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(71), 4,
      anon_sym_LT,
      anon_sym_GT,
      anon_sym_AMP,
      anon_sym_PIPE,
    ACTIONS(69), 18,
      anon_sym_DASH,
      anon_sym_COMMA,
      anon_sym_RPAREN,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
      anon_sym_RBRACE,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
      anon_sym_PLUS,
      anon_sym_CARET,
      anon_sym_AMP_AMP,
      anon_sym_PIPE_PIPE,
  [548] = 10,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(53), 1,
      anon_sym_AMP,
    ACTIONS(55), 1,
      anon_sym_PIPE,
    ACTIONS(57), 1,
      anon_sym_AMP_AMP,
    ACTIONS(59), 1,
      anon_sym_PIPE_PIPE,
    ACTIONS(49), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(73), 2,
      anon_sym_COMMA,
      anon_sym_RPAREN,
    ACTIONS(41), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_CARET,
    ACTIONS(47), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
    ACTIONS(51), 6,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
  [591] = 10,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(53), 1,
      anon_sym_AMP,
    ACTIONS(55), 1,
      anon_sym_PIPE,
    ACTIONS(57), 1,
      anon_sym_AMP_AMP,
    ACTIONS(59), 1,
      anon_sym_PIPE_PIPE,
    ACTIONS(75), 1,
      anon_sym_RBRACE,
    ACTIONS(49), 2,
      anon_sym_LT,
      anon_sym_GT,
    ACTIONS(41), 3,
      anon_sym_DASH,
      anon_sym_PLUS,
      anon_sym_CARET,
    ACTIONS(47), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_LT_EQ,
      anon_sym_GT_EQ,
    ACTIONS(51), 6,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_AMP_CARET,
  [633] = 11,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(77), 1,
      sym_identifier,
    ACTIONS(79), 1,
      anon_sym_RPAREN,
    ACTIONS(83), 1,
      sym_int_literal,
    ACTIONS(87), 1,
      sym_null_literal,
    STATE(3), 1,
      sym_selector_expression,
    STATE(10), 1,
      sym_expression,
    ACTIONS(81), 2,
      sym_float_literal,
      sym_uint_literal,
    ACTIONS(85), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(89), 3,
      sym_string_literal,
      sym_semgrep_ellipsis,
      sym_semgrep_metavariable,
    STATE(5), 4,
      sym_binary_expression,
      sym_call_expression,
      sym_number_literal,
      sym_boolean_literal,
  [674] = 10,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(77), 1,
      sym_identifier,
    ACTIONS(83), 1,
      sym_int_literal,
    ACTIONS(87), 1,
      sym_null_literal,
    STATE(3), 1,
      sym_selector_expression,
    STATE(16), 1,
      sym_expression,
    ACTIONS(81), 2,
      sym_float_literal,
      sym_uint_literal,
    ACTIONS(85), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(89), 3,
      sym_string_literal,
      sym_semgrep_ellipsis,
      sym_semgrep_metavariable,
    STATE(5), 4,
      sym_binary_expression,
      sym_call_expression,
      sym_number_literal,
      sym_boolean_literal,
  [712] = 10,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(77), 1,
      sym_identifier,
    ACTIONS(83), 1,
      sym_int_literal,
    ACTIONS(87), 1,
      sym_null_literal,
    STATE(3), 1,
      sym_selector_expression,
    STATE(19), 1,
      sym_expression,
    ACTIONS(81), 2,
      sym_float_literal,
      sym_uint_literal,
    ACTIONS(85), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(89), 3,
      sym_string_literal,
      sym_semgrep_ellipsis,
      sym_semgrep_metavariable,
    STATE(5), 4,
      sym_binary_expression,
      sym_call_expression,
      sym_number_literal,
      sym_boolean_literal,
  [750] = 10,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(77), 1,
      sym_identifier,
    ACTIONS(83), 1,
      sym_int_literal,
    ACTIONS(87), 1,
      sym_null_literal,
    STATE(3), 1,
      sym_selector_expression,
    STATE(15), 1,
      sym_expression,
    ACTIONS(81), 2,
      sym_float_literal,
      sym_uint_literal,
    ACTIONS(85), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(89), 3,
      sym_string_literal,
      sym_semgrep_ellipsis,
      sym_semgrep_metavariable,
    STATE(5), 4,
      sym_binary_expression,
      sym_call_expression,
      sym_number_literal,
      sym_boolean_literal,
  [788] = 10,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(77), 1,
      sym_identifier,
    ACTIONS(83), 1,
      sym_int_literal,
    ACTIONS(87), 1,
      sym_null_literal,
    STATE(3), 1,
      sym_selector_expression,
    STATE(14), 1,
      sym_expression,
    ACTIONS(81), 2,
      sym_float_literal,
      sym_uint_literal,
    ACTIONS(85), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(89), 3,
      sym_string_literal,
      sym_semgrep_ellipsis,
      sym_semgrep_metavariable,
    STATE(5), 4,
      sym_binary_expression,
      sym_call_expression,
      sym_number_literal,
      sym_boolean_literal,
  [826] = 10,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(77), 1,
      sym_identifier,
    ACTIONS(83), 1,
      sym_int_literal,
    ACTIONS(87), 1,
      sym_null_literal,
    STATE(3), 1,
      sym_selector_expression,
    STATE(18), 1,
      sym_expression,
    ACTIONS(81), 2,
      sym_float_literal,
      sym_uint_literal,
    ACTIONS(85), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(89), 3,
      sym_string_literal,
      sym_semgrep_ellipsis,
      sym_semgrep_metavariable,
    STATE(5), 4,
      sym_binary_expression,
      sym_call_expression,
      sym_number_literal,
      sym_boolean_literal,
  [864] = 10,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(77), 1,
      sym_identifier,
    ACTIONS(83), 1,
      sym_int_literal,
    ACTIONS(87), 1,
      sym_null_literal,
    STATE(3), 1,
      sym_selector_expression,
    STATE(12), 1,
      sym_expression,
    ACTIONS(81), 2,
      sym_float_literal,
      sym_uint_literal,
    ACTIONS(85), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(89), 3,
      sym_string_literal,
      sym_semgrep_ellipsis,
      sym_semgrep_metavariable,
    STATE(5), 4,
      sym_binary_expression,
      sym_call_expression,
      sym_number_literal,
      sym_boolean_literal,
  [902] = 10,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(77), 1,
      sym_identifier,
    ACTIONS(83), 1,
      sym_int_literal,
    ACTIONS(87), 1,
      sym_null_literal,
    STATE(3), 1,
      sym_selector_expression,
    STATE(13), 1,
      sym_expression,
    ACTIONS(81), 2,
      sym_float_literal,
      sym_uint_literal,
    ACTIONS(85), 2,
      anon_sym_true,
      anon_sym_false,
    ACTIONS(89), 3,
      sym_string_literal,
      sym_semgrep_ellipsis,
      sym_semgrep_metavariable,
    STATE(5), 4,
      sym_binary_expression,
      sym_call_expression,
      sym_number_literal,
      sym_boolean_literal,
  [940] = 6,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(95), 1,
      anon_sym_from,
    STATE(32), 1,
      aux_sym_relation_def_repeat1,
    STATE(87), 1,
      sym_operator,
    ACTIONS(93), 3,
      anon_sym_or,
      anon_sym_and,
      anon_sym_butnot,
    ACTIONS(91), 6,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [966] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(99), 1,
      sym_semgrep_metavariable,
    STATE(84), 1,
      sym_type_identifier,
    ACTIONS(97), 10,
      anon_sym_string,
      anon_sym_int,
      anon_sym_map,
      anon_sym_uint,
      anon_sym_list,
      anon_sym_timestamp,
      anon_sym_bool,
      anon_sym_duration,
      anon_sym_double,
      anon_sym_ipaddress,
  [988] = 6,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(95), 1,
      anon_sym_from,
    STATE(35), 1,
      aux_sym_relation_def_repeat1,
    STATE(87), 1,
      sym_operator,
    ACTIONS(93), 3,
      anon_sym_or,
      anon_sym_and,
      anon_sym_butnot,
    ACTIONS(101), 6,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1014] = 5,
    ACTIONS(11), 1,
      sym_comment,
    STATE(32), 1,
      aux_sym_relation_def_repeat1,
    STATE(87), 1,
      sym_operator,
    ACTIONS(93), 3,
      anon_sym_or,
      anon_sym_and,
      anon_sym_butnot,
    ACTIONS(91), 6,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1037] = 5,
    ACTIONS(11), 1,
      sym_comment,
    STATE(34), 1,
      aux_sym_relation_def_repeat1,
    STATE(87), 1,
      sym_operator,
    ACTIONS(93), 3,
      anon_sym_or,
      anon_sym_and,
      anon_sym_butnot,
    ACTIONS(103), 6,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1060] = 5,
    ACTIONS(11), 1,
      sym_comment,
    STATE(35), 1,
      aux_sym_relation_def_repeat1,
    STATE(87), 1,
      sym_operator,
    ACTIONS(93), 3,
      anon_sym_or,
      anon_sym_and,
      anon_sym_butnot,
    ACTIONS(101), 6,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1083] = 5,
    ACTIONS(11), 1,
      sym_comment,
    STATE(34), 1,
      aux_sym_relation_def_repeat1,
    STATE(87), 1,
      sym_operator,
    ACTIONS(107), 3,
      anon_sym_or,
      anon_sym_and,
      anon_sym_butnot,
    ACTIONS(105), 6,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1106] = 5,
    ACTIONS(11), 1,
      sym_comment,
    STATE(34), 1,
      aux_sym_relation_def_repeat1,
    STATE(87), 1,
      sym_operator,
    ACTIONS(93), 3,
      anon_sym_or,
      anon_sym_and,
      anon_sym_butnot,
    ACTIONS(110), 6,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1129] = 4,
    ACTIONS(11), 1,
      sym_comment,
    STATE(88), 1,
      sym_operator,
    ACTIONS(93), 3,
      anon_sym_or,
      anon_sym_and,
      anon_sym_butnot,
    ACTIONS(91), 6,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1149] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(95), 1,
      anon_sym_from,
    ACTIONS(105), 9,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_or,
      anon_sym_and,
      anon_sym_butnot,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1167] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(112), 9,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_or,
      anon_sym_and,
      anon_sym_butnot,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1182] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(105), 9,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_or,
      anon_sym_and,
      anon_sym_butnot,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1197] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(114), 9,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_or,
      anon_sym_and,
      anon_sym_butnot,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1212] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(116), 9,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_or,
      anon_sym_and,
      anon_sym_butnot,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1227] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(118), 9,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_or,
      anon_sym_and,
      anon_sym_butnot,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1242] = 5,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(122), 1,
      anon_sym_define,
    ACTIONS(124), 1,
      sym_semgrep_ellipsis,
    STATE(48), 2,
      sym_definition,
      aux_sym_relations_repeat1,
    ACTIONS(120), 4,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_condition,
  [1262] = 7,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(127), 1,
      ts_builtin_sym_end,
    ACTIONS(129), 1,
      anon_sym_extend,
    ACTIONS(131), 1,
      anon_sym_type,
    ACTIONS(133), 1,
      anon_sym_condition,
    ACTIONS(135), 1,
      sym_semgrep_ellipsis,
    STATE(45), 3,
      sym_type_declaration,
      sym_condition_declaration,
      aux_sym_module_file_repeat1,
  [1286] = 7,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(137), 1,
      ts_builtin_sym_end,
    ACTIONS(139), 1,
      anon_sym_extend,
    ACTIONS(142), 1,
      anon_sym_type,
    ACTIONS(145), 1,
      anon_sym_condition,
    ACTIONS(148), 1,
      sym_semgrep_ellipsis,
    STATE(45), 3,
      sym_type_declaration,
      sym_condition_declaration,
      aux_sym_module_file_repeat1,
  [1310] = 5,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(122), 1,
      anon_sym_define,
    ACTIONS(153), 1,
      sym_semgrep_ellipsis,
    STATE(43), 2,
      sym_definition,
      aux_sym_relations_repeat1,
    ACTIONS(151), 4,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_condition,
  [1330] = 7,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(129), 1,
      anon_sym_extend,
    ACTIONS(131), 1,
      anon_sym_type,
    ACTIONS(133), 1,
      anon_sym_condition,
    ACTIONS(156), 1,
      ts_builtin_sym_end,
    ACTIONS(158), 1,
      sym_semgrep_ellipsis,
    STATE(44), 3,
      sym_type_declaration,
      sym_condition_declaration,
      aux_sym_module_file_repeat1,
  [1354] = 5,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(162), 1,
      anon_sym_define,
    ACTIONS(165), 1,
      sym_semgrep_ellipsis,
    STATE(48), 2,
      sym_definition,
      aux_sym_relations_repeat1,
    ACTIONS(160), 4,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_condition,
  [1374] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(170), 1,
      anon_sym_relations,
    STATE(63), 1,
      sym_relations,
    ACTIONS(168), 5,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1391] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(170), 1,
      anon_sym_relations,
    STATE(60), 1,
      sym_relations,
    ACTIONS(172), 5,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1408] = 8,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(174), 1,
      anon_sym_COMMA,
    ACTIONS(176), 1,
      anon_sym_RBRACK,
    ACTIONS(178), 1,
      anon_sym_with,
    ACTIONS(180), 1,
      anon_sym_POUND,
    ACTIONS(182), 1,
      anon_sym_COLON_STAR,
    STATE(74), 1,
      sym_conditional,
    STATE(76), 1,
      aux_sym_direct_relationship_repeat1,
  [1433] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(184), 6,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_define,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1445] = 6,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(178), 1,
      anon_sym_with,
    ACTIONS(180), 1,
      anon_sym_POUND,
    ACTIONS(182), 1,
      anon_sym_COLON_STAR,
    STATE(99), 1,
      sym_conditional,
    ACTIONS(186), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
  [1465] = 6,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(188), 1,
      sym_identifier,
    ACTIONS(190), 1,
      anon_sym_LBRACK,
    STATE(31), 1,
      sym_indirect_relation,
    STATE(36), 1,
      sym_direct_relationship,
    STATE(52), 1,
      sym_relation_def,
  [1484] = 6,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(174), 1,
      anon_sym_COMMA,
    ACTIONS(176), 1,
      anon_sym_RBRACK,
    ACTIONS(178), 1,
      anon_sym_with,
    STATE(74), 1,
      sym_conditional,
    STATE(76), 1,
      aux_sym_direct_relationship_repeat1,
  [1503] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(192), 5,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1514] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(194), 5,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1525] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(196), 5,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1536] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(198), 5,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1547] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(168), 5,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1558] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(200), 5,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1569] = 5,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(202), 1,
      sym_identifier,
    ACTIONS(204), 1,
      sym_semgrep_ellipsis,
    ACTIONS(206), 1,
      sym_semgrep_metavariable,
    STATE(55), 2,
      sym_relation_ref,
      sym_all,
  [1586] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(208), 5,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1597] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(210), 5,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1608] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(212), 5,
      ts_builtin_sym_end,
      anon_sym_extend,
      anon_sym_type,
      anon_sym_condition,
      sym_semgrep_ellipsis,
  [1619] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(216), 1,
      anon_sym_RPAREN,
    STATE(73), 1,
      sym_param,
    ACTIONS(214), 2,
      sym_identifier,
      sym_semgrep_metavariable,
  [1633] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(218), 1,
      sym_identifier,
    ACTIONS(220), 1,
      sym_semgrep_metavariable,
    STATE(68), 2,
      sym_relation_ref,
      sym_all,
  [1647] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(178), 1,
      anon_sym_with,
    STATE(99), 1,
      sym_conditional,
    ACTIONS(186), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
  [1661] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(222), 1,
      anon_sym_COMMA,
    ACTIONS(225), 1,
      anon_sym_RPAREN,
    STATE(69), 1,
      aux_sym_condition_declaration_repeat1,
  [1674] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(227), 1,
      ts_builtin_sym_end,
    ACTIONS(229), 1,
      anon_sym_DASH,
    STATE(70), 1,
      aux_sym_contents_repeat1,
  [1687] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(232), 1,
      ts_builtin_sym_end,
    ACTIONS(234), 1,
      anon_sym_DASH,
    STATE(70), 1,
      aux_sym_contents_repeat1,
  [1700] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(234), 1,
      anon_sym_DASH,
    ACTIONS(236), 1,
      ts_builtin_sym_end,
    STATE(71), 1,
      aux_sym_contents_repeat1,
  [1713] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(238), 1,
      anon_sym_COMMA,
    ACTIONS(240), 1,
      anon_sym_RPAREN,
    STATE(80), 1,
      aux_sym_condition_declaration_repeat1,
  [1726] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(174), 1,
      anon_sym_COMMA,
    ACTIONS(242), 1,
      anon_sym_RBRACK,
    STATE(82), 1,
      aux_sym_direct_relationship_repeat1,
  [1739] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(244), 3,
      anon_sym_COMMA,
      anon_sym_RBRACK,
      anon_sym_with,
  [1748] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(174), 1,
      anon_sym_COMMA,
    ACTIONS(242), 1,
      anon_sym_RBRACK,
    STATE(83), 1,
      aux_sym_direct_relationship_repeat1,
  [1761] = 3,
    ACTIONS(11), 1,
      sym_comment,
    STATE(89), 1,
      sym_param,
    ACTIONS(214), 2,
      sym_identifier,
      sym_semgrep_metavariable,
  [1772] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(73), 1,
      anon_sym_RPAREN,
    ACTIONS(246), 1,
      anon_sym_COMMA,
    STATE(78), 1,
      aux_sym_argument_list_repeat1,
  [1785] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(43), 1,
      anon_sym_COMMA,
    ACTIONS(249), 1,
      anon_sym_RPAREN,
    STATE(78), 1,
      aux_sym_argument_list_repeat1,
  [1798] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(238), 1,
      anon_sym_COMMA,
    ACTIONS(251), 1,
      anon_sym_RPAREN,
    STATE(69), 1,
      aux_sym_condition_declaration_repeat1,
  [1811] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(253), 3,
      anon_sym_COMMA,
      anon_sym_RBRACK,
      anon_sym_with,
  [1820] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(174), 1,
      anon_sym_COMMA,
    ACTIONS(255), 1,
      anon_sym_RBRACK,
    STATE(83), 1,
      aux_sym_direct_relationship_repeat1,
  [1833] = 4,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(186), 1,
      anon_sym_RBRACK,
    ACTIONS(257), 1,
      anon_sym_COMMA,
    STATE(83), 1,
      aux_sym_direct_relationship_repeat1,
  [1846] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(260), 2,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [1854] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(262), 1,
      anon_sym_contents_COLON,
    STATE(124), 1,
      sym_contents,
  [1864] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(264), 1,
      anon_sym_LBRACE,
    STATE(64), 1,
      sym_condition_body,
  [1874] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(266), 1,
      sym_identifier,
    STATE(39), 1,
      sym_indirect_relation,
  [1884] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(268), 1,
      sym_identifier,
    STATE(33), 1,
      sym_indirect_relation,
  [1894] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(225), 2,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [1902] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(270), 2,
      sym_identifier,
      sym_semgrep_metavariable,
  [1910] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(272), 2,
      sym_identifier,
      sym_semgrep_metavariable,
  [1918] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(264), 1,
      anon_sym_LBRACE,
    STATE(58), 1,
      sym_condition_body,
  [1928] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(274), 2,
      anon_sym_COMMA,
      anon_sym_RPAREN,
  [1936] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(276), 1,
      anon_sym_SQUOTE,
    STATE(109), 1,
      sym_quoted_version,
  [1946] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(264), 1,
      anon_sym_LBRACE,
    STATE(56), 1,
      sym_condition_body,
  [1956] = 3,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(278), 1,
      anon_sym_schema,
    STATE(57), 1,
      sym_schema,
  [1966] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(227), 2,
      ts_builtin_sym_end,
      anon_sym_DASH,
  [1974] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(280), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
  [1982] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(282), 2,
      anon_sym_COMMA,
      anon_sym_RBRACK,
  [1990] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(284), 2,
      sym_identifier,
      sym_semgrep_metavariable,
  [1998] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(286), 1,
      sym_identifier,
  [2005] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(288), 1,
      sym_identifier,
  [2012] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(290), 1,
      anon_sym_LF,
  [2019] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(292), 1,
      sym_version,
  [2026] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(294), 1,
      anon_sym_COLON,
  [2033] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(176), 1,
      anon_sym_RBRACK,
  [2040] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(296), 1,
      sym_version,
  [2047] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(298), 1,
      anon_sym_LPAREN,
  [2054] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(300), 1,
      anon_sym_contents_COLON,
  [2061] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(302), 1,
      sym_identifier,
  [2068] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(304), 1,
      sym_file,
  [2075] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(306), 1,
      anon_sym_contents_COLON,
  [2082] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(308), 1,
      anon_sym_COLON,
  [2089] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(310), 1,
      anon_sym_LF,
  [2096] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(312), 1,
      anon_sym_type,
  [2103] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(314), 1,
      sym_identifier,
  [2110] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(316), 1,
      sym_identifier,
  [2117] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(318), 1,
      anon_sym_SQUOTE,
  [2124] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(320), 1,
      anon_sym_LF,
  [2131] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(322), 1,
      ts_builtin_sym_end,
  [2138] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(324), 1,
      sym_identifier,
  [2145] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(326), 1,
      ts_builtin_sym_end,
  [2152] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(328), 1,
      sym_identifier,
  [2159] = 2,
    ACTIONS(11), 1,
      sym_comment,
    ACTIONS(330), 1,
      ts_builtin_sym_end,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 39,
  [SMALL_STATE(4)] = 75,
  [SMALL_STATE(5)] = 106,
  [SMALL_STATE(6)] = 136,
  [SMALL_STATE(7)] = 166,
  [SMALL_STATE(8)] = 196,
  [SMALL_STATE(9)] = 226,
  [SMALL_STATE(10)] = 256,
  [SMALL_STATE(11)] = 304,
  [SMALL_STATE(12)] = 334,
  [SMALL_STATE(13)] = 368,
  [SMALL_STATE(14)] = 406,
  [SMALL_STATE(15)] = 436,
  [SMALL_STATE(16)] = 476,
  [SMALL_STATE(17)] = 518,
  [SMALL_STATE(18)] = 548,
  [SMALL_STATE(19)] = 591,
  [SMALL_STATE(20)] = 633,
  [SMALL_STATE(21)] = 674,
  [SMALL_STATE(22)] = 712,
  [SMALL_STATE(23)] = 750,
  [SMALL_STATE(24)] = 788,
  [SMALL_STATE(25)] = 826,
  [SMALL_STATE(26)] = 864,
  [SMALL_STATE(27)] = 902,
  [SMALL_STATE(28)] = 940,
  [SMALL_STATE(29)] = 966,
  [SMALL_STATE(30)] = 988,
  [SMALL_STATE(31)] = 1014,
  [SMALL_STATE(32)] = 1037,
  [SMALL_STATE(33)] = 1060,
  [SMALL_STATE(34)] = 1083,
  [SMALL_STATE(35)] = 1106,
  [SMALL_STATE(36)] = 1129,
  [SMALL_STATE(37)] = 1149,
  [SMALL_STATE(38)] = 1167,
  [SMALL_STATE(39)] = 1182,
  [SMALL_STATE(40)] = 1197,
  [SMALL_STATE(41)] = 1212,
  [SMALL_STATE(42)] = 1227,
  [SMALL_STATE(43)] = 1242,
  [SMALL_STATE(44)] = 1262,
  [SMALL_STATE(45)] = 1286,
  [SMALL_STATE(46)] = 1310,
  [SMALL_STATE(47)] = 1330,
  [SMALL_STATE(48)] = 1354,
  [SMALL_STATE(49)] = 1374,
  [SMALL_STATE(50)] = 1391,
  [SMALL_STATE(51)] = 1408,
  [SMALL_STATE(52)] = 1433,
  [SMALL_STATE(53)] = 1445,
  [SMALL_STATE(54)] = 1465,
  [SMALL_STATE(55)] = 1484,
  [SMALL_STATE(56)] = 1503,
  [SMALL_STATE(57)] = 1514,
  [SMALL_STATE(58)] = 1525,
  [SMALL_STATE(59)] = 1536,
  [SMALL_STATE(60)] = 1547,
  [SMALL_STATE(61)] = 1558,
  [SMALL_STATE(62)] = 1569,
  [SMALL_STATE(63)] = 1586,
  [SMALL_STATE(64)] = 1597,
  [SMALL_STATE(65)] = 1608,
  [SMALL_STATE(66)] = 1619,
  [SMALL_STATE(67)] = 1633,
  [SMALL_STATE(68)] = 1647,
  [SMALL_STATE(69)] = 1661,
  [SMALL_STATE(70)] = 1674,
  [SMALL_STATE(71)] = 1687,
  [SMALL_STATE(72)] = 1700,
  [SMALL_STATE(73)] = 1713,
  [SMALL_STATE(74)] = 1726,
  [SMALL_STATE(75)] = 1739,
  [SMALL_STATE(76)] = 1748,
  [SMALL_STATE(77)] = 1761,
  [SMALL_STATE(78)] = 1772,
  [SMALL_STATE(79)] = 1785,
  [SMALL_STATE(80)] = 1798,
  [SMALL_STATE(81)] = 1811,
  [SMALL_STATE(82)] = 1820,
  [SMALL_STATE(83)] = 1833,
  [SMALL_STATE(84)] = 1846,
  [SMALL_STATE(85)] = 1854,
  [SMALL_STATE(86)] = 1864,
  [SMALL_STATE(87)] = 1874,
  [SMALL_STATE(88)] = 1884,
  [SMALL_STATE(89)] = 1894,
  [SMALL_STATE(90)] = 1902,
  [SMALL_STATE(91)] = 1910,
  [SMALL_STATE(92)] = 1918,
  [SMALL_STATE(93)] = 1928,
  [SMALL_STATE(94)] = 1936,
  [SMALL_STATE(95)] = 1946,
  [SMALL_STATE(96)] = 1956,
  [SMALL_STATE(97)] = 1966,
  [SMALL_STATE(98)] = 1974,
  [SMALL_STATE(99)] = 1982,
  [SMALL_STATE(100)] = 1990,
  [SMALL_STATE(101)] = 1998,
  [SMALL_STATE(102)] = 2005,
  [SMALL_STATE(103)] = 2012,
  [SMALL_STATE(104)] = 2019,
  [SMALL_STATE(105)] = 2026,
  [SMALL_STATE(106)] = 2033,
  [SMALL_STATE(107)] = 2040,
  [SMALL_STATE(108)] = 2047,
  [SMALL_STATE(109)] = 2054,
  [SMALL_STATE(110)] = 2061,
  [SMALL_STATE(111)] = 2068,
  [SMALL_STATE(112)] = 2075,
  [SMALL_STATE(113)] = 2082,
  [SMALL_STATE(114)] = 2089,
  [SMALL_STATE(115)] = 2096,
  [SMALL_STATE(116)] = 2103,
  [SMALL_STATE(117)] = 2110,
  [SMALL_STATE(118)] = 2117,
  [SMALL_STATE(119)] = 2124,
  [SMALL_STATE(120)] = 2131,
  [SMALL_STATE(121)] = 2138,
  [SMALL_STATE(122)] = 2145,
  [SMALL_STATE(123)] = 2152,
  [SMALL_STATE(124)] = 2159,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(94),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(114),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(110),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [13] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression, 1, 0, 0),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [17] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expression, 1, 0, 0),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(121),
  [21] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_selector_expression, 3, 0, 6),
  [23] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_selector_expression, 3, 0, 6),
  [25] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_argument_list, 4, 0, 0),
  [27] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_argument_list, 4, 0, 0),
  [29] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_boolean_literal, 1, 0, 0),
  [31] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_boolean_literal, 1, 0, 0),
  [33] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_number_literal, 1, 0, 0),
  [35] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_number_literal, 1, 0, 0),
  [37] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_call_expression, 2, 0, 3),
  [39] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_call_expression, 2, 0, 3),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [43] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [45] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [47] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [49] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [51] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [53] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [55] = {.entry = {.count = 1, .reusable = false}}, SHIFT(26),
  [57] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [59] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [61] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_argument_list, 2, 0, 0),
  [63] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_argument_list, 2, 0, 0),
  [65] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_expression, 3, 0, 7),
  [67] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary_expression, 3, 0, 7),
  [69] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_argument_list, 3, 0, 0),
  [71] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_argument_list, 3, 0, 0),
  [73] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_argument_list_repeat1, 2, 0, 0),
  [75] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [77] = {.entry = {.count = 1, .reusable = false}}, SHIFT(2),
  [79] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [81] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [83] = {.entry = {.count = 1, .reusable = false}}, SHIFT(8),
  [85] = {.entry = {.count = 1, .reusable = false}}, SHIFT(7),
  [87] = {.entry = {.count = 1, .reusable = false}}, SHIFT(5),
  [89] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [91] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_relation_def, 1, 0, 0),
  [93] = {.entry = {.count = 1, .reusable = true}}, SHIFT(117),
  [95] = {.entry = {.count = 1, .reusable = true}}, SHIFT(123),
  [97] = {.entry = {.count = 1, .reusable = true}}, SHIFT(93),
  [99] = {.entry = {.count = 1, .reusable = true}}, SHIFT(84),
  [101] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_relation_def, 3, 0, 0),
  [103] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_relation_def, 2, 0, 0),
  [105] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_relation_def_repeat1, 2, 0, 0),
  [107] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_relation_def_repeat1, 2, 0, 0), SHIFT_REPEAT(117),
  [110] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_relation_def, 4, 0, 0),
  [112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_direct_relationship, 3, 0, 0),
  [114] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_indirect_relation, 3, 0, 0),
  [116] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_direct_relationship, 4, 0, 0),
  [118] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_direct_relationship, 5, 0, 0),
  [120] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_relations, 2, 0, 0),
  [122] = {.entry = {.count = 1, .reusable = true}}, SHIFT(91),
  [124] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_relations, 2, 0, 0), SHIFT(48),
  [127] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_file, 2, 0, 0),
  [129] = {.entry = {.count = 1, .reusable = true}}, SHIFT(115),
  [131] = {.entry = {.count = 1, .reusable = true}}, SHIFT(100),
  [133] = {.entry = {.count = 1, .reusable = true}}, SHIFT(116),
  [135] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [137] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_module_file_repeat1, 2, 0, 0),
  [139] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_module_file_repeat1, 2, 0, 0), SHIFT_REPEAT(115),
  [142] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_module_file_repeat1, 2, 0, 0), SHIFT_REPEAT(100),
  [145] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_module_file_repeat1, 2, 0, 0), SHIFT_REPEAT(116),
  [148] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_module_file_repeat1, 2, 0, 0), SHIFT_REPEAT(45),
  [151] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_relations, 1, 0, 0),
  [153] = {.entry = {.count = 2, .reusable = true}}, REDUCE(sym_relations, 1, 0, 0), SHIFT(43),
  [156] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module_file, 1, 0, 0),
  [158] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [160] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_relations_repeat1, 2, 0, 0),
  [162] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_relations_repeat1, 2, 0, 0), SHIFT_REPEAT(91),
  [165] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_relations_repeat1, 2, 0, 0), SHIFT_REPEAT(48),
  [168] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_declaration, 4, 0, 0),
  [170] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [172] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_declaration, 3, 0, 0),
  [174] = {.entry = {.count = 1, .reusable = true}}, SHIFT(67),
  [176] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [178] = {.entry = {.count = 1, .reusable = true}}, SHIFT(101),
  [180] = {.entry = {.count = 1, .reusable = true}}, SHIFT(102),
  [182] = {.entry = {.count = 1, .reusable = true}}, SHIFT(75),
  [184] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_definition, 4, 0, 5),
  [186] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_direct_relationship_repeat1, 2, 0, 0),
  [188] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [190] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [192] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_condition_declaration, 6, 0, 2),
  [194] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_model, 3, 0, 0),
  [196] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_condition_declaration, 7, 0, 4),
  [198] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_condition_body, 3, 0, 0),
  [200] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_schema, 2, 0, 0),
  [202] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [204] = {.entry = {.count = 1, .reusable = true}}, SHIFT(106),
  [206] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [208] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_declaration, 5, 0, 0),
  [210] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_condition_declaration, 5, 0, 1),
  [212] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_module, 2, 0, 0),
  [214] = {.entry = {.count = 1, .reusable = true}}, SHIFT(105),
  [216] = {.entry = {.count = 1, .reusable = true}}, SHIFT(86),
  [218] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [220] = {.entry = {.count = 1, .reusable = true}}, SHIFT(68),
  [222] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_condition_declaration_repeat1, 2, 0, 0), SHIFT_REPEAT(77),
  [225] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_condition_declaration_repeat1, 2, 0, 0),
  [227] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_contents_repeat1, 2, 0, 0),
  [229] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_contents_repeat1, 2, 0, 0), SHIFT_REPEAT(111),
  [232] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_contents, 2, 0, 0),
  [234] = {.entry = {.count = 1, .reusable = true}}, SHIFT(111),
  [236] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_contents, 1, 0, 0),
  [238] = {.entry = {.count = 1, .reusable = true}}, SHIFT(77),
  [240] = {.entry = {.count = 1, .reusable = true}}, SHIFT(95),
  [242] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [244] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_all, 2, 0, 8),
  [246] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_argument_list_repeat1, 2, 0, 0), SHIFT_REPEAT(25),
  [249] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [251] = {.entry = {.count = 1, .reusable = true}}, SHIFT(92),
  [253] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_relation_ref, 3, 0, 9),
  [255] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [257] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_direct_relationship_repeat1, 2, 0, 0), SHIFT_REPEAT(67),
  [260] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_param, 3, 0, 0),
  [262] = {.entry = {.count = 1, .reusable = true}}, SHIFT(72),
  [264] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [266] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [268] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [270] = {.entry = {.count = 1, .reusable = true}}, SHIFT(119),
  [272] = {.entry = {.count = 1, .reusable = true}}, SHIFT(113),
  [274] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_identifier, 1, 0, 0),
  [276] = {.entry = {.count = 1, .reusable = true}}, SHIFT(107),
  [278] = {.entry = {.count = 1, .reusable = true}}, SHIFT(104),
  [280] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_conditional, 2, 0, 0),
  [282] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_direct_relationship_repeat1, 3, 0, 0),
  [284] = {.entry = {.count = 1, .reusable = true}}, SHIFT(103),
  [286] = {.entry = {.count = 1, .reusable = true}}, SHIFT(98),
  [288] = {.entry = {.count = 1, .reusable = true}}, SHIFT(81),
  [290] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [292] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [294] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [296] = {.entry = {.count = 1, .reusable = true}}, SHIFT(118),
  [298] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [300] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quoted_schema, 2, 0, 0),
  [302] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [304] = {.entry = {.count = 1, .reusable = true}}, SHIFT(97),
  [306] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_quoted_version, 3, 0, 0),
  [308] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [310] = {.entry = {.count = 1, .reusable = true}}, SHIFT(96),
  [312] = {.entry = {.count = 1, .reusable = true}}, SHIFT(90),
  [314] = {.entry = {.count = 1, .reusable = true}}, SHIFT(108),
  [316] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_operator, 1, 0, 0),
  [318] = {.entry = {.count = 1, .reusable = true}}, SHIFT(112),
  [320] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [322] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [324] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [326] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1, 0, 0),
  [328] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [330] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_project_file, 2, 0, 0),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef TREE_SITTER_HIDE_SYMBOLS
#define TS_PUBLIC
#elif defined(_WIN32)
#define TS_PUBLIC __declspec(dllexport)
#else
#define TS_PUBLIC __attribute__((visibility("default")))
#endif

TS_PUBLIC const TSLanguage *tree_sitter_fga(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .field_names = ts_field_names,
    .field_map_slices = ts_field_map_slices,
    .field_map_entries = ts_field_map_entries,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .keyword_lex_fn = ts_lex_keywords,
    .keyword_capture_token = sym_identifier,
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
