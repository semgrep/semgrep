#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#ifdef _MSC_VER
#pragma optimize("", off)
#elif defined(__clang__)
#pragma clang optimize off
#elif defined(__GNUC__)
#pragma GCC optimize ("O0")
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 428
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 168
#define ALIAS_COUNT 61
#define TOKEN_COUNT 89
#define EXTERNAL_TOKEN_COUNT 5
#define FIELD_COUNT 9
#define MAX_ALIAS_SEQUENCE_LENGTH 5
#define PRODUCTION_ID_COUNT 69

enum ts_symbol_identifiers {
  anon_sym_LF = 1,
  aux_sym_from_instruction_token1 = 2,
  aux_sym_from_instruction_token2 = 3,
  aux_sym_run_instruction_token1 = 4,
  aux_sym_cmd_instruction_token1 = 5,
  aux_sym_label_instruction_token1 = 6,
  aux_sym_expose_instruction_token1 = 7,
  aux_sym_env_instruction_token1 = 8,
  aux_sym_add_instruction_token1 = 9,
  aux_sym_copy_instruction_token1 = 10,
  aux_sym_entrypoint_instruction_token1 = 11,
  aux_sym_volume_instruction_token1 = 12,
  aux_sym_user_instruction_token1 = 13,
  anon_sym_COLON = 14,
  aux_sym_user_name_or_group_token1 = 15,
  aux_sym_immediate_user_name_or_group_fragment_token1 = 16,
  aux_sym_workdir_instruction_token1 = 17,
  aux_sym_arg_instruction_token1 = 18,
  aux_sym_arg_instruction_token2 = 19,
  anon_sym_EQ = 20,
  aux_sym_onbuild_instruction_token1 = 21,
  aux_sym_stopsignal_instruction_token1 = 22,
  aux_sym_stopsignal_value_token1 = 23,
  aux_sym_stopsignal_value_token2 = 24,
  aux_sym_healthcheck_instruction_token1 = 25,
  anon_sym_NONE = 26,
  aux_sym_shell_instruction_token1 = 27,
  aux_sym_maintainer_instruction_token1 = 28,
  aux_sym_maintainer_instruction_token2 = 29,
  aux_sym_cross_build_instruction_token1 = 30,
  aux_sym_path_token1 = 31,
  aux_sym_path_token2 = 32,
  aux_sym_path_token3 = 33,
  aux_sym_path_with_heredoc_token1 = 34,
  anon_sym_DOLLAR = 35,
  anon_sym_DOLLAR2 = 36,
  anon_sym_LBRACE = 37,
  aux_sym_expansion_body_token1 = 38,
  anon_sym_RBRACE = 39,
  sym_variable = 40,
  aux_sym_spaced_env_pair_token1 = 41,
  aux_sym_env_key_token1 = 42,
  aux_sym_expose_port_token1 = 43,
  anon_sym_SLASHtcp = 44,
  anon_sym_SLASHudp = 45,
  aux_sym_label_pair_token1 = 46,
  aux_sym_image_name_token1 = 47,
  aux_sym_image_name_token2 = 48,
  aux_sym_image_tag_token1 = 49,
  anon_sym_AT = 50,
  aux_sym_image_digest_token1 = 51,
  anon_sym_DASH_DASH = 52,
  aux_sym_param_token1 = 53,
  aux_sym_param_token2 = 54,
  anon_sym_mount = 55,
  anon_sym_COMMA = 56,
  aux_sym_mount_param_param_token1 = 57,
  aux_sym_image_alias_token1 = 58,
  aux_sym_image_alias_token2 = 59,
  aux_sym_shell_fragment_token1 = 60,
  aux_sym_shell_fragment_token2 = 61,
  aux_sym_shell_fragment_token3 = 62,
  aux_sym_shell_fragment_token4 = 63,
  sym_line_continuation = 64,
  sym_required_line_continuation = 65,
  anon_sym_LBRACK = 66,
  anon_sym_COMMA2 = 67,
  anon_sym_RBRACK = 68,
  anon_sym_DQUOTE = 69,
  aux_sym_json_string_token1 = 70,
  sym_json_escape_sequence = 71,
  aux_sym_double_quoted_string_token1 = 72,
  anon_sym_BSLASH = 73,
  anon_sym_SQUOTE = 74,
  aux_sym_single_quoted_string_token1 = 75,
  aux_sym_unquoted_string_token1 = 76,
  anon_sym_BSLASH2 = 77,
  sym_double_quoted_escape_sequence = 78,
  sym_single_quoted_escape_sequence = 79,
  sym_non_newline_whitespace = 80,
  sym_comment = 81,
  sym_semgrep_metavariable = 82,
  sym_semgrep_ellipsis = 83,
  sym_heredoc_marker = 84,
  sym_heredoc_line = 85,
  sym_heredoc_end = 86,
  sym_heredoc_nl = 87,
  sym_error_sentinel = 88,
  sym_source_file = 89,
  sym_instruction = 90,
  sym_from_instruction = 91,
  sym_run_instruction = 92,
  sym_cmd_instruction = 93,
  sym_label_instruction = 94,
  sym_expose_instruction = 95,
  sym_env_instruction = 96,
  sym_add_instruction = 97,
  sym_copy_instruction = 98,
  sym_entrypoint_instruction = 99,
  sym_volume_instruction = 100,
  sym_user_instruction = 101,
  sym_user_name_or_group = 102,
  sym_immediate_user_name_or_group = 103,
  sym_immediate_user_name_or_group_fragment = 104,
  sym_workdir_instruction = 105,
  sym_arg_instruction = 106,
  sym_onbuild_instruction = 107,
  sym_stopsignal_instruction = 108,
  sym_stopsignal_value = 109,
  sym_healthcheck_instruction = 110,
  sym_shell_instruction = 111,
  sym_maintainer_instruction = 112,
  sym_cross_build_instruction = 113,
  sym_heredoc_block = 114,
  sym_path = 115,
  sym_path_with_heredoc = 116,
  sym_expansion = 117,
  sym_immediate_expansion = 118,
  sym_imm_expansion = 119,
  sym_expansion_body = 120,
  sym_env_pair = 121,
  sym_spaced_env_pair = 122,
  sym_env_key = 123,
  sym_expose_port = 124,
  sym_label_pair = 125,
  sym_image_spec = 126,
  sym_image_name = 127,
  sym_image_tag = 128,
  sym_image_digest = 129,
  sym_param = 130,
  sym_mount_param = 131,
  sym_mount_param_param = 132,
  sym_image_alias = 133,
  sym_shell_command = 134,
  sym_shell_fragment = 135,
  sym_json_string_array = 136,
  sym_json_string = 137,
  sym_double_quoted_string = 138,
  sym_single_quoted_string = 139,
  sym_unquoted_string = 140,
  sym_array_element = 141,
  aux_sym_source_file_repeat1 = 142,
  aux_sym_run_instruction_repeat1 = 143,
  aux_sym_run_instruction_repeat2 = 144,
  aux_sym_label_instruction_repeat1 = 145,
  aux_sym_expose_instruction_repeat1 = 146,
  aux_sym_env_instruction_repeat1 = 147,
  aux_sym_add_instruction_repeat1 = 148,
  aux_sym_add_instruction_repeat2 = 149,
  aux_sym_volume_instruction_repeat1 = 150,
  aux_sym_user_name_or_group_repeat1 = 151,
  aux_sym_stopsignal_value_repeat1 = 152,
  aux_sym_healthcheck_instruction_repeat1 = 153,
  aux_sym_heredoc_block_repeat1 = 154,
  aux_sym_path_repeat1 = 155,
  aux_sym_image_name_repeat1 = 156,
  aux_sym_image_tag_repeat1 = 157,
  aux_sym_image_digest_repeat1 = 158,
  aux_sym_mount_param_repeat1 = 159,
  aux_sym_image_alias_repeat1 = 160,
  aux_sym_shell_command_repeat1 = 161,
  aux_sym_shell_fragment_repeat1 = 162,
  aux_sym_json_string_array_repeat1 = 163,
  aux_sym_json_string_repeat1 = 164,
  aux_sym_double_quoted_string_repeat1 = 165,
  aux_sym_single_quoted_string_repeat1 = 166,
  aux_sym_unquoted_string_repeat1 = 167,
  alias_sym_imm_tok_at = 168,
  alias_sym_imm_tok_bslashspace = 169,
  alias_sym_imm_tok_colon = 170,
  alias_sym_imm_tok_comma = 171,
  alias_sym_imm_tok_dollar = 172,
  alias_sym_imm_tok_eq = 173,
  alias_sym_imm_tok_lcurl = 174,
  alias_sym_imm_tok_mount = 175,
  alias_sym_imm_tok_pat_0ab9261 = 176,
  alias_sym_imm_tok_pat_0c7fc22 = 177,
  alias_sym_imm_tok_pat_2b37705 = 178,
  alias_sym_imm_tok_pat_3a2a380 = 179,
  alias_sym_imm_tok_pat_3d340f6 = 180,
  alias_sym_imm_tok_pat_441cd81 = 181,
  alias_sym_imm_tok_pat_589b0f8 = 182,
  alias_sym_imm_tok_pat_7642c4f = 183,
  alias_sym_imm_tok_pat_8713919 = 184,
  alias_sym_imm_tok_pat_9a14b5c = 185,
  alias_sym_imm_tok_pat_9f6bbb9 = 186,
  alias_sym_imm_tok_pat_bcfc287 = 187,
  alias_sym_imm_tok_pat_d2727a0 = 188,
  alias_sym_imm_tok_pat_f43f746 = 189,
  alias_sym_imm_tok_pat_f46f69d = 190,
  alias_sym_imm_tok_pat_f6e1de8 = 191,
  alias_sym_imm_tok_rcurl = 192,
  alias_sym_pat_05444c2 = 193,
  alias_sym_pat_0851d06 = 194,
  alias_sym_pat_2b6adbc = 195,
  alias_sym_pat_4128122 = 196,
  alias_sym_pat_441cd81 = 197,
  alias_sym_pat_4a2f38a = 198,
  alias_sym_pat_4de4cb9 = 199,
  alias_sym_pat_4fd4a56 = 200,
  alias_sym_pat_8165e5f = 201,
  alias_sym_pat_9873c86 = 202,
  alias_sym_pat_9a14b5c = 203,
  alias_sym_pat_a667757 = 204,
  alias_sym_pat_add = 205,
  alias_sym_pat_arg = 206,
  alias_sym_pat_as = 207,
  alias_sym_pat_b1120d3 = 208,
  alias_sym_pat_cmd = 209,
  alias_sym_pat_copy = 210,
  alias_sym_pat_e0f3805 = 211,
  alias_sym_pat_ea34a52 = 212,
  alias_sym_pat_eda9032 = 213,
  alias_sym_pat_entr = 214,
  alias_sym_pat_env = 215,
  alias_sym_pat_expose = 216,
  alias_sym_pat_f8ab07f = 217,
  alias_sym_pat_from = 218,
  alias_sym_pat_heal = 219,
  alias_sym_pat_label = 220,
  alias_sym_pat_main = 221,
  alias_sym_pat_onbu = 222,
  alias_sym_pat_run = 223,
  alias_sym_pat_shell = 224,
  alias_sym_pat_stop = 225,
  alias_sym_pat_user = 226,
  alias_sym_pat_volume = 227,
  alias_sym_pat_work = 228,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_LF] = "\n",
  [aux_sym_from_instruction_token1] = "from_instruction_token1",
  [aux_sym_from_instruction_token2] = "from_instruction_token2",
  [aux_sym_run_instruction_token1] = "run_instruction_token1",
  [aux_sym_cmd_instruction_token1] = "cmd_instruction_token1",
  [aux_sym_label_instruction_token1] = "label_instruction_token1",
  [aux_sym_expose_instruction_token1] = "expose_instruction_token1",
  [aux_sym_env_instruction_token1] = "env_instruction_token1",
  [aux_sym_add_instruction_token1] = "add_instruction_token1",
  [aux_sym_copy_instruction_token1] = "copy_instruction_token1",
  [aux_sym_entrypoint_instruction_token1] = "entrypoint_instruction_token1",
  [aux_sym_volume_instruction_token1] = "volume_instruction_token1",
  [aux_sym_user_instruction_token1] = "user_instruction_token1",
  [anon_sym_COLON] = ":",
  [aux_sym_user_name_or_group_token1] = "user_name_or_group_token1",
  [aux_sym_immediate_user_name_or_group_fragment_token1] = "immediate_user_name_or_group_fragment_token1",
  [aux_sym_workdir_instruction_token1] = "workdir_instruction_token1",
  [aux_sym_arg_instruction_token1] = "arg_instruction_token1",
  [aux_sym_arg_instruction_token2] = "arg_instruction_token2",
  [anon_sym_EQ] = "=",
  [aux_sym_onbuild_instruction_token1] = "onbuild_instruction_token1",
  [aux_sym_stopsignal_instruction_token1] = "stopsignal_instruction_token1",
  [aux_sym_stopsignal_value_token1] = "stopsignal_value_token1",
  [aux_sym_stopsignal_value_token2] = "stopsignal_value_token2",
  [aux_sym_healthcheck_instruction_token1] = "healthcheck_instruction_token1",
  [anon_sym_NONE] = "NONE",
  [aux_sym_shell_instruction_token1] = "shell_instruction_token1",
  [aux_sym_maintainer_instruction_token1] = "maintainer_instruction_token1",
  [aux_sym_maintainer_instruction_token2] = "maintainer_instruction_token2",
  [aux_sym_cross_build_instruction_token1] = "cross_build_instruction_token1",
  [aux_sym_path_token1] = "path_token1",
  [aux_sym_path_token2] = "path_token2",
  [aux_sym_path_token3] = "path_token3",
  [aux_sym_path_with_heredoc_token1] = "path_with_heredoc_token1",
  [anon_sym_DOLLAR] = "$",
  [anon_sym_DOLLAR2] = "$",
  [anon_sym_LBRACE] = "{",
  [aux_sym_expansion_body_token1] = "expansion_body_token1",
  [anon_sym_RBRACE] = "}",
  [sym_variable] = "variable",
  [aux_sym_spaced_env_pair_token1] = "spaced_env_pair_token1",
  [aux_sym_env_key_token1] = "env_key_token1",
  [aux_sym_expose_port_token1] = "expose_port_token1",
  [anon_sym_SLASHtcp] = "/tcp",
  [anon_sym_SLASHudp] = "/udp",
  [aux_sym_label_pair_token1] = "label_pair_token1",
  [aux_sym_image_name_token1] = "image_name_token1",
  [aux_sym_image_name_token2] = "image_name_token2",
  [aux_sym_image_tag_token1] = "image_tag_token1",
  [anon_sym_AT] = "@",
  [aux_sym_image_digest_token1] = "image_digest_token1",
  [anon_sym_DASH_DASH] = "--",
  [aux_sym_param_token1] = "param_token1",
  [aux_sym_param_token2] = "param_token2",
  [anon_sym_mount] = "mount",
  [anon_sym_COMMA] = ",",
  [aux_sym_mount_param_param_token1] = "mount_param_param_token1",
  [aux_sym_image_alias_token1] = "image_alias_token1",
  [aux_sym_image_alias_token2] = "image_alias_token2",
  [aux_sym_shell_fragment_token1] = "shell_fragment_token1",
  [aux_sym_shell_fragment_token2] = "shell_fragment_token2",
  [aux_sym_shell_fragment_token3] = "shell_fragment_token3",
  [aux_sym_shell_fragment_token4] = "shell_fragment_token4",
  [sym_line_continuation] = "line_continuation",
  [sym_required_line_continuation] = "required_line_continuation",
  [anon_sym_LBRACK] = "[",
  [anon_sym_COMMA2] = ",",
  [anon_sym_RBRACK] = "]",
  [anon_sym_DQUOTE] = "\"",
  [aux_sym_json_string_token1] = "json_string_token1",
  [sym_json_escape_sequence] = "json_escape_sequence",
  [aux_sym_double_quoted_string_token1] = "double_quoted_string_token1",
  [anon_sym_BSLASH] = "\\",
  [anon_sym_SQUOTE] = "'",
  [aux_sym_single_quoted_string_token1] = "single_quoted_string_token1",
  [aux_sym_unquoted_string_token1] = "unquoted_string_token1",
  [anon_sym_BSLASH2] = "\\ ",
  [sym_double_quoted_escape_sequence] = "double_quoted_escape_sequence",
  [sym_single_quoted_escape_sequence] = "single_quoted_escape_sequence",
  [sym_non_newline_whitespace] = "non_newline_whitespace",
  [sym_comment] = "comment",
  [sym_semgrep_metavariable] = "semgrep_metavariable",
  [sym_semgrep_ellipsis] = "semgrep_ellipsis",
  [sym_heredoc_marker] = "heredoc_marker",
  [sym_heredoc_line] = "heredoc_line",
  [sym_heredoc_end] = "heredoc_end",
  [sym_heredoc_nl] = "heredoc_nl",
  [sym_error_sentinel] = "error_sentinel",
  [sym_source_file] = "source_file",
  [sym_instruction] = "instruction",
  [sym_from_instruction] = "from_instruction",
  [sym_run_instruction] = "run_instruction",
  [sym_cmd_instruction] = "cmd_instruction",
  [sym_label_instruction] = "label_instruction",
  [sym_expose_instruction] = "expose_instruction",
  [sym_env_instruction] = "env_instruction",
  [sym_add_instruction] = "add_instruction",
  [sym_copy_instruction] = "copy_instruction",
  [sym_entrypoint_instruction] = "entrypoint_instruction",
  [sym_volume_instruction] = "volume_instruction",
  [sym_user_instruction] = "user_instruction",
  [sym_user_name_or_group] = "user_name_or_group",
  [sym_immediate_user_name_or_group] = "immediate_user_name_or_group",
  [sym_immediate_user_name_or_group_fragment] = "immediate_user_name_or_group_fragment",
  [sym_workdir_instruction] = "workdir_instruction",
  [sym_arg_instruction] = "arg_instruction",
  [sym_onbuild_instruction] = "onbuild_instruction",
  [sym_stopsignal_instruction] = "stopsignal_instruction",
  [sym_stopsignal_value] = "stopsignal_value",
  [sym_healthcheck_instruction] = "healthcheck_instruction",
  [sym_shell_instruction] = "shell_instruction",
  [sym_maintainer_instruction] = "maintainer_instruction",
  [sym_cross_build_instruction] = "cross_build_instruction",
  [sym_heredoc_block] = "heredoc_block",
  [sym_path] = "path",
  [sym_path_with_heredoc] = "path_with_heredoc",
  [sym_expansion] = "expansion",
  [sym_immediate_expansion] = "immediate_expansion",
  [sym_imm_expansion] = "imm_expansion",
  [sym_expansion_body] = "expansion_body",
  [sym_env_pair] = "env_pair",
  [sym_spaced_env_pair] = "spaced_env_pair",
  [sym_env_key] = "env_key",
  [sym_expose_port] = "expose_port",
  [sym_label_pair] = "label_pair",
  [sym_image_spec] = "image_spec",
  [sym_image_name] = "image_name",
  [sym_image_tag] = "image_tag",
  [sym_image_digest] = "image_digest",
  [sym_param] = "param",
  [sym_mount_param] = "mount_param",
  [sym_mount_param_param] = "mount_param_param",
  [sym_image_alias] = "image_alias",
  [sym_shell_command] = "shell_command",
  [sym_shell_fragment] = "shell_fragment",
  [sym_json_string_array] = "json_string_array",
  [sym_json_string] = "json_string",
  [sym_double_quoted_string] = "double_quoted_string",
  [sym_single_quoted_string] = "single_quoted_string",
  [sym_unquoted_string] = "unquoted_string",
  [sym_array_element] = "array_element",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_run_instruction_repeat1] = "run_instruction_repeat1",
  [aux_sym_run_instruction_repeat2] = "run_instruction_repeat2",
  [aux_sym_label_instruction_repeat1] = "label_instruction_repeat1",
  [aux_sym_expose_instruction_repeat1] = "expose_instruction_repeat1",
  [aux_sym_env_instruction_repeat1] = "env_instruction_repeat1",
  [aux_sym_add_instruction_repeat1] = "add_instruction_repeat1",
  [aux_sym_add_instruction_repeat2] = "add_instruction_repeat2",
  [aux_sym_volume_instruction_repeat1] = "volume_instruction_repeat1",
  [aux_sym_user_name_or_group_repeat1] = "user_name_or_group_repeat1",
  [aux_sym_stopsignal_value_repeat1] = "stopsignal_value_repeat1",
  [aux_sym_healthcheck_instruction_repeat1] = "healthcheck_instruction_repeat1",
  [aux_sym_heredoc_block_repeat1] = "heredoc_block_repeat1",
  [aux_sym_path_repeat1] = "path_repeat1",
  [aux_sym_image_name_repeat1] = "image_name_repeat1",
  [aux_sym_image_tag_repeat1] = "image_tag_repeat1",
  [aux_sym_image_digest_repeat1] = "image_digest_repeat1",
  [aux_sym_mount_param_repeat1] = "mount_param_repeat1",
  [aux_sym_image_alias_repeat1] = "image_alias_repeat1",
  [aux_sym_shell_command_repeat1] = "shell_command_repeat1",
  [aux_sym_shell_fragment_repeat1] = "shell_fragment_repeat1",
  [aux_sym_json_string_array_repeat1] = "json_string_array_repeat1",
  [aux_sym_json_string_repeat1] = "json_string_repeat1",
  [aux_sym_double_quoted_string_repeat1] = "double_quoted_string_repeat1",
  [aux_sym_single_quoted_string_repeat1] = "single_quoted_string_repeat1",
  [aux_sym_unquoted_string_repeat1] = "unquoted_string_repeat1",
  [alias_sym_imm_tok_at] = "imm_tok_at",
  [alias_sym_imm_tok_bslashspace] = "imm_tok_bslashspace",
  [alias_sym_imm_tok_colon] = "imm_tok_colon",
  [alias_sym_imm_tok_comma] = "imm_tok_comma",
  [alias_sym_imm_tok_dollar] = "imm_tok_dollar",
  [alias_sym_imm_tok_eq] = "imm_tok_eq",
  [alias_sym_imm_tok_lcurl] = "imm_tok_lcurl",
  [alias_sym_imm_tok_mount] = "imm_tok_mount",
  [alias_sym_imm_tok_pat_0ab9261] = "imm_tok_pat_0ab9261",
  [alias_sym_imm_tok_pat_0c7fc22] = "imm_tok_pat_0c7fc22",
  [alias_sym_imm_tok_pat_2b37705] = "imm_tok_pat_2b37705",
  [alias_sym_imm_tok_pat_3a2a380] = "imm_tok_pat_3a2a380",
  [alias_sym_imm_tok_pat_3d340f6] = "imm_tok_pat_3d340f6",
  [alias_sym_imm_tok_pat_441cd81] = "imm_tok_pat_441cd81",
  [alias_sym_imm_tok_pat_589b0f8] = "imm_tok_pat_589b0f8",
  [alias_sym_imm_tok_pat_7642c4f] = "imm_tok_pat_7642c4f",
  [alias_sym_imm_tok_pat_8713919] = "imm_tok_pat_8713919",
  [alias_sym_imm_tok_pat_9a14b5c] = "imm_tok_pat_9a14b5c",
  [alias_sym_imm_tok_pat_9f6bbb9] = "imm_tok_pat_9f6bbb9",
  [alias_sym_imm_tok_pat_bcfc287] = "imm_tok_pat_bcfc287",
  [alias_sym_imm_tok_pat_d2727a0] = "imm_tok_pat_d2727a0",
  [alias_sym_imm_tok_pat_f43f746] = "imm_tok_pat_f43f746",
  [alias_sym_imm_tok_pat_f46f69d] = "imm_tok_pat_f46f69d",
  [alias_sym_imm_tok_pat_f6e1de8] = "imm_tok_pat_f6e1de8",
  [alias_sym_imm_tok_rcurl] = "imm_tok_rcurl",
  [alias_sym_pat_05444c2] = "pat_05444c2",
  [alias_sym_pat_0851d06] = "pat_0851d06",
  [alias_sym_pat_2b6adbc] = "pat_2b6adbc",
  [alias_sym_pat_4128122] = "pat_4128122",
  [alias_sym_pat_441cd81] = "pat_441cd81",
  [alias_sym_pat_4a2f38a] = "pat_4a2f38a",
  [alias_sym_pat_4de4cb9] = "pat_4de4cb9",
  [alias_sym_pat_4fd4a56] = "pat_4fd4a56",
  [alias_sym_pat_8165e5f] = "pat_8165e5f",
  [alias_sym_pat_9873c86] = "pat_9873c86",
  [alias_sym_pat_9a14b5c] = "pat_9a14b5c",
  [alias_sym_pat_a667757] = "pat_a667757",
  [alias_sym_pat_add] = "pat_add",
  [alias_sym_pat_arg] = "pat_arg",
  [alias_sym_pat_as] = "pat_as",
  [alias_sym_pat_b1120d3] = "pat_b1120d3",
  [alias_sym_pat_cmd] = "pat_cmd",
  [alias_sym_pat_copy] = "pat_copy",
  [alias_sym_pat_e0f3805] = "pat_e0f3805",
  [alias_sym_pat_ea34a52] = "pat_ea34a52",
  [alias_sym_pat_eda9032] = "pat_eda9032",
  [alias_sym_pat_entr] = "pat_entr",
  [alias_sym_pat_env] = "pat_env",
  [alias_sym_pat_expose] = "pat_expose",
  [alias_sym_pat_f8ab07f] = "pat_f8ab07f",
  [alias_sym_pat_from] = "pat_from",
  [alias_sym_pat_heal] = "pat_heal",
  [alias_sym_pat_label] = "pat_label",
  [alias_sym_pat_main] = "pat_main",
  [alias_sym_pat_onbu] = "pat_onbu",
  [alias_sym_pat_run] = "pat_run",
  [alias_sym_pat_shell] = "pat_shell",
  [alias_sym_pat_stop] = "pat_stop",
  [alias_sym_pat_user] = "pat_user",
  [alias_sym_pat_volume] = "pat_volume",
  [alias_sym_pat_work] = "pat_work",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_LF] = anon_sym_LF,
  [aux_sym_from_instruction_token1] = aux_sym_from_instruction_token1,
  [aux_sym_from_instruction_token2] = aux_sym_from_instruction_token2,
  [aux_sym_run_instruction_token1] = aux_sym_run_instruction_token1,
  [aux_sym_cmd_instruction_token1] = aux_sym_cmd_instruction_token1,
  [aux_sym_label_instruction_token1] = aux_sym_label_instruction_token1,
  [aux_sym_expose_instruction_token1] = aux_sym_expose_instruction_token1,
  [aux_sym_env_instruction_token1] = aux_sym_env_instruction_token1,
  [aux_sym_add_instruction_token1] = aux_sym_add_instruction_token1,
  [aux_sym_copy_instruction_token1] = aux_sym_copy_instruction_token1,
  [aux_sym_entrypoint_instruction_token1] = aux_sym_entrypoint_instruction_token1,
  [aux_sym_volume_instruction_token1] = aux_sym_volume_instruction_token1,
  [aux_sym_user_instruction_token1] = aux_sym_user_instruction_token1,
  [anon_sym_COLON] = anon_sym_COLON,
  [aux_sym_user_name_or_group_token1] = aux_sym_user_name_or_group_token1,
  [aux_sym_immediate_user_name_or_group_fragment_token1] = aux_sym_immediate_user_name_or_group_fragment_token1,
  [aux_sym_workdir_instruction_token1] = aux_sym_workdir_instruction_token1,
  [aux_sym_arg_instruction_token1] = aux_sym_arg_instruction_token1,
  [aux_sym_arg_instruction_token2] = aux_sym_arg_instruction_token2,
  [anon_sym_EQ] = anon_sym_EQ,
  [aux_sym_onbuild_instruction_token1] = aux_sym_onbuild_instruction_token1,
  [aux_sym_stopsignal_instruction_token1] = aux_sym_stopsignal_instruction_token1,
  [aux_sym_stopsignal_value_token1] = aux_sym_stopsignal_value_token1,
  [aux_sym_stopsignal_value_token2] = aux_sym_stopsignal_value_token2,
  [aux_sym_healthcheck_instruction_token1] = aux_sym_healthcheck_instruction_token1,
  [anon_sym_NONE] = anon_sym_NONE,
  [aux_sym_shell_instruction_token1] = aux_sym_shell_instruction_token1,
  [aux_sym_maintainer_instruction_token1] = aux_sym_maintainer_instruction_token1,
  [aux_sym_maintainer_instruction_token2] = aux_sym_maintainer_instruction_token2,
  [aux_sym_cross_build_instruction_token1] = aux_sym_cross_build_instruction_token1,
  [aux_sym_path_token1] = aux_sym_path_token1,
  [aux_sym_path_token2] = aux_sym_path_token2,
  [aux_sym_path_token3] = aux_sym_path_token3,
  [aux_sym_path_with_heredoc_token1] = aux_sym_path_with_heredoc_token1,
  [anon_sym_DOLLAR] = anon_sym_DOLLAR,
  [anon_sym_DOLLAR2] = anon_sym_DOLLAR,
  [anon_sym_LBRACE] = anon_sym_LBRACE,
  [aux_sym_expansion_body_token1] = aux_sym_expansion_body_token1,
  [anon_sym_RBRACE] = anon_sym_RBRACE,
  [sym_variable] = sym_variable,
  [aux_sym_spaced_env_pair_token1] = aux_sym_spaced_env_pair_token1,
  [aux_sym_env_key_token1] = aux_sym_env_key_token1,
  [aux_sym_expose_port_token1] = aux_sym_expose_port_token1,
  [anon_sym_SLASHtcp] = anon_sym_SLASHtcp,
  [anon_sym_SLASHudp] = anon_sym_SLASHudp,
  [aux_sym_label_pair_token1] = aux_sym_label_pair_token1,
  [aux_sym_image_name_token1] = aux_sym_image_name_token1,
  [aux_sym_image_name_token2] = aux_sym_image_name_token2,
  [aux_sym_image_tag_token1] = aux_sym_image_tag_token1,
  [anon_sym_AT] = anon_sym_AT,
  [aux_sym_image_digest_token1] = aux_sym_image_digest_token1,
  [anon_sym_DASH_DASH] = anon_sym_DASH_DASH,
  [aux_sym_param_token1] = aux_sym_param_token1,
  [aux_sym_param_token2] = aux_sym_param_token2,
  [anon_sym_mount] = anon_sym_mount,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [aux_sym_mount_param_param_token1] = aux_sym_mount_param_param_token1,
  [aux_sym_image_alias_token1] = aux_sym_image_alias_token1,
  [aux_sym_image_alias_token2] = aux_sym_image_alias_token2,
  [aux_sym_shell_fragment_token1] = aux_sym_shell_fragment_token1,
  [aux_sym_shell_fragment_token2] = aux_sym_shell_fragment_token2,
  [aux_sym_shell_fragment_token3] = aux_sym_shell_fragment_token3,
  [aux_sym_shell_fragment_token4] = aux_sym_shell_fragment_token4,
  [sym_line_continuation] = sym_line_continuation,
  [sym_required_line_continuation] = sym_required_line_continuation,
  [anon_sym_LBRACK] = anon_sym_LBRACK,
  [anon_sym_COMMA2] = anon_sym_COMMA,
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [anon_sym_DQUOTE] = anon_sym_DQUOTE,
  [aux_sym_json_string_token1] = aux_sym_json_string_token1,
  [sym_json_escape_sequence] = sym_json_escape_sequence,
  [aux_sym_double_quoted_string_token1] = aux_sym_double_quoted_string_token1,
  [anon_sym_BSLASH] = anon_sym_BSLASH,
  [anon_sym_SQUOTE] = anon_sym_SQUOTE,
  [aux_sym_single_quoted_string_token1] = aux_sym_single_quoted_string_token1,
  [aux_sym_unquoted_string_token1] = aux_sym_unquoted_string_token1,
  [anon_sym_BSLASH2] = anon_sym_BSLASH2,
  [sym_double_quoted_escape_sequence] = sym_double_quoted_escape_sequence,
  [sym_single_quoted_escape_sequence] = sym_single_quoted_escape_sequence,
  [sym_non_newline_whitespace] = sym_non_newline_whitespace,
  [sym_comment] = sym_comment,
  [sym_semgrep_metavariable] = sym_semgrep_metavariable,
  [sym_semgrep_ellipsis] = sym_semgrep_ellipsis,
  [sym_heredoc_marker] = sym_heredoc_marker,
  [sym_heredoc_line] = sym_heredoc_line,
  [sym_heredoc_end] = sym_heredoc_end,
  [sym_heredoc_nl] = sym_heredoc_nl,
  [sym_error_sentinel] = sym_error_sentinel,
  [sym_source_file] = sym_source_file,
  [sym_instruction] = sym_instruction,
  [sym_from_instruction] = sym_from_instruction,
  [sym_run_instruction] = sym_run_instruction,
  [sym_cmd_instruction] = sym_cmd_instruction,
  [sym_label_instruction] = sym_label_instruction,
  [sym_expose_instruction] = sym_expose_instruction,
  [sym_env_instruction] = sym_env_instruction,
  [sym_add_instruction] = sym_add_instruction,
  [sym_copy_instruction] = sym_copy_instruction,
  [sym_entrypoint_instruction] = sym_entrypoint_instruction,
  [sym_volume_instruction] = sym_volume_instruction,
  [sym_user_instruction] = sym_user_instruction,
  [sym_user_name_or_group] = sym_user_name_or_group,
  [sym_immediate_user_name_or_group] = sym_immediate_user_name_or_group,
  [sym_immediate_user_name_or_group_fragment] = sym_immediate_user_name_or_group_fragment,
  [sym_workdir_instruction] = sym_workdir_instruction,
  [sym_arg_instruction] = sym_arg_instruction,
  [sym_onbuild_instruction] = sym_onbuild_instruction,
  [sym_stopsignal_instruction] = sym_stopsignal_instruction,
  [sym_stopsignal_value] = sym_stopsignal_value,
  [sym_healthcheck_instruction] = sym_healthcheck_instruction,
  [sym_shell_instruction] = sym_shell_instruction,
  [sym_maintainer_instruction] = sym_maintainer_instruction,
  [sym_cross_build_instruction] = sym_cross_build_instruction,
  [sym_heredoc_block] = sym_heredoc_block,
  [sym_path] = sym_path,
  [sym_path_with_heredoc] = sym_path_with_heredoc,
  [sym_expansion] = sym_expansion,
  [sym_immediate_expansion] = sym_immediate_expansion,
  [sym_imm_expansion] = sym_imm_expansion,
  [sym_expansion_body] = sym_expansion_body,
  [sym_env_pair] = sym_env_pair,
  [sym_spaced_env_pair] = sym_spaced_env_pair,
  [sym_env_key] = sym_env_key,
  [sym_expose_port] = sym_expose_port,
  [sym_label_pair] = sym_label_pair,
  [sym_image_spec] = sym_image_spec,
  [sym_image_name] = sym_image_name,
  [sym_image_tag] = sym_image_tag,
  [sym_image_digest] = sym_image_digest,
  [sym_param] = sym_param,
  [sym_mount_param] = sym_mount_param,
  [sym_mount_param_param] = sym_mount_param_param,
  [sym_image_alias] = sym_image_alias,
  [sym_shell_command] = sym_shell_command,
  [sym_shell_fragment] = sym_shell_fragment,
  [sym_json_string_array] = sym_json_string_array,
  [sym_json_string] = sym_json_string,
  [sym_double_quoted_string] = sym_double_quoted_string,
  [sym_single_quoted_string] = sym_single_quoted_string,
  [sym_unquoted_string] = sym_unquoted_string,
  [sym_array_element] = sym_array_element,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_run_instruction_repeat1] = aux_sym_run_instruction_repeat1,
  [aux_sym_run_instruction_repeat2] = aux_sym_run_instruction_repeat2,
  [aux_sym_label_instruction_repeat1] = aux_sym_label_instruction_repeat1,
  [aux_sym_expose_instruction_repeat1] = aux_sym_expose_instruction_repeat1,
  [aux_sym_env_instruction_repeat1] = aux_sym_env_instruction_repeat1,
  [aux_sym_add_instruction_repeat1] = aux_sym_add_instruction_repeat1,
  [aux_sym_add_instruction_repeat2] = aux_sym_add_instruction_repeat2,
  [aux_sym_volume_instruction_repeat1] = aux_sym_volume_instruction_repeat1,
  [aux_sym_user_name_or_group_repeat1] = aux_sym_user_name_or_group_repeat1,
  [aux_sym_stopsignal_value_repeat1] = aux_sym_stopsignal_value_repeat1,
  [aux_sym_healthcheck_instruction_repeat1] = aux_sym_healthcheck_instruction_repeat1,
  [aux_sym_heredoc_block_repeat1] = aux_sym_heredoc_block_repeat1,
  [aux_sym_path_repeat1] = aux_sym_path_repeat1,
  [aux_sym_image_name_repeat1] = aux_sym_image_name_repeat1,
  [aux_sym_image_tag_repeat1] = aux_sym_image_tag_repeat1,
  [aux_sym_image_digest_repeat1] = aux_sym_image_digest_repeat1,
  [aux_sym_mount_param_repeat1] = aux_sym_mount_param_repeat1,
  [aux_sym_image_alias_repeat1] = aux_sym_image_alias_repeat1,
  [aux_sym_shell_command_repeat1] = aux_sym_shell_command_repeat1,
  [aux_sym_shell_fragment_repeat1] = aux_sym_shell_fragment_repeat1,
  [aux_sym_json_string_array_repeat1] = aux_sym_json_string_array_repeat1,
  [aux_sym_json_string_repeat1] = aux_sym_json_string_repeat1,
  [aux_sym_double_quoted_string_repeat1] = aux_sym_double_quoted_string_repeat1,
  [aux_sym_single_quoted_string_repeat1] = aux_sym_single_quoted_string_repeat1,
  [aux_sym_unquoted_string_repeat1] = aux_sym_unquoted_string_repeat1,
  [alias_sym_imm_tok_at] = alias_sym_imm_tok_at,
  [alias_sym_imm_tok_bslashspace] = alias_sym_imm_tok_bslashspace,
  [alias_sym_imm_tok_colon] = alias_sym_imm_tok_colon,
  [alias_sym_imm_tok_comma] = alias_sym_imm_tok_comma,
  [alias_sym_imm_tok_dollar] = alias_sym_imm_tok_dollar,
  [alias_sym_imm_tok_eq] = alias_sym_imm_tok_eq,
  [alias_sym_imm_tok_lcurl] = alias_sym_imm_tok_lcurl,
  [alias_sym_imm_tok_mount] = alias_sym_imm_tok_mount,
  [alias_sym_imm_tok_pat_0ab9261] = alias_sym_imm_tok_pat_0ab9261,
  [alias_sym_imm_tok_pat_0c7fc22] = alias_sym_imm_tok_pat_0c7fc22,
  [alias_sym_imm_tok_pat_2b37705] = alias_sym_imm_tok_pat_2b37705,
  [alias_sym_imm_tok_pat_3a2a380] = alias_sym_imm_tok_pat_3a2a380,
  [alias_sym_imm_tok_pat_3d340f6] = alias_sym_imm_tok_pat_3d340f6,
  [alias_sym_imm_tok_pat_441cd81] = alias_sym_imm_tok_pat_441cd81,
  [alias_sym_imm_tok_pat_589b0f8] = alias_sym_imm_tok_pat_589b0f8,
  [alias_sym_imm_tok_pat_7642c4f] = alias_sym_imm_tok_pat_7642c4f,
  [alias_sym_imm_tok_pat_8713919] = alias_sym_imm_tok_pat_8713919,
  [alias_sym_imm_tok_pat_9a14b5c] = alias_sym_imm_tok_pat_9a14b5c,
  [alias_sym_imm_tok_pat_9f6bbb9] = alias_sym_imm_tok_pat_9f6bbb9,
  [alias_sym_imm_tok_pat_bcfc287] = alias_sym_imm_tok_pat_bcfc287,
  [alias_sym_imm_tok_pat_d2727a0] = alias_sym_imm_tok_pat_d2727a0,
  [alias_sym_imm_tok_pat_f43f746] = alias_sym_imm_tok_pat_f43f746,
  [alias_sym_imm_tok_pat_f46f69d] = alias_sym_imm_tok_pat_f46f69d,
  [alias_sym_imm_tok_pat_f6e1de8] = alias_sym_imm_tok_pat_f6e1de8,
  [alias_sym_imm_tok_rcurl] = alias_sym_imm_tok_rcurl,
  [alias_sym_pat_05444c2] = alias_sym_pat_05444c2,
  [alias_sym_pat_0851d06] = alias_sym_pat_0851d06,
  [alias_sym_pat_2b6adbc] = alias_sym_pat_2b6adbc,
  [alias_sym_pat_4128122] = alias_sym_pat_4128122,
  [alias_sym_pat_441cd81] = alias_sym_pat_441cd81,
  [alias_sym_pat_4a2f38a] = alias_sym_pat_4a2f38a,
  [alias_sym_pat_4de4cb9] = alias_sym_pat_4de4cb9,
  [alias_sym_pat_4fd4a56] = alias_sym_pat_4fd4a56,
  [alias_sym_pat_8165e5f] = alias_sym_pat_8165e5f,
  [alias_sym_pat_9873c86] = alias_sym_pat_9873c86,
  [alias_sym_pat_9a14b5c] = alias_sym_pat_9a14b5c,
  [alias_sym_pat_a667757] = alias_sym_pat_a667757,
  [alias_sym_pat_add] = alias_sym_pat_add,
  [alias_sym_pat_arg] = alias_sym_pat_arg,
  [alias_sym_pat_as] = alias_sym_pat_as,
  [alias_sym_pat_b1120d3] = alias_sym_pat_b1120d3,
  [alias_sym_pat_cmd] = alias_sym_pat_cmd,
  [alias_sym_pat_copy] = alias_sym_pat_copy,
  [alias_sym_pat_e0f3805] = alias_sym_pat_e0f3805,
  [alias_sym_pat_ea34a52] = alias_sym_pat_ea34a52,
  [alias_sym_pat_eda9032] = alias_sym_pat_eda9032,
  [alias_sym_pat_entr] = alias_sym_pat_entr,
  [alias_sym_pat_env] = alias_sym_pat_env,
  [alias_sym_pat_expose] = alias_sym_pat_expose,
  [alias_sym_pat_f8ab07f] = alias_sym_pat_f8ab07f,
  [alias_sym_pat_from] = alias_sym_pat_from,
  [alias_sym_pat_heal] = alias_sym_pat_heal,
  [alias_sym_pat_label] = alias_sym_pat_label,
  [alias_sym_pat_main] = alias_sym_pat_main,
  [alias_sym_pat_onbu] = alias_sym_pat_onbu,
  [alias_sym_pat_run] = alias_sym_pat_run,
  [alias_sym_pat_shell] = alias_sym_pat_shell,
  [alias_sym_pat_stop] = alias_sym_pat_stop,
  [alias_sym_pat_user] = alias_sym_pat_user,
  [alias_sym_pat_volume] = alias_sym_pat_volume,
  [alias_sym_pat_work] = alias_sym_pat_work,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_LF] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_from_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_from_instruction_token2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_run_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_cmd_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_label_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_expose_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_env_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_add_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_copy_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_entrypoint_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_volume_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_user_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_user_name_or_group_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_immediate_user_name_or_group_fragment_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_workdir_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_arg_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_arg_instruction_token2] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_onbuild_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_stopsignal_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_stopsignal_value_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_stopsignal_value_token2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_healthcheck_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_NONE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_shell_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_maintainer_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_maintainer_instruction_token2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_cross_build_instruction_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_path_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_path_token2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_path_token3] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_path_with_heredoc_token1] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_DOLLAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOLLAR2] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_expansion_body_token1] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_RBRACE] = {
    .visible = true,
    .named = false,
  },
  [sym_variable] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_spaced_env_pair_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_env_key_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_expose_port_token1] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_SLASHtcp] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SLASHudp] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_label_pair_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_image_name_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_image_name_token2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_image_tag_token1] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_AT] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_image_digest_token1] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_DASH_DASH] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_param_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_param_token2] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_mount] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_mount_param_param_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_image_alias_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_image_alias_token2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_shell_fragment_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_shell_fragment_token2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_shell_fragment_token3] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_shell_fragment_token4] = {
    .visible = false,
    .named = false,
  },
  [sym_line_continuation] = {
    .visible = true,
    .named = true,
  },
  [sym_required_line_continuation] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_LBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA2] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DQUOTE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_json_string_token1] = {
    .visible = false,
    .named = false,
  },
  [sym_json_escape_sequence] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_double_quoted_string_token1] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_BSLASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SQUOTE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_single_quoted_string_token1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_unquoted_string_token1] = {
    .visible = false,
    .named = false,
  },
  [anon_sym_BSLASH2] = {
    .visible = true,
    .named = false,
  },
  [sym_double_quoted_escape_sequence] = {
    .visible = true,
    .named = true,
  },
  [sym_single_quoted_escape_sequence] = {
    .visible = true,
    .named = true,
  },
  [sym_non_newline_whitespace] = {
    .visible = true,
    .named = true,
  },
  [sym_comment] = {
    .visible = true,
    .named = true,
  },
  [sym_semgrep_metavariable] = {
    .visible = true,
    .named = true,
  },
  [sym_semgrep_ellipsis] = {
    .visible = true,
    .named = true,
  },
  [sym_heredoc_marker] = {
    .visible = true,
    .named = true,
  },
  [sym_heredoc_line] = {
    .visible = true,
    .named = true,
  },
  [sym_heredoc_end] = {
    .visible = true,
    .named = true,
  },
  [sym_heredoc_nl] = {
    .visible = true,
    .named = true,
  },
  [sym_error_sentinel] = {
    .visible = true,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_from_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_run_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_cmd_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_label_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_expose_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_env_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_add_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_copy_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_entrypoint_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_volume_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_user_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_user_name_or_group] = {
    .visible = true,
    .named = true,
  },
  [sym_immediate_user_name_or_group] = {
    .visible = true,
    .named = true,
  },
  [sym_immediate_user_name_or_group_fragment] = {
    .visible = true,
    .named = true,
  },
  [sym_workdir_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_arg_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_onbuild_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_stopsignal_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_stopsignal_value] = {
    .visible = true,
    .named = true,
  },
  [sym_healthcheck_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_shell_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_maintainer_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_cross_build_instruction] = {
    .visible = true,
    .named = true,
  },
  [sym_heredoc_block] = {
    .visible = true,
    .named = true,
  },
  [sym_path] = {
    .visible = true,
    .named = true,
  },
  [sym_path_with_heredoc] = {
    .visible = true,
    .named = true,
  },
  [sym_expansion] = {
    .visible = true,
    .named = true,
  },
  [sym_immediate_expansion] = {
    .visible = true,
    .named = true,
  },
  [sym_imm_expansion] = {
    .visible = true,
    .named = true,
  },
  [sym_expansion_body] = {
    .visible = true,
    .named = true,
  },
  [sym_env_pair] = {
    .visible = true,
    .named = true,
  },
  [sym_spaced_env_pair] = {
    .visible = true,
    .named = true,
  },
  [sym_env_key] = {
    .visible = true,
    .named = true,
  },
  [sym_expose_port] = {
    .visible = true,
    .named = true,
  },
  [sym_label_pair] = {
    .visible = true,
    .named = true,
  },
  [sym_image_spec] = {
    .visible = true,
    .named = true,
  },
  [sym_image_name] = {
    .visible = true,
    .named = true,
  },
  [sym_image_tag] = {
    .visible = true,
    .named = true,
  },
  [sym_image_digest] = {
    .visible = true,
    .named = true,
  },
  [sym_param] = {
    .visible = true,
    .named = true,
  },
  [sym_mount_param] = {
    .visible = true,
    .named = true,
  },
  [sym_mount_param_param] = {
    .visible = true,
    .named = true,
  },
  [sym_image_alias] = {
    .visible = true,
    .named = true,
  },
  [sym_shell_command] = {
    .visible = true,
    .named = true,
  },
  [sym_shell_fragment] = {
    .visible = true,
    .named = true,
  },
  [sym_json_string_array] = {
    .visible = true,
    .named = true,
  },
  [sym_json_string] = {
    .visible = true,
    .named = true,
  },
  [sym_double_quoted_string] = {
    .visible = true,
    .named = true,
  },
  [sym_single_quoted_string] = {
    .visible = true,
    .named = true,
  },
  [sym_unquoted_string] = {
    .visible = true,
    .named = true,
  },
  [sym_array_element] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_run_instruction_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_run_instruction_repeat2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_label_instruction_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_expose_instruction_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_env_instruction_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_add_instruction_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_add_instruction_repeat2] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_volume_instruction_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_user_name_or_group_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_stopsignal_value_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_healthcheck_instruction_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_heredoc_block_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_path_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_image_name_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_image_tag_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_image_digest_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_mount_param_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_image_alias_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_shell_command_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_shell_fragment_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_json_string_array_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_json_string_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_double_quoted_string_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_single_quoted_string_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_unquoted_string_repeat1] = {
    .visible = false,
    .named = false,
  },
  [alias_sym_imm_tok_at] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_bslashspace] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_colon] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_comma] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_dollar] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_eq] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_lcurl] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_mount] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_0ab9261] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_0c7fc22] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_2b37705] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_3a2a380] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_3d340f6] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_441cd81] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_589b0f8] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_7642c4f] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_8713919] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_9a14b5c] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_9f6bbb9] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_bcfc287] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_d2727a0] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_f43f746] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_f46f69d] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_pat_f6e1de8] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_imm_tok_rcurl] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_05444c2] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_0851d06] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_2b6adbc] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_4128122] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_441cd81] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_4a2f38a] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_4de4cb9] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_4fd4a56] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_8165e5f] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_9873c86] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_9a14b5c] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_a667757] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_add] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_arg] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_as] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_b1120d3] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_cmd] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_copy] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_e0f3805] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_ea34a52] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_eda9032] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_entr] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_env] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_expose] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_f8ab07f] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_from] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_heal] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_label] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_main] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_onbu] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_run] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_shell] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_stop] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_user] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_volume] = {
    .visible = true,
    .named = true,
  },
  [alias_sym_pat_work] = {
    .visible = true,
    .named = true,
  },
};

enum ts_field_identifiers {
  field_as = 1,
  field_default = 2,
  field_digest = 3,
  field_group = 4,
  field_key = 5,
  field_name = 6,
  field_tag = 7,
  field_user = 8,
  field_value = 9,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_as] = "as",
  [field_default] = "default",
  [field_digest] = "digest",
  [field_group] = "group",
  [field_key] = "key",
  [field_name] = "name",
  [field_tag] = "tag",
  [field_user] = "user",
  [field_value] = "value",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [3] = {.index = 0, .length = 1},
  [20] = {.index = 1, .length = 1},
  [22] = {.index = 2, .length = 1},
  [23] = {.index = 2, .length = 1},
  [32] = {.index = 3, .length = 2},
  [33] = {.index = 5, .length = 2},
  [37] = {.index = 0, .length = 1},
  [45] = {.index = 7, .length = 1},
  [50] = {.index = 8, .length = 3},
  [54] = {.index = 11, .length = 2},
  [55] = {.index = 11, .length = 2},
  [56] = {.index = 13, .length = 2},
  [57] = {.index = 13, .length = 2},
  [58] = {.index = 15, .length = 2},
  [59] = {.index = 17, .length = 2},
  [60] = {.index = 17, .length = 2},
  [62] = {.index = 19, .length = 2},
  [64] = {.index = 21, .length = 1},
  [65] = {.index = 19, .length = 2},
  [66] = {.index = 22, .length = 3},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_name, 0},
  [1] =
    {field_user, 1},
  [2] =
    {field_name, 1},
  [3] =
    {field_name, 0},
    {field_tag, 1},
  [5] =
    {field_digest, 1},
    {field_name, 0},
  [7] =
    {field_as, 3},
  [8] =
    {field_digest, 2},
    {field_name, 0},
    {field_tag, 1},
  [11] =
    {field_key, 0},
    {field_value, 2},
  [13] =
    {field_name, 0},
    {field_value, 2},
  [15] =
    {field_group, 3},
    {field_user, 1},
  [17] =
    {field_default, 3},
    {field_name, 1},
  [19] =
    {field_name, 1},
    {field_value, 3},
  [21] =
    {field_as, 4},
  [22] =
    {field_name, 1},
    {field_value, 3},
    {field_value, 4},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
  [1] = {
    [0] = alias_sym_pat_8165e5f,
  },
  [2] = {
    [0] = alias_sym_pat_from,
  },
  [4] = {
    [0] = alias_sym_pat_a667757,
  },
  [5] = {
    [0] = alias_sym_pat_b1120d3,
  },
  [6] = {
    [0] = alias_sym_pat_f8ab07f,
  },
  [7] = {
    [0] = alias_sym_pat_eda9032,
  },
  [8] = {
    [0] = alias_sym_pat_run,
  },
  [9] = {
    [0] = alias_sym_pat_cmd,
  },
  [10] = {
    [0] = alias_sym_pat_label,
  },
  [11] = {
    [0] = alias_sym_pat_e0f3805,
  },
  [12] = {
    [0] = alias_sym_pat_expose,
  },
  [13] = {
    [0] = alias_sym_pat_2b6adbc,
  },
  [14] = {
    [0] = alias_sym_pat_env,
  },
  [15] = {
    [0] = alias_sym_pat_9873c86,
  },
  [16] = {
    [0] = alias_sym_pat_0851d06,
  },
  [17] = {
    [0] = alias_sym_pat_entr,
  },
  [18] = {
    [0] = alias_sym_pat_volume,
  },
  [19] = {
    [0] = alias_sym_pat_05444c2,
  },
  [20] = {
    [0] = alias_sym_pat_user,
  },
  [21] = {
    [0] = alias_sym_pat_work,
  },
  [22] = {
    [0] = alias_sym_pat_arg,
    [1] = alias_sym_pat_4de4cb9,
  },
  [23] = {
    [0] = alias_sym_pat_arg,
  },
  [24] = {
    [0] = alias_sym_pat_onbu,
  },
  [25] = {
    [0] = alias_sym_pat_441cd81,
  },
  [26] = {
    [0] = alias_sym_pat_stop,
  },
  [27] = {
    [0] = alias_sym_pat_heal,
  },
  [28] = {
    [0] = alias_sym_pat_shell,
  },
  [29] = {
    [0] = alias_sym_pat_main,
    [1] = alias_sym_pat_4fd4a56,
  },
  [30] = {
    [0] = alias_sym_pat_4a2f38a,
    [1] = alias_sym_pat_4fd4a56,
  },
  [31] = {
    [0] = alias_sym_imm_tok_pat_2b37705,
  },
  [34] = {
    [1] = alias_sym_pat_ea34a52,
  },
  [35] = {
    [0] = alias_sym_imm_tok_pat_589b0f8,
  },
  [36] = {
    [0] = alias_sym_imm_tok_pat_0ab9261,
  },
  [37] = {
    [1] = alias_sym_imm_tok_eq,
  },
  [38] = {
    [0] = alias_sym_imm_tok_pat_0c7fc22,
  },
  [39] = {
    [0] = alias_sym_pat_add,
  },
  [40] = {
    [0] = alias_sym_pat_copy,
  },
  [41] = {
    [0] = alias_sym_imm_tok_pat_7642c4f,
  },
  [42] = {
    [0] = alias_sym_imm_tok_pat_441cd81,
  },
  [43] = {
    [0] = alias_sym_imm_tok_dollar,
  },
  [44] = {
    [0] = alias_sym_pat_9a14b5c,
  },
  [45] = {
    [0] = alias_sym_pat_from,
    [2] = alias_sym_pat_as,
  },
  [46] = {
    [0] = alias_sym_imm_tok_pat_bcfc287,
  },
  [47] = {
    [0] = alias_sym_imm_tok_colon,
  },
  [48] = {
    [0] = alias_sym_imm_tok_pat_d2727a0,
  },
  [49] = {
    [0] = alias_sym_imm_tok_at,
  },
  [51] = {
    [0] = alias_sym_imm_tok_pat_3a2a380,
  },
  [52] = {
    [0] = alias_sym_imm_tok_pat_9f6bbb9,
  },
  [53] = {
    [0] = alias_sym_imm_tok_bslashspace,
  },
  [54] = {
    [0] = alias_sym_pat_4128122,
    [1] = alias_sym_imm_tok_eq,
  },
  [55] = {
    [1] = alias_sym_imm_tok_eq,
  },
  [56] = {
    [1] = alias_sym_imm_tok_eq,
  },
  [57] = {
    [1] = alias_sym_imm_tok_pat_3d340f6,
  },
  [58] = {
    [0] = alias_sym_pat_user,
    [2] = alias_sym_imm_tok_colon,
  },
  [59] = {
    [0] = alias_sym_pat_arg,
    [1] = alias_sym_pat_4de4cb9,
    [2] = alias_sym_imm_tok_eq,
  },
  [60] = {
    [0] = alias_sym_pat_arg,
    [2] = alias_sym_imm_tok_eq,
  },
  [61] = {
    [0] = alias_sym_imm_tok_lcurl,
    [1] = alias_sym_imm_tok_pat_8713919,
    [2] = alias_sym_imm_tok_rcurl,
  },
  [62] = {
    [1] = alias_sym_imm_tok_pat_f43f746,
    [2] = alias_sym_imm_tok_eq,
    [3] = alias_sym_imm_tok_pat_f6e1de8,
  },
  [63] = {
    [0] = alias_sym_imm_tok_pat_9a14b5c,
  },
  [64] = {
    [0] = alias_sym_pat_from,
    [3] = alias_sym_pat_as,
  },
  [65] = {
    [1] = alias_sym_imm_tok_mount,
    [2] = alias_sym_imm_tok_eq,
  },
  [66] = {
    [1] = alias_sym_imm_tok_mount,
    [2] = alias_sym_imm_tok_eq,
  },
  [67] = {
    [0] = alias_sym_imm_tok_pat_f46f69d,
    [1] = alias_sym_imm_tok_eq,
    [2] = alias_sym_imm_tok_pat_f46f69d,
  },
  [68] = {
    [0] = alias_sym_imm_tok_comma,
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
  [40] = 9,
  [41] = 10,
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
  [57] = 37,
  [58] = 58,
  [59] = 59,
  [60] = 60,
  [61] = 33,
  [62] = 62,
  [63] = 63,
  [64] = 55,
  [65] = 65,
  [66] = 66,
  [67] = 67,
  [68] = 68,
  [69] = 69,
  [70] = 70,
  [71] = 71,
  [72] = 72,
  [73] = 54,
  [74] = 55,
  [75] = 54,
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
  [87] = 56,
  [88] = 65,
  [89] = 67,
  [90] = 69,
  [91] = 91,
  [92] = 49,
  [93] = 82,
  [94] = 68,
  [95] = 95,
  [96] = 96,
  [97] = 9,
  [98] = 10,
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
  [110] = 43,
  [111] = 111,
  [112] = 112,
  [113] = 113,
  [114] = 114,
  [115] = 115,
  [116] = 79,
  [117] = 80,
  [118] = 81,
  [119] = 68,
  [120] = 120,
  [121] = 121,
  [122] = 122,
  [123] = 59,
  [124] = 124,
  [125] = 62,
  [126] = 63,
  [127] = 101,
  [128] = 103,
  [129] = 104,
  [130] = 130,
  [131] = 36,
  [132] = 46,
  [133] = 133,
  [134] = 134,
  [135] = 135,
  [136] = 136,
  [137] = 45,
  [138] = 138,
  [139] = 68,
  [140] = 140,
  [141] = 141,
  [142] = 58,
  [143] = 143,
  [144] = 42,
  [145] = 43,
  [146] = 146,
  [147] = 147,
  [148] = 42,
  [149] = 44,
  [150] = 45,
  [151] = 151,
  [152] = 152,
  [153] = 60,
  [154] = 154,
  [155] = 44,
  [156] = 151,
  [157] = 35,
  [158] = 45,
  [159] = 159,
  [160] = 160,
  [161] = 42,
  [162] = 162,
  [163] = 163,
  [164] = 43,
  [165] = 165,
  [166] = 166,
  [167] = 167,
  [168] = 168,
  [169] = 169,
  [170] = 170,
  [171] = 66,
  [172] = 172,
  [173] = 173,
  [174] = 174,
  [175] = 111,
  [176] = 42,
  [177] = 43,
  [178] = 45,
  [179] = 44,
  [180] = 45,
  [181] = 44,
  [182] = 174,
  [183] = 42,
  [184] = 43,
  [185] = 185,
  [186] = 169,
  [187] = 174,
  [188] = 188,
  [189] = 189,
  [190] = 169,
  [191] = 191,
  [192] = 44,
  [193] = 43,
  [194] = 194,
  [195] = 44,
  [196] = 111,
  [197] = 42,
  [198] = 198,
  [199] = 43,
  [200] = 200,
  [201] = 45,
  [202] = 44,
  [203] = 111,
  [204] = 42,
  [205] = 205,
  [206] = 45,
  [207] = 207,
  [208] = 44,
  [209] = 209,
  [210] = 210,
  [211] = 111,
  [212] = 212,
  [213] = 42,
  [214] = 43,
  [215] = 215,
  [216] = 216,
  [217] = 217,
  [218] = 218,
  [219] = 219,
  [220] = 220,
  [221] = 221,
  [222] = 44,
  [223] = 42,
  [224] = 43,
  [225] = 45,
  [226] = 226,
  [227] = 227,
  [228] = 228,
  [229] = 229,
  [230] = 230,
  [231] = 231,
  [232] = 44,
  [233] = 45,
  [234] = 45,
  [235] = 235,
  [236] = 236,
  [237] = 237,
  [238] = 238,
  [239] = 35,
  [240] = 36,
  [241] = 241,
  [242] = 242,
  [243] = 172,
  [244] = 244,
  [245] = 245,
  [246] = 246,
  [247] = 247,
  [248] = 248,
  [249] = 245,
  [250] = 209,
  [251] = 251,
  [252] = 252,
  [253] = 227,
  [254] = 254,
  [255] = 255,
  [256] = 256,
  [257] = 44,
  [258] = 111,
  [259] = 42,
  [260] = 122,
  [261] = 106,
  [262] = 172,
  [263] = 43,
  [264] = 133,
  [265] = 134,
  [266] = 45,
  [267] = 267,
  [268] = 268,
  [269] = 66,
  [270] = 172,
  [271] = 271,
  [272] = 44,
  [273] = 111,
  [274] = 42,
  [275] = 43,
  [276] = 45,
  [277] = 44,
  [278] = 111,
  [279] = 42,
  [280] = 43,
  [281] = 45,
  [282] = 229,
  [283] = 111,
  [284] = 284,
  [285] = 44,
  [286] = 45,
  [287] = 245,
  [288] = 288,
  [289] = 289,
  [290] = 42,
  [291] = 43,
  [292] = 292,
  [293] = 268,
  [294] = 247,
  [295] = 254,
  [296] = 268,
  [297] = 297,
  [298] = 245,
  [299] = 268,
  [300] = 245,
  [301] = 268,
  [302] = 245,
  [303] = 268,
  [304] = 245,
  [305] = 268,
  [306] = 245,
  [307] = 268,
  [308] = 245,
  [309] = 268,
  [310] = 245,
  [311] = 245,
  [312] = 245,
  [313] = 245,
  [314] = 245,
  [315] = 315,
  [316] = 316,
  [317] = 317,
  [318] = 318,
  [319] = 319,
  [320] = 320,
  [321] = 321,
  [322] = 322,
  [323] = 323,
  [324] = 324,
  [325] = 325,
  [326] = 326,
  [327] = 327,
  [328] = 66,
  [329] = 329,
  [330] = 330,
  [331] = 331,
  [332] = 332,
  [333] = 256,
  [334] = 334,
  [335] = 335,
  [336] = 336,
  [337] = 337,
  [338] = 338,
  [339] = 339,
  [340] = 340,
  [341] = 341,
  [342] = 342,
  [343] = 343,
  [344] = 344,
  [345] = 345,
  [346] = 346,
  [347] = 347,
  [348] = 326,
  [349] = 349,
  [350] = 106,
  [351] = 351,
  [352] = 338,
  [353] = 353,
  [354] = 354,
  [355] = 355,
  [356] = 344,
  [357] = 357,
  [358] = 358,
  [359] = 359,
  [360] = 360,
  [361] = 361,
  [362] = 362,
  [363] = 363,
  [364] = 364,
  [365] = 122,
  [366] = 366,
  [367] = 367,
  [368] = 357,
  [369] = 369,
  [370] = 370,
  [371] = 329,
  [372] = 334,
  [373] = 373,
  [374] = 374,
  [375] = 375,
  [376] = 367,
  [377] = 357,
  [378] = 378,
  [379] = 379,
  [380] = 367,
  [381] = 357,
  [382] = 382,
  [383] = 383,
  [384] = 367,
  [385] = 360,
  [386] = 386,
  [387] = 367,
  [388] = 246,
  [389] = 133,
  [390] = 367,
  [391] = 391,
  [392] = 392,
  [393] = 367,
  [394] = 394,
  [395] = 134,
  [396] = 367,
  [397] = 397,
  [398] = 367,
  [399] = 399,
  [400] = 367,
  [401] = 401,
  [402] = 367,
  [403] = 403,
  [404] = 367,
  [405] = 405,
  [406] = 367,
  [407] = 367,
  [408] = 392,
  [409] = 344,
  [410] = 392,
  [411] = 344,
  [412] = 392,
  [413] = 336,
  [414] = 392,
  [415] = 392,
  [416] = 392,
  [417] = 392,
  [418] = 392,
  [419] = 392,
  [420] = 392,
  [421] = 392,
  [422] = 392,
  [423] = 392,
  [424] = 392,
  [425] = 353,
  [426] = 353,
  [427] = 367,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(174);
      ADVANCE_MAP(
        '"', 282,
        '#', 212,
        '$', 220,
        '\'', 293,
        ',', 262,
        '-', 270,
        ':', 188,
        '<', 238,
        '=', 197,
        '@', 249,
        '[', 279,
        '\\', 290,
        ']', 281,
        '_', 228,
        '{', 222,
        '}', 227,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(171);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(192);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      if (lookahead != 0) ADVANCE(210);
      END_STATE();
    case 1:
      if (lookahead == '\t') ADVANCE(3);
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == ' ') ADVANCE(305);
      END_STATE();
    case 2:
      ADVANCE_MAP(
        '\n', 277,
        'u', 167,
        '\t', 3,
        ' ', 3,
        '"', 286,
        '/', 286,
        '\\', 286,
        'b', 286,
        'f', 286,
        'n', 286,
        'r', 286,
        't', 286,
      );
      END_STATE();
    case 3:
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(3);
      END_STATE();
    case 4:
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(276);
      if (lookahead != 0 &&
          lookahead != ',' &&
          lookahead != '-' &&
          lookahead != '=') ADVANCE(275);
      END_STATE();
    case 5:
      ADVANCE_MAP(
        '\n', 175,
        '"', 282,
        '#', 309,
        '$', 168,
        '\'', 293,
        '.', 236,
        ':', 188,
        '=', 197,
        '\\', 3,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(6);
      if (lookahead == '-' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(237);
      END_STATE();
    case 6:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '"') ADVANCE(282);
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(168);
      if (lookahead == '\'') ADVANCE(293);
      if (lookahead == '.') ADVANCE(236);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(6);
      if (lookahead == '-' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(237);
      END_STATE();
    case 7:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '"') ADVANCE(282);
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '\'') ADVANCE(293);
      if (lookahead == '.') ADVANCE(63);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(7);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(230);
      END_STATE();
    case 8:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '"') ADVANCE(282);
      if (lookahead == '#') ADVANCE(301);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\'') ADVANCE(293);
      if (lookahead == '.') ADVANCE(299);
      if (lookahead == '\\') ADVANCE(1);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(7);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(302);
      if (lookahead != 0) ADVANCE(304);
      END_STATE();
    case 9:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '"') ADVANCE(282);
      if (lookahead == '#') ADVANCE(301);
      if (lookahead == '$') ADVANCE(221);
      if (lookahead == '\'') ADVANCE(293);
      if (lookahead == '.') ADVANCE(300);
      if (lookahead == '\\') ADVANCE(1);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(6);
      if (lookahead == '-' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(303);
      if (lookahead != 0) ADVANCE(304);
      END_STATE();
    case 10:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == ':') ADVANCE(188);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(20);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(192);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(193);
      END_STATE();
    case 11:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\\') ADVANCE(3);
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(250);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(19);
      if (('0' <= lookahead && lookahead <= ':') ||
          ('B' <= lookahead && lookahead <= 'Z') ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(251);
      END_STATE();
    case 12:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(20);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z')) ADVANCE(201);
      END_STATE();
    case 13:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(20);
      if (lookahead == '-' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(267);
      END_STATE();
    case 14:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(219);
      if (lookahead == '.') ADVANCE(63);
      if (lookahead == '/') ADVANCE(72);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(14);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(231);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(230);
      END_STATE();
    case 15:
      ADVANCE_MAP(
        '\n', 175,
        '#', 309,
        '-', 61,
        '.', 63,
        ':', 188,
        '@', 249,
        '\\', 3,
        'A', 148,
        'a', 148,
        'C', 120,
        'c', 120,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(16);
      END_STATE();
    case 16:
      ADVANCE_MAP(
        '\n', 175,
        '#', 309,
        '-', 61,
        '.', 63,
        '\\', 3,
        'A', 148,
        'a', 148,
        'C', 120,
        'c', 120,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(16);
      END_STATE();
    case 17:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '<') ADVANCE(273);
      if (lookahead == '\\') ADVANCE(26);
      if (lookahead == ',' ||
          lookahead == '-' ||
          lookahead == '=') ADVANCE(269);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(17);
      if (lookahead != 0 &&
          lookahead != '[' &&
          lookahead != '\\') ADVANCE(274);
      END_STATE();
    case 18:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '\\') ADVANCE(3);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(308);
      if ((0x0b <= lookahead && lookahead <= '\r')) SKIP(20);
      END_STATE();
    case 19:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '\\') ADVANCE(3);
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(148);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(19);
      END_STATE();
    case 20:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(20);
      END_STATE();
    case 21:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '#') ADVANCE(301);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\\') ADVANCE(1);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(20);
      if (lookahead != 0 &&
          (lookahead < '"' || '$' < lookahead) &&
          lookahead != '\'') ADVANCE(304);
      END_STATE();
    case 22:
      ADVANCE_MAP(
        '\n', 175,
        '#', 243,
        '$', 220,
        ':', 188,
        '@', 249,
        '\\', 241,
        'A', 242,
        'a', 242,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(19);
      if (lookahead != 0) ADVANCE(244);
      END_STATE();
    case 23:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '#') ADVANCE(216);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\\') ADVANCE(215);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(308);
      if ((0x0b <= lookahead && lookahead <= '\r')) SKIP(20);
      if (lookahead != 0) ADVANCE(217);
      END_STATE();
    case 24:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '#') ADVANCE(216);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\\') ADVANCE(215);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(20);
      if (lookahead != 0) ADVANCE(217);
      END_STATE();
    case 25:
      if (lookahead == '\n') ADVANCE(175);
      if (lookahead == '#') ADVANCE(247);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '@') ADVANCE(249);
      if (lookahead == '\\') ADVANCE(245);
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(246);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(19);
      if (lookahead != 0) ADVANCE(248);
      END_STATE();
    case 26:
      if (lookahead == '\n') ADVANCE(278);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(276);
      if (lookahead != 0 &&
          lookahead != ',' &&
          lookahead != '-' &&
          lookahead != '=') ADVANCE(275);
      END_STATE();
    case 27:
      if (lookahead == '\n') SKIP(31);
      if (lookahead == '"') ADVANCE(282);
      if (lookahead == '#') ADVANCE(288);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\\') ADVANCE(291);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(287);
      if (lookahead != 0) ADVANCE(289);
      END_STATE();
    case 28:
      if (lookahead == '\n') SKIP(45);
      if (lookahead == '#') ADVANCE(295);
      if (lookahead == '\'') ADVANCE(293);
      if (lookahead == '\\') ADVANCE(292);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(294);
      if (lookahead != 0) ADVANCE(296);
      END_STATE();
    case 29:
      if (lookahead == '"') ADVANCE(282);
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(168);
      if (lookahead == '\'') ADVANCE(293);
      if (lookahead == '.') ADVANCE(236);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(29);
      if (lookahead == '-' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(237);
      END_STATE();
    case 30:
      if (lookahead == '"') ADVANCE(282);
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '\'') ADVANCE(293);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(30);
      END_STATE();
    case 31:
      if (lookahead == '"') ADVANCE(282);
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '\\') ADVANCE(290);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(31);
      END_STATE();
    case 32:
      if (lookahead == '"') ADVANCE(282);
      if (lookahead == '#') ADVANCE(301);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\'') ADVANCE(293);
      if (lookahead == '\\') ADVANCE(1);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(30);
      if (lookahead != 0) ADVANCE(304);
      END_STATE();
    case 33:
      if (lookahead == '"') ADVANCE(282);
      if (lookahead == '#') ADVANCE(283);
      if (lookahead == '\\') ADVANCE(2);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(284);
      if (lookahead != 0) ADVANCE(285);
      END_STATE();
    case 34:
      if (lookahead == '#') ADVANCE(212);
      if (lookahead == '$') ADVANCE(219);
      if (lookahead == '-') ADVANCE(61);
      if (lookahead == '<') ADVANCE(169);
      if (lookahead == '\\') ADVANCE(211);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(34);
      if (lookahead != 0) ADVANCE(210);
      END_STATE();
    case 35:
      if (lookahead == '#') ADVANCE(212);
      if (lookahead == '$') ADVANCE(219);
      if (lookahead == '<') ADVANCE(170);
      if (lookahead == '[') ADVANCE(279);
      if (lookahead == '\\') ADVANCE(211);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(35);
      if (lookahead != 0 &&
          lookahead != '-') ADVANCE(210);
      END_STATE();
    case 36:
      if (lookahead == '#') ADVANCE(212);
      if (lookahead == '$') ADVANCE(219);
      if (lookahead == '<') ADVANCE(170);
      if (lookahead == '\\') ADVANCE(211);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(36);
      if (lookahead != 0 &&
          lookahead != '-') ADVANCE(210);
      END_STATE();
    case 37:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(53);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(192);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(193);
      END_STATE();
    case 38:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(53);
      if (('0' <= lookahead && lookahead <= ':') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(251);
      END_STATE();
    case 39:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(219);
      if (lookahead == '.') ADVANCE(63);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(39);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(231);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(230);
      END_STATE();
    case 40:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(219);
      if (lookahead == '\\') ADVANCE(3);
      if (lookahead == 'm') ADVANCE(254);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(42);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(257);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z')) ADVANCE(200);
      END_STATE();
    case 41:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(219);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(41);
      if (lookahead == '-' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(266);
      END_STATE();
    case 42:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(219);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(42);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z')) ADVANCE(200);
      END_STATE();
    case 43:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(219);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(43);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(189);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(190);
      END_STATE();
    case 44:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '$') ADVANCE(168);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(44);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(196);
      END_STATE();
    case 45:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '\'') ADVANCE(293);
      if (lookahead == '\\') ADVANCE(290);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(45);
      END_STATE();
    case 46:
      ADVANCE_MAP(
        '#', 309,
        ',', 262,
        '-', 270,
        '.', 272,
        '<', 273,
        '=', 269,
        '[', 279,
        '\\', 4,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(47);
      if (lookahead != 0) ADVANCE(274);
      END_STATE();
    case 47:
      ADVANCE_MAP(
        '#', 309,
        '-', 270,
        '.', 272,
        '<', 273,
        '[', 279,
        '\\', 4,
        ',', 269,
        '=', 269,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(47);
      if (lookahead != 0) ADVANCE(274);
      END_STATE();
    case 48:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '.') ADVANCE(272);
      if (lookahead == '<') ADVANCE(273);
      if (lookahead == '[') ADVANCE(279);
      if (lookahead == '\\') ADVANCE(4);
      if (lookahead == ',' ||
          lookahead == '-' ||
          lookahead == '=') ADVANCE(269);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(48);
      if (lookahead != 0) ADVANCE(274);
      END_STATE();
    case 49:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '<') ADVANCE(273);
      if (lookahead == '\\') ADVANCE(4);
      if (lookahead == ',' ||
          lookahead == '-' ||
          lookahead == '=') ADVANCE(269);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(49);
      if (lookahead != 0 &&
          lookahead != '[' &&
          lookahead != '\\') ADVANCE(274);
      END_STATE();
    case 50:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '=') ADVANCE(197);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(229);
      END_STATE();
    case 51:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '\\') ADVANCE(3);
      if (lookahead == '{') ADVANCE(222);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(53);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(228);
      END_STATE();
    case 52:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '\\') ADVANCE(3);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(308);
      if (('\n' <= lookahead && lookahead <= '\r')) SKIP(53);
      END_STATE();
    case 53:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(53);
      END_STATE();
    case 54:
      if (lookahead == '#') ADVANCE(309);
      if (lookahead == '\\') ADVANCE(3);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(53);
      if (('a' <= lookahead && lookahead <= 'z')) ADVANCE(257);
      END_STATE();
    case 55:
      if (lookahead == '#') ADVANCE(216);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\\') ADVANCE(215);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(308);
      if (('\n' <= lookahead && lookahead <= '\r')) SKIP(53);
      if (lookahead != 0) ADVANCE(217);
      END_STATE();
    case 56:
      if (lookahead == '#') ADVANCE(247);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\\') ADVANCE(245);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(53);
      if (lookahead != 0 &&
          lookahead != '@') ADVANCE(248);
      END_STATE();
    case 57:
      if (lookahead == '#') ADVANCE(240);
      if (lookahead == '$') ADVANCE(219);
      if (lookahead == '-') ADVANCE(61);
      if (lookahead == '\\') ADVANCE(239);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(57);
      if (lookahead != 0 &&
          lookahead != ':' &&
          lookahead != '@') ADVANCE(238);
      END_STATE();
    case 58:
      if (lookahead == '#') ADVANCE(264);
      if (lookahead == '\\') ADVANCE(263);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(53);
      if (lookahead != 0 &&
          lookahead != ',' &&
          lookahead != '=') ADVANCE(265);
      END_STATE();
    case 59:
      if (lookahead == '#') ADVANCE(259);
      if (lookahead == '\\') ADVANCE(258);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(53);
      if (lookahead != 0) ADVANCE(260);
      END_STATE();
    case 60:
      if (lookahead == '#') ADVANCE(223);
      if (lookahead == '\\') ADVANCE(224);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(225);
      if (lookahead != 0 &&
          lookahead != '}') ADVANCE(226);
      END_STATE();
    case 61:
      if (lookahead == '-') ADVANCE(252);
      END_STATE();
    case 62:
      if (lookahead == '.') ADVANCE(311);
      END_STATE();
    case 63:
      if (lookahead == '.') ADVANCE(62);
      END_STATE();
    case 64:
      if (lookahead == 'E') ADVANCE(203);
      END_STATE();
    case 65:
      if (lookahead == 'N') ADVANCE(64);
      END_STATE();
    case 66:
      if (lookahead == 'O') ADVANCE(65);
      END_STATE();
    case 67:
      if (lookahead == '_') ADVANCE(80);
      END_STATE();
    case 68:
      if (lookahead == 'c') ADVANCE(70);
      END_STATE();
    case 69:
      if (lookahead == 'd') ADVANCE(71);
      END_STATE();
    case 70:
      if (lookahead == 'p') ADVANCE(233);
      END_STATE();
    case 71:
      if (lookahead == 'p') ADVANCE(234);
      END_STATE();
    case 72:
      if (lookahead == 't') ADVANCE(68);
      if (lookahead == 'u') ADVANCE(69);
      END_STATE();
    case 73:
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(79);
      END_STATE();
    case 74:
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(103);
      END_STATE();
    case 75:
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(111);
      END_STATE();
    case 76:
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(114);
      END_STATE();
    case 77:
      if (lookahead == 'A' ||
          lookahead == 'a') ADVANCE(104);
      END_STATE();
    case 78:
      if (lookahead == 'B' ||
          lookahead == 'b') ADVANCE(158);
      END_STATE();
    case 79:
      if (lookahead == 'B' ||
          lookahead == 'b') ADVANCE(95);
      END_STATE();
    case 80:
      if (lookahead == 'B' ||
          lookahead == 'b') ADVANCE(160);
      END_STATE();
    case 81:
      if (lookahead == 'C' ||
          lookahead == 'c') ADVANCE(109);
      END_STATE();
    case 82:
      if (lookahead == 'C' ||
          lookahead == 'c') ADVANCE(101);
      END_STATE();
    case 83:
      if (lookahead == 'D' ||
          lookahead == 'd') ADVANCE(84);
      if (lookahead == 'R' ||
          lookahead == 'r') ADVANCE(97);
      END_STATE();
    case 84:
      if (lookahead == 'D' ||
          lookahead == 'd') ADVANCE(183);
      END_STATE();
    case 85:
      if (lookahead == 'D' ||
          lookahead == 'd') ADVANCE(179);
      END_STATE();
    case 86:
      if (lookahead == 'D' ||
          lookahead == 'd') ADVANCE(198);
      END_STATE();
    case 87:
      if (lookahead == 'D' ||
          lookahead == 'd') ADVANCE(209);
      END_STATE();
    case 88:
      if (lookahead == 'D' ||
          lookahead == 'd') ADVANCE(105);
      END_STATE();
    case 89:
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(181);
      END_STATE();
    case 90:
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(186);
      END_STATE();
    case 91:
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(81);
      END_STATE();
    case 92:
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(75);
      END_STATE();
    case 93:
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(118);
      END_STATE();
    case 94:
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(142);
      END_STATE();
    case 95:
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(112);
      END_STATE();
    case 96:
      if (lookahead == 'E' ||
          lookahead == 'e') ADVANCE(144);
      END_STATE();
    case 97:
      if (lookahead == 'G' ||
          lookahead == 'g') ADVANCE(195);
      END_STATE();
    case 98:
      if (lookahead == 'G' ||
          lookahead == 'g') ADVANCE(128);
      END_STATE();
    case 99:
      if (lookahead == 'H' ||
          lookahead == 'h') ADVANCE(82);
      END_STATE();
    case 100:
      if (lookahead == 'H' ||
          lookahead == 'h') ADVANCE(93);
      if (lookahead == 'T' ||
          lookahead == 't') ADVANCE(134);
      END_STATE();
    case 101:
      if (lookahead == 'H' ||
          lookahead == 'h') ADVANCE(91);
      END_STATE();
    case 102:
      if (lookahead == 'I' ||
          lookahead == 'i') ADVANCE(98);
      END_STATE();
    case 103:
      if (lookahead == 'I' ||
          lookahead == 'i') ADVANCE(126);
      END_STATE();
    case 104:
      if (lookahead == 'I' ||
          lookahead == 'i') ADVANCE(129);
      END_STATE();
    case 105:
      if (lookahead == 'I' ||
          lookahead == 'i') ADVANCE(143);
      END_STATE();
    case 106:
      if (lookahead == 'I' ||
          lookahead == 'i') ADVANCE(127);
      END_STATE();
    case 107:
      if (lookahead == 'I' ||
          lookahead == 'i') ADVANCE(116);
      END_STATE();
    case 108:
      if (lookahead == 'I' ||
          lookahead == 'i') ADVANCE(117);
      END_STATE();
    case 109:
      if (lookahead == 'K' ||
          lookahead == 'k') ADVANCE(202);
      END_STATE();
    case 110:
      if (lookahead == 'K' ||
          lookahead == 'k') ADVANCE(88);
      END_STATE();
    case 111:
      if (lookahead == 'L' ||
          lookahead == 'l') ADVANCE(153);
      END_STATE();
    case 112:
      if (lookahead == 'L' ||
          lookahead == 'l') ADVANCE(180);
      END_STATE();
    case 113:
      if (lookahead == 'L' ||
          lookahead == 'l') ADVANCE(204);
      END_STATE();
    case 114:
      if (lookahead == 'L' ||
          lookahead == 'l') ADVANCE(199);
      END_STATE();
    case 115:
      if (lookahead == 'L' ||
          lookahead == 'l') ADVANCE(159);
      END_STATE();
    case 116:
      if (lookahead == 'L' ||
          lookahead == 'l') ADVANCE(86);
      END_STATE();
    case 117:
      if (lookahead == 'L' ||
          lookahead == 'l') ADVANCE(87);
      END_STATE();
    case 118:
      if (lookahead == 'L' ||
          lookahead == 'l') ADVANCE(113);
      END_STATE();
    case 119:
      if (lookahead == 'M' ||
          lookahead == 'm') ADVANCE(176);
      END_STATE();
    case 120:
      if (lookahead == 'M' ||
          lookahead == 'm') ADVANCE(85);
      END_STATE();
    case 121:
      if (lookahead == 'M' ||
          lookahead == 'm') ADVANCE(85);
      if (lookahead == 'O' ||
          lookahead == 'o') ADVANCE(137);
      if (lookahead == 'R' ||
          lookahead == 'r') ADVANCE(133);
      END_STATE();
    case 122:
      if (lookahead == 'M' ||
          lookahead == 'm') ADVANCE(90);
      END_STATE();
    case 123:
      if (lookahead == 'N' ||
          lookahead == 'n') ADVANCE(155);
      if (lookahead == 'X' ||
          lookahead == 'x') ADVANCE(139);
      END_STATE();
    case 124:
      if (lookahead == 'N' ||
          lookahead == 'n') ADVANCE(178);
      END_STATE();
    case 125:
      if (lookahead == 'N' ||
          lookahead == 'n') ADVANCE(78);
      END_STATE();
    case 126:
      if (lookahead == 'N' ||
          lookahead == 'n') ADVANCE(156);
      END_STATE();
    case 127:
      if (lookahead == 'N' ||
          lookahead == 'n') ADVANCE(154);
      END_STATE();
    case 128:
      if (lookahead == 'N' ||
          lookahead == 'n') ADVANCE(76);
      END_STATE();
    case 129:
      if (lookahead == 'N' ||
          lookahead == 'n') ADVANCE(96);
      END_STATE();
    case 130:
      if (lookahead == 'O' ||
          lookahead == 'o') ADVANCE(115);
      END_STATE();
    case 131:
      if (lookahead == 'O' ||
          lookahead == 'o') ADVANCE(119);
      END_STATE();
    case 132:
      if (lookahead == 'O' ||
          lookahead == 'o') ADVANCE(141);
      END_STATE();
    case 133:
      if (lookahead == 'O' ||
          lookahead == 'o') ADVANCE(150);
      END_STATE();
    case 134:
      if (lookahead == 'O' ||
          lookahead == 'o') ADVANCE(138);
      END_STATE();
    case 135:
      if (lookahead == 'O' ||
          lookahead == 'o') ADVANCE(152);
      END_STATE();
    case 136:
      if (lookahead == 'O' ||
          lookahead == 'o') ADVANCE(106);
      END_STATE();
    case 137:
      if (lookahead == 'P' ||
          lookahead == 'p') ADVANCE(161);
      END_STATE();
    case 138:
      if (lookahead == 'P' ||
          lookahead == 'p') ADVANCE(151);
      END_STATE();
    case 139:
      if (lookahead == 'P' ||
          lookahead == 'p') ADVANCE(135);
      END_STATE();
    case 140:
      if (lookahead == 'P' ||
          lookahead == 'p') ADVANCE(136);
      END_STATE();
    case 141:
      if (lookahead == 'R' ||
          lookahead == 'r') ADVANCE(110);
      END_STATE();
    case 142:
      if (lookahead == 'R' ||
          lookahead == 'r') ADVANCE(187);
      END_STATE();
    case 143:
      if (lookahead == 'R' ||
          lookahead == 'r') ADVANCE(194);
      END_STATE();
    case 144:
      if (lookahead == 'R' ||
          lookahead == 'r') ADVANCE(205);
      END_STATE();
    case 145:
      if (lookahead == 'R' ||
          lookahead == 'r') ADVANCE(162);
      END_STATE();
    case 146:
      if (lookahead == 'R' ||
          lookahead == 'r') ADVANCE(131);
      END_STATE();
    case 147:
      if (lookahead == 'S' ||
          lookahead == 's') ADVANCE(67);
      END_STATE();
    case 148:
      if (lookahead == 'S' ||
          lookahead == 's') ADVANCE(177);
      END_STATE();
    case 149:
      if (lookahead == 'S' ||
          lookahead == 's') ADVANCE(94);
      END_STATE();
    case 150:
      if (lookahead == 'S' ||
          lookahead == 's') ADVANCE(147);
      END_STATE();
    case 151:
      if (lookahead == 'S' ||
          lookahead == 's') ADVANCE(102);
      END_STATE();
    case 152:
      if (lookahead == 'S' ||
          lookahead == 's') ADVANCE(89);
      END_STATE();
    case 153:
      if (lookahead == 'T' ||
          lookahead == 't') ADVANCE(99);
      END_STATE();
    case 154:
      if (lookahead == 'T' ||
          lookahead == 't') ADVANCE(185);
      END_STATE();
    case 155:
      if (lookahead == 'T' ||
          lookahead == 't') ADVANCE(145);
      if (lookahead == 'V' ||
          lookahead == 'v') ADVANCE(182);
      END_STATE();
    case 156:
      if (lookahead == 'T' ||
          lookahead == 't') ADVANCE(77);
      END_STATE();
    case 157:
      if (lookahead == 'U' ||
          lookahead == 'u') ADVANCE(124);
      END_STATE();
    case 158:
      if (lookahead == 'U' ||
          lookahead == 'u') ADVANCE(107);
      END_STATE();
    case 159:
      if (lookahead == 'U' ||
          lookahead == 'u') ADVANCE(122);
      END_STATE();
    case 160:
      if (lookahead == 'U' ||
          lookahead == 'u') ADVANCE(108);
      END_STATE();
    case 161:
      if (lookahead == 'Y' ||
          lookahead == 'y') ADVANCE(184);
      END_STATE();
    case 162:
      if (lookahead == 'Y' ||
          lookahead == 'y') ADVANCE(140);
      END_STATE();
    case 163:
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(232);
      END_STATE();
    case 164:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(286);
      END_STATE();
    case 165:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(164);
      END_STATE();
    case 166:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(165);
      END_STATE();
    case 167:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(166);
      END_STATE();
    case 168:
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_') ADVANCE(310);
      END_STATE();
    case 169:
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '$' &&
          lookahead != '-' &&
          lookahead != '<') ADVANCE(218);
      END_STATE();
    case 170:
      if (lookahead != 0 &&
          lookahead != '<') ADVANCE(213);
      END_STATE();
    case 171:
      if (eof) ADVANCE(174);
      ADVANCE_MAP(
        '"', 282,
        '#', 212,
        '$', 219,
        '\'', 293,
        ',', 280,
        '-', 270,
        '<', 238,
        '=', 210,
        '[', 279,
        '\\', 290,
        ']', 281,
        ':', 210,
        '@', 210,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(171);
      if (lookahead != 0) ADVANCE(210);
      END_STATE();
    case 172:
      if (eof) ADVANCE(174);
      ADVANCE_MAP(
        '"', 282,
        '#', 309,
        '$', 168,
        ',', 280,
        '-', 61,
        '.', 63,
        '=', 197,
        'N', 66,
        '[', 279,
        '\\', 3,
        ']', 281,
        '}', 227,
        'A', 83,
        'a', 83,
        'C', 121,
        'c', 121,
        'E', 123,
        'e', 123,
        'F', 146,
        'f', 146,
        'H', 92,
        'h', 92,
        'L', 73,
        'l', 73,
        'M', 74,
        'm', 74,
        'O', 125,
        'o', 125,
        'R', 157,
        'r', 157,
        'S', 100,
        's', 100,
        'U', 149,
        'u', 149,
        'V', 130,
        'v', 130,
        'W', 132,
        'w', 132,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(173);
      END_STATE();
    case 173:
      if (eof) ADVANCE(174);
      ADVANCE_MAP(
        '"', 282,
        '#', 309,
        '$', 168,
        ',', 280,
        '-', 61,
        '.', 63,
        'N', 66,
        '[', 279,
        '\\', 3,
        ']', 281,
        'A', 83,
        'a', 83,
        'C', 121,
        'c', 121,
        'E', 123,
        'e', 123,
        'F', 146,
        'f', 146,
        'H', 92,
        'h', 92,
        'L', 73,
        'l', 73,
        'M', 74,
        'm', 74,
        'O', 125,
        'o', 125,
        'R', 157,
        'r', 157,
        'S', 100,
        's', 100,
        'U', 149,
        'u', 149,
        'V', 130,
        'v', 130,
        'W', 132,
        'w', 132,
      );
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') SKIP(173);
      END_STATE();
    case 174:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 175:
      ACCEPT_TOKEN(anon_sym_LF);
      if (lookahead == '\n') ADVANCE(175);
      END_STATE();
    case 176:
      ACCEPT_TOKEN(aux_sym_from_instruction_token1);
      END_STATE();
    case 177:
      ACCEPT_TOKEN(aux_sym_from_instruction_token2);
      END_STATE();
    case 178:
      ACCEPT_TOKEN(aux_sym_run_instruction_token1);
      END_STATE();
    case 179:
      ACCEPT_TOKEN(aux_sym_cmd_instruction_token1);
      END_STATE();
    case 180:
      ACCEPT_TOKEN(aux_sym_label_instruction_token1);
      END_STATE();
    case 181:
      ACCEPT_TOKEN(aux_sym_expose_instruction_token1);
      END_STATE();
    case 182:
      ACCEPT_TOKEN(aux_sym_env_instruction_token1);
      END_STATE();
    case 183:
      ACCEPT_TOKEN(aux_sym_add_instruction_token1);
      END_STATE();
    case 184:
      ACCEPT_TOKEN(aux_sym_copy_instruction_token1);
      END_STATE();
    case 185:
      ACCEPT_TOKEN(aux_sym_entrypoint_instruction_token1);
      END_STATE();
    case 186:
      ACCEPT_TOKEN(aux_sym_volume_instruction_token1);
      END_STATE();
    case 187:
      ACCEPT_TOKEN(aux_sym_user_instruction_token1);
      END_STATE();
    case 188:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 189:
      ACCEPT_TOKEN(aux_sym_user_name_or_group_token1);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(189);
      END_STATE();
    case 190:
      ACCEPT_TOKEN(aux_sym_user_name_or_group_token1);
      if (lookahead == '-' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(190);
      END_STATE();
    case 191:
      ACCEPT_TOKEN(aux_sym_immediate_user_name_or_group_fragment_token1);
      if (lookahead == '-') ADVANCE(193);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 192:
      ACCEPT_TOKEN(aux_sym_immediate_user_name_or_group_fragment_token1);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(192);
      END_STATE();
    case 193:
      ACCEPT_TOKEN(aux_sym_immediate_user_name_or_group_fragment_token1);
      if (lookahead == '-' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(193);
      END_STATE();
    case 194:
      ACCEPT_TOKEN(aux_sym_workdir_instruction_token1);
      END_STATE();
    case 195:
      ACCEPT_TOKEN(aux_sym_arg_instruction_token1);
      END_STATE();
    case 196:
      ACCEPT_TOKEN(aux_sym_arg_instruction_token2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(196);
      END_STATE();
    case 197:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 198:
      ACCEPT_TOKEN(aux_sym_onbuild_instruction_token1);
      END_STATE();
    case 199:
      ACCEPT_TOKEN(aux_sym_stopsignal_instruction_token1);
      END_STATE();
    case 200:
      ACCEPT_TOKEN(aux_sym_stopsignal_value_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z')) ADVANCE(200);
      END_STATE();
    case 201:
      ACCEPT_TOKEN(aux_sym_stopsignal_value_token2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z')) ADVANCE(201);
      END_STATE();
    case 202:
      ACCEPT_TOKEN(aux_sym_healthcheck_instruction_token1);
      END_STATE();
    case 203:
      ACCEPT_TOKEN(anon_sym_NONE);
      END_STATE();
    case 204:
      ACCEPT_TOKEN(aux_sym_shell_instruction_token1);
      END_STATE();
    case 205:
      ACCEPT_TOKEN(aux_sym_maintainer_instruction_token1);
      END_STATE();
    case 206:
      ACCEPT_TOKEN(aux_sym_maintainer_instruction_token2);
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(206);
      if (lookahead != 0) ADVANCE(208);
      END_STATE();
    case 207:
      ACCEPT_TOKEN(aux_sym_maintainer_instruction_token2);
      if (lookahead == '#') ADVANCE(208);
      if (lookahead == '\\') ADVANCE(206);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(207);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(208);
      END_STATE();
    case 208:
      ACCEPT_TOKEN(aux_sym_maintainer_instruction_token2);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(208);
      END_STATE();
    case 209:
      ACCEPT_TOKEN(aux_sym_cross_build_instruction_token1);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(209);
      END_STATE();
    case 210:
      ACCEPT_TOKEN(aux_sym_path_token1);
      END_STATE();
    case 211:
      ACCEPT_TOKEN(aux_sym_path_token1);
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(3);
      END_STATE();
    case 212:
      ACCEPT_TOKEN(aux_sym_path_token1);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(309);
      END_STATE();
    case 213:
      ACCEPT_TOKEN(aux_sym_path_token2);
      END_STATE();
    case 214:
      ACCEPT_TOKEN(aux_sym_path_token2);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '<' &&
          lookahead != '\\') ADVANCE(274);
      END_STATE();
    case 215:
      ACCEPT_TOKEN(aux_sym_path_token3);
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(3);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != '$') ADVANCE(217);
      END_STATE();
    case 216:
      ACCEPT_TOKEN(aux_sym_path_token3);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '$') ADVANCE(309);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(216);
      END_STATE();
    case 217:
      ACCEPT_TOKEN(aux_sym_path_token3);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '$') ADVANCE(217);
      END_STATE();
    case 218:
      ACCEPT_TOKEN(aux_sym_path_with_heredoc_token1);
      END_STATE();
    case 219:
      ACCEPT_TOKEN(anon_sym_DOLLAR);
      END_STATE();
    case 220:
      ACCEPT_TOKEN(anon_sym_DOLLAR2);
      END_STATE();
    case 221:
      ACCEPT_TOKEN(anon_sym_DOLLAR2);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_') ADVANCE(310);
      END_STATE();
    case 222:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 223:
      ACCEPT_TOKEN(aux_sym_expansion_body_token1);
      if (lookahead == '\n') ADVANCE(226);
      if (lookahead == '}') ADVANCE(309);
      if (lookahead != 0) ADVANCE(223);
      END_STATE();
    case 224:
      ACCEPT_TOKEN(aux_sym_expansion_body_token1);
      if (lookahead == '\n') ADVANCE(226);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(224);
      if (lookahead != 0 &&
          lookahead != '}') ADVANCE(226);
      END_STATE();
    case 225:
      ACCEPT_TOKEN(aux_sym_expansion_body_token1);
      if (lookahead == '#') ADVANCE(223);
      if (lookahead == '\\') ADVANCE(224);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(225);
      if (lookahead != 0 &&
          lookahead != '}') ADVANCE(226);
      END_STATE();
    case 226:
      ACCEPT_TOKEN(aux_sym_expansion_body_token1);
      if (lookahead != 0 &&
          lookahead != '}') ADVANCE(226);
      END_STATE();
    case 227:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 228:
      ACCEPT_TOKEN(sym_variable);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(228);
      END_STATE();
    case 229:
      ACCEPT_TOKEN(aux_sym_spaced_env_pair_token1);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(229);
      END_STATE();
    case 230:
      ACCEPT_TOKEN(aux_sym_env_key_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(230);
      END_STATE();
    case 231:
      ACCEPT_TOKEN(aux_sym_expose_port_token1);
      if (lookahead == '-') ADVANCE(163);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(231);
      END_STATE();
    case 232:
      ACCEPT_TOKEN(aux_sym_expose_port_token1);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(232);
      END_STATE();
    case 233:
      ACCEPT_TOKEN(anon_sym_SLASHtcp);
      END_STATE();
    case 234:
      ACCEPT_TOKEN(anon_sym_SLASHudp);
      END_STATE();
    case 235:
      ACCEPT_TOKEN(aux_sym_label_pair_token1);
      if (lookahead == '.') ADVANCE(313);
      if (lookahead == '-' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(237);
      END_STATE();
    case 236:
      ACCEPT_TOKEN(aux_sym_label_pair_token1);
      if (lookahead == '.') ADVANCE(235);
      if (lookahead == '-' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(237);
      END_STATE();
    case 237:
      ACCEPT_TOKEN(aux_sym_label_pair_token1);
      if (lookahead == '-' ||
          lookahead == '.' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(237);
      END_STATE();
    case 238:
      ACCEPT_TOKEN(aux_sym_image_name_token1);
      END_STATE();
    case 239:
      ACCEPT_TOKEN(aux_sym_image_name_token1);
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(3);
      END_STATE();
    case 240:
      ACCEPT_TOKEN(aux_sym_image_name_token1);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(309);
      END_STATE();
    case 241:
      ACCEPT_TOKEN(aux_sym_image_name_token2);
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(3);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != '$' &&
          lookahead != ':' &&
          lookahead != '@') ADVANCE(244);
      END_STATE();
    case 242:
      ACCEPT_TOKEN(aux_sym_image_name_token2);
      if (lookahead == 'S' ||
          lookahead == 's') ADVANCE(244);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '$' &&
          lookahead != ':' &&
          lookahead != '@') ADVANCE(244);
      END_STATE();
    case 243:
      ACCEPT_TOKEN(aux_sym_image_name_token2);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '$' ||
          lookahead == ':' ||
          lookahead == '@') ADVANCE(309);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(243);
      END_STATE();
    case 244:
      ACCEPT_TOKEN(aux_sym_image_name_token2);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '$' &&
          lookahead != ':' &&
          lookahead != '@') ADVANCE(244);
      END_STATE();
    case 245:
      ACCEPT_TOKEN(aux_sym_image_tag_token1);
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(3);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != '$' &&
          lookahead != '@') ADVANCE(248);
      END_STATE();
    case 246:
      ACCEPT_TOKEN(aux_sym_image_tag_token1);
      if (lookahead == 'S' ||
          lookahead == 's') ADVANCE(248);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '$' &&
          lookahead != '@') ADVANCE(248);
      END_STATE();
    case 247:
      ACCEPT_TOKEN(aux_sym_image_tag_token1);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '$' ||
          lookahead == '@') ADVANCE(309);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(247);
      END_STATE();
    case 248:
      ACCEPT_TOKEN(aux_sym_image_tag_token1);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '$' &&
          lookahead != '@') ADVANCE(248);
      END_STATE();
    case 249:
      ACCEPT_TOKEN(anon_sym_AT);
      END_STATE();
    case 250:
      ACCEPT_TOKEN(aux_sym_image_digest_token1);
      if (lookahead == 'S' ||
          lookahead == 's') ADVANCE(251);
      if (('0' <= lookahead && lookahead <= ':') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(251);
      END_STATE();
    case 251:
      ACCEPT_TOKEN(aux_sym_image_digest_token1);
      if (('0' <= lookahead && lookahead <= ':') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(251);
      END_STATE();
    case 252:
      ACCEPT_TOKEN(anon_sym_DASH_DASH);
      END_STATE();
    case 253:
      ACCEPT_TOKEN(aux_sym_param_token1);
      if (lookahead == 'n') ADVANCE(255);
      if (lookahead == '-' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(257);
      END_STATE();
    case 254:
      ACCEPT_TOKEN(aux_sym_param_token1);
      if (lookahead == 'o') ADVANCE(256);
      if (lookahead == '-' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(257);
      END_STATE();
    case 255:
      ACCEPT_TOKEN(aux_sym_param_token1);
      if (lookahead == 't') ADVANCE(261);
      if (lookahead == '-' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(257);
      END_STATE();
    case 256:
      ACCEPT_TOKEN(aux_sym_param_token1);
      if (lookahead == 'u') ADVANCE(253);
      if (lookahead == '-' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(257);
      END_STATE();
    case 257:
      ACCEPT_TOKEN(aux_sym_param_token1);
      if (lookahead == '-' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(257);
      END_STATE();
    case 258:
      ACCEPT_TOKEN(aux_sym_param_token2);
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(3);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(260);
      END_STATE();
    case 259:
      ACCEPT_TOKEN(aux_sym_param_token2);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(309);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(259);
      END_STATE();
    case 260:
      ACCEPT_TOKEN(aux_sym_param_token2);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ') ADVANCE(260);
      END_STATE();
    case 261:
      ACCEPT_TOKEN(anon_sym_mount);
      if (lookahead == '-' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(257);
      END_STATE();
    case 262:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 263:
      ACCEPT_TOKEN(aux_sym_mount_param_param_token1);
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(3);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ',' &&
          lookahead != '=') ADVANCE(265);
      END_STATE();
    case 264:
      ACCEPT_TOKEN(aux_sym_mount_param_param_token1);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == ',' ||
          lookahead == '=') ADVANCE(309);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(264);
      END_STATE();
    case 265:
      ACCEPT_TOKEN(aux_sym_mount_param_param_token1);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != ',' &&
          lookahead != '=') ADVANCE(265);
      END_STATE();
    case 266:
      ACCEPT_TOKEN(aux_sym_image_alias_token1);
      if (lookahead == '-' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(266);
      END_STATE();
    case 267:
      ACCEPT_TOKEN(aux_sym_image_alias_token2);
      if (lookahead == '-' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(267);
      END_STATE();
    case 268:
      ACCEPT_TOKEN(aux_sym_shell_fragment_token1);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(268);
      END_STATE();
    case 269:
      ACCEPT_TOKEN(aux_sym_shell_fragment_token2);
      END_STATE();
    case 270:
      ACCEPT_TOKEN(aux_sym_shell_fragment_token2);
      if (lookahead == '-') ADVANCE(252);
      END_STATE();
    case 271:
      ACCEPT_TOKEN(aux_sym_shell_fragment_token3);
      if (lookahead == '.') ADVANCE(315);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '<' &&
          lookahead != '\\') ADVANCE(274);
      END_STATE();
    case 272:
      ACCEPT_TOKEN(aux_sym_shell_fragment_token3);
      if (lookahead == '.') ADVANCE(271);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '<' &&
          lookahead != '\\') ADVANCE(274);
      END_STATE();
    case 273:
      ACCEPT_TOKEN(aux_sym_shell_fragment_token3);
      if (lookahead == '\n' ||
          lookahead == '\\') ADVANCE(213);
      if (lookahead != 0 &&
          lookahead != '<') ADVANCE(214);
      END_STATE();
    case 274:
      ACCEPT_TOKEN(aux_sym_shell_fragment_token3);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '<' &&
          lookahead != '\\') ADVANCE(274);
      END_STATE();
    case 275:
      ACCEPT_TOKEN(aux_sym_shell_fragment_token4);
      END_STATE();
    case 276:
      ACCEPT_TOKEN(aux_sym_shell_fragment_token4);
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(3);
      END_STATE();
    case 277:
      ACCEPT_TOKEN(sym_line_continuation);
      END_STATE();
    case 278:
      ACCEPT_TOKEN(sym_required_line_continuation);
      END_STATE();
    case 279:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 280:
      ACCEPT_TOKEN(anon_sym_COMMA2);
      END_STATE();
    case 281:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 282:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    case 283:
      ACCEPT_TOKEN(aux_sym_json_string_token1);
      if (lookahead == '\n') ADVANCE(285);
      if (lookahead == '"' ||
          lookahead == '\\') ADVANCE(309);
      if (lookahead != 0) ADVANCE(283);
      END_STATE();
    case 284:
      ACCEPT_TOKEN(aux_sym_json_string_token1);
      if (lookahead == '#') ADVANCE(283);
      if (('\t' <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(284);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '#' &&
          lookahead != '\\') ADVANCE(285);
      END_STATE();
    case 285:
      ACCEPT_TOKEN(aux_sym_json_string_token1);
      if (lookahead != 0 &&
          lookahead != '"' &&
          lookahead != '\\') ADVANCE(285);
      END_STATE();
    case 286:
      ACCEPT_TOKEN(sym_json_escape_sequence);
      END_STATE();
    case 287:
      ACCEPT_TOKEN(aux_sym_double_quoted_string_token1);
      if (lookahead == '#') ADVANCE(288);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(287);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          (lookahead < '"' || '$' < lookahead) &&
          lookahead != '\\') ADVANCE(289);
      END_STATE();
    case 288:
      ACCEPT_TOKEN(aux_sym_double_quoted_string_token1);
      if (lookahead == '"' ||
          lookahead == '$' ||
          lookahead == '\\') ADVANCE(309);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(288);
      END_STATE();
    case 289:
      ACCEPT_TOKEN(aux_sym_double_quoted_string_token1);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '\\') ADVANCE(289);
      END_STATE();
    case 290:
      ACCEPT_TOKEN(anon_sym_BSLASH);
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(3);
      END_STATE();
    case 291:
      ACCEPT_TOKEN(anon_sym_BSLASH);
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(3);
      if (lookahead == '"' ||
          lookahead == '\\') ADVANCE(306);
      END_STATE();
    case 292:
      ACCEPT_TOKEN(anon_sym_BSLASH);
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(3);
      if (lookahead == '\'' ||
          lookahead == '\\') ADVANCE(307);
      END_STATE();
    case 293:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 294:
      ACCEPT_TOKEN(aux_sym_single_quoted_string_token1);
      if (lookahead == '#') ADVANCE(295);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ') ADVANCE(294);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(296);
      END_STATE();
    case 295:
      ACCEPT_TOKEN(aux_sym_single_quoted_string_token1);
      if (lookahead == '\'' ||
          lookahead == '\\') ADVANCE(309);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(295);
      END_STATE();
    case 296:
      ACCEPT_TOKEN(aux_sym_single_quoted_string_token1);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(296);
      END_STATE();
    case 297:
      ACCEPT_TOKEN(aux_sym_unquoted_string_token1);
      if (lookahead == '.') ADVANCE(314);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(304);
      END_STATE();
    case 298:
      ACCEPT_TOKEN(aux_sym_unquoted_string_token1);
      if (lookahead == '.') ADVANCE(312);
      if (lookahead == '-' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(303);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(304);
      END_STATE();
    case 299:
      ACCEPT_TOKEN(aux_sym_unquoted_string_token1);
      if (lookahead == '.') ADVANCE(297);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(304);
      END_STATE();
    case 300:
      ACCEPT_TOKEN(aux_sym_unquoted_string_token1);
      if (lookahead == '.') ADVANCE(298);
      if (lookahead == '-' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(303);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(304);
      END_STATE();
    case 301:
      ACCEPT_TOKEN(aux_sym_unquoted_string_token1);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '"' ||
          lookahead == '$' ||
          lookahead == '\'' ||
          lookahead == '\\') ADVANCE(309);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(301);
      END_STATE();
    case 302:
      ACCEPT_TOKEN(aux_sym_unquoted_string_token1);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(302);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(304);
      END_STATE();
    case 303:
      ACCEPT_TOKEN(aux_sym_unquoted_string_token1);
      if (lookahead == '-' ||
          lookahead == '.' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(303);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(304);
      END_STATE();
    case 304:
      ACCEPT_TOKEN(aux_sym_unquoted_string_token1);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(304);
      END_STATE();
    case 305:
      ACCEPT_TOKEN(anon_sym_BSLASH2);
      if (lookahead == '\n') ADVANCE(277);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(3);
      END_STATE();
    case 306:
      ACCEPT_TOKEN(sym_double_quoted_escape_sequence);
      END_STATE();
    case 307:
      ACCEPT_TOKEN(sym_single_quoted_escape_sequence);
      END_STATE();
    case 308:
      ACCEPT_TOKEN(sym_non_newline_whitespace);
      if (lookahead == '\t' ||
          lookahead == ' ') ADVANCE(308);
      END_STATE();
    case 309:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(309);
      END_STATE();
    case 310:
      ACCEPT_TOKEN(sym_semgrep_metavariable);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_') ADVANCE(310);
      END_STATE();
    case 311:
      ACCEPT_TOKEN(sym_semgrep_ellipsis);
      END_STATE();
    case 312:
      ACCEPT_TOKEN(sym_semgrep_ellipsis);
      if (lookahead == '-' ||
          lookahead == '.' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(303);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(304);
      END_STATE();
    case 313:
      ACCEPT_TOKEN(sym_semgrep_ellipsis);
      if (lookahead == '-' ||
          lookahead == '.' ||
          ('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(237);
      END_STATE();
    case 314:
      ACCEPT_TOKEN(sym_semgrep_ellipsis);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '"' &&
          lookahead != '$' &&
          lookahead != '\'' &&
          lookahead != '\\') ADVANCE(304);
      END_STATE();
    case 315:
      ACCEPT_TOKEN(sym_semgrep_ellipsis);
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '<' &&
          lookahead != '\\') ADVANCE(274);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0, .external_lex_state = 1},
  [1] = {.lex_state = 172},
  [2] = {.lex_state = 172},
  [3] = {.lex_state = 172},
  [4] = {.lex_state = 172},
  [5] = {.lex_state = 172},
  [6] = {.lex_state = 47, .external_lex_state = 2},
  [7] = {.lex_state = 47, .external_lex_state = 2},
  [8] = {.lex_state = 8},
  [9] = {.lex_state = 9},
  [10] = {.lex_state = 9},
  [11] = {.lex_state = 48, .external_lex_state = 2},
  [12] = {.lex_state = 48, .external_lex_state = 2},
  [13] = {.lex_state = 47, .external_lex_state = 2},
  [14] = {.lex_state = 32},
  [15] = {.lex_state = 32},
  [16] = {.lex_state = 32},
  [17] = {.lex_state = 32},
  [18] = {.lex_state = 32},
  [19] = {.lex_state = 34, .external_lex_state = 2},
  [20] = {.lex_state = 34, .external_lex_state = 2},
  [21] = {.lex_state = 34, .external_lex_state = 2},
  [22] = {.lex_state = 5},
  [23] = {.lex_state = 34, .external_lex_state = 2},
  [24] = {.lex_state = 5},
  [25] = {.lex_state = 46, .external_lex_state = 2},
  [26] = {.lex_state = 46, .external_lex_state = 2},
  [27] = {.lex_state = 46, .external_lex_state = 2},
  [28] = {.lex_state = 22},
  [29] = {.lex_state = 29},
  [30] = {.lex_state = 22},
  [31] = {.lex_state = 22},
  [32] = {.lex_state = 22},
  [33] = {.lex_state = 17, .external_lex_state = 3},
  [34] = {.lex_state = 22},
  [35] = {.lex_state = 9},
  [36] = {.lex_state = 9},
  [37] = {.lex_state = 17, .external_lex_state = 3},
  [38] = {.lex_state = 46, .external_lex_state = 2},
  [39] = {.lex_state = 46, .external_lex_state = 2},
  [40] = {.lex_state = 8},
  [41] = {.lex_state = 8},
  [42] = {.lex_state = 9},
  [43] = {.lex_state = 9},
  [44] = {.lex_state = 9},
  [45] = {.lex_state = 9},
  [46] = {.lex_state = 23, .external_lex_state = 4},
  [47] = {.lex_state = 25},
  [48] = {.lex_state = 10},
  [49] = {.lex_state = 17, .external_lex_state = 3},
  [50] = {.lex_state = 172},
  [51] = {.lex_state = 27},
  [52] = {.lex_state = 10},
  [53] = {.lex_state = 25},
  [54] = {.lex_state = 27},
  [55] = {.lex_state = 27},
  [56] = {.lex_state = 17, .external_lex_state = 3},
  [57] = {.lex_state = 17, .external_lex_state = 2},
  [58] = {.lex_state = 23, .external_lex_state = 4},
  [59] = {.lex_state = 23, .external_lex_state = 4},
  [60] = {.lex_state = 23, .external_lex_state = 4},
  [61] = {.lex_state = 17, .external_lex_state = 2},
  [62] = {.lex_state = 23, .external_lex_state = 4},
  [63] = {.lex_state = 23, .external_lex_state = 4},
  [64] = {.lex_state = 27},
  [65] = {.lex_state = 17, .external_lex_state = 3},
  [66] = {.lex_state = 47, .external_lex_state = 2},
  [67] = {.lex_state = 17, .external_lex_state = 3},
  [68] = {.lex_state = 23, .external_lex_state = 4},
  [69] = {.lex_state = 17, .external_lex_state = 3},
  [70] = {.lex_state = 10},
  [71] = {.lex_state = 10},
  [72] = {.lex_state = 10},
  [73] = {.lex_state = 27},
  [74] = {.lex_state = 27},
  [75] = {.lex_state = 27},
  [76] = {.lex_state = 11},
  [77] = {.lex_state = 37},
  [78] = {.lex_state = 11},
  [79] = {.lex_state = 23},
  [80] = {.lex_state = 23},
  [81] = {.lex_state = 23},
  [82] = {.lex_state = 49, .external_lex_state = 2},
  [83] = {.lex_state = 10},
  [84] = {.lex_state = 57},
  [85] = {.lex_state = 34, .external_lex_state = 2},
  [86] = {.lex_state = 35},
  [87] = {.lex_state = 17, .external_lex_state = 2},
  [88] = {.lex_state = 17, .external_lex_state = 2},
  [89] = {.lex_state = 17, .external_lex_state = 2},
  [90] = {.lex_state = 17, .external_lex_state = 2},
  [91] = {.lex_state = 14},
  [92] = {.lex_state = 17, .external_lex_state = 2},
  [93] = {.lex_state = 49, .external_lex_state = 2},
  [94] = {.lex_state = 23},
  [95] = {.lex_state = 34, .external_lex_state = 2},
  [96] = {.lex_state = 34, .external_lex_state = 2},
  [97] = {.lex_state = 21},
  [98] = {.lex_state = 21},
  [99] = {.lex_state = 34, .external_lex_state = 2},
  [100] = {.lex_state = 34, .external_lex_state = 2},
  [101] = {.lex_state = 23},
  [102] = {.lex_state = 14},
  [103] = {.lex_state = 23},
  [104] = {.lex_state = 23},
  [105] = {.lex_state = 34, .external_lex_state = 2},
  [106] = {.lex_state = 5},
  [107] = {.lex_state = 14},
  [108] = {.lex_state = 12},
  [109] = {.lex_state = 5},
  [110] = {.lex_state = 22},
  [111] = {.lex_state = 22},
  [112] = {.lex_state = 12},
  [113] = {.lex_state = 12},
  [114] = {.lex_state = 5},
  [115] = {.lex_state = 172},
  [116] = {.lex_state = 24},
  [117] = {.lex_state = 24},
  [118] = {.lex_state = 24},
  [119] = {.lex_state = 55},
  [120] = {.lex_state = 13},
  [121] = {.lex_state = 39},
  [122] = {.lex_state = 5},
  [123] = {.lex_state = 55},
  [124] = {.lex_state = 14},
  [125] = {.lex_state = 55},
  [126] = {.lex_state = 55},
  [127] = {.lex_state = 24},
  [128] = {.lex_state = 24},
  [129] = {.lex_state = 24},
  [130] = {.lex_state = 39},
  [131] = {.lex_state = 8},
  [132] = {.lex_state = 55},
  [133] = {.lex_state = 5},
  [134] = {.lex_state = 5},
  [135] = {.lex_state = 13},
  [136] = {.lex_state = 12},
  [137] = {.lex_state = 22},
  [138] = {.lex_state = 13},
  [139] = {.lex_state = 24},
  [140] = {.lex_state = 13},
  [141] = {.lex_state = 22},
  [142] = {.lex_state = 55},
  [143] = {.lex_state = 12},
  [144] = {.lex_state = 8},
  [145] = {.lex_state = 8},
  [146] = {.lex_state = 5},
  [147] = {.lex_state = 13},
  [148] = {.lex_state = 22},
  [149] = {.lex_state = 8},
  [150] = {.lex_state = 8},
  [151] = {.lex_state = 172},
  [152] = {.lex_state = 15},
  [153] = {.lex_state = 55},
  [154] = {.lex_state = 14},
  [155] = {.lex_state = 22},
  [156] = {.lex_state = 172},
  [157] = {.lex_state = 8},
  [158] = {.lex_state = 25},
  [159] = {.lex_state = 36},
  [160] = {.lex_state = 172},
  [161] = {.lex_state = 27},
  [162] = {.lex_state = 18, .external_lex_state = 4},
  [163] = {.lex_state = 28},
  [164] = {.lex_state = 27},
  [165] = {.lex_state = 18, .external_lex_state = 4},
  [166] = {.lex_state = 18, .external_lex_state = 4},
  [167] = {.lex_state = 18, .external_lex_state = 4},
  [168] = {.lex_state = 57},
  [169] = {.lex_state = 28},
  [170] = {.lex_state = 36},
  [171] = {.lex_state = 34, .external_lex_state = 2},
  [172] = {.lex_state = 23, .external_lex_state = 4},
  [173] = {.lex_state = 27},
  [174] = {.lex_state = 28},
  [175] = {.lex_state = 23, .external_lex_state = 4},
  [176] = {.lex_state = 25},
  [177] = {.lex_state = 25},
  [178] = {.lex_state = 27},
  [179] = {.lex_state = 23, .external_lex_state = 4},
  [180] = {.lex_state = 23, .external_lex_state = 4},
  [181] = {.lex_state = 25},
  [182] = {.lex_state = 28},
  [183] = {.lex_state = 23, .external_lex_state = 4},
  [184] = {.lex_state = 23, .external_lex_state = 4},
  [185] = {.lex_state = 172},
  [186] = {.lex_state = 28},
  [187] = {.lex_state = 28},
  [188] = {.lex_state = 56},
  [189] = {.lex_state = 38},
  [190] = {.lex_state = 28},
  [191] = {.lex_state = 25},
  [192] = {.lex_state = 27},
  [193] = {.lex_state = 10},
  [194] = {.lex_state = 10},
  [195] = {.lex_state = 23},
  [196] = {.lex_state = 23},
  [197] = {.lex_state = 23},
  [198] = {.lex_state = 5, .external_lex_state = 4},
  [199] = {.lex_state = 23},
  [200] = {.lex_state = 43},
  [201] = {.lex_state = 23},
  [202] = {.lex_state = 10},
  [203] = {.lex_state = 10},
  [204] = {.lex_state = 10},
  [205] = {.lex_state = 15},
  [206] = {.lex_state = 10},
  [207] = {.lex_state = 41},
  [208] = {.lex_state = 14},
  [209] = {.lex_state = 17, .external_lex_state = 4},
  [210] = {.lex_state = 5, .external_lex_state = 4},
  [211] = {.lex_state = 14},
  [212] = {.lex_state = 33},
  [213] = {.lex_state = 11},
  [214] = {.lex_state = 11},
  [215] = {.lex_state = 5, .external_lex_state = 4},
  [216] = {.lex_state = 5, .external_lex_state = 4},
  [217] = {.lex_state = 11},
  [218] = {.lex_state = 5, .external_lex_state = 4},
  [219] = {.lex_state = 28},
  [220] = {.lex_state = 5, .external_lex_state = 4},
  [221] = {.lex_state = 5, .external_lex_state = 4},
  [222] = {.lex_state = 11},
  [223] = {.lex_state = 21},
  [224] = {.lex_state = 21},
  [225] = {.lex_state = 11},
  [226] = {.lex_state = 5, .external_lex_state = 4},
  [227] = {.lex_state = 17, .external_lex_state = 4},
  [228] = {.lex_state = 5, .external_lex_state = 4},
  [229] = {.lex_state = 17, .external_lex_state = 4},
  [230] = {.lex_state = 14},
  [231] = {.lex_state = 14},
  [232] = {.lex_state = 21},
  [233] = {.lex_state = 21},
  [234] = {.lex_state = 14},
  [235] = {.lex_state = 15},
  [236] = {.lex_state = 34, .external_lex_state = 2},
  [237] = {.lex_state = 41},
  [238] = {.lex_state = 33},
  [239] = {.lex_state = 21},
  [240] = {.lex_state = 21},
  [241] = {.lex_state = 40},
  [242] = {.lex_state = 10},
  [243] = {.lex_state = 23},
  [244] = {.lex_state = 33},
  [245] = {.lex_state = 51},
  [246] = {.lex_state = 18, .external_lex_state = 4},
  [247] = {.lex_state = 172},
  [248] = {.lex_state = 172, .external_lex_state = 5},
  [249] = {.lex_state = 51},
  [250] = {.lex_state = 17},
  [251] = {.lex_state = 18},
  [252] = {.lex_state = 33},
  [253] = {.lex_state = 17},
  [254] = {.lex_state = 172},
  [255] = {.lex_state = 172, .external_lex_state = 5},
  [256] = {.lex_state = 17, .external_lex_state = 4},
  [257] = {.lex_state = 55},
  [258] = {.lex_state = 55},
  [259] = {.lex_state = 55},
  [260] = {.lex_state = 14},
  [261] = {.lex_state = 14},
  [262] = {.lex_state = 24},
  [263] = {.lex_state = 55},
  [264] = {.lex_state = 14},
  [265] = {.lex_state = 14},
  [266] = {.lex_state = 55},
  [267] = {.lex_state = 14},
  [268] = {.lex_state = 51},
  [269] = {.lex_state = 172},
  [270] = {.lex_state = 55},
  [271] = {.lex_state = 18},
  [272] = {.lex_state = 24},
  [273] = {.lex_state = 24},
  [274] = {.lex_state = 24},
  [275] = {.lex_state = 24},
  [276] = {.lex_state = 24},
  [277] = {.lex_state = 12},
  [278] = {.lex_state = 12},
  [279] = {.lex_state = 12},
  [280] = {.lex_state = 12},
  [281] = {.lex_state = 12},
  [282] = {.lex_state = 17},
  [283] = {.lex_state = 13},
  [284] = {.lex_state = 13},
  [285] = {.lex_state = 13},
  [286] = {.lex_state = 13},
  [287] = {.lex_state = 51},
  [288] = {.lex_state = 172},
  [289] = {.lex_state = 172, .external_lex_state = 5},
  [290] = {.lex_state = 13},
  [291] = {.lex_state = 13},
  [292] = {.lex_state = 18},
  [293] = {.lex_state = 51},
  [294] = {.lex_state = 172},
  [295] = {.lex_state = 172},
  [296] = {.lex_state = 51},
  [297] = {.lex_state = 12},
  [298] = {.lex_state = 51},
  [299] = {.lex_state = 51},
  [300] = {.lex_state = 51},
  [301] = {.lex_state = 51},
  [302] = {.lex_state = 51},
  [303] = {.lex_state = 51},
  [304] = {.lex_state = 51},
  [305] = {.lex_state = 51},
  [306] = {.lex_state = 51},
  [307] = {.lex_state = 51},
  [308] = {.lex_state = 51},
  [309] = {.lex_state = 51},
  [310] = {.lex_state = 51},
  [311] = {.lex_state = 51},
  [312] = {.lex_state = 51},
  [313] = {.lex_state = 51},
  [314] = {.lex_state = 51},
  [315] = {.lex_state = 14},
  [316] = {.lex_state = 58},
  [317] = {.lex_state = 40},
  [318] = {.lex_state = 172},
  [319] = {.lex_state = 58},
  [320] = {.lex_state = 44},
  [321] = {.lex_state = 15},
  [322] = {.lex_state = 5},
  [323] = {.lex_state = 172},
  [324] = {.lex_state = 15},
  [325] = {.lex_state = 15},
  [326] = {.lex_state = 50},
  [327] = {.lex_state = 5},
  [328] = {.lex_state = 57},
  [329] = {.lex_state = 5, .external_lex_state = 4},
  [330] = {.lex_state = 18},
  [331] = {.lex_state = 50},
  [332] = {.lex_state = 172},
  [333] = {.lex_state = 17},
  [334] = {.lex_state = 5, .external_lex_state = 4},
  [335] = {.lex_state = 172},
  [336] = {.lex_state = 5, .external_lex_state = 4},
  [337] = {.lex_state = 172},
  [338] = {.lex_state = 5, .external_lex_state = 4},
  [339] = {.lex_state = 15},
  [340] = {.lex_state = 172, .external_lex_state = 5},
  [341] = {.lex_state = 5, .external_lex_state = 4},
  [342] = {.lex_state = 5, .external_lex_state = 4},
  [343] = {.lex_state = 5},
  [344] = {.lex_state = 172},
  [345] = {.lex_state = 5},
  [346] = {.lex_state = 5},
  [347] = {.lex_state = 172},
  [348] = {.lex_state = 172},
  [349] = {.lex_state = 5},
  [350] = {.lex_state = 172},
  [351] = {.lex_state = 5},
  [352] = {.lex_state = 5},
  [353] = {.lex_state = 54},
  [354] = {.lex_state = 172},
  [355] = {.lex_state = 5},
  [356] = {.lex_state = 172},
  [357] = {.lex_state = 59},
  [358] = {.lex_state = 52},
  [359] = {.lex_state = 5},
  [360] = {.lex_state = 268},
  [361] = {.lex_state = 58},
  [362] = {.lex_state = 5},
  [363] = {.lex_state = 207},
  [364] = {.lex_state = 5},
  [365] = {.lex_state = 172},
  [366] = {.lex_state = 207},
  [367] = {.lex_state = 172},
  [368] = {.lex_state = 59},
  [369] = {.lex_state = 5},
  [370] = {.lex_state = 5},
  [371] = {.lex_state = 5},
  [372] = {.lex_state = 5},
  [373] = {.lex_state = 5},
  [374] = {.lex_state = 172},
  [375] = {.lex_state = 5},
  [376] = {.lex_state = 172},
  [377] = {.lex_state = 59},
  [378] = {.lex_state = 5},
  [379] = {.lex_state = 5},
  [380] = {.lex_state = 172},
  [381] = {.lex_state = 59},
  [382] = {.lex_state = 5},
  [383] = {.lex_state = 5},
  [384] = {.lex_state = 172},
  [385] = {.lex_state = 268},
  [386] = {.lex_state = 5},
  [387] = {.lex_state = 172},
  [388] = {.lex_state = 52},
  [389] = {.lex_state = 172},
  [390] = {.lex_state = 172},
  [391] = {.lex_state = 5},
  [392] = {.lex_state = 60},
  [393] = {.lex_state = 172},
  [394] = {.lex_state = 5},
  [395] = {.lex_state = 172},
  [396] = {.lex_state = 172},
  [397] = {.lex_state = 172},
  [398] = {.lex_state = 172},
  [399] = {.lex_state = 5},
  [400] = {.lex_state = 172},
  [401] = {.lex_state = 172},
  [402] = {.lex_state = 172},
  [403] = {.lex_state = 5},
  [404] = {.lex_state = 172},
  [405] = {.lex_state = 172},
  [406] = {.lex_state = 172},
  [407] = {.lex_state = 172},
  [408] = {.lex_state = 60},
  [409] = {.lex_state = 172},
  [410] = {.lex_state = 60},
  [411] = {.lex_state = 172},
  [412] = {.lex_state = 60},
  [413] = {.lex_state = 5},
  [414] = {.lex_state = 60},
  [415] = {.lex_state = 60},
  [416] = {.lex_state = 60},
  [417] = {.lex_state = 60},
  [418] = {.lex_state = 60},
  [419] = {.lex_state = 60},
  [420] = {.lex_state = 60},
  [421] = {.lex_state = 60},
  [422] = {.lex_state = 60},
  [423] = {.lex_state = 60},
  [424] = {.lex_state = 60},
  [425] = {.lex_state = 54},
  [426] = {.lex_state = 54},
  [427] = {.lex_state = 172},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [aux_sym_immediate_user_name_or_group_fragment_token1] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [aux_sym_path_token1] = ACTIONS(1),
    [anon_sym_DOLLAR] = ACTIONS(1),
    [anon_sym_DOLLAR2] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
    [sym_variable] = ACTIONS(1),
    [aux_sym_image_name_token1] = ACTIONS(1),
    [anon_sym_AT] = ACTIONS(1),
    [anon_sym_DASH_DASH] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [aux_sym_shell_fragment_token2] = ACTIONS(1),
    [sym_line_continuation] = ACTIONS(3),
    [anon_sym_LBRACK] = ACTIONS(1),
    [anon_sym_COMMA2] = ACTIONS(1),
    [anon_sym_RBRACK] = ACTIONS(1),
    [anon_sym_DQUOTE] = ACTIONS(1),
    [anon_sym_BSLASH] = ACTIONS(1),
    [anon_sym_SQUOTE] = ACTIONS(1),
    [sym_comment] = ACTIONS(5),
    [sym_heredoc_marker] = ACTIONS(1),
    [sym_heredoc_line] = ACTIONS(1),
    [sym_heredoc_end] = ACTIONS(1),
    [sym_heredoc_nl] = ACTIONS(1),
    [sym_error_sentinel] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(354),
    [sym_instruction] = STATE(375),
    [sym_from_instruction] = STATE(364),
    [sym_run_instruction] = STATE(364),
    [sym_cmd_instruction] = STATE(364),
    [sym_label_instruction] = STATE(364),
    [sym_expose_instruction] = STATE(364),
    [sym_env_instruction] = STATE(364),
    [sym_add_instruction] = STATE(364),
    [sym_copy_instruction] = STATE(364),
    [sym_entrypoint_instruction] = STATE(364),
    [sym_volume_instruction] = STATE(364),
    [sym_user_instruction] = STATE(364),
    [sym_workdir_instruction] = STATE(364),
    [sym_arg_instruction] = STATE(364),
    [sym_onbuild_instruction] = STATE(364),
    [sym_stopsignal_instruction] = STATE(364),
    [sym_healthcheck_instruction] = STATE(364),
    [sym_shell_instruction] = STATE(364),
    [sym_maintainer_instruction] = STATE(364),
    [sym_cross_build_instruction] = STATE(364),
    [aux_sym_source_file_repeat1] = STATE(3),
    [ts_builtin_sym_end] = ACTIONS(7),
    [aux_sym_from_instruction_token1] = ACTIONS(9),
    [aux_sym_run_instruction_token1] = ACTIONS(11),
    [aux_sym_cmd_instruction_token1] = ACTIONS(13),
    [aux_sym_label_instruction_token1] = ACTIONS(15),
    [aux_sym_expose_instruction_token1] = ACTIONS(17),
    [aux_sym_env_instruction_token1] = ACTIONS(19),
    [aux_sym_add_instruction_token1] = ACTIONS(21),
    [aux_sym_copy_instruction_token1] = ACTIONS(23),
    [aux_sym_entrypoint_instruction_token1] = ACTIONS(25),
    [aux_sym_volume_instruction_token1] = ACTIONS(27),
    [aux_sym_user_instruction_token1] = ACTIONS(29),
    [aux_sym_workdir_instruction_token1] = ACTIONS(31),
    [aux_sym_arg_instruction_token1] = ACTIONS(33),
    [aux_sym_onbuild_instruction_token1] = ACTIONS(35),
    [aux_sym_stopsignal_instruction_token1] = ACTIONS(37),
    [aux_sym_healthcheck_instruction_token1] = ACTIONS(39),
    [aux_sym_shell_instruction_token1] = ACTIONS(41),
    [aux_sym_maintainer_instruction_token1] = ACTIONS(43),
    [aux_sym_cross_build_instruction_token1] = ACTIONS(45),
    [sym_line_continuation] = ACTIONS(3),
    [sym_comment] = ACTIONS(3),
    [sym_semgrep_metavariable] = ACTIONS(47),
    [sym_semgrep_ellipsis] = ACTIONS(47),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 25,
    ACTIONS(49), 1,
      ts_builtin_sym_end,
    ACTIONS(51), 1,
      aux_sym_from_instruction_token1,
    ACTIONS(54), 1,
      aux_sym_run_instruction_token1,
    ACTIONS(57), 1,
      aux_sym_cmd_instruction_token1,
    ACTIONS(60), 1,
      aux_sym_label_instruction_token1,
    ACTIONS(63), 1,
      aux_sym_expose_instruction_token1,
    ACTIONS(66), 1,
      aux_sym_env_instruction_token1,
    ACTIONS(69), 1,
      aux_sym_add_instruction_token1,
    ACTIONS(72), 1,
      aux_sym_copy_instruction_token1,
    ACTIONS(75), 1,
      aux_sym_entrypoint_instruction_token1,
    ACTIONS(78), 1,
      aux_sym_volume_instruction_token1,
    ACTIONS(81), 1,
      aux_sym_user_instruction_token1,
    ACTIONS(84), 1,
      aux_sym_workdir_instruction_token1,
    ACTIONS(87), 1,
      aux_sym_arg_instruction_token1,
    ACTIONS(90), 1,
      aux_sym_onbuild_instruction_token1,
    ACTIONS(93), 1,
      aux_sym_stopsignal_instruction_token1,
    ACTIONS(96), 1,
      aux_sym_healthcheck_instruction_token1,
    ACTIONS(99), 1,
      aux_sym_shell_instruction_token1,
    ACTIONS(102), 1,
      aux_sym_maintainer_instruction_token1,
    ACTIONS(105), 1,
      aux_sym_cross_build_instruction_token1,
    STATE(2), 1,
      aux_sym_source_file_repeat1,
    STATE(375), 1,
      sym_instruction,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(108), 2,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
    STATE(364), 19,
      sym_from_instruction,
      sym_run_instruction,
      sym_cmd_instruction,
      sym_label_instruction,
      sym_expose_instruction,
      sym_env_instruction,
      sym_add_instruction,
      sym_copy_instruction,
      sym_entrypoint_instruction,
      sym_volume_instruction,
      sym_user_instruction,
      sym_workdir_instruction,
      sym_arg_instruction,
      sym_onbuild_instruction,
      sym_stopsignal_instruction,
      sym_healthcheck_instruction,
      sym_shell_instruction,
      sym_maintainer_instruction,
      sym_cross_build_instruction,
  [96] = 25,
    ACTIONS(9), 1,
      aux_sym_from_instruction_token1,
    ACTIONS(11), 1,
      aux_sym_run_instruction_token1,
    ACTIONS(13), 1,
      aux_sym_cmd_instruction_token1,
    ACTIONS(15), 1,
      aux_sym_label_instruction_token1,
    ACTIONS(17), 1,
      aux_sym_expose_instruction_token1,
    ACTIONS(19), 1,
      aux_sym_env_instruction_token1,
    ACTIONS(21), 1,
      aux_sym_add_instruction_token1,
    ACTIONS(23), 1,
      aux_sym_copy_instruction_token1,
    ACTIONS(25), 1,
      aux_sym_entrypoint_instruction_token1,
    ACTIONS(27), 1,
      aux_sym_volume_instruction_token1,
    ACTIONS(29), 1,
      aux_sym_user_instruction_token1,
    ACTIONS(31), 1,
      aux_sym_workdir_instruction_token1,
    ACTIONS(33), 1,
      aux_sym_arg_instruction_token1,
    ACTIONS(35), 1,
      aux_sym_onbuild_instruction_token1,
    ACTIONS(37), 1,
      aux_sym_stopsignal_instruction_token1,
    ACTIONS(39), 1,
      aux_sym_healthcheck_instruction_token1,
    ACTIONS(41), 1,
      aux_sym_shell_instruction_token1,
    ACTIONS(43), 1,
      aux_sym_maintainer_instruction_token1,
    ACTIONS(45), 1,
      aux_sym_cross_build_instruction_token1,
    ACTIONS(111), 1,
      ts_builtin_sym_end,
    STATE(2), 1,
      aux_sym_source_file_repeat1,
    STATE(375), 1,
      sym_instruction,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(47), 2,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
    STATE(364), 19,
      sym_from_instruction,
      sym_run_instruction,
      sym_cmd_instruction,
      sym_label_instruction,
      sym_expose_instruction,
      sym_env_instruction,
      sym_add_instruction,
      sym_copy_instruction,
      sym_entrypoint_instruction,
      sym_volume_instruction,
      sym_user_instruction,
      sym_workdir_instruction,
      sym_arg_instruction,
      sym_onbuild_instruction,
      sym_stopsignal_instruction,
      sym_healthcheck_instruction,
      sym_shell_instruction,
      sym_maintainer_instruction,
      sym_cross_build_instruction,
  [192] = 23,
    ACTIONS(9), 1,
      aux_sym_from_instruction_token1,
    ACTIONS(11), 1,
      aux_sym_run_instruction_token1,
    ACTIONS(13), 1,
      aux_sym_cmd_instruction_token1,
    ACTIONS(15), 1,
      aux_sym_label_instruction_token1,
    ACTIONS(17), 1,
      aux_sym_expose_instruction_token1,
    ACTIONS(19), 1,
      aux_sym_env_instruction_token1,
    ACTIONS(21), 1,
      aux_sym_add_instruction_token1,
    ACTIONS(23), 1,
      aux_sym_copy_instruction_token1,
    ACTIONS(25), 1,
      aux_sym_entrypoint_instruction_token1,
    ACTIONS(27), 1,
      aux_sym_volume_instruction_token1,
    ACTIONS(29), 1,
      aux_sym_user_instruction_token1,
    ACTIONS(31), 1,
      aux_sym_workdir_instruction_token1,
    ACTIONS(33), 1,
      aux_sym_arg_instruction_token1,
    ACTIONS(35), 1,
      aux_sym_onbuild_instruction_token1,
    ACTIONS(37), 1,
      aux_sym_stopsignal_instruction_token1,
    ACTIONS(39), 1,
      aux_sym_healthcheck_instruction_token1,
    ACTIONS(41), 1,
      aux_sym_shell_instruction_token1,
    ACTIONS(43), 1,
      aux_sym_maintainer_instruction_token1,
    ACTIONS(45), 1,
      aux_sym_cross_build_instruction_token1,
    STATE(373), 1,
      sym_instruction,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(47), 2,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
    STATE(364), 19,
      sym_from_instruction,
      sym_run_instruction,
      sym_cmd_instruction,
      sym_label_instruction,
      sym_expose_instruction,
      sym_env_instruction,
      sym_add_instruction,
      sym_copy_instruction,
      sym_entrypoint_instruction,
      sym_volume_instruction,
      sym_user_instruction,
      sym_workdir_instruction,
      sym_arg_instruction,
      sym_onbuild_instruction,
      sym_stopsignal_instruction,
      sym_healthcheck_instruction,
      sym_shell_instruction,
      sym_maintainer_instruction,
      sym_cross_build_instruction,
  [282] = 2,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(49), 22,
      ts_builtin_sym_end,
      aux_sym_from_instruction_token1,
      aux_sym_run_instruction_token1,
      aux_sym_cmd_instruction_token1,
      aux_sym_label_instruction_token1,
      aux_sym_expose_instruction_token1,
      aux_sym_env_instruction_token1,
      aux_sym_add_instruction_token1,
      aux_sym_copy_instruction_token1,
      aux_sym_entrypoint_instruction_token1,
      aux_sym_volume_instruction_token1,
      aux_sym_user_instruction_token1,
      aux_sym_workdir_instruction_token1,
      aux_sym_arg_instruction_token1,
      aux_sym_onbuild_instruction_token1,
      aux_sym_stopsignal_instruction_token1,
      aux_sym_healthcheck_instruction_token1,
      aux_sym_shell_instruction_token1,
      aux_sym_maintainer_instruction_token1,
      aux_sym_cross_build_instruction_token1,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [311] = 13,
    ACTIONS(113), 1,
      aux_sym_path_token2,
    ACTIONS(115), 1,
      anon_sym_DASH_DASH,
    ACTIONS(117), 1,
      aux_sym_shell_fragment_token2,
    ACTIONS(119), 1,
      aux_sym_shell_fragment_token3,
    ACTIONS(121), 1,
      aux_sym_shell_fragment_token4,
    ACTIONS(123), 1,
      anon_sym_LBRACK,
    ACTIONS(125), 1,
      sym_semgrep_ellipsis,
    ACTIONS(127), 1,
      sym_heredoc_marker,
    STATE(37), 1,
      aux_sym_shell_fragment_repeat1,
    STATE(229), 1,
      sym_shell_fragment,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    STATE(216), 2,
      sym_shell_command,
      sym_json_string_array,
    STATE(7), 3,
      sym_param,
      sym_mount_param,
      aux_sym_run_instruction_repeat1,
  [355] = 13,
    ACTIONS(113), 1,
      aux_sym_path_token2,
    ACTIONS(115), 1,
      anon_sym_DASH_DASH,
    ACTIONS(117), 1,
      aux_sym_shell_fragment_token2,
    ACTIONS(119), 1,
      aux_sym_shell_fragment_token3,
    ACTIONS(121), 1,
      aux_sym_shell_fragment_token4,
    ACTIONS(123), 1,
      anon_sym_LBRACK,
    ACTIONS(125), 1,
      sym_semgrep_ellipsis,
    ACTIONS(127), 1,
      sym_heredoc_marker,
    STATE(37), 1,
      aux_sym_shell_fragment_repeat1,
    STATE(229), 1,
      sym_shell_fragment,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    STATE(210), 2,
      sym_shell_command,
      sym_json_string_array,
    STATE(13), 3,
      sym_param,
      sym_mount_param,
      aux_sym_run_instruction_repeat1,
  [399] = 11,
    ACTIONS(129), 1,
      anon_sym_LF,
    ACTIONS(131), 1,
      anon_sym_DOLLAR2,
    ACTIONS(135), 1,
      anon_sym_DQUOTE,
    ACTIONS(137), 1,
      anon_sym_SQUOTE,
    ACTIONS(139), 1,
      aux_sym_unquoted_string_token1,
    ACTIONS(141), 1,
      anon_sym_BSLASH2,
    STATE(144), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(133), 2,
      aux_sym_env_key_token1,
      sym_semgrep_ellipsis,
    STATE(40), 2,
      sym_immediate_expansion,
      aux_sym_unquoted_string_repeat1,
    STATE(267), 3,
      sym_double_quoted_string,
      sym_single_quoted_string,
      sym_unquoted_string,
  [438] = 8,
    ACTIONS(143), 1,
      anon_sym_LF,
    ACTIONS(145), 1,
      anon_sym_DOLLAR2,
    ACTIONS(149), 1,
      aux_sym_unquoted_string_token1,
    ACTIONS(151), 1,
      anon_sym_BSLASH2,
    STATE(42), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(10), 2,
      sym_immediate_expansion,
      aux_sym_unquoted_string_repeat1,
    ACTIONS(147), 5,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [469] = 8,
    ACTIONS(153), 1,
      anon_sym_LF,
    ACTIONS(155), 1,
      anon_sym_DOLLAR2,
    ACTIONS(160), 1,
      aux_sym_unquoted_string_token1,
    ACTIONS(163), 1,
      anon_sym_BSLASH2,
    STATE(42), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(10), 2,
      sym_immediate_expansion,
      aux_sym_unquoted_string_repeat1,
    ACTIONS(158), 5,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [500] = 11,
    ACTIONS(166), 1,
      aux_sym_path_token2,
    ACTIONS(168), 1,
      aux_sym_shell_fragment_token2,
    ACTIONS(170), 1,
      aux_sym_shell_fragment_token3,
    ACTIONS(172), 1,
      aux_sym_shell_fragment_token4,
    ACTIONS(174), 1,
      anon_sym_LBRACK,
    ACTIONS(176), 1,
      sym_semgrep_ellipsis,
    ACTIONS(178), 1,
      sym_heredoc_marker,
    STATE(57), 1,
      aux_sym_shell_fragment_repeat1,
    STATE(282), 1,
      sym_shell_fragment,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    STATE(362), 2,
      sym_shell_command,
      sym_json_string_array,
  [536] = 11,
    ACTIONS(166), 1,
      aux_sym_path_token2,
    ACTIONS(168), 1,
      aux_sym_shell_fragment_token2,
    ACTIONS(170), 1,
      aux_sym_shell_fragment_token3,
    ACTIONS(172), 1,
      aux_sym_shell_fragment_token4,
    ACTIONS(174), 1,
      anon_sym_LBRACK,
    ACTIONS(176), 1,
      sym_semgrep_ellipsis,
    ACTIONS(178), 1,
      sym_heredoc_marker,
    STATE(57), 1,
      aux_sym_shell_fragment_repeat1,
    STATE(282), 1,
      sym_shell_fragment,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    STATE(370), 2,
      sym_shell_command,
      sym_json_string_array,
  [572] = 5,
    ACTIONS(182), 1,
      anon_sym_DASH_DASH,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(185), 2,
      sym_heredoc_marker,
      anon_sym_LBRACK,
    STATE(13), 3,
      sym_param,
      sym_mount_param,
      aux_sym_run_instruction_repeat1,
    ACTIONS(180), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_semgrep_ellipsis,
  [596] = 10,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(149), 1,
      aux_sym_unquoted_string_token1,
    ACTIONS(151), 1,
      anon_sym_BSLASH2,
    ACTIONS(187), 1,
      anon_sym_DOLLAR2,
    ACTIONS(189), 1,
      anon_sym_DQUOTE,
    ACTIONS(191), 1,
      anon_sym_SQUOTE,
    STATE(42), 1,
      sym_imm_expansion,
    STATE(9), 2,
      sym_immediate_expansion,
      aux_sym_unquoted_string_repeat1,
    STATE(114), 3,
      sym_double_quoted_string,
      sym_single_quoted_string,
      sym_unquoted_string,
  [630] = 10,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(189), 1,
      anon_sym_DQUOTE,
    ACTIONS(191), 1,
      anon_sym_SQUOTE,
    ACTIONS(193), 1,
      anon_sym_DOLLAR2,
    ACTIONS(195), 1,
      aux_sym_unquoted_string_token1,
    ACTIONS(197), 1,
      anon_sym_BSLASH2,
    STATE(223), 1,
      sym_imm_expansion,
    STATE(97), 2,
      sym_immediate_expansion,
      aux_sym_unquoted_string_repeat1,
    STATE(382), 3,
      sym_double_quoted_string,
      sym_single_quoted_string,
      sym_unquoted_string,
  [664] = 10,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(189), 1,
      anon_sym_DQUOTE,
    ACTIONS(191), 1,
      anon_sym_SQUOTE,
    ACTIONS(193), 1,
      anon_sym_DOLLAR2,
    ACTIONS(195), 1,
      aux_sym_unquoted_string_token1,
    ACTIONS(197), 1,
      anon_sym_BSLASH2,
    STATE(223), 1,
      sym_imm_expansion,
    STATE(97), 2,
      sym_immediate_expansion,
      aux_sym_unquoted_string_repeat1,
    STATE(383), 3,
      sym_double_quoted_string,
      sym_single_quoted_string,
      sym_unquoted_string,
  [698] = 10,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(189), 1,
      anon_sym_DQUOTE,
    ACTIONS(191), 1,
      anon_sym_SQUOTE,
    ACTIONS(193), 1,
      anon_sym_DOLLAR2,
    ACTIONS(195), 1,
      aux_sym_unquoted_string_token1,
    ACTIONS(197), 1,
      anon_sym_BSLASH2,
    STATE(223), 1,
      sym_imm_expansion,
    STATE(97), 2,
      sym_immediate_expansion,
      aux_sym_unquoted_string_repeat1,
    STATE(345), 3,
      sym_double_quoted_string,
      sym_single_quoted_string,
      sym_unquoted_string,
  [732] = 10,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(149), 1,
      aux_sym_unquoted_string_token1,
    ACTIONS(151), 1,
      anon_sym_BSLASH2,
    ACTIONS(187), 1,
      anon_sym_DOLLAR2,
    ACTIONS(189), 1,
      anon_sym_DQUOTE,
    ACTIONS(191), 1,
      anon_sym_SQUOTE,
    STATE(42), 1,
      sym_imm_expansion,
    STATE(9), 2,
      sym_immediate_expansion,
      aux_sym_unquoted_string_repeat1,
    STATE(109), 3,
      sym_double_quoted_string,
      sym_single_quoted_string,
      sym_unquoted_string,
  [766] = 11,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(199), 1,
      aux_sym_path_token1,
    ACTIONS(201), 1,
      aux_sym_path_with_heredoc_token1,
    ACTIONS(203), 1,
      anon_sym_DOLLAR,
    ACTIONS(205), 1,
      anon_sym_DASH_DASH,
    ACTIONS(207), 1,
      sym_heredoc_marker,
    STATE(105), 1,
      aux_sym_add_instruction_repeat2,
    STATE(153), 1,
      sym_expansion,
    STATE(358), 1,
      sym_path_with_heredoc,
    STATE(23), 2,
      sym_param,
      aux_sym_add_instruction_repeat1,
  [801] = 11,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(199), 1,
      aux_sym_path_token1,
    ACTIONS(201), 1,
      aux_sym_path_with_heredoc_token1,
    ACTIONS(203), 1,
      anon_sym_DOLLAR,
    ACTIONS(205), 1,
      anon_sym_DASH_DASH,
    ACTIONS(207), 1,
      sym_heredoc_marker,
    STATE(85), 1,
      aux_sym_add_instruction_repeat2,
    STATE(153), 1,
      sym_expansion,
    STATE(358), 1,
      sym_path_with_heredoc,
    STATE(21), 2,
      sym_param,
      aux_sym_add_instruction_repeat1,
  [836] = 11,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(199), 1,
      aux_sym_path_token1,
    ACTIONS(201), 1,
      aux_sym_path_with_heredoc_token1,
    ACTIONS(203), 1,
      anon_sym_DOLLAR,
    ACTIONS(205), 1,
      anon_sym_DASH_DASH,
    ACTIONS(207), 1,
      sym_heredoc_marker,
    STATE(96), 1,
      aux_sym_add_instruction_repeat2,
    STATE(153), 1,
      sym_expansion,
    STATE(358), 1,
      sym_path_with_heredoc,
    STATE(95), 2,
      sym_param,
      aux_sym_add_instruction_repeat1,
  [871] = 9,
    ACTIONS(209), 1,
      anon_sym_LF,
    ACTIONS(211), 1,
      aux_sym_label_pair_token1,
    ACTIONS(213), 1,
      anon_sym_DQUOTE,
    ACTIONS(215), 1,
      anon_sym_SQUOTE,
    ACTIONS(217), 1,
      sym_semgrep_metavariable,
    ACTIONS(219), 1,
      sym_semgrep_ellipsis,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(24), 2,
      sym_label_pair,
      aux_sym_label_instruction_repeat1,
    STATE(401), 2,
      sym_double_quoted_string,
      sym_single_quoted_string,
  [902] = 11,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(199), 1,
      aux_sym_path_token1,
    ACTIONS(201), 1,
      aux_sym_path_with_heredoc_token1,
    ACTIONS(203), 1,
      anon_sym_DOLLAR,
    ACTIONS(205), 1,
      anon_sym_DASH_DASH,
    ACTIONS(207), 1,
      sym_heredoc_marker,
    STATE(100), 1,
      aux_sym_add_instruction_repeat2,
    STATE(153), 1,
      sym_expansion,
    STATE(358), 1,
      sym_path_with_heredoc,
    STATE(95), 2,
      sym_param,
      aux_sym_add_instruction_repeat1,
  [937] = 9,
    ACTIONS(221), 1,
      anon_sym_LF,
    ACTIONS(223), 1,
      aux_sym_label_pair_token1,
    ACTIONS(226), 1,
      anon_sym_DQUOTE,
    ACTIONS(229), 1,
      anon_sym_SQUOTE,
    ACTIONS(232), 1,
      sym_semgrep_metavariable,
    ACTIONS(235), 1,
      sym_semgrep_ellipsis,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(24), 2,
      sym_label_pair,
      aux_sym_label_instruction_repeat1,
    STATE(401), 2,
      sym_double_quoted_string,
      sym_single_quoted_string,
  [968] = 5,
    ACTIONS(242), 1,
      anon_sym_COMMA,
    STATE(27), 1,
      aux_sym_mount_param_repeat1,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(240), 3,
      sym_heredoc_marker,
      anon_sym_DASH_DASH,
      anon_sym_LBRACK,
    ACTIONS(238), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_semgrep_ellipsis,
  [991] = 5,
    ACTIONS(242), 1,
      anon_sym_COMMA,
    STATE(25), 1,
      aux_sym_mount_param_repeat1,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(246), 3,
      sym_heredoc_marker,
      anon_sym_DASH_DASH,
      anon_sym_LBRACK,
    ACTIONS(244), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_semgrep_ellipsis,
  [1014] = 5,
    ACTIONS(252), 1,
      anon_sym_COMMA,
    STATE(27), 1,
      aux_sym_mount_param_repeat1,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(250), 3,
      sym_heredoc_marker,
      anon_sym_DASH_DASH,
      anon_sym_LBRACK,
    ACTIONS(248), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_semgrep_ellipsis,
  [1037] = 7,
    ACTIONS(257), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(259), 1,
      anon_sym_DOLLAR2,
    ACTIONS(261), 1,
      aux_sym_image_name_token2,
    STATE(148), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(32), 2,
      sym_immediate_expansion,
      aux_sym_image_name_repeat1,
    ACTIONS(255), 3,
      anon_sym_LF,
      anon_sym_COLON,
      anon_sym_AT,
  [1063] = 8,
    ACTIONS(211), 1,
      aux_sym_label_pair_token1,
    ACTIONS(219), 1,
      sym_semgrep_ellipsis,
    ACTIONS(263), 1,
      anon_sym_DQUOTE,
    ACTIONS(265), 1,
      anon_sym_SQUOTE,
    ACTIONS(267), 1,
      sym_semgrep_metavariable,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    STATE(22), 2,
      sym_label_pair,
      aux_sym_label_instruction_repeat1,
    STATE(401), 2,
      sym_double_quoted_string,
      sym_single_quoted_string,
  [1091] = 7,
    ACTIONS(259), 1,
      anon_sym_DOLLAR2,
    ACTIONS(261), 1,
      aux_sym_image_name_token2,
    ACTIONS(271), 1,
      aux_sym_from_instruction_token2,
    STATE(148), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(31), 2,
      sym_immediate_expansion,
      aux_sym_image_name_repeat1,
    ACTIONS(269), 3,
      anon_sym_LF,
      anon_sym_COLON,
      anon_sym_AT,
  [1117] = 7,
    ACTIONS(259), 1,
      anon_sym_DOLLAR2,
    ACTIONS(261), 1,
      aux_sym_image_name_token2,
    ACTIONS(275), 1,
      aux_sym_from_instruction_token2,
    STATE(148), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(34), 2,
      sym_immediate_expansion,
      aux_sym_image_name_repeat1,
    ACTIONS(273), 3,
      anon_sym_LF,
      anon_sym_COLON,
      anon_sym_AT,
  [1143] = 7,
    ACTIONS(259), 1,
      anon_sym_DOLLAR2,
    ACTIONS(261), 1,
      aux_sym_image_name_token2,
    ACTIONS(279), 1,
      aux_sym_from_instruction_token2,
    STATE(148), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(34), 2,
      sym_immediate_expansion,
      aux_sym_image_name_repeat1,
    ACTIONS(277), 3,
      anon_sym_LF,
      anon_sym_COLON,
      anon_sym_AT,
  [1169] = 9,
    ACTIONS(283), 1,
      aux_sym_path_token2,
    ACTIONS(286), 1,
      aux_sym_shell_fragment_token2,
    ACTIONS(289), 1,
      aux_sym_shell_fragment_token3,
    ACTIONS(292), 1,
      aux_sym_shell_fragment_token4,
    ACTIONS(295), 1,
      sym_required_line_continuation,
    ACTIONS(297), 1,
      sym_heredoc_marker,
    STATE(33), 1,
      aux_sym_shell_fragment_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(281), 2,
      sym_heredoc_nl,
      anon_sym_LF,
  [1199] = 7,
    ACTIONS(302), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(304), 1,
      anon_sym_DOLLAR2,
    ACTIONS(307), 1,
      aux_sym_image_name_token2,
    STATE(148), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(34), 2,
      sym_immediate_expansion,
      aux_sym_image_name_repeat1,
    ACTIONS(300), 3,
      anon_sym_LF,
      anon_sym_COLON,
      anon_sym_AT,
  [1225] = 3,
    ACTIONS(310), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(312), 8,
      anon_sym_DOLLAR2,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [1243] = 3,
    ACTIONS(314), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(316), 8,
      anon_sym_DOLLAR2,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [1261] = 9,
    ACTIONS(113), 1,
      aux_sym_path_token2,
    ACTIONS(117), 1,
      aux_sym_shell_fragment_token2,
    ACTIONS(119), 1,
      aux_sym_shell_fragment_token3,
    ACTIONS(121), 1,
      aux_sym_shell_fragment_token4,
    ACTIONS(127), 1,
      sym_heredoc_marker,
    ACTIONS(320), 1,
      sym_required_line_continuation,
    STATE(33), 1,
      aux_sym_shell_fragment_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(318), 2,
      sym_heredoc_nl,
      anon_sym_LF,
  [1291] = 3,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(324), 4,
      sym_heredoc_marker,
      anon_sym_DASH_DASH,
      anon_sym_COMMA,
      anon_sym_LBRACK,
    ACTIONS(322), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_semgrep_ellipsis,
  [1309] = 3,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(328), 4,
      sym_heredoc_marker,
      anon_sym_DASH_DASH,
      anon_sym_COMMA,
      anon_sym_LBRACK,
    ACTIONS(326), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_semgrep_ellipsis,
  [1327] = 8,
    ACTIONS(131), 1,
      anon_sym_DOLLAR2,
    ACTIONS(139), 1,
      aux_sym_unquoted_string_token1,
    ACTIONS(141), 1,
      anon_sym_BSLASH2,
    ACTIONS(143), 1,
      anon_sym_LF,
    STATE(144), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(147), 2,
      aux_sym_env_key_token1,
      sym_semgrep_ellipsis,
    STATE(41), 2,
      sym_immediate_expansion,
      aux_sym_unquoted_string_repeat1,
  [1355] = 8,
    ACTIONS(153), 1,
      anon_sym_LF,
    ACTIONS(330), 1,
      anon_sym_DOLLAR2,
    ACTIONS(333), 1,
      aux_sym_unquoted_string_token1,
    ACTIONS(336), 1,
      anon_sym_BSLASH2,
    STATE(144), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(158), 2,
      aux_sym_env_key_token1,
      sym_semgrep_ellipsis,
    STATE(41), 2,
      sym_immediate_expansion,
      aux_sym_unquoted_string_repeat1,
  [1383] = 3,
    ACTIONS(339), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(341), 8,
      anon_sym_DOLLAR2,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [1401] = 3,
    ACTIONS(343), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(345), 8,
      anon_sym_DOLLAR2,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [1419] = 3,
    ACTIONS(347), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(349), 8,
      anon_sym_DOLLAR2,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [1437] = 3,
    ACTIONS(351), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(353), 8,
      anon_sym_DOLLAR2,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [1455] = 7,
    ACTIONS(355), 1,
      anon_sym_LF,
    ACTIONS(357), 1,
      aux_sym_path_token3,
    ACTIONS(359), 1,
      anon_sym_DOLLAR2,
    STATE(183), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(361), 2,
      sym_heredoc_nl,
      sym_non_newline_whitespace,
    STATE(68), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [1480] = 7,
    ACTIONS(365), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(367), 1,
      anon_sym_DOLLAR2,
    ACTIONS(369), 1,
      aux_sym_image_tag_token1,
    STATE(176), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(363), 2,
      anon_sym_LF,
      anon_sym_AT,
    STATE(53), 2,
      sym_immediate_expansion,
      aux_sym_image_tag_repeat1,
  [1505] = 7,
    ACTIONS(373), 1,
      aux_sym_immediate_user_name_or_group_fragment_token1,
    ACTIONS(375), 1,
      anon_sym_DOLLAR2,
    STATE(194), 1,
      sym_immediate_expansion,
    STATE(204), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(371), 2,
      anon_sym_LF,
      anon_sym_COLON,
    STATE(71), 2,
      sym_immediate_user_name_or_group_fragment,
      aux_sym_user_name_or_group_repeat1,
  [1530] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(377), 3,
      sym_heredoc_marker,
      sym_heredoc_nl,
      anon_sym_LF,
    ACTIONS(379), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_required_line_continuation,
  [1547] = 7,
    ACTIONS(13), 1,
      aux_sym_cmd_instruction_token1,
    ACTIONS(383), 1,
      anon_sym_DASH_DASH,
    ACTIONS(385), 1,
      sym_semgrep_ellipsis,
    STATE(399), 1,
      sym_cmd_instruction,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(381), 2,
      anon_sym_NONE,
      sym_semgrep_metavariable,
    STATE(115), 2,
      sym_param,
      aux_sym_healthcheck_instruction_repeat1,
  [1572] = 8,
    ACTIONS(387), 1,
      anon_sym_DOLLAR2,
    ACTIONS(390), 1,
      anon_sym_DQUOTE,
    ACTIONS(392), 1,
      aux_sym_double_quoted_string_token1,
    ACTIONS(395), 1,
      anon_sym_BSLASH,
    ACTIONS(398), 1,
      sym_double_quoted_escape_sequence,
    STATE(161), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(51), 2,
      sym_immediate_expansion,
      aux_sym_double_quoted_string_repeat1,
  [1599] = 7,
    ACTIONS(403), 1,
      aux_sym_immediate_user_name_or_group_fragment_token1,
    ACTIONS(406), 1,
      anon_sym_DOLLAR2,
    STATE(194), 1,
      sym_immediate_expansion,
    STATE(204), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(401), 2,
      anon_sym_LF,
      anon_sym_COLON,
    STATE(52), 2,
      sym_immediate_user_name_or_group_fragment,
      aux_sym_user_name_or_group_repeat1,
  [1624] = 7,
    ACTIONS(411), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(413), 1,
      anon_sym_DOLLAR2,
    ACTIONS(416), 1,
      aux_sym_image_tag_token1,
    STATE(176), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(409), 2,
      anon_sym_LF,
      anon_sym_AT,
    STATE(53), 2,
      sym_immediate_expansion,
      aux_sym_image_tag_repeat1,
  [1649] = 8,
    ACTIONS(419), 1,
      anon_sym_DOLLAR2,
    ACTIONS(421), 1,
      anon_sym_DQUOTE,
    ACTIONS(423), 1,
      aux_sym_double_quoted_string_token1,
    ACTIONS(425), 1,
      anon_sym_BSLASH,
    ACTIONS(427), 1,
      sym_double_quoted_escape_sequence,
    STATE(161), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(55), 2,
      sym_immediate_expansion,
      aux_sym_double_quoted_string_repeat1,
  [1676] = 8,
    ACTIONS(419), 1,
      anon_sym_DOLLAR2,
    ACTIONS(423), 1,
      aux_sym_double_quoted_string_token1,
    ACTIONS(429), 1,
      anon_sym_DQUOTE,
    ACTIONS(431), 1,
      anon_sym_BSLASH,
    ACTIONS(433), 1,
      sym_double_quoted_escape_sequence,
    STATE(161), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(51), 2,
      sym_immediate_expansion,
      aux_sym_double_quoted_string_repeat1,
  [1703] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(435), 3,
      sym_heredoc_marker,
      sym_heredoc_nl,
      anon_sym_LF,
    ACTIONS(437), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_required_line_continuation,
  [1720] = 9,
    ACTIONS(166), 1,
      aux_sym_path_token2,
    ACTIONS(170), 1,
      aux_sym_shell_fragment_token3,
    ACTIONS(172), 1,
      aux_sym_shell_fragment_token4,
    ACTIONS(178), 1,
      sym_heredoc_marker,
    ACTIONS(318), 1,
      anon_sym_LF,
    ACTIONS(320), 1,
      sym_required_line_continuation,
    ACTIONS(439), 1,
      aux_sym_shell_fragment_token2,
    STATE(61), 1,
      aux_sym_shell_fragment_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [1749] = 7,
    ACTIONS(357), 1,
      aux_sym_path_token3,
    ACTIONS(359), 1,
      anon_sym_DOLLAR2,
    ACTIONS(441), 1,
      anon_sym_LF,
    STATE(183), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(443), 2,
      sym_heredoc_nl,
      sym_non_newline_whitespace,
    STATE(62), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [1774] = 7,
    ACTIONS(357), 1,
      aux_sym_path_token3,
    ACTIONS(359), 1,
      anon_sym_DOLLAR2,
    ACTIONS(445), 1,
      anon_sym_LF,
    STATE(183), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(447), 2,
      sym_heredoc_nl,
      sym_non_newline_whitespace,
    STATE(63), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [1799] = 7,
    ACTIONS(357), 1,
      aux_sym_path_token3,
    ACTIONS(359), 1,
      anon_sym_DOLLAR2,
    ACTIONS(449), 1,
      anon_sym_LF,
    STATE(183), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(451), 2,
      sym_heredoc_nl,
      sym_non_newline_whitespace,
    STATE(46), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [1824] = 9,
    ACTIONS(281), 1,
      anon_sym_LF,
    ACTIONS(295), 1,
      sym_required_line_continuation,
    ACTIONS(453), 1,
      aux_sym_path_token2,
    ACTIONS(456), 1,
      aux_sym_shell_fragment_token2,
    ACTIONS(459), 1,
      aux_sym_shell_fragment_token3,
    ACTIONS(462), 1,
      aux_sym_shell_fragment_token4,
    ACTIONS(465), 1,
      sym_heredoc_marker,
    STATE(61), 1,
      aux_sym_shell_fragment_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [1853] = 7,
    ACTIONS(357), 1,
      aux_sym_path_token3,
    ACTIONS(359), 1,
      anon_sym_DOLLAR2,
    ACTIONS(468), 1,
      anon_sym_LF,
    STATE(183), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(470), 2,
      sym_heredoc_nl,
      sym_non_newline_whitespace,
    STATE(68), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [1878] = 7,
    ACTIONS(357), 1,
      aux_sym_path_token3,
    ACTIONS(359), 1,
      anon_sym_DOLLAR2,
    ACTIONS(472), 1,
      anon_sym_LF,
    STATE(183), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(474), 2,
      sym_heredoc_nl,
      sym_non_newline_whitespace,
    STATE(68), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [1903] = 8,
    ACTIONS(419), 1,
      anon_sym_DOLLAR2,
    ACTIONS(423), 1,
      aux_sym_double_quoted_string_token1,
    ACTIONS(431), 1,
      anon_sym_BSLASH,
    ACTIONS(433), 1,
      sym_double_quoted_escape_sequence,
    ACTIONS(476), 1,
      anon_sym_DQUOTE,
    STATE(161), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(51), 2,
      sym_immediate_expansion,
      aux_sym_double_quoted_string_repeat1,
  [1930] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(478), 3,
      sym_heredoc_marker,
      sym_heredoc_nl,
      anon_sym_LF,
    ACTIONS(480), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_required_line_continuation,
  [1947] = 3,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(484), 3,
      sym_heredoc_marker,
      anon_sym_DASH_DASH,
      anon_sym_LBRACK,
    ACTIONS(482), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_semgrep_ellipsis,
  [1964] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(486), 3,
      sym_heredoc_marker,
      sym_heredoc_nl,
      anon_sym_LF,
    ACTIONS(488), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_required_line_continuation,
  [1981] = 7,
    ACTIONS(490), 1,
      anon_sym_LF,
    ACTIONS(492), 1,
      aux_sym_path_token3,
    ACTIONS(495), 1,
      anon_sym_DOLLAR2,
    STATE(183), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(498), 2,
      sym_heredoc_nl,
      sym_non_newline_whitespace,
    STATE(68), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [2006] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(500), 3,
      sym_heredoc_marker,
      sym_heredoc_nl,
      anon_sym_LF,
    ACTIONS(502), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_required_line_continuation,
  [2023] = 7,
    ACTIONS(373), 1,
      aux_sym_immediate_user_name_or_group_fragment_token1,
    ACTIONS(375), 1,
      anon_sym_DOLLAR2,
    STATE(194), 1,
      sym_immediate_expansion,
    STATE(204), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(504), 2,
      anon_sym_LF,
      anon_sym_COLON,
    STATE(52), 2,
      sym_immediate_user_name_or_group_fragment,
      aux_sym_user_name_or_group_repeat1,
  [2048] = 7,
    ACTIONS(373), 1,
      aux_sym_immediate_user_name_or_group_fragment_token1,
    ACTIONS(375), 1,
      anon_sym_DOLLAR2,
    STATE(194), 1,
      sym_immediate_expansion,
    STATE(204), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(506), 2,
      anon_sym_LF,
      anon_sym_COLON,
    STATE(52), 2,
      sym_immediate_user_name_or_group_fragment,
      aux_sym_user_name_or_group_repeat1,
  [2073] = 7,
    ACTIONS(373), 1,
      aux_sym_immediate_user_name_or_group_fragment_token1,
    ACTIONS(375), 1,
      anon_sym_DOLLAR2,
    STATE(194), 1,
      sym_immediate_expansion,
    STATE(204), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(508), 2,
      anon_sym_LF,
      anon_sym_COLON,
    STATE(70), 2,
      sym_immediate_user_name_or_group_fragment,
      aux_sym_user_name_or_group_repeat1,
  [2098] = 8,
    ACTIONS(419), 1,
      anon_sym_DOLLAR2,
    ACTIONS(423), 1,
      aux_sym_double_quoted_string_token1,
    ACTIONS(510), 1,
      anon_sym_DQUOTE,
    ACTIONS(512), 1,
      anon_sym_BSLASH,
    ACTIONS(514), 1,
      sym_double_quoted_escape_sequence,
    STATE(161), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(74), 2,
      sym_immediate_expansion,
      aux_sym_double_quoted_string_repeat1,
  [2125] = 8,
    ACTIONS(419), 1,
      anon_sym_DOLLAR2,
    ACTIONS(423), 1,
      aux_sym_double_quoted_string_token1,
    ACTIONS(431), 1,
      anon_sym_BSLASH,
    ACTIONS(433), 1,
      sym_double_quoted_escape_sequence,
    ACTIONS(516), 1,
      anon_sym_DQUOTE,
    STATE(161), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(51), 2,
      sym_immediate_expansion,
      aux_sym_double_quoted_string_repeat1,
  [2152] = 8,
    ACTIONS(419), 1,
      anon_sym_DOLLAR2,
    ACTIONS(423), 1,
      aux_sym_double_quoted_string_token1,
    ACTIONS(518), 1,
      anon_sym_DQUOTE,
    ACTIONS(520), 1,
      anon_sym_BSLASH,
    ACTIONS(522), 1,
      sym_double_quoted_escape_sequence,
    STATE(161), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(64), 2,
      sym_immediate_expansion,
      aux_sym_double_quoted_string_repeat1,
  [2179] = 7,
    ACTIONS(524), 1,
      anon_sym_LF,
    ACTIONS(526), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(528), 1,
      anon_sym_DOLLAR2,
    ACTIONS(531), 1,
      aux_sym_image_digest_token1,
    STATE(213), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(76), 2,
      sym_immediate_expansion,
      aux_sym_image_digest_repeat1,
  [2203] = 7,
    ACTIONS(373), 1,
      aux_sym_immediate_user_name_or_group_fragment_token1,
    ACTIONS(375), 1,
      anon_sym_DOLLAR2,
    STATE(194), 1,
      sym_immediate_expansion,
    STATE(204), 1,
      sym_imm_expansion,
    STATE(378), 1,
      sym_immediate_user_name_or_group,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    STATE(83), 2,
      sym_immediate_user_name_or_group_fragment,
      aux_sym_user_name_or_group_repeat1,
  [2227] = 7,
    ACTIONS(534), 1,
      anon_sym_LF,
    ACTIONS(536), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(538), 1,
      anon_sym_DOLLAR2,
    ACTIONS(540), 1,
      aux_sym_image_digest_token1,
    STATE(213), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(76), 2,
      sym_immediate_expansion,
      aux_sym_image_digest_repeat1,
  [2251] = 7,
    ACTIONS(542), 1,
      anon_sym_LF,
    ACTIONS(544), 1,
      aux_sym_path_token3,
    ACTIONS(546), 1,
      anon_sym_DOLLAR2,
    ACTIONS(548), 1,
      sym_non_newline_whitespace,
    STATE(197), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(101), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [2275] = 7,
    ACTIONS(544), 1,
      aux_sym_path_token3,
    ACTIONS(546), 1,
      anon_sym_DOLLAR2,
    ACTIONS(550), 1,
      anon_sym_LF,
    ACTIONS(552), 1,
      sym_non_newline_whitespace,
    STATE(197), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(103), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [2299] = 7,
    ACTIONS(544), 1,
      aux_sym_path_token3,
    ACTIONS(546), 1,
      anon_sym_DOLLAR2,
    ACTIONS(554), 1,
      anon_sym_LF,
    ACTIONS(556), 1,
      sym_non_newline_whitespace,
    STATE(197), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(104), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [2323] = 8,
    ACTIONS(113), 1,
      aux_sym_path_token2,
    ACTIONS(119), 1,
      aux_sym_shell_fragment_token3,
    ACTIONS(121), 1,
      aux_sym_shell_fragment_token4,
    ACTIONS(127), 1,
      sym_heredoc_marker,
    ACTIONS(558), 1,
      aux_sym_shell_fragment_token2,
    STATE(37), 1,
      aux_sym_shell_fragment_repeat1,
    STATE(256), 1,
      sym_shell_fragment,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [2349] = 7,
    ACTIONS(373), 1,
      aux_sym_immediate_user_name_or_group_fragment_token1,
    ACTIONS(375), 1,
      anon_sym_DOLLAR2,
    ACTIONS(560), 1,
      anon_sym_LF,
    STATE(194), 1,
      sym_immediate_expansion,
    STATE(204), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(52), 2,
      sym_immediate_user_name_or_group_fragment,
      aux_sym_user_name_or_group_repeat1,
  [2373] = 9,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(562), 1,
      anon_sym_DOLLAR,
    ACTIONS(564), 1,
      aux_sym_image_name_token1,
    ACTIONS(566), 1,
      anon_sym_DASH_DASH,
    STATE(28), 1,
      sym_expansion,
    STATE(152), 1,
      sym_image_name,
    STATE(168), 1,
      sym_param,
    STATE(321), 1,
      sym_image_spec,
  [2401] = 9,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(568), 1,
      aux_sym_path_token1,
    ACTIONS(570), 1,
      aux_sym_path_with_heredoc_token1,
    ACTIONS(572), 1,
      anon_sym_DOLLAR,
    ACTIONS(574), 1,
      sym_heredoc_marker,
    STATE(60), 1,
      sym_expansion,
    STATE(99), 1,
      aux_sym_add_instruction_repeat2,
    STATE(162), 1,
      sym_path_with_heredoc,
  [2429] = 9,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(174), 1,
      anon_sym_LBRACK,
    ACTIONS(576), 1,
      aux_sym_path_token1,
    ACTIONS(578), 1,
      aux_sym_path_token2,
    ACTIONS(580), 1,
      anon_sym_DOLLAR,
    STATE(81), 1,
      sym_expansion,
    STATE(251), 1,
      sym_path,
    STATE(346), 1,
      sym_json_string_array,
  [2457] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(435), 2,
      sym_heredoc_marker,
      anon_sym_LF,
    ACTIONS(437), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_required_line_continuation,
  [2473] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(478), 2,
      sym_heredoc_marker,
      anon_sym_LF,
    ACTIONS(480), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_required_line_continuation,
  [2489] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(486), 2,
      sym_heredoc_marker,
      anon_sym_LF,
    ACTIONS(488), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_required_line_continuation,
  [2505] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(500), 2,
      sym_heredoc_marker,
      anon_sym_LF,
    ACTIONS(502), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_required_line_continuation,
  [2521] = 6,
    ACTIONS(582), 1,
      anon_sym_LF,
    ACTIONS(584), 1,
      anon_sym_DOLLAR,
    ACTIONS(587), 1,
      aux_sym_expose_port_token1,
    ACTIONS(590), 1,
      sym_semgrep_ellipsis,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(91), 3,
      sym_expansion,
      sym_expose_port,
      aux_sym_expose_instruction_repeat1,
  [2543] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(377), 2,
      sym_heredoc_marker,
      anon_sym_LF,
    ACTIONS(379), 5,
      aux_sym_path_token2,
      aux_sym_shell_fragment_token2,
      aux_sym_shell_fragment_token3,
      aux_sym_shell_fragment_token4,
      sym_required_line_continuation,
  [2559] = 8,
    ACTIONS(166), 1,
      aux_sym_path_token2,
    ACTIONS(168), 1,
      aux_sym_shell_fragment_token2,
    ACTIONS(170), 1,
      aux_sym_shell_fragment_token3,
    ACTIONS(172), 1,
      aux_sym_shell_fragment_token4,
    ACTIONS(178), 1,
      sym_heredoc_marker,
    STATE(57), 1,
      aux_sym_shell_fragment_repeat1,
    STATE(333), 1,
      sym_shell_fragment,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [2585] = 7,
    ACTIONS(490), 1,
      anon_sym_LF,
    ACTIONS(498), 1,
      sym_non_newline_whitespace,
    ACTIONS(593), 1,
      aux_sym_path_token3,
    ACTIONS(596), 1,
      anon_sym_DOLLAR2,
    STATE(197), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(94), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [2609] = 6,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(599), 1,
      aux_sym_path_token1,
    ACTIONS(603), 1,
      anon_sym_DASH_DASH,
    STATE(95), 2,
      sym_param,
      aux_sym_add_instruction_repeat1,
    ACTIONS(601), 3,
      sym_heredoc_marker,
      aux_sym_path_with_heredoc_token1,
      anon_sym_DOLLAR,
  [2631] = 9,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(568), 1,
      aux_sym_path_token1,
    ACTIONS(570), 1,
      aux_sym_path_with_heredoc_token1,
    ACTIONS(572), 1,
      anon_sym_DOLLAR,
    ACTIONS(574), 1,
      sym_heredoc_marker,
    STATE(60), 1,
      sym_expansion,
    STATE(99), 1,
      aux_sym_add_instruction_repeat2,
    STATE(165), 1,
      sym_path_with_heredoc,
  [2659] = 7,
    ACTIONS(143), 1,
      anon_sym_LF,
    ACTIONS(193), 1,
      anon_sym_DOLLAR2,
    ACTIONS(195), 1,
      aux_sym_unquoted_string_token1,
    ACTIONS(197), 1,
      anon_sym_BSLASH2,
    STATE(223), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(98), 2,
      sym_immediate_expansion,
      aux_sym_unquoted_string_repeat1,
  [2683] = 7,
    ACTIONS(153), 1,
      anon_sym_LF,
    ACTIONS(606), 1,
      anon_sym_DOLLAR2,
    ACTIONS(609), 1,
      aux_sym_unquoted_string_token1,
    ACTIONS(612), 1,
      anon_sym_BSLASH2,
    STATE(223), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(98), 2,
      sym_immediate_expansion,
      aux_sym_unquoted_string_repeat1,
  [2707] = 9,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(615), 1,
      aux_sym_path_token1,
    ACTIONS(618), 1,
      aux_sym_path_with_heredoc_token1,
    ACTIONS(621), 1,
      anon_sym_DOLLAR,
    ACTIONS(624), 1,
      sym_heredoc_marker,
    STATE(99), 1,
      aux_sym_add_instruction_repeat2,
    STATE(153), 1,
      sym_expansion,
    STATE(358), 1,
      sym_path_with_heredoc,
  [2735] = 9,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(568), 1,
      aux_sym_path_token1,
    ACTIONS(570), 1,
      aux_sym_path_with_heredoc_token1,
    ACTIONS(572), 1,
      anon_sym_DOLLAR,
    ACTIONS(574), 1,
      sym_heredoc_marker,
    STATE(60), 1,
      sym_expansion,
    STATE(99), 1,
      aux_sym_add_instruction_repeat2,
    STATE(166), 1,
      sym_path_with_heredoc,
  [2763] = 7,
    ACTIONS(544), 1,
      aux_sym_path_token3,
    ACTIONS(546), 1,
      anon_sym_DOLLAR2,
    ACTIONS(627), 1,
      anon_sym_LF,
    ACTIONS(629), 1,
      sym_non_newline_whitespace,
    STATE(197), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(94), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [2787] = 6,
    ACTIONS(631), 1,
      anon_sym_LF,
    ACTIONS(633), 1,
      anon_sym_DOLLAR,
    ACTIONS(635), 1,
      aux_sym_expose_port_token1,
    ACTIONS(637), 1,
      sym_semgrep_ellipsis,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(91), 3,
      sym_expansion,
      sym_expose_port,
      aux_sym_expose_instruction_repeat1,
  [2809] = 7,
    ACTIONS(544), 1,
      aux_sym_path_token3,
    ACTIONS(546), 1,
      anon_sym_DOLLAR2,
    ACTIONS(639), 1,
      anon_sym_LF,
    ACTIONS(641), 1,
      sym_non_newline_whitespace,
    STATE(197), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(94), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [2833] = 7,
    ACTIONS(544), 1,
      aux_sym_path_token3,
    ACTIONS(546), 1,
      anon_sym_DOLLAR2,
    ACTIONS(643), 1,
      anon_sym_LF,
    ACTIONS(645), 1,
      sym_non_newline_whitespace,
    STATE(197), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(94), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [2857] = 9,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(568), 1,
      aux_sym_path_token1,
    ACTIONS(570), 1,
      aux_sym_path_with_heredoc_token1,
    ACTIONS(572), 1,
      anon_sym_DOLLAR,
    ACTIONS(574), 1,
      sym_heredoc_marker,
    STATE(60), 1,
      sym_expansion,
    STATE(99), 1,
      aux_sym_add_instruction_repeat2,
    STATE(167), 1,
      sym_path_with_heredoc,
  [2885] = 3,
    ACTIONS(647), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(649), 5,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [2900] = 4,
    ACTIONS(651), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(655), 2,
      anon_sym_SLASHtcp,
      anon_sym_SLASHudp,
    ACTIONS(653), 3,
      anon_sym_DOLLAR,
      aux_sym_expose_port_token1,
      sym_semgrep_ellipsis,
  [2917] = 6,
    ACTIONS(657), 1,
      anon_sym_LF,
    ACTIONS(659), 1,
      aux_sym_stopsignal_value_token2,
    ACTIONS(661), 1,
      anon_sym_DOLLAR2,
    STATE(279), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(136), 2,
      sym_immediate_expansion,
      aux_sym_stopsignal_value_repeat1,
  [2938] = 3,
    ACTIONS(663), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(665), 5,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [2953] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(345), 2,
      aux_sym_from_instruction_token2,
      aux_sym_image_name_token2,
    ACTIONS(343), 4,
      anon_sym_LF,
      anon_sym_COLON,
      anon_sym_DOLLAR2,
      anon_sym_AT,
  [2968] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(669), 2,
      aux_sym_from_instruction_token2,
      aux_sym_image_name_token2,
    ACTIONS(667), 4,
      anon_sym_LF,
      anon_sym_COLON,
      anon_sym_DOLLAR2,
      anon_sym_AT,
  [2983] = 6,
    ACTIONS(659), 1,
      aux_sym_stopsignal_value_token2,
    ACTIONS(661), 1,
      anon_sym_DOLLAR2,
    ACTIONS(671), 1,
      anon_sym_LF,
    STATE(279), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(143), 2,
      sym_immediate_expansion,
      aux_sym_stopsignal_value_repeat1,
  [3004] = 6,
    ACTIONS(659), 1,
      aux_sym_stopsignal_value_token2,
    ACTIONS(661), 1,
      anon_sym_DOLLAR2,
    ACTIONS(673), 1,
      anon_sym_LF,
    STATE(279), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(108), 2,
      sym_immediate_expansion,
      aux_sym_stopsignal_value_repeat1,
  [3025] = 3,
    ACTIONS(675), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(677), 5,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [3040] = 6,
    ACTIONS(13), 1,
      aux_sym_cmd_instruction_token1,
    ACTIONS(383), 1,
      anon_sym_DASH_DASH,
    ACTIONS(679), 1,
      sym_semgrep_ellipsis,
    STATE(359), 1,
      sym_cmd_instruction,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    STATE(185), 2,
      sym_param,
      aux_sym_healthcheck_instruction_repeat1,
  [3061] = 6,
    ACTIONS(548), 1,
      anon_sym_LF,
    ACTIONS(681), 1,
      aux_sym_path_token3,
    ACTIONS(683), 1,
      anon_sym_DOLLAR2,
    STATE(274), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(127), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [3082] = 6,
    ACTIONS(552), 1,
      anon_sym_LF,
    ACTIONS(681), 1,
      aux_sym_path_token3,
    ACTIONS(683), 1,
      anon_sym_DOLLAR2,
    STATE(274), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(128), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [3103] = 6,
    ACTIONS(556), 1,
      anon_sym_LF,
    ACTIONS(681), 1,
      aux_sym_path_token3,
    ACTIONS(683), 1,
      anon_sym_DOLLAR2,
    STATE(274), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(129), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [3124] = 6,
    ACTIONS(498), 1,
      sym_non_newline_whitespace,
    ACTIONS(685), 1,
      aux_sym_path_token3,
    ACTIONS(688), 1,
      anon_sym_DOLLAR2,
    STATE(259), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(119), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [3145] = 6,
    ACTIONS(691), 1,
      anon_sym_LF,
    ACTIONS(693), 1,
      anon_sym_DOLLAR2,
    ACTIONS(695), 1,
      aux_sym_image_alias_token2,
    STATE(290), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(138), 2,
      sym_immediate_expansion,
      aux_sym_image_alias_repeat1,
  [3166] = 5,
    ACTIONS(697), 1,
      anon_sym_DOLLAR,
    ACTIONS(699), 1,
      aux_sym_expose_port_token1,
    ACTIONS(701), 1,
      sym_semgrep_ellipsis,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    STATE(102), 3,
      sym_expansion,
      sym_expose_port,
      aux_sym_expose_instruction_repeat1,
  [3185] = 3,
    ACTIONS(703), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(705), 5,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [3200] = 6,
    ACTIONS(447), 1,
      sym_non_newline_whitespace,
    ACTIONS(707), 1,
      aux_sym_path_token3,
    ACTIONS(709), 1,
      anon_sym_DOLLAR2,
    STATE(259), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(126), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [3221] = 6,
    ACTIONS(711), 1,
      anon_sym_LF,
    ACTIONS(713), 1,
      aux_sym_env_key_token1,
    ACTIONS(716), 1,
      sym_semgrep_ellipsis,
    STATE(347), 1,
      sym_env_key,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(124), 2,
      sym_env_pair,
      aux_sym_env_instruction_repeat1,
  [3242] = 6,
    ACTIONS(470), 1,
      sym_non_newline_whitespace,
    ACTIONS(707), 1,
      aux_sym_path_token3,
    ACTIONS(709), 1,
      anon_sym_DOLLAR2,
    STATE(259), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(119), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [3263] = 6,
    ACTIONS(474), 1,
      sym_non_newline_whitespace,
    ACTIONS(707), 1,
      aux_sym_path_token3,
    ACTIONS(709), 1,
      anon_sym_DOLLAR2,
    STATE(259), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(119), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [3284] = 6,
    ACTIONS(629), 1,
      anon_sym_LF,
    ACTIONS(681), 1,
      aux_sym_path_token3,
    ACTIONS(683), 1,
      anon_sym_DOLLAR2,
    STATE(274), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(139), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [3305] = 6,
    ACTIONS(641), 1,
      anon_sym_LF,
    ACTIONS(681), 1,
      aux_sym_path_token3,
    ACTIONS(683), 1,
      anon_sym_DOLLAR2,
    STATE(274), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(139), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [3326] = 6,
    ACTIONS(645), 1,
      anon_sym_LF,
    ACTIONS(681), 1,
      aux_sym_path_token3,
    ACTIONS(683), 1,
      anon_sym_DOLLAR2,
    STATE(274), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(139), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [3347] = 6,
    ACTIONS(719), 1,
      aux_sym_env_key_token1,
    ACTIONS(721), 1,
      sym_semgrep_ellipsis,
    STATE(331), 1,
      sym_env_key,
    STATE(386), 1,
      sym_spaced_env_pair,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    STATE(154), 2,
      sym_env_pair,
      aux_sym_env_instruction_repeat1,
  [3368] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(314), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
    ACTIONS(316), 4,
      aux_sym_env_key_token1,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
      sym_semgrep_ellipsis,
  [3383] = 6,
    ACTIONS(361), 1,
      sym_non_newline_whitespace,
    ACTIONS(707), 1,
      aux_sym_path_token3,
    ACTIONS(709), 1,
      anon_sym_DOLLAR2,
    STATE(259), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(119), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [3404] = 3,
    ACTIONS(723), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(725), 5,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [3419] = 3,
    ACTIONS(727), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(729), 5,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [3434] = 6,
    ACTIONS(693), 1,
      anon_sym_DOLLAR2,
    ACTIONS(695), 1,
      aux_sym_image_alias_token2,
    ACTIONS(731), 1,
      anon_sym_LF,
    STATE(290), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(140), 2,
      sym_immediate_expansion,
      aux_sym_image_alias_repeat1,
  [3455] = 6,
    ACTIONS(733), 1,
      anon_sym_LF,
    ACTIONS(735), 1,
      aux_sym_stopsignal_value_token2,
    ACTIONS(738), 1,
      anon_sym_DOLLAR2,
    STATE(279), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(136), 2,
      sym_immediate_expansion,
      aux_sym_stopsignal_value_repeat1,
  [3476] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(353), 2,
      aux_sym_from_instruction_token2,
      aux_sym_image_name_token2,
    ACTIONS(351), 4,
      anon_sym_LF,
      anon_sym_COLON,
      anon_sym_DOLLAR2,
      anon_sym_AT,
  [3491] = 6,
    ACTIONS(693), 1,
      anon_sym_DOLLAR2,
    ACTIONS(695), 1,
      aux_sym_image_alias_token2,
    ACTIONS(741), 1,
      anon_sym_LF,
    STATE(290), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(147), 2,
      sym_immediate_expansion,
      aux_sym_image_alias_repeat1,
  [3512] = 6,
    ACTIONS(498), 1,
      anon_sym_LF,
    ACTIONS(743), 1,
      aux_sym_path_token3,
    ACTIONS(746), 1,
      anon_sym_DOLLAR2,
    STATE(274), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(139), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [3533] = 6,
    ACTIONS(693), 1,
      anon_sym_DOLLAR2,
    ACTIONS(695), 1,
      aux_sym_image_alias_token2,
    ACTIONS(749), 1,
      anon_sym_LF,
    STATE(290), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(147), 2,
      sym_immediate_expansion,
      aux_sym_image_alias_repeat1,
  [3554] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(753), 2,
      aux_sym_from_instruction_token2,
      aux_sym_image_name_token2,
    ACTIONS(751), 4,
      anon_sym_LF,
      anon_sym_COLON,
      anon_sym_DOLLAR2,
      anon_sym_AT,
  [3569] = 6,
    ACTIONS(443), 1,
      sym_non_newline_whitespace,
    ACTIONS(707), 1,
      aux_sym_path_token3,
    ACTIONS(709), 1,
      anon_sym_DOLLAR2,
    STATE(259), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(125), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [3590] = 6,
    ACTIONS(659), 1,
      aux_sym_stopsignal_value_token2,
    ACTIONS(661), 1,
      anon_sym_DOLLAR2,
    ACTIONS(755), 1,
      anon_sym_LF,
    STATE(279), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(136), 2,
      sym_immediate_expansion,
      aux_sym_stopsignal_value_repeat1,
  [3611] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(339), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
    ACTIONS(341), 4,
      aux_sym_env_key_token1,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
      sym_semgrep_ellipsis,
  [3626] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(343), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
    ACTIONS(345), 4,
      aux_sym_env_key_token1,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
      sym_semgrep_ellipsis,
  [3641] = 3,
    ACTIONS(757), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(759), 5,
      aux_sym_label_pair_token1,
      anon_sym_DQUOTE,
      anon_sym_SQUOTE,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [3656] = 6,
    ACTIONS(761), 1,
      anon_sym_LF,
    ACTIONS(763), 1,
      anon_sym_DOLLAR2,
    ACTIONS(766), 1,
      aux_sym_image_alias_token2,
    STATE(290), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(147), 2,
      sym_immediate_expansion,
      aux_sym_image_alias_repeat1,
  [3677] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(341), 2,
      aux_sym_from_instruction_token2,
      aux_sym_image_name_token2,
    ACTIONS(339), 4,
      anon_sym_LF,
      anon_sym_COLON,
      anon_sym_DOLLAR2,
      anon_sym_AT,
  [3692] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(347), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
    ACTIONS(349), 4,
      aux_sym_env_key_token1,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
      sym_semgrep_ellipsis,
  [3707] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(351), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
    ACTIONS(353), 4,
      aux_sym_env_key_token1,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
      sym_semgrep_ellipsis,
  [3722] = 6,
    ACTIONS(769), 1,
      anon_sym_RBRACK,
    ACTIONS(771), 1,
      anon_sym_DQUOTE,
    STATE(294), 1,
      sym_array_element,
    STATE(323), 1,
      sym_json_string,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(773), 2,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [3743] = 7,
    ACTIONS(775), 1,
      anon_sym_LF,
    ACTIONS(777), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(779), 1,
      anon_sym_COLON,
    ACTIONS(781), 1,
      anon_sym_AT,
    STATE(205), 1,
      sym_image_tag,
    STATE(324), 1,
      sym_image_digest,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [3766] = 6,
    ACTIONS(451), 1,
      sym_non_newline_whitespace,
    ACTIONS(707), 1,
      aux_sym_path_token3,
    ACTIONS(709), 1,
      anon_sym_DOLLAR2,
    STATE(259), 1,
      sym_imm_expansion,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(132), 2,
      sym_immediate_expansion,
      aux_sym_path_repeat1,
  [3787] = 6,
    ACTIONS(783), 1,
      anon_sym_LF,
    ACTIONS(785), 1,
      aux_sym_env_key_token1,
    ACTIONS(787), 1,
      sym_semgrep_ellipsis,
    STATE(347), 1,
      sym_env_key,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(124), 2,
      sym_env_pair,
      aux_sym_env_instruction_repeat1,
  [3808] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(349), 2,
      aux_sym_from_instruction_token2,
      aux_sym_image_name_token2,
    ACTIONS(347), 4,
      anon_sym_LF,
      anon_sym_COLON,
      anon_sym_DOLLAR2,
      anon_sym_AT,
  [3823] = 6,
    ACTIONS(771), 1,
      anon_sym_DQUOTE,
    ACTIONS(789), 1,
      anon_sym_RBRACK,
    STATE(247), 1,
      sym_array_element,
    STATE(323), 1,
      sym_json_string,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(773), 2,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [3844] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(310), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
    ACTIONS(312), 4,
      aux_sym_env_key_token1,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
      sym_semgrep_ellipsis,
  [3859] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(353), 2,
      aux_sym_from_instruction_token2,
      aux_sym_image_tag_token1,
    ACTIONS(351), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      anon_sym_AT,
  [3873] = 7,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(791), 1,
      aux_sym_path_token1,
    ACTIONS(793), 1,
      aux_sym_path_token2,
    ACTIONS(795), 1,
      anon_sym_DOLLAR,
    STATE(118), 1,
      sym_expansion,
    STATE(349), 1,
      sym_path,
  [3895] = 5,
    ACTIONS(771), 1,
      anon_sym_DQUOTE,
    STATE(323), 1,
      sym_json_string,
    STATE(337), 1,
      sym_array_element,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(773), 2,
      sym_semgrep_metavariable,
      sym_semgrep_ellipsis,
  [3913] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(339), 2,
      anon_sym_DOLLAR2,
      sym_double_quoted_escape_sequence,
    ACTIONS(341), 3,
      anon_sym_DQUOTE,
      aux_sym_double_quoted_string_token1,
      anon_sym_BSLASH,
  [3927] = 5,
    ACTIONS(797), 1,
      anon_sym_LF,
    ACTIONS(799), 1,
      sym_non_newline_whitespace,
    ACTIONS(801), 1,
      sym_heredoc_nl,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(215), 2,
      sym_heredoc_block,
      aux_sym_run_instruction_repeat2,
  [3945] = 6,
    ACTIONS(803), 1,
      anon_sym_BSLASH,
    ACTIONS(806), 1,
      anon_sym_SQUOTE,
    ACTIONS(808), 1,
      aux_sym_single_quoted_string_token1,
    ACTIONS(811), 1,
      sym_single_quoted_escape_sequence,
    STATE(163), 1,
      aux_sym_single_quoted_string_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [3965] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(343), 2,
      anon_sym_DOLLAR2,
      sym_double_quoted_escape_sequence,
    ACTIONS(345), 3,
      anon_sym_DQUOTE,
      aux_sym_double_quoted_string_token1,
      anon_sym_BSLASH,
  [3979] = 5,
    ACTIONS(799), 1,
      sym_non_newline_whitespace,
    ACTIONS(801), 1,
      sym_heredoc_nl,
    ACTIONS(814), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(220), 2,
      sym_heredoc_block,
      aux_sym_run_instruction_repeat2,
  [3997] = 5,
    ACTIONS(799), 1,
      sym_non_newline_whitespace,
    ACTIONS(801), 1,
      sym_heredoc_nl,
    ACTIONS(816), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(221), 2,
      sym_heredoc_block,
      aux_sym_run_instruction_repeat2,
  [4015] = 5,
    ACTIONS(799), 1,
      sym_non_newline_whitespace,
    ACTIONS(801), 1,
      sym_heredoc_nl,
    ACTIONS(818), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(226), 2,
      sym_heredoc_block,
      aux_sym_run_instruction_repeat2,
  [4033] = 7,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(562), 1,
      anon_sym_DOLLAR,
    ACTIONS(564), 1,
      aux_sym_image_name_token1,
    STATE(28), 1,
      sym_expansion,
    STATE(152), 1,
      sym_image_name,
    STATE(325), 1,
      sym_image_spec,
  [4055] = 6,
    ACTIONS(820), 1,
      anon_sym_BSLASH,
    ACTIONS(822), 1,
      anon_sym_SQUOTE,
    ACTIONS(824), 1,
      aux_sym_single_quoted_string_token1,
    ACTIONS(826), 1,
      sym_single_quoted_escape_sequence,
    STATE(182), 1,
      aux_sym_single_quoted_string_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [4075] = 7,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(576), 1,
      aux_sym_path_token1,
    ACTIONS(578), 1,
      aux_sym_path_token2,
    ACTIONS(580), 1,
      anon_sym_DOLLAR,
    STATE(81), 1,
      sym_expansion,
    STATE(330), 1,
      sym_path,
  [4097] = 4,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(482), 1,
      aux_sym_path_token1,
    ACTIONS(484), 4,
      sym_heredoc_marker,
      aux_sym_path_with_heredoc_token1,
      anon_sym_DOLLAR,
      anon_sym_DASH_DASH,
  [4113] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(828), 2,
      anon_sym_LF,
      aux_sym_path_token3,
    ACTIONS(830), 3,
      sym_heredoc_nl,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
  [4127] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(832), 2,
      anon_sym_DOLLAR2,
      sym_double_quoted_escape_sequence,
    ACTIONS(834), 3,
      anon_sym_DQUOTE,
      aux_sym_double_quoted_string_token1,
      anon_sym_BSLASH,
  [4141] = 6,
    ACTIONS(824), 1,
      aux_sym_single_quoted_string_token1,
    ACTIONS(836), 1,
      anon_sym_BSLASH,
    ACTIONS(838), 1,
      anon_sym_SQUOTE,
    ACTIONS(840), 1,
      sym_single_quoted_escape_sequence,
    STATE(163), 1,
      aux_sym_single_quoted_string_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [4161] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(669), 2,
      anon_sym_LF,
      aux_sym_path_token3,
    ACTIONS(667), 3,
      sym_heredoc_nl,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
  [4175] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(341), 2,
      aux_sym_from_instruction_token2,
      aux_sym_image_tag_token1,
    ACTIONS(339), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      anon_sym_AT,
  [4189] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(345), 2,
      aux_sym_from_instruction_token2,
      aux_sym_image_tag_token1,
    ACTIONS(343), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      anon_sym_AT,
  [4203] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(351), 2,
      anon_sym_DOLLAR2,
      sym_double_quoted_escape_sequence,
    ACTIONS(353), 3,
      anon_sym_DQUOTE,
      aux_sym_double_quoted_string_token1,
      anon_sym_BSLASH,
  [4217] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(349), 2,
      anon_sym_LF,
      aux_sym_path_token3,
    ACTIONS(347), 3,
      sym_heredoc_nl,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
  [4231] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(353), 2,
      anon_sym_LF,
      aux_sym_path_token3,
    ACTIONS(351), 3,
      sym_heredoc_nl,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
  [4245] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(349), 2,
      aux_sym_from_instruction_token2,
      aux_sym_image_tag_token1,
    ACTIONS(347), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      anon_sym_AT,
  [4259] = 6,
    ACTIONS(824), 1,
      aux_sym_single_quoted_string_token1,
    ACTIONS(836), 1,
      anon_sym_BSLASH,
    ACTIONS(840), 1,
      sym_single_quoted_escape_sequence,
    ACTIONS(842), 1,
      anon_sym_SQUOTE,
    STATE(163), 1,
      aux_sym_single_quoted_string_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [4279] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(341), 2,
      anon_sym_LF,
      aux_sym_path_token3,
    ACTIONS(339), 3,
      sym_heredoc_nl,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
  [4293] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(345), 2,
      anon_sym_LF,
      aux_sym_path_token3,
    ACTIONS(343), 3,
      sym_heredoc_nl,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
  [4307] = 5,
    ACTIONS(844), 1,
      aux_sym_cmd_instruction_token1,
    ACTIONS(846), 1,
      anon_sym_DASH_DASH,
    ACTIONS(849), 1,
      sym_semgrep_ellipsis,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    STATE(185), 2,
      sym_param,
      aux_sym_healthcheck_instruction_repeat1,
  [4325] = 6,
    ACTIONS(824), 1,
      aux_sym_single_quoted_string_token1,
    ACTIONS(852), 1,
      anon_sym_BSLASH,
    ACTIONS(854), 1,
      anon_sym_SQUOTE,
    ACTIONS(856), 1,
      sym_single_quoted_escape_sequence,
    STATE(187), 1,
      aux_sym_single_quoted_string_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [4345] = 6,
    ACTIONS(824), 1,
      aux_sym_single_quoted_string_token1,
    ACTIONS(836), 1,
      anon_sym_BSLASH,
    ACTIONS(840), 1,
      sym_single_quoted_escape_sequence,
    ACTIONS(858), 1,
      anon_sym_SQUOTE,
    STATE(163), 1,
      aux_sym_single_quoted_string_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [4365] = 6,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(367), 1,
      anon_sym_DOLLAR2,
    ACTIONS(369), 1,
      aux_sym_image_tag_token1,
    STATE(176), 1,
      sym_imm_expansion,
    STATE(47), 2,
      sym_immediate_expansion,
      aux_sym_image_tag_repeat1,
  [4385] = 5,
    ACTIONS(538), 1,
      anon_sym_DOLLAR2,
    ACTIONS(540), 1,
      aux_sym_image_digest_token1,
    STATE(213), 1,
      sym_imm_expansion,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    STATE(78), 2,
      sym_immediate_expansion,
      aux_sym_image_digest_repeat1,
  [4403] = 6,
    ACTIONS(824), 1,
      aux_sym_single_quoted_string_token1,
    ACTIONS(860), 1,
      anon_sym_BSLASH,
    ACTIONS(862), 1,
      anon_sym_SQUOTE,
    ACTIONS(864), 1,
      sym_single_quoted_escape_sequence,
    STATE(174), 1,
      aux_sym_single_quoted_string_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [4423] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(868), 2,
      aux_sym_from_instruction_token2,
      aux_sym_image_tag_token1,
    ACTIONS(866), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      anon_sym_AT,
  [4437] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(347), 2,
      anon_sym_DOLLAR2,
      sym_double_quoted_escape_sequence,
    ACTIONS(349), 3,
      anon_sym_DQUOTE,
      aux_sym_double_quoted_string_token1,
      anon_sym_BSLASH,
  [4451] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(343), 4,
      anon_sym_LF,
      anon_sym_COLON,
      aux_sym_immediate_user_name_or_group_fragment_token1,
      anon_sym_DOLLAR2,
  [4462] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(870), 4,
      anon_sym_LF,
      anon_sym_COLON,
      aux_sym_immediate_user_name_or_group_fragment_token1,
      anon_sym_DOLLAR2,
  [4473] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(347), 2,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
    ACTIONS(349), 2,
      anon_sym_LF,
      aux_sym_path_token3,
  [4486] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(667), 2,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
    ACTIONS(669), 2,
      anon_sym_LF,
      aux_sym_path_token3,
  [4499] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(339), 2,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
    ACTIONS(341), 2,
      anon_sym_LF,
      aux_sym_path_token3,
  [4512] = 4,
    ACTIONS(801), 1,
      sym_heredoc_nl,
    ACTIONS(872), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(218), 2,
      sym_heredoc_block,
      aux_sym_run_instruction_repeat2,
  [4527] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(343), 2,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
    ACTIONS(345), 2,
      anon_sym_LF,
      aux_sym_path_token3,
  [4540] = 5,
    ACTIONS(874), 1,
      aux_sym_user_name_or_group_token1,
    ACTIONS(876), 1,
      anon_sym_DOLLAR,
    STATE(48), 1,
      sym_expansion,
    STATE(327), 1,
      sym_user_name_or_group,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [4557] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(351), 2,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
    ACTIONS(353), 2,
      anon_sym_LF,
      aux_sym_path_token3,
  [4570] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(347), 4,
      anon_sym_LF,
      anon_sym_COLON,
      aux_sym_immediate_user_name_or_group_fragment_token1,
      anon_sym_DOLLAR2,
  [4581] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(667), 4,
      anon_sym_LF,
      anon_sym_COLON,
      aux_sym_immediate_user_name_or_group_fragment_token1,
      anon_sym_DOLLAR2,
  [4592] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(339), 4,
      anon_sym_LF,
      anon_sym_COLON,
      aux_sym_immediate_user_name_or_group_fragment_token1,
      anon_sym_DOLLAR2,
  [4603] = 5,
    ACTIONS(781), 1,
      anon_sym_AT,
    ACTIONS(878), 1,
      anon_sym_LF,
    ACTIONS(880), 1,
      aux_sym_from_instruction_token2,
    STATE(339), 1,
      sym_image_digest,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [4620] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(351), 4,
      anon_sym_LF,
      anon_sym_COLON,
      aux_sym_immediate_user_name_or_group_fragment_token1,
      anon_sym_DOLLAR2,
  [4631] = 5,
    ACTIONS(882), 1,
      anon_sym_DOLLAR,
    ACTIONS(884), 1,
      aux_sym_image_alias_token1,
    STATE(135), 1,
      sym_expansion,
    STATE(403), 1,
      sym_image_alias,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [4648] = 3,
    ACTIONS(347), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(349), 3,
      anon_sym_DOLLAR,
      aux_sym_expose_port_token1,
      sym_semgrep_ellipsis,
  [4661] = 4,
    ACTIONS(888), 1,
      sym_required_line_continuation,
    STATE(227), 1,
      aux_sym_shell_command_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(886), 2,
      sym_heredoc_nl,
      anon_sym_LF,
  [4676] = 4,
    ACTIONS(801), 1,
      sym_heredoc_nl,
    ACTIONS(872), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(228), 2,
      sym_heredoc_block,
      aux_sym_run_instruction_repeat2,
  [4691] = 3,
    ACTIONS(667), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(669), 3,
      anon_sym_DOLLAR,
      aux_sym_expose_port_token1,
      sym_semgrep_ellipsis,
  [4704] = 5,
    ACTIONS(890), 1,
      anon_sym_DQUOTE,
    ACTIONS(892), 1,
      aux_sym_json_string_token1,
    ACTIONS(894), 1,
      sym_json_escape_sequence,
    STATE(244), 1,
      aux_sym_json_string_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [4721] = 3,
    ACTIONS(341), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(339), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      aux_sym_image_digest_token1,
  [4734] = 3,
    ACTIONS(345), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(343), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      aux_sym_image_digest_token1,
  [4747] = 4,
    ACTIONS(801), 1,
      sym_heredoc_nl,
    ACTIONS(896), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(218), 2,
      sym_heredoc_block,
      aux_sym_run_instruction_repeat2,
  [4762] = 4,
    ACTIONS(801), 1,
      sym_heredoc_nl,
    ACTIONS(898), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(198), 2,
      sym_heredoc_block,
      aux_sym_run_instruction_repeat2,
  [4777] = 3,
    ACTIONS(902), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(900), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      aux_sym_image_digest_token1,
  [4790] = 4,
    ACTIONS(904), 1,
      anon_sym_LF,
    ACTIONS(906), 1,
      sym_heredoc_nl,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(218), 2,
      sym_heredoc_block,
      aux_sym_run_instruction_repeat2,
  [4805] = 3,
    ACTIONS(911), 1,
      sym_single_quoted_escape_sequence,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(909), 3,
      anon_sym_BSLASH,
      anon_sym_SQUOTE,
      aux_sym_single_quoted_string_token1,
  [4818] = 4,
    ACTIONS(801), 1,
      sym_heredoc_nl,
    ACTIONS(913), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(218), 2,
      sym_heredoc_block,
      aux_sym_run_instruction_repeat2,
  [4833] = 4,
    ACTIONS(801), 1,
      sym_heredoc_nl,
    ACTIONS(915), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(218), 2,
      sym_heredoc_block,
      aux_sym_run_instruction_repeat2,
  [4848] = 3,
    ACTIONS(349), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(347), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      aux_sym_image_digest_token1,
  [4861] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(339), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
    ACTIONS(341), 2,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
  [4874] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(343), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
    ACTIONS(345), 2,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
  [4887] = 3,
    ACTIONS(353), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(351), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      aux_sym_image_digest_token1,
  [4900] = 4,
    ACTIONS(801), 1,
      sym_heredoc_nl,
    ACTIONS(917), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(218), 2,
      sym_heredoc_block,
      aux_sym_run_instruction_repeat2,
  [4915] = 4,
    ACTIONS(921), 1,
      sym_required_line_continuation,
    STATE(227), 1,
      aux_sym_shell_command_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(919), 2,
      sym_heredoc_nl,
      anon_sym_LF,
  [4930] = 4,
    ACTIONS(801), 1,
      sym_heredoc_nl,
    ACTIONS(924), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    STATE(218), 2,
      sym_heredoc_block,
      aux_sym_run_instruction_repeat2,
  [4945] = 4,
    ACTIONS(888), 1,
      sym_required_line_continuation,
    STATE(209), 1,
      aux_sym_shell_command_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(926), 2,
      sym_heredoc_nl,
      anon_sym_LF,
  [4960] = 3,
    ACTIONS(928), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(930), 3,
      anon_sym_DOLLAR,
      aux_sym_expose_port_token1,
      sym_semgrep_ellipsis,
  [4973] = 3,
    ACTIONS(932), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(934), 3,
      anon_sym_DOLLAR,
      aux_sym_expose_port_token1,
      sym_semgrep_ellipsis,
  [4986] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(347), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
    ACTIONS(349), 2,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
  [4999] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(351), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
    ACTIONS(353), 2,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
  [5012] = 3,
    ACTIONS(351), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(353), 3,
      anon_sym_DOLLAR,
      aux_sym_expose_port_token1,
      sym_semgrep_ellipsis,
  [5025] = 3,
    ACTIONS(936), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(938), 3,
      aux_sym_cmd_instruction_token1,
      anon_sym_DASH_DASH,
      sym_semgrep_ellipsis,
  [5038] = 4,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(940), 1,
      aux_sym_path_token1,
    ACTIONS(942), 3,
      sym_heredoc_marker,
      aux_sym_path_with_heredoc_token1,
      anon_sym_DOLLAR,
  [5053] = 5,
    ACTIONS(882), 1,
      anon_sym_DOLLAR,
    ACTIONS(884), 1,
      aux_sym_image_alias_token1,
    STATE(135), 1,
      sym_expansion,
    STATE(369), 1,
      sym_image_alias,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5070] = 5,
    ACTIONS(892), 1,
      aux_sym_json_string_token1,
    ACTIONS(944), 1,
      anon_sym_DQUOTE,
    ACTIONS(946), 1,
      sym_json_escape_sequence,
    STATE(212), 1,
      aux_sym_json_string_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [5087] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(310), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
    ACTIONS(312), 2,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
  [5100] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(314), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
    ACTIONS(316), 2,
      aux_sym_unquoted_string_token1,
      anon_sym_BSLASH2,
  [5113] = 5,
    ACTIONS(948), 1,
      aux_sym_stopsignal_value_token1,
    ACTIONS(950), 1,
      anon_sym_DOLLAR,
    STATE(113), 1,
      sym_expansion,
    STATE(394), 1,
      sym_stopsignal_value,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5130] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(952), 4,
      anon_sym_LF,
      anon_sym_COLON,
      aux_sym_immediate_user_name_or_group_fragment_token1,
      anon_sym_DOLLAR2,
  [5141] = 3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(828), 2,
      anon_sym_LF,
      aux_sym_path_token3,
    ACTIONS(830), 2,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
  [5154] = 5,
    ACTIONS(954), 1,
      anon_sym_DQUOTE,
    ACTIONS(956), 1,
      aux_sym_json_string_token1,
    ACTIONS(959), 1,
      sym_json_escape_sequence,
    STATE(244), 1,
      aux_sym_json_string_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [5171] = 4,
    ACTIONS(962), 1,
      anon_sym_LBRACE,
    ACTIONS(964), 1,
      sym_variable,
    STATE(263), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5185] = 3,
    ACTIONS(449), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(451), 2,
      sym_heredoc_nl,
      sym_non_newline_whitespace,
  [5197] = 4,
    ACTIONS(966), 1,
      anon_sym_COMMA2,
    ACTIONS(968), 1,
      anon_sym_RBRACK,
    STATE(254), 1,
      aux_sym_json_string_array_repeat1,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5211] = 4,
    ACTIONS(970), 1,
      sym_heredoc_line,
    ACTIONS(972), 1,
      sym_heredoc_end,
    STATE(255), 1,
      aux_sym_heredoc_block_repeat1,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5225] = 4,
    ACTIONS(974), 1,
      anon_sym_LBRACE,
    ACTIONS(976), 1,
      sym_variable,
    STATE(164), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5239] = 4,
    ACTIONS(886), 1,
      anon_sym_LF,
    ACTIONS(978), 1,
      sym_required_line_continuation,
    STATE(253), 1,
      aux_sym_shell_command_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [5253] = 4,
    ACTIONS(980), 1,
      anon_sym_LF,
    ACTIONS(982), 1,
      sym_non_newline_whitespace,
    STATE(292), 1,
      aux_sym_volume_instruction_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [5267] = 3,
    ACTIONS(986), 1,
      sym_json_escape_sequence,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(984), 2,
      anon_sym_DQUOTE,
      aux_sym_json_string_token1,
  [5279] = 4,
    ACTIONS(919), 1,
      anon_sym_LF,
    ACTIONS(988), 1,
      sym_required_line_continuation,
    STATE(253), 1,
      aux_sym_shell_command_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [5293] = 4,
    ACTIONS(966), 1,
      anon_sym_COMMA2,
    ACTIONS(991), 1,
      anon_sym_RBRACK,
    STATE(288), 1,
      aux_sym_json_string_array_repeat1,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5307] = 4,
    ACTIONS(970), 1,
      sym_heredoc_line,
    ACTIONS(993), 1,
      sym_heredoc_end,
    STATE(289), 1,
      aux_sym_heredoc_block_repeat1,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5321] = 3,
    ACTIONS(995), 1,
      sym_required_line_continuation,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(919), 2,
      sym_heredoc_nl,
      anon_sym_LF,
  [5333] = 3,
    ACTIONS(349), 1,
      aux_sym_path_token3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(347), 2,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
  [5345] = 3,
    ACTIONS(669), 1,
      aux_sym_path_token3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(667), 2,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
  [5357] = 3,
    ACTIONS(341), 1,
      aux_sym_path_token3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(339), 2,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
  [5369] = 3,
    ACTIONS(703), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(705), 2,
      aux_sym_env_key_token1,
      sym_semgrep_ellipsis,
  [5381] = 3,
    ACTIONS(647), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(649), 2,
      aux_sym_env_key_token1,
      sym_semgrep_ellipsis,
  [5393] = 3,
    ACTIONS(828), 1,
      aux_sym_path_token3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(830), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
  [5405] = 3,
    ACTIONS(345), 1,
      aux_sym_path_token3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(343), 2,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
  [5417] = 3,
    ACTIONS(723), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(725), 2,
      aux_sym_env_key_token1,
      sym_semgrep_ellipsis,
  [5429] = 3,
    ACTIONS(727), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(729), 2,
      aux_sym_env_key_token1,
      sym_semgrep_ellipsis,
  [5441] = 3,
    ACTIONS(353), 1,
      aux_sym_path_token3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(351), 2,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
  [5453] = 3,
    ACTIONS(997), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(999), 2,
      aux_sym_env_key_token1,
      sym_semgrep_ellipsis,
  [5465] = 4,
    ACTIONS(1001), 1,
      anon_sym_LBRACE,
    ACTIONS(1003), 1,
      sym_variable,
    STATE(111), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5479] = 2,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(484), 3,
      aux_sym_cmd_instruction_token1,
      anon_sym_DASH_DASH,
      sym_semgrep_ellipsis,
  [5489] = 3,
    ACTIONS(828), 1,
      aux_sym_path_token3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(830), 2,
      anon_sym_DOLLAR2,
      sym_non_newline_whitespace,
  [5501] = 4,
    ACTIONS(1005), 1,
      anon_sym_LF,
    ACTIONS(1007), 1,
      sym_non_newline_whitespace,
    STATE(271), 1,
      aux_sym_volume_instruction_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [5515] = 3,
    ACTIONS(349), 1,
      aux_sym_path_token3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(347), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
  [5527] = 3,
    ACTIONS(669), 1,
      aux_sym_path_token3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(667), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
  [5539] = 3,
    ACTIONS(341), 1,
      aux_sym_path_token3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(339), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
  [5551] = 3,
    ACTIONS(345), 1,
      aux_sym_path_token3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(343), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
  [5563] = 3,
    ACTIONS(353), 1,
      aux_sym_path_token3,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(351), 2,
      anon_sym_LF,
      anon_sym_DOLLAR2,
  [5575] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(347), 3,
      anon_sym_LF,
      aux_sym_stopsignal_value_token2,
      anon_sym_DOLLAR2,
  [5585] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(667), 3,
      anon_sym_LF,
      aux_sym_stopsignal_value_token2,
      anon_sym_DOLLAR2,
  [5595] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(339), 3,
      anon_sym_LF,
      aux_sym_stopsignal_value_token2,
      anon_sym_DOLLAR2,
  [5605] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(343), 3,
      anon_sym_LF,
      aux_sym_stopsignal_value_token2,
      anon_sym_DOLLAR2,
  [5615] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(351), 3,
      anon_sym_LF,
      aux_sym_stopsignal_value_token2,
      anon_sym_DOLLAR2,
  [5625] = 4,
    ACTIONS(926), 1,
      anon_sym_LF,
    ACTIONS(978), 1,
      sym_required_line_continuation,
    STATE(250), 1,
      aux_sym_shell_command_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [5639] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(667), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      aux_sym_image_alias_token2,
  [5649] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(1010), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      aux_sym_image_alias_token2,
  [5659] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(347), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      aux_sym_image_alias_token2,
  [5669] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(351), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      aux_sym_image_alias_token2,
  [5679] = 4,
    ACTIONS(1001), 1,
      anon_sym_LBRACE,
    ACTIONS(1003), 1,
      sym_variable,
    STATE(110), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5693] = 4,
    ACTIONS(1012), 1,
      anon_sym_COMMA2,
    ACTIONS(1015), 1,
      anon_sym_RBRACK,
    STATE(288), 1,
      aux_sym_json_string_array_repeat1,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5707] = 4,
    ACTIONS(1017), 1,
      sym_heredoc_line,
    ACTIONS(1020), 1,
      sym_heredoc_end,
    STATE(289), 1,
      aux_sym_heredoc_block_repeat1,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5721] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(339), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      aux_sym_image_alias_token2,
  [5731] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(343), 3,
      anon_sym_LF,
      anon_sym_DOLLAR2,
      aux_sym_image_alias_token2,
  [5741] = 4,
    ACTIONS(982), 1,
      sym_non_newline_whitespace,
    ACTIONS(1022), 1,
      anon_sym_LF,
    STATE(271), 1,
      aux_sym_volume_instruction_repeat1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [5755] = 4,
    ACTIONS(1024), 1,
      anon_sym_LBRACE,
    ACTIONS(1026), 1,
      sym_variable,
    STATE(211), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5769] = 4,
    ACTIONS(966), 1,
      anon_sym_COMMA2,
    ACTIONS(1028), 1,
      anon_sym_RBRACK,
    STATE(295), 1,
      aux_sym_json_string_array_repeat1,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5783] = 4,
    ACTIONS(966), 1,
      anon_sym_COMMA2,
    ACTIONS(1030), 1,
      anon_sym_RBRACK,
    STATE(288), 1,
      aux_sym_json_string_array_repeat1,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5797] = 4,
    ACTIONS(962), 1,
      anon_sym_LBRACE,
    ACTIONS(964), 1,
      sym_variable,
    STATE(258), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5811] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(1032), 3,
      anon_sym_LF,
      aux_sym_stopsignal_value_token2,
      anon_sym_DOLLAR2,
  [5821] = 4,
    ACTIONS(1034), 1,
      anon_sym_LBRACE,
    ACTIONS(1036), 1,
      sym_variable,
    STATE(199), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5835] = 4,
    ACTIONS(1034), 1,
      anon_sym_LBRACE,
    ACTIONS(1036), 1,
      sym_variable,
    STATE(196), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5849] = 4,
    ACTIONS(1038), 1,
      anon_sym_LBRACE,
    ACTIONS(1040), 1,
      sym_variable,
    STATE(193), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5863] = 4,
    ACTIONS(1038), 1,
      anon_sym_LBRACE,
    ACTIONS(1040), 1,
      sym_variable,
    STATE(203), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5877] = 4,
    ACTIONS(1042), 1,
      anon_sym_LBRACE,
    ACTIONS(1044), 1,
      sym_variable,
    STATE(275), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5891] = 4,
    ACTIONS(1042), 1,
      anon_sym_LBRACE,
    ACTIONS(1044), 1,
      sym_variable,
    STATE(273), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5905] = 4,
    ACTIONS(1046), 1,
      anon_sym_LBRACE,
    ACTIONS(1048), 1,
      sym_variable,
    STATE(280), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5919] = 4,
    ACTIONS(1046), 1,
      anon_sym_LBRACE,
    ACTIONS(1048), 1,
      sym_variable,
    STATE(278), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5933] = 4,
    ACTIONS(1050), 1,
      anon_sym_LBRACE,
    ACTIONS(1052), 1,
      sym_variable,
    STATE(177), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5947] = 4,
    ACTIONS(1054), 1,
      anon_sym_LBRACE,
    ACTIONS(1056), 1,
      sym_variable,
    STATE(175), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5961] = 4,
    ACTIONS(1058), 1,
      anon_sym_LBRACE,
    ACTIONS(1060), 1,
      sym_variable,
    STATE(214), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5975] = 4,
    ACTIONS(1062), 1,
      anon_sym_LBRACE,
    ACTIONS(1064), 1,
      sym_variable,
    STATE(283), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [5989] = 4,
    ACTIONS(1066), 1,
      anon_sym_LBRACE,
    ACTIONS(1068), 1,
      sym_variable,
    STATE(43), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6003] = 4,
    ACTIONS(1070), 1,
      anon_sym_LBRACE,
    ACTIONS(1072), 1,
      sym_variable,
    STATE(145), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6017] = 4,
    ACTIONS(1074), 1,
      anon_sym_LBRACE,
    ACTIONS(1076), 1,
      sym_variable,
    STATE(224), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6031] = 4,
    ACTIONS(1054), 1,
      anon_sym_LBRACE,
    ACTIONS(1056), 1,
      sym_variable,
    STATE(184), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6045] = 4,
    ACTIONS(1062), 1,
      anon_sym_LBRACE,
    ACTIONS(1064), 1,
      sym_variable,
    STATE(291), 1,
      sym_expansion_body,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6059] = 3,
    ACTIONS(1078), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(1080), 2,
      aux_sym_env_key_token1,
      sym_semgrep_ellipsis,
  [6071] = 4,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(1082), 1,
      aux_sym_mount_param_param_token1,
    STATE(26), 1,
      sym_mount_param_param,
  [6084] = 3,
    ACTIONS(1084), 1,
      aux_sym_param_token1,
    ACTIONS(1086), 1,
      anon_sym_mount,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6095] = 2,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(1088), 2,
      anon_sym_COMMA2,
      anon_sym_RBRACK,
  [6104] = 4,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(1082), 1,
      aux_sym_mount_param_param_token1,
    STATE(39), 1,
      sym_mount_param_param,
  [6117] = 3,
    ACTIONS(1090), 1,
      aux_sym_arg_instruction_token2,
    ACTIONS(1092), 1,
      sym_semgrep_metavariable,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6128] = 3,
    ACTIONS(1094), 1,
      anon_sym_LF,
    ACTIONS(1096), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6139] = 3,
    ACTIONS(1098), 1,
      anon_sym_LF,
    ACTIONS(1100), 1,
      anon_sym_EQ,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6150] = 2,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(1102), 2,
      anon_sym_COMMA2,
      anon_sym_RBRACK,
  [6159] = 3,
    ACTIONS(1104), 1,
      anon_sym_LF,
    ACTIONS(1106), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6170] = 3,
    ACTIONS(1108), 1,
      anon_sym_LF,
    ACTIONS(1110), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6181] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(1112), 2,
      anon_sym_EQ,
      aux_sym_spaced_env_pair_token1,
  [6190] = 3,
    ACTIONS(1114), 1,
      anon_sym_LF,
    ACTIONS(1116), 1,
      anon_sym_COLON,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6201] = 4,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(482), 1,
      aux_sym_image_name_token1,
    ACTIONS(484), 1,
      anon_sym_DOLLAR,
  [6214] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(1118), 2,
      sym_heredoc_nl,
      anon_sym_LF,
  [6223] = 3,
    ACTIONS(1005), 1,
      anon_sym_LF,
    ACTIONS(1120), 1,
      sym_non_newline_whitespace,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6234] = 3,
    ACTIONS(1122), 1,
      anon_sym_EQ,
    ACTIONS(1124), 1,
      aux_sym_spaced_env_pair_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6245] = 3,
    ACTIONS(174), 1,
      anon_sym_LBRACK,
    STATE(351), 1,
      sym_json_string_array,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6256] = 3,
    ACTIONS(919), 1,
      anon_sym_LF,
    ACTIONS(995), 1,
      sym_required_line_continuation,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6267] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(1126), 2,
      sym_heredoc_nl,
      anon_sym_LF,
  [6276] = 2,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(1128), 2,
      anon_sym_COMMA2,
      anon_sym_RBRACK,
  [6285] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(926), 2,
      sym_heredoc_nl,
      anon_sym_LF,
  [6294] = 2,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(1015), 2,
      anon_sym_COMMA2,
      anon_sym_RBRACK,
  [6303] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(1130), 2,
      sym_heredoc_nl,
      anon_sym_LF,
  [6312] = 3,
    ACTIONS(1132), 1,
      anon_sym_LF,
    ACTIONS(1134), 1,
      aux_sym_from_instruction_token2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6323] = 2,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(1020), 2,
      sym_heredoc_line,
      sym_heredoc_end,
  [6332] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(1136), 2,
      sym_heredoc_nl,
      anon_sym_LF,
  [6341] = 2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
    ACTIONS(1138), 2,
      sym_heredoc_nl,
      anon_sym_LF,
  [6350] = 3,
    ACTIONS(1140), 1,
      anon_sym_LF,
    ACTIONS(1142), 1,
      anon_sym_EQ,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6361] = 2,
    ACTIONS(1144), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6369] = 2,
    ACTIONS(1146), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6377] = 2,
    ACTIONS(1148), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6385] = 2,
    ACTIONS(1122), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6393] = 2,
    ACTIONS(1112), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6401] = 2,
    ACTIONS(1150), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6409] = 2,
    ACTIONS(647), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6417] = 2,
    ACTIONS(1152), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6425] = 2,
    ACTIONS(1130), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6433] = 2,
    ACTIONS(1154), 1,
      aux_sym_param_token1,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6441] = 2,
    ACTIONS(1156), 1,
      ts_builtin_sym_end,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6449] = 2,
    ACTIONS(1158), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6457] = 2,
    ACTIONS(1160), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6465] = 3,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(1162), 1,
      aux_sym_param_token2,
  [6475] = 2,
    ACTIONS(799), 1,
      sym_non_newline_whitespace,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6483] = 2,
    ACTIONS(1164), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6491] = 2,
    ACTIONS(1166), 1,
      aux_sym_shell_fragment_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6499] = 3,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(1168), 1,
      aux_sym_mount_param_param_token1,
  [6509] = 2,
    ACTIONS(1170), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6517] = 2,
    ACTIONS(1172), 1,
      aux_sym_maintainer_instruction_token2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6525] = 2,
    ACTIONS(1174), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6533] = 2,
    ACTIONS(703), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6541] = 2,
    ACTIONS(1176), 1,
      aux_sym_maintainer_instruction_token2,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6549] = 2,
    ACTIONS(1178), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6557] = 3,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(1180), 1,
      aux_sym_param_token2,
  [6567] = 2,
    ACTIONS(1182), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6575] = 2,
    ACTIONS(1184), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6583] = 2,
    ACTIONS(1118), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6591] = 2,
    ACTIONS(1126), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6599] = 2,
    ACTIONS(1186), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6607] = 2,
    ACTIONS(1188), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6615] = 2,
    ACTIONS(1190), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6623] = 2,
    ACTIONS(1192), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6631] = 3,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(1194), 1,
      aux_sym_param_token2,
  [6641] = 2,
    ACTIONS(1196), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6649] = 2,
    ACTIONS(1198), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6657] = 2,
    ACTIONS(1200), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6665] = 3,
    ACTIONS(3), 1,
      sym_line_continuation,
    ACTIONS(5), 1,
      sym_comment,
    ACTIONS(1202), 1,
      aux_sym_param_token2,
  [6675] = 2,
    ACTIONS(1204), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6683] = 2,
    ACTIONS(1206), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6691] = 2,
    ACTIONS(1208), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6699] = 2,
    ACTIONS(1210), 1,
      aux_sym_shell_fragment_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6707] = 2,
    ACTIONS(783), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6715] = 2,
    ACTIONS(1212), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6723] = 2,
    ACTIONS(451), 1,
      sym_non_newline_whitespace,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6731] = 2,
    ACTIONS(723), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6739] = 2,
    ACTIONS(1214), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6747] = 2,
    ACTIONS(1216), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6755] = 2,
    ACTIONS(1218), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6763] = 2,
    ACTIONS(1220), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6771] = 2,
    ACTIONS(1222), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6779] = 2,
    ACTIONS(727), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6787] = 2,
    ACTIONS(1224), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6795] = 2,
    ACTIONS(1226), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6803] = 2,
    ACTIONS(1228), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6811] = 2,
    ACTIONS(936), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6819] = 2,
    ACTIONS(1230), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6827] = 2,
    ACTIONS(1232), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6835] = 2,
    ACTIONS(1234), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6843] = 2,
    ACTIONS(1236), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6851] = 2,
    ACTIONS(1238), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6859] = 2,
    ACTIONS(1240), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6867] = 2,
    ACTIONS(1242), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6875] = 2,
    ACTIONS(1244), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6883] = 2,
    ACTIONS(1246), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6891] = 2,
    ACTIONS(1248), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6899] = 2,
    ACTIONS(1250), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6907] = 2,
    ACTIONS(1252), 1,
      anon_sym_EQ,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [6915] = 2,
    ACTIONS(1254), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6923] = 2,
    ACTIONS(926), 1,
      anon_sym_LF,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6931] = 2,
    ACTIONS(1256), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6939] = 2,
    ACTIONS(1258), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6947] = 2,
    ACTIONS(1260), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6955] = 2,
    ACTIONS(1262), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6963] = 2,
    ACTIONS(1264), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6971] = 2,
    ACTIONS(1266), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6979] = 2,
    ACTIONS(1268), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6987] = 2,
    ACTIONS(1270), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [6995] = 2,
    ACTIONS(1272), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [7003] = 2,
    ACTIONS(1274), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [7011] = 2,
    ACTIONS(1276), 1,
      aux_sym_expansion_body_token1,
    ACTIONS(5), 2,
      sym_line_continuation,
      sym_comment,
  [7019] = 2,
    ACTIONS(1278), 1,
      aux_sym_param_token1,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [7027] = 2,
    ACTIONS(1280), 1,
      aux_sym_param_token1,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
  [7035] = 2,
    ACTIONS(1282), 1,
      anon_sym_RBRACE,
    ACTIONS(3), 2,
      sym_line_continuation,
      sym_comment,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 96,
  [SMALL_STATE(4)] = 192,
  [SMALL_STATE(5)] = 282,
  [SMALL_STATE(6)] = 311,
  [SMALL_STATE(7)] = 355,
  [SMALL_STATE(8)] = 399,
  [SMALL_STATE(9)] = 438,
  [SMALL_STATE(10)] = 469,
  [SMALL_STATE(11)] = 500,
  [SMALL_STATE(12)] = 536,
  [SMALL_STATE(13)] = 572,
  [SMALL_STATE(14)] = 596,
  [SMALL_STATE(15)] = 630,
  [SMALL_STATE(16)] = 664,
  [SMALL_STATE(17)] = 698,
  [SMALL_STATE(18)] = 732,
  [SMALL_STATE(19)] = 766,
  [SMALL_STATE(20)] = 801,
  [SMALL_STATE(21)] = 836,
  [SMALL_STATE(22)] = 871,
  [SMALL_STATE(23)] = 902,
  [SMALL_STATE(24)] = 937,
  [SMALL_STATE(25)] = 968,
  [SMALL_STATE(26)] = 991,
  [SMALL_STATE(27)] = 1014,
  [SMALL_STATE(28)] = 1037,
  [SMALL_STATE(29)] = 1063,
  [SMALL_STATE(30)] = 1091,
  [SMALL_STATE(31)] = 1117,
  [SMALL_STATE(32)] = 1143,
  [SMALL_STATE(33)] = 1169,
  [SMALL_STATE(34)] = 1199,
  [SMALL_STATE(35)] = 1225,
  [SMALL_STATE(36)] = 1243,
  [SMALL_STATE(37)] = 1261,
  [SMALL_STATE(38)] = 1291,
  [SMALL_STATE(39)] = 1309,
  [SMALL_STATE(40)] = 1327,
  [SMALL_STATE(41)] = 1355,
  [SMALL_STATE(42)] = 1383,
  [SMALL_STATE(43)] = 1401,
  [SMALL_STATE(44)] = 1419,
  [SMALL_STATE(45)] = 1437,
  [SMALL_STATE(46)] = 1455,
  [SMALL_STATE(47)] = 1480,
  [SMALL_STATE(48)] = 1505,
  [SMALL_STATE(49)] = 1530,
  [SMALL_STATE(50)] = 1547,
  [SMALL_STATE(51)] = 1572,
  [SMALL_STATE(52)] = 1599,
  [SMALL_STATE(53)] = 1624,
  [SMALL_STATE(54)] = 1649,
  [SMALL_STATE(55)] = 1676,
  [SMALL_STATE(56)] = 1703,
  [SMALL_STATE(57)] = 1720,
  [SMALL_STATE(58)] = 1749,
  [SMALL_STATE(59)] = 1774,
  [SMALL_STATE(60)] = 1799,
  [SMALL_STATE(61)] = 1824,
  [SMALL_STATE(62)] = 1853,
  [SMALL_STATE(63)] = 1878,
  [SMALL_STATE(64)] = 1903,
  [SMALL_STATE(65)] = 1930,
  [SMALL_STATE(66)] = 1947,
  [SMALL_STATE(67)] = 1964,
  [SMALL_STATE(68)] = 1981,
  [SMALL_STATE(69)] = 2006,
  [SMALL_STATE(70)] = 2023,
  [SMALL_STATE(71)] = 2048,
  [SMALL_STATE(72)] = 2073,
  [SMALL_STATE(73)] = 2098,
  [SMALL_STATE(74)] = 2125,
  [SMALL_STATE(75)] = 2152,
  [SMALL_STATE(76)] = 2179,
  [SMALL_STATE(77)] = 2203,
  [SMALL_STATE(78)] = 2227,
  [SMALL_STATE(79)] = 2251,
  [SMALL_STATE(80)] = 2275,
  [SMALL_STATE(81)] = 2299,
  [SMALL_STATE(82)] = 2323,
  [SMALL_STATE(83)] = 2349,
  [SMALL_STATE(84)] = 2373,
  [SMALL_STATE(85)] = 2401,
  [SMALL_STATE(86)] = 2429,
  [SMALL_STATE(87)] = 2457,
  [SMALL_STATE(88)] = 2473,
  [SMALL_STATE(89)] = 2489,
  [SMALL_STATE(90)] = 2505,
  [SMALL_STATE(91)] = 2521,
  [SMALL_STATE(92)] = 2543,
  [SMALL_STATE(93)] = 2559,
  [SMALL_STATE(94)] = 2585,
  [SMALL_STATE(95)] = 2609,
  [SMALL_STATE(96)] = 2631,
  [SMALL_STATE(97)] = 2659,
  [SMALL_STATE(98)] = 2683,
  [SMALL_STATE(99)] = 2707,
  [SMALL_STATE(100)] = 2735,
  [SMALL_STATE(101)] = 2763,
  [SMALL_STATE(102)] = 2787,
  [SMALL_STATE(103)] = 2809,
  [SMALL_STATE(104)] = 2833,
  [SMALL_STATE(105)] = 2857,
  [SMALL_STATE(106)] = 2885,
  [SMALL_STATE(107)] = 2900,
  [SMALL_STATE(108)] = 2917,
  [SMALL_STATE(109)] = 2938,
  [SMALL_STATE(110)] = 2953,
  [SMALL_STATE(111)] = 2968,
  [SMALL_STATE(112)] = 2983,
  [SMALL_STATE(113)] = 3004,
  [SMALL_STATE(114)] = 3025,
  [SMALL_STATE(115)] = 3040,
  [SMALL_STATE(116)] = 3061,
  [SMALL_STATE(117)] = 3082,
  [SMALL_STATE(118)] = 3103,
  [SMALL_STATE(119)] = 3124,
  [SMALL_STATE(120)] = 3145,
  [SMALL_STATE(121)] = 3166,
  [SMALL_STATE(122)] = 3185,
  [SMALL_STATE(123)] = 3200,
  [SMALL_STATE(124)] = 3221,
  [SMALL_STATE(125)] = 3242,
  [SMALL_STATE(126)] = 3263,
  [SMALL_STATE(127)] = 3284,
  [SMALL_STATE(128)] = 3305,
  [SMALL_STATE(129)] = 3326,
  [SMALL_STATE(130)] = 3347,
  [SMALL_STATE(131)] = 3368,
  [SMALL_STATE(132)] = 3383,
  [SMALL_STATE(133)] = 3404,
  [SMALL_STATE(134)] = 3419,
  [SMALL_STATE(135)] = 3434,
  [SMALL_STATE(136)] = 3455,
  [SMALL_STATE(137)] = 3476,
  [SMALL_STATE(138)] = 3491,
  [SMALL_STATE(139)] = 3512,
  [SMALL_STATE(140)] = 3533,
  [SMALL_STATE(141)] = 3554,
  [SMALL_STATE(142)] = 3569,
  [SMALL_STATE(143)] = 3590,
  [SMALL_STATE(144)] = 3611,
  [SMALL_STATE(145)] = 3626,
  [SMALL_STATE(146)] = 3641,
  [SMALL_STATE(147)] = 3656,
  [SMALL_STATE(148)] = 3677,
  [SMALL_STATE(149)] = 3692,
  [SMALL_STATE(150)] = 3707,
  [SMALL_STATE(151)] = 3722,
  [SMALL_STATE(152)] = 3743,
  [SMALL_STATE(153)] = 3766,
  [SMALL_STATE(154)] = 3787,
  [SMALL_STATE(155)] = 3808,
  [SMALL_STATE(156)] = 3823,
  [SMALL_STATE(157)] = 3844,
  [SMALL_STATE(158)] = 3859,
  [SMALL_STATE(159)] = 3873,
  [SMALL_STATE(160)] = 3895,
  [SMALL_STATE(161)] = 3913,
  [SMALL_STATE(162)] = 3927,
  [SMALL_STATE(163)] = 3945,
  [SMALL_STATE(164)] = 3965,
  [SMALL_STATE(165)] = 3979,
  [SMALL_STATE(166)] = 3997,
  [SMALL_STATE(167)] = 4015,
  [SMALL_STATE(168)] = 4033,
  [SMALL_STATE(169)] = 4055,
  [SMALL_STATE(170)] = 4075,
  [SMALL_STATE(171)] = 4097,
  [SMALL_STATE(172)] = 4113,
  [SMALL_STATE(173)] = 4127,
  [SMALL_STATE(174)] = 4141,
  [SMALL_STATE(175)] = 4161,
  [SMALL_STATE(176)] = 4175,
  [SMALL_STATE(177)] = 4189,
  [SMALL_STATE(178)] = 4203,
  [SMALL_STATE(179)] = 4217,
  [SMALL_STATE(180)] = 4231,
  [SMALL_STATE(181)] = 4245,
  [SMALL_STATE(182)] = 4259,
  [SMALL_STATE(183)] = 4279,
  [SMALL_STATE(184)] = 4293,
  [SMALL_STATE(185)] = 4307,
  [SMALL_STATE(186)] = 4325,
  [SMALL_STATE(187)] = 4345,
  [SMALL_STATE(188)] = 4365,
  [SMALL_STATE(189)] = 4385,
  [SMALL_STATE(190)] = 4403,
  [SMALL_STATE(191)] = 4423,
  [SMALL_STATE(192)] = 4437,
  [SMALL_STATE(193)] = 4451,
  [SMALL_STATE(194)] = 4462,
  [SMALL_STATE(195)] = 4473,
  [SMALL_STATE(196)] = 4486,
  [SMALL_STATE(197)] = 4499,
  [SMALL_STATE(198)] = 4512,
  [SMALL_STATE(199)] = 4527,
  [SMALL_STATE(200)] = 4540,
  [SMALL_STATE(201)] = 4557,
  [SMALL_STATE(202)] = 4570,
  [SMALL_STATE(203)] = 4581,
  [SMALL_STATE(204)] = 4592,
  [SMALL_STATE(205)] = 4603,
  [SMALL_STATE(206)] = 4620,
  [SMALL_STATE(207)] = 4631,
  [SMALL_STATE(208)] = 4648,
  [SMALL_STATE(209)] = 4661,
  [SMALL_STATE(210)] = 4676,
  [SMALL_STATE(211)] = 4691,
  [SMALL_STATE(212)] = 4704,
  [SMALL_STATE(213)] = 4721,
  [SMALL_STATE(214)] = 4734,
  [SMALL_STATE(215)] = 4747,
  [SMALL_STATE(216)] = 4762,
  [SMALL_STATE(217)] = 4777,
  [SMALL_STATE(218)] = 4790,
  [SMALL_STATE(219)] = 4805,
  [SMALL_STATE(220)] = 4818,
  [SMALL_STATE(221)] = 4833,
  [SMALL_STATE(222)] = 4848,
  [SMALL_STATE(223)] = 4861,
  [SMALL_STATE(224)] = 4874,
  [SMALL_STATE(225)] = 4887,
  [SMALL_STATE(226)] = 4900,
  [SMALL_STATE(227)] = 4915,
  [SMALL_STATE(228)] = 4930,
  [SMALL_STATE(229)] = 4945,
  [SMALL_STATE(230)] = 4960,
  [SMALL_STATE(231)] = 4973,
  [SMALL_STATE(232)] = 4986,
  [SMALL_STATE(233)] = 4999,
  [SMALL_STATE(234)] = 5012,
  [SMALL_STATE(235)] = 5025,
  [SMALL_STATE(236)] = 5038,
  [SMALL_STATE(237)] = 5053,
  [SMALL_STATE(238)] = 5070,
  [SMALL_STATE(239)] = 5087,
  [SMALL_STATE(240)] = 5100,
  [SMALL_STATE(241)] = 5113,
  [SMALL_STATE(242)] = 5130,
  [SMALL_STATE(243)] = 5141,
  [SMALL_STATE(244)] = 5154,
  [SMALL_STATE(245)] = 5171,
  [SMALL_STATE(246)] = 5185,
  [SMALL_STATE(247)] = 5197,
  [SMALL_STATE(248)] = 5211,
  [SMALL_STATE(249)] = 5225,
  [SMALL_STATE(250)] = 5239,
  [SMALL_STATE(251)] = 5253,
  [SMALL_STATE(252)] = 5267,
  [SMALL_STATE(253)] = 5279,
  [SMALL_STATE(254)] = 5293,
  [SMALL_STATE(255)] = 5307,
  [SMALL_STATE(256)] = 5321,
  [SMALL_STATE(257)] = 5333,
  [SMALL_STATE(258)] = 5345,
  [SMALL_STATE(259)] = 5357,
  [SMALL_STATE(260)] = 5369,
  [SMALL_STATE(261)] = 5381,
  [SMALL_STATE(262)] = 5393,
  [SMALL_STATE(263)] = 5405,
  [SMALL_STATE(264)] = 5417,
  [SMALL_STATE(265)] = 5429,
  [SMALL_STATE(266)] = 5441,
  [SMALL_STATE(267)] = 5453,
  [SMALL_STATE(268)] = 5465,
  [SMALL_STATE(269)] = 5479,
  [SMALL_STATE(270)] = 5489,
  [SMALL_STATE(271)] = 5501,
  [SMALL_STATE(272)] = 5515,
  [SMALL_STATE(273)] = 5527,
  [SMALL_STATE(274)] = 5539,
  [SMALL_STATE(275)] = 5551,
  [SMALL_STATE(276)] = 5563,
  [SMALL_STATE(277)] = 5575,
  [SMALL_STATE(278)] = 5585,
  [SMALL_STATE(279)] = 5595,
  [SMALL_STATE(280)] = 5605,
  [SMALL_STATE(281)] = 5615,
  [SMALL_STATE(282)] = 5625,
  [SMALL_STATE(283)] = 5639,
  [SMALL_STATE(284)] = 5649,
  [SMALL_STATE(285)] = 5659,
  [SMALL_STATE(286)] = 5669,
  [SMALL_STATE(287)] = 5679,
  [SMALL_STATE(288)] = 5693,
  [SMALL_STATE(289)] = 5707,
  [SMALL_STATE(290)] = 5721,
  [SMALL_STATE(291)] = 5731,
  [SMALL_STATE(292)] = 5741,
  [SMALL_STATE(293)] = 5755,
  [SMALL_STATE(294)] = 5769,
  [SMALL_STATE(295)] = 5783,
  [SMALL_STATE(296)] = 5797,
  [SMALL_STATE(297)] = 5811,
  [SMALL_STATE(298)] = 5821,
  [SMALL_STATE(299)] = 5835,
  [SMALL_STATE(300)] = 5849,
  [SMALL_STATE(301)] = 5863,
  [SMALL_STATE(302)] = 5877,
  [SMALL_STATE(303)] = 5891,
  [SMALL_STATE(304)] = 5905,
  [SMALL_STATE(305)] = 5919,
  [SMALL_STATE(306)] = 5933,
  [SMALL_STATE(307)] = 5947,
  [SMALL_STATE(308)] = 5961,
  [SMALL_STATE(309)] = 5975,
  [SMALL_STATE(310)] = 5989,
  [SMALL_STATE(311)] = 6003,
  [SMALL_STATE(312)] = 6017,
  [SMALL_STATE(313)] = 6031,
  [SMALL_STATE(314)] = 6045,
  [SMALL_STATE(315)] = 6059,
  [SMALL_STATE(316)] = 6071,
  [SMALL_STATE(317)] = 6084,
  [SMALL_STATE(318)] = 6095,
  [SMALL_STATE(319)] = 6104,
  [SMALL_STATE(320)] = 6117,
  [SMALL_STATE(321)] = 6128,
  [SMALL_STATE(322)] = 6139,
  [SMALL_STATE(323)] = 6150,
  [SMALL_STATE(324)] = 6159,
  [SMALL_STATE(325)] = 6170,
  [SMALL_STATE(326)] = 6181,
  [SMALL_STATE(327)] = 6190,
  [SMALL_STATE(328)] = 6201,
  [SMALL_STATE(329)] = 6214,
  [SMALL_STATE(330)] = 6223,
  [SMALL_STATE(331)] = 6234,
  [SMALL_STATE(332)] = 6245,
  [SMALL_STATE(333)] = 6256,
  [SMALL_STATE(334)] = 6267,
  [SMALL_STATE(335)] = 6276,
  [SMALL_STATE(336)] = 6285,
  [SMALL_STATE(337)] = 6294,
  [SMALL_STATE(338)] = 6303,
  [SMALL_STATE(339)] = 6312,
  [SMALL_STATE(340)] = 6323,
  [SMALL_STATE(341)] = 6332,
  [SMALL_STATE(342)] = 6341,
  [SMALL_STATE(343)] = 6350,
  [SMALL_STATE(344)] = 6361,
  [SMALL_STATE(345)] = 6369,
  [SMALL_STATE(346)] = 6377,
  [SMALL_STATE(347)] = 6385,
  [SMALL_STATE(348)] = 6393,
  [SMALL_STATE(349)] = 6401,
  [SMALL_STATE(350)] = 6409,
  [SMALL_STATE(351)] = 6417,
  [SMALL_STATE(352)] = 6425,
  [SMALL_STATE(353)] = 6433,
  [SMALL_STATE(354)] = 6441,
  [SMALL_STATE(355)] = 6449,
  [SMALL_STATE(356)] = 6457,
  [SMALL_STATE(357)] = 6465,
  [SMALL_STATE(358)] = 6475,
  [SMALL_STATE(359)] = 6483,
  [SMALL_STATE(360)] = 6491,
  [SMALL_STATE(361)] = 6499,
  [SMALL_STATE(362)] = 6509,
  [SMALL_STATE(363)] = 6517,
  [SMALL_STATE(364)] = 6525,
  [SMALL_STATE(365)] = 6533,
  [SMALL_STATE(366)] = 6541,
  [SMALL_STATE(367)] = 6549,
  [SMALL_STATE(368)] = 6557,
  [SMALL_STATE(369)] = 6567,
  [SMALL_STATE(370)] = 6575,
  [SMALL_STATE(371)] = 6583,
  [SMALL_STATE(372)] = 6591,
  [SMALL_STATE(373)] = 6599,
  [SMALL_STATE(374)] = 6607,
  [SMALL_STATE(375)] = 6615,
  [SMALL_STATE(376)] = 6623,
  [SMALL_STATE(377)] = 6631,
  [SMALL_STATE(378)] = 6641,
  [SMALL_STATE(379)] = 6649,
  [SMALL_STATE(380)] = 6657,
  [SMALL_STATE(381)] = 6665,
  [SMALL_STATE(382)] = 6675,
  [SMALL_STATE(383)] = 6683,
  [SMALL_STATE(384)] = 6691,
  [SMALL_STATE(385)] = 6699,
  [SMALL_STATE(386)] = 6707,
  [SMALL_STATE(387)] = 6715,
  [SMALL_STATE(388)] = 6723,
  [SMALL_STATE(389)] = 6731,
  [SMALL_STATE(390)] = 6739,
  [SMALL_STATE(391)] = 6747,
  [SMALL_STATE(392)] = 6755,
  [SMALL_STATE(393)] = 6763,
  [SMALL_STATE(394)] = 6771,
  [SMALL_STATE(395)] = 6779,
  [SMALL_STATE(396)] = 6787,
  [SMALL_STATE(397)] = 6795,
  [SMALL_STATE(398)] = 6803,
  [SMALL_STATE(399)] = 6811,
  [SMALL_STATE(400)] = 6819,
  [SMALL_STATE(401)] = 6827,
  [SMALL_STATE(402)] = 6835,
  [SMALL_STATE(403)] = 6843,
  [SMALL_STATE(404)] = 6851,
  [SMALL_STATE(405)] = 6859,
  [SMALL_STATE(406)] = 6867,
  [SMALL_STATE(407)] = 6875,
  [SMALL_STATE(408)] = 6883,
  [SMALL_STATE(409)] = 6891,
  [SMALL_STATE(410)] = 6899,
  [SMALL_STATE(411)] = 6907,
  [SMALL_STATE(412)] = 6915,
  [SMALL_STATE(413)] = 6923,
  [SMALL_STATE(414)] = 6931,
  [SMALL_STATE(415)] = 6939,
  [SMALL_STATE(416)] = 6947,
  [SMALL_STATE(417)] = 6955,
  [SMALL_STATE(418)] = 6963,
  [SMALL_STATE(419)] = 6971,
  [SMALL_STATE(420)] = 6979,
  [SMALL_STATE(421)] = 6987,
  [SMALL_STATE(422)] = 6995,
  [SMALL_STATE(423)] = 7003,
  [SMALL_STATE(424)] = 7011,
  [SMALL_STATE(425)] = 7019,
  [SMALL_STATE(426)] = 7027,
  [SMALL_STATE(427)] = 7035,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [7] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0, 0, 0),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(84),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(121),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(130),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(86),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(200),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(159),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(320),
  [35] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [37] = {.entry = {.count = 1, .reusable = true}}, SHIFT(241),
  [39] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(332),
  [43] = {.entry = {.count = 1, .reusable = true}}, SHIFT(363),
  [45] = {.entry = {.count = 1, .reusable = true}}, SHIFT(366),
  [47] = {.entry = {.count = 1, .reusable = true}}, SHIFT(364),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0),
  [51] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(84),
  [54] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(6),
  [57] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(12),
  [60] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(29),
  [63] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(121),
  [66] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(130),
  [69] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(20),
  [72] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(19),
  [75] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(11),
  [78] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(86),
  [81] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(200),
  [84] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(159),
  [87] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(320),
  [90] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(4),
  [93] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(241),
  [96] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(50),
  [99] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(332),
  [102] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(363),
  [105] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(366),
  [108] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(364),
  [111] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1, 0, 0),
  [113] = {.entry = {.count = 1, .reusable = false}}, SHIFT(56),
  [115] = {.entry = {.count = 1, .reusable = true}}, SHIFT(317),
  [117] = {.entry = {.count = 1, .reusable = false}}, SHIFT(65),
  [119] = {.entry = {.count = 1, .reusable = false}}, SHIFT(67),
  [121] = {.entry = {.count = 1, .reusable = false}}, SHIFT(69),
  [123] = {.entry = {.count = 1, .reusable = true}}, SHIFT(156),
  [125] = {.entry = {.count = 1, .reusable = false}}, SHIFT(336),
  [127] = {.entry = {.count = 1, .reusable = true}}, SHIFT(385),
  [129] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_env_pair, 2, 0, 37),
  [131] = {.entry = {.count = 1, .reusable = true}}, SHIFT(311),
  [133] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_env_pair, 2, 0, 37),
  [135] = {.entry = {.count = 1, .reusable = false}}, SHIFT(75),
  [137] = {.entry = {.count = 1, .reusable = false}}, SHIFT(190),
  [139] = {.entry = {.count = 1, .reusable = false}}, SHIFT(157),
  [141] = {.entry = {.count = 1, .reusable = false}}, SHIFT(131),
  [143] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unquoted_string, 1, 0, 0),
  [145] = {.entry = {.count = 1, .reusable = false}}, SHIFT(310),
  [147] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unquoted_string, 1, 0, 0),
  [149] = {.entry = {.count = 1, .reusable = false}}, SHIFT(35),
  [151] = {.entry = {.count = 1, .reusable = false}}, SHIFT(36),
  [153] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_unquoted_string_repeat1, 2, 0, 0),
  [155] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_unquoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(310),
  [158] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_unquoted_string_repeat1, 2, 0, 0),
  [160] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_unquoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(35),
  [163] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_unquoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(36),
  [166] = {.entry = {.count = 1, .reusable = false}}, SHIFT(87),
  [168] = {.entry = {.count = 1, .reusable = true}}, SHIFT(88),
  [170] = {.entry = {.count = 1, .reusable = false}}, SHIFT(89),
  [172] = {.entry = {.count = 1, .reusable = false}}, SHIFT(90),
  [174] = {.entry = {.count = 1, .reusable = true}}, SHIFT(151),
  [176] = {.entry = {.count = 1, .reusable = false}}, SHIFT(413),
  [178] = {.entry = {.count = 1, .reusable = true}}, SHIFT(360),
  [180] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_run_instruction_repeat1, 2, 0, 0),
  [182] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_run_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(317),
  [185] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_run_instruction_repeat1, 2, 0, 0),
  [187] = {.entry = {.count = 1, .reusable = true}}, SHIFT(310),
  [189] = {.entry = {.count = 1, .reusable = true}}, SHIFT(73),
  [191] = {.entry = {.count = 1, .reusable = true}}, SHIFT(186),
  [193] = {.entry = {.count = 1, .reusable = true}}, SHIFT(312),
  [195] = {.entry = {.count = 1, .reusable = false}}, SHIFT(239),
  [197] = {.entry = {.count = 1, .reusable = false}}, SHIFT(240),
  [199] = {.entry = {.count = 1, .reusable = false}}, SHIFT(142),
  [201] = {.entry = {.count = 1, .reusable = true}}, SHIFT(123),
  [203] = {.entry = {.count = 1, .reusable = true}}, SHIFT(296),
  [205] = {.entry = {.count = 1, .reusable = true}}, SHIFT(425),
  [207] = {.entry = {.count = 1, .reusable = true}}, SHIFT(388),
  [209] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_label_instruction, 2, 0, 10),
  [211] = {.entry = {.count = 1, .reusable = false}}, SHIFT(374),
  [213] = {.entry = {.count = 1, .reusable = false}}, SHIFT(54),
  [215] = {.entry = {.count = 1, .reusable = false}}, SHIFT(169),
  [217] = {.entry = {.count = 1, .reusable = false}}, SHIFT(401),
  [219] = {.entry = {.count = 1, .reusable = false}}, SHIFT(146),
  [221] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_label_instruction_repeat1, 2, 0, 0),
  [223] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_label_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(374),
  [226] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_label_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(54),
  [229] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_label_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(169),
  [232] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_label_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(401),
  [235] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_label_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(146),
  [238] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_mount_param, 5, 0, 66),
  [240] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_mount_param, 5, 0, 66),
  [242] = {.entry = {.count = 1, .reusable = true}}, SHIFT(319),
  [244] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_mount_param, 4, 0, 65),
  [246] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_mount_param, 4, 0, 65),
  [248] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_mount_param_repeat1, 2, 0, 0),
  [250] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_mount_param_repeat1, 2, 0, 0),
  [252] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_mount_param_repeat1, 2, 0, 0), SHIFT_REPEAT(319),
  [255] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_image_name, 1, 0, 0),
  [257] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_image_name, 1, 0, 0),
  [259] = {.entry = {.count = 1, .reusable = true}}, SHIFT(287),
  [261] = {.entry = {.count = 1, .reusable = false}}, SHIFT(141),
  [263] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [265] = {.entry = {.count = 1, .reusable = true}}, SHIFT(169),
  [267] = {.entry = {.count = 1, .reusable = true}}, SHIFT(401),
  [269] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_image_name, 1, 0, 1),
  [271] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_image_name, 1, 0, 1),
  [273] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_image_name, 2, 0, 1),
  [275] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_image_name, 2, 0, 1),
  [277] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_image_name, 2, 0, 0),
  [279] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_image_name, 2, 0, 0),
  [281] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_shell_fragment_repeat1, 2, 0, 0),
  [283] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_shell_fragment_repeat1, 2, 0, 0), SHIFT_REPEAT(56),
  [286] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_shell_fragment_repeat1, 2, 0, 0), SHIFT_REPEAT(65),
  [289] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_shell_fragment_repeat1, 2, 0, 0), SHIFT_REPEAT(67),
  [292] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_shell_fragment_repeat1, 2, 0, 0), SHIFT_REPEAT(69),
  [295] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_shell_fragment_repeat1, 2, 0, 0),
  [297] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_shell_fragment_repeat1, 2, 0, 0), SHIFT_REPEAT(385),
  [300] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_image_name_repeat1, 2, 0, 0),
  [302] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_image_name_repeat1, 2, 0, 0),
  [304] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_image_name_repeat1, 2, 0, 0), SHIFT_REPEAT(287),
  [307] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_image_name_repeat1, 2, 0, 0), SHIFT_REPEAT(141),
  [310] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_unquoted_string_repeat1, 1, 0, 52),
  [312] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_unquoted_string_repeat1, 1, 0, 52),
  [314] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_unquoted_string_repeat1, 1, 0, 53),
  [316] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_unquoted_string_repeat1, 1, 0, 53),
  [318] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_shell_fragment, 1, 0, 0),
  [320] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_shell_fragment, 1, 0, 0),
  [322] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_mount_param_param, 3, 0, 67),
  [324] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_mount_param_param, 3, 0, 67),
  [326] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_mount_param_repeat1, 2, 0, 68),
  [328] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_mount_param_repeat1, 2, 0, 68),
  [330] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_unquoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(311),
  [333] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_unquoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(157),
  [336] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_unquoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(131),
  [339] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_immediate_expansion, 1, 0, 0),
  [341] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_immediate_expansion, 1, 0, 0),
  [343] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_imm_expansion, 2, 0, 43),
  [345] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_imm_expansion, 2, 0, 43),
  [347] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expansion_body, 1, 0, 0),
  [349] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expansion_body, 1, 0, 0),
  [351] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expansion_body, 3, 0, 61),
  [353] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expansion_body, 3, 0, 61),
  [355] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_path_with_heredoc, 2, 0, 0),
  [357] = {.entry = {.count = 1, .reusable = false}}, SHIFT(172),
  [359] = {.entry = {.count = 1, .reusable = true}}, SHIFT(313),
  [361] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_path_with_heredoc, 2, 0, 0),
  [363] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_image_tag, 2, 0, 47),
  [365] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_image_tag, 2, 0, 47),
  [367] = {.entry = {.count = 1, .reusable = true}}, SHIFT(306),
  [369] = {.entry = {.count = 1, .reusable = false}}, SHIFT(191),
  [371] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_user_name_or_group, 1, 0, 0),
  [373] = {.entry = {.count = 1, .reusable = true}}, SHIFT(242),
  [375] = {.entry = {.count = 1, .reusable = true}}, SHIFT(300),
  [377] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_shell_fragment_repeat1, 2, 0, 34),
  [379] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_shell_fragment_repeat1, 2, 0, 34),
  [381] = {.entry = {.count = 1, .reusable = true}}, SHIFT(399),
  [383] = {.entry = {.count = 1, .reusable = true}}, SHIFT(426),
  [385] = {.entry = {.count = 1, .reusable = true}}, SHIFT(235),
  [387] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_double_quoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(249),
  [390] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_double_quoted_string_repeat1, 2, 0, 0),
  [392] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_double_quoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(173),
  [395] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_double_quoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(51),
  [398] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_double_quoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(51),
  [401] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_user_name_or_group_repeat1, 2, 0, 0),
  [403] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_user_name_or_group_repeat1, 2, 0, 0), SHIFT_REPEAT(242),
  [406] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_user_name_or_group_repeat1, 2, 0, 0), SHIFT_REPEAT(300),
  [409] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_image_tag_repeat1, 2, 0, 0),
  [411] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_image_tag_repeat1, 2, 0, 0),
  [413] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_image_tag_repeat1, 2, 0, 0), SHIFT_REPEAT(306),
  [416] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_image_tag_repeat1, 2, 0, 0), SHIFT_REPEAT(191),
  [419] = {.entry = {.count = 1, .reusable = true}}, SHIFT(249),
  [421] = {.entry = {.count = 1, .reusable = false}}, SHIFT(365),
  [423] = {.entry = {.count = 1, .reusable = false}}, SHIFT(173),
  [425] = {.entry = {.count = 1, .reusable = false}}, SHIFT(55),
  [427] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [429] = {.entry = {.count = 1, .reusable = false}}, SHIFT(389),
  [431] = {.entry = {.count = 1, .reusable = false}}, SHIFT(51),
  [433] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [435] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_shell_fragment_repeat1, 1, 0, 4),
  [437] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_shell_fragment_repeat1, 1, 0, 4),
  [439] = {.entry = {.count = 1, .reusable = false}}, SHIFT(88),
  [441] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_path_with_heredoc, 1, 0, 15),
  [443] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_path_with_heredoc, 1, 0, 15),
  [445] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_path_with_heredoc, 1, 0, 16),
  [447] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_path_with_heredoc, 1, 0, 16),
  [449] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_path_with_heredoc, 1, 0, 0),
  [451] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_path_with_heredoc, 1, 0, 0),
  [453] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_shell_fragment_repeat1, 2, 0, 0), SHIFT_REPEAT(87),
  [456] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_shell_fragment_repeat1, 2, 0, 0), SHIFT_REPEAT(88),
  [459] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_shell_fragment_repeat1, 2, 0, 0), SHIFT_REPEAT(89),
  [462] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_shell_fragment_repeat1, 2, 0, 0), SHIFT_REPEAT(90),
  [465] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_shell_fragment_repeat1, 2, 0, 0), SHIFT_REPEAT(360),
  [468] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_path_with_heredoc, 2, 0, 15),
  [470] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_path_with_heredoc, 2, 0, 15),
  [472] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_path_with_heredoc, 2, 0, 16),
  [474] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_path_with_heredoc, 2, 0, 16),
  [476] = {.entry = {.count = 1, .reusable = false}}, SHIFT(264),
  [478] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_shell_fragment_repeat1, 1, 0, 5),
  [480] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_shell_fragment_repeat1, 1, 0, 5),
  [482] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_param, 4, 0, 62),
  [484] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_param, 4, 0, 62),
  [486] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_shell_fragment_repeat1, 1, 0, 6),
  [488] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_shell_fragment_repeat1, 1, 0, 6),
  [490] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_path_repeat1, 2, 0, 0),
  [492] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_path_repeat1, 2, 0, 0), SHIFT_REPEAT(172),
  [495] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_path_repeat1, 2, 0, 0), SHIFT_REPEAT(313),
  [498] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_path_repeat1, 2, 0, 0),
  [500] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_shell_fragment_repeat1, 1, 0, 7),
  [502] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_shell_fragment_repeat1, 1, 0, 7),
  [504] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_user_name_or_group, 2, 0, 19),
  [506] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_user_name_or_group, 2, 0, 0),
  [508] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_user_name_or_group, 1, 0, 19),
  [510] = {.entry = {.count = 1, .reusable = false}}, SHIFT(122),
  [512] = {.entry = {.count = 1, .reusable = false}}, SHIFT(74),
  [514] = {.entry = {.count = 1, .reusable = true}}, SHIFT(74),
  [516] = {.entry = {.count = 1, .reusable = false}}, SHIFT(133),
  [518] = {.entry = {.count = 1, .reusable = false}}, SHIFT(260),
  [520] = {.entry = {.count = 1, .reusable = false}}, SHIFT(64),
  [522] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [524] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_image_digest_repeat1, 2, 0, 0),
  [526] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_image_digest_repeat1, 2, 0, 0),
  [528] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_image_digest_repeat1, 2, 0, 0), SHIFT_REPEAT(308),
  [531] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_image_digest_repeat1, 2, 0, 0), SHIFT_REPEAT(217),
  [534] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_image_digest, 2, 0, 49),
  [536] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_image_digest, 2, 0, 49),
  [538] = {.entry = {.count = 1, .reusable = true}}, SHIFT(308),
  [540] = {.entry = {.count = 1, .reusable = true}}, SHIFT(217),
  [542] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_path, 1, 0, 15),
  [544] = {.entry = {.count = 1, .reusable = false}}, SHIFT(243),
  [546] = {.entry = {.count = 1, .reusable = true}}, SHIFT(298),
  [548] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_path, 1, 0, 15),
  [550] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_path, 1, 0, 4),
  [552] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_path, 1, 0, 4),
  [554] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_path, 1, 0, 0),
  [556] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_path, 1, 0, 0),
  [558] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [560] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_immediate_user_name_or_group, 1, 0, 0),
  [562] = {.entry = {.count = 1, .reusable = true}}, SHIFT(268),
  [564] = {.entry = {.count = 1, .reusable = false}}, SHIFT(30),
  [566] = {.entry = {.count = 1, .reusable = true}}, SHIFT(353),
  [568] = {.entry = {.count = 1, .reusable = false}}, SHIFT(58),
  [570] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [572] = {.entry = {.count = 1, .reusable = true}}, SHIFT(307),
  [574] = {.entry = {.count = 1, .reusable = true}}, SHIFT(246),
  [576] = {.entry = {.count = 1, .reusable = false}}, SHIFT(79),
  [578] = {.entry = {.count = 1, .reusable = true}}, SHIFT(80),
  [580] = {.entry = {.count = 1, .reusable = true}}, SHIFT(299),
  [582] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_expose_instruction_repeat1, 2, 0, 0),
  [584] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_expose_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(293),
  [587] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_expose_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(107),
  [590] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_expose_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(230),
  [593] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_path_repeat1, 2, 0, 0), SHIFT_REPEAT(243),
  [596] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_path_repeat1, 2, 0, 0), SHIFT_REPEAT(298),
  [599] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_add_instruction_repeat1, 2, 0, 0),
  [601] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_add_instruction_repeat1, 2, 0, 0),
  [603] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_add_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(425),
  [606] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_unquoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(312),
  [609] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_unquoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(239),
  [612] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_unquoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(240),
  [615] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_add_instruction_repeat2, 2, 0, 0), SHIFT_REPEAT(142),
  [618] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_add_instruction_repeat2, 2, 0, 0), SHIFT_REPEAT(123),
  [621] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_add_instruction_repeat2, 2, 0, 0), SHIFT_REPEAT(296),
  [624] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_add_instruction_repeat2, 2, 0, 0), SHIFT_REPEAT(388),
  [627] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_path, 2, 0, 15),
  [629] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_path, 2, 0, 15),
  [631] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expose_instruction, 2, 0, 12),
  [633] = {.entry = {.count = 1, .reusable = false}}, SHIFT(293),
  [635] = {.entry = {.count = 1, .reusable = false}}, SHIFT(107),
  [637] = {.entry = {.count = 1, .reusable = false}}, SHIFT(230),
  [639] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_path, 2, 0, 4),
  [641] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_path, 2, 0, 4),
  [643] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_path, 2, 0, 0),
  [645] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_path, 2, 0, 0),
  [647] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_single_quoted_string, 2, 0, 0),
  [649] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_single_quoted_string, 2, 0, 0),
  [651] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expose_port, 1, 0, 11),
  [653] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expose_port, 1, 0, 11),
  [655] = {.entry = {.count = 1, .reusable = false}}, SHIFT(231),
  [657] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_stopsignal_value, 2, 0, 0),
  [659] = {.entry = {.count = 1, .reusable = true}}, SHIFT(297),
  [661] = {.entry = {.count = 1, .reusable = true}}, SHIFT(304),
  [663] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_label_pair, 3, 0, 54),
  [665] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_label_pair, 3, 0, 54),
  [667] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expansion, 2, 0, 0),
  [669] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expansion, 2, 0, 0),
  [671] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_stopsignal_value, 1, 0, 25),
  [673] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_stopsignal_value, 1, 0, 0),
  [675] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_label_pair, 3, 0, 55),
  [677] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_label_pair, 3, 0, 55),
  [679] = {.entry = {.count = 1, .reusable = true}}, SHIFT(185),
  [681] = {.entry = {.count = 1, .reusable = false}}, SHIFT(262),
  [683] = {.entry = {.count = 1, .reusable = true}}, SHIFT(302),
  [685] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_path_repeat1, 2, 0, 0), SHIFT_REPEAT(270),
  [688] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_path_repeat1, 2, 0, 0), SHIFT_REPEAT(245),
  [691] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_image_alias, 1, 0, 44),
  [693] = {.entry = {.count = 1, .reusable = true}}, SHIFT(314),
  [695] = {.entry = {.count = 1, .reusable = true}}, SHIFT(284),
  [697] = {.entry = {.count = 1, .reusable = true}}, SHIFT(293),
  [699] = {.entry = {.count = 1, .reusable = true}}, SHIFT(107),
  [701] = {.entry = {.count = 1, .reusable = true}}, SHIFT(230),
  [703] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_double_quoted_string, 2, 0, 0),
  [705] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_double_quoted_string, 2, 0, 0),
  [707] = {.entry = {.count = 1, .reusable = false}}, SHIFT(270),
  [709] = {.entry = {.count = 1, .reusable = true}}, SHIFT(245),
  [711] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_env_instruction_repeat1, 2, 0, 0),
  [713] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_env_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(348),
  [716] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_env_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(315),
  [719] = {.entry = {.count = 1, .reusable = true}}, SHIFT(326),
  [721] = {.entry = {.count = 1, .reusable = true}}, SHIFT(315),
  [723] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_double_quoted_string, 3, 0, 0),
  [725] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_double_quoted_string, 3, 0, 0),
  [727] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_single_quoted_string, 3, 0, 0),
  [729] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_single_quoted_string, 3, 0, 0),
  [731] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_image_alias, 1, 0, 0),
  [733] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_stopsignal_value_repeat1, 2, 0, 0),
  [735] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_stopsignal_value_repeat1, 2, 0, 0), SHIFT_REPEAT(297),
  [738] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_stopsignal_value_repeat1, 2, 0, 0), SHIFT_REPEAT(304),
  [741] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_image_alias, 2, 0, 44),
  [743] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_path_repeat1, 2, 0, 0), SHIFT_REPEAT(262),
  [746] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_path_repeat1, 2, 0, 0), SHIFT_REPEAT(302),
  [749] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_image_alias, 2, 0, 0),
  [751] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_image_name_repeat1, 1, 0, 31),
  [753] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_image_name_repeat1, 1, 0, 31),
  [755] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_stopsignal_value, 2, 0, 25),
  [757] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_label_pair, 1, 0, 0),
  [759] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_label_pair, 1, 0, 0),
  [761] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_image_alias_repeat1, 2, 0, 0),
  [763] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_image_alias_repeat1, 2, 0, 0), SHIFT_REPEAT(314),
  [766] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_image_alias_repeat1, 2, 0, 0), SHIFT_REPEAT(284),
  [769] = {.entry = {.count = 1, .reusable = true}}, SHIFT(372),
  [771] = {.entry = {.count = 1, .reusable = true}}, SHIFT(238),
  [773] = {.entry = {.count = 1, .reusable = true}}, SHIFT(323),
  [775] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_image_spec, 1, 0, 3),
  [777] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_image_spec, 1, 0, 3),
  [779] = {.entry = {.count = 1, .reusable = true}}, SHIFT(188),
  [781] = {.entry = {.count = 1, .reusable = true}}, SHIFT(189),
  [783] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_env_instruction, 2, 0, 14),
  [785] = {.entry = {.count = 1, .reusable = false}}, SHIFT(348),
  [787] = {.entry = {.count = 1, .reusable = false}}, SHIFT(315),
  [789] = {.entry = {.count = 1, .reusable = true}}, SHIFT(334),
  [791] = {.entry = {.count = 1, .reusable = false}}, SHIFT(116),
  [793] = {.entry = {.count = 1, .reusable = true}}, SHIFT(117),
  [795] = {.entry = {.count = 1, .reusable = true}}, SHIFT(303),
  [797] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_add_instruction, 3, 0, 39),
  [799] = {.entry = {.count = 1, .reusable = true}}, SHIFT(236),
  [801] = {.entry = {.count = 1, .reusable = true}}, SHIFT(248),
  [803] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_single_quoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(163),
  [806] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_single_quoted_string_repeat1, 2, 0, 0),
  [808] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_single_quoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(219),
  [811] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_single_quoted_string_repeat1, 2, 0, 0), SHIFT_REPEAT(163),
  [814] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_add_instruction, 4, 0, 39),
  [816] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_copy_instruction, 4, 0, 40),
  [818] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_copy_instruction, 3, 0, 40),
  [820] = {.entry = {.count = 1, .reusable = false}}, SHIFT(182),
  [822] = {.entry = {.count = 1, .reusable = false}}, SHIFT(350),
  [824] = {.entry = {.count = 1, .reusable = false}}, SHIFT(219),
  [826] = {.entry = {.count = 1, .reusable = true}}, SHIFT(182),
  [828] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_path_repeat1, 1, 0, 38),
  [830] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_path_repeat1, 1, 0, 38),
  [832] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_double_quoted_string_repeat1, 1, 0, 35),
  [834] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_double_quoted_string_repeat1, 1, 0, 35),
  [836] = {.entry = {.count = 1, .reusable = false}}, SHIFT(163),
  [838] = {.entry = {.count = 1, .reusable = false}}, SHIFT(265),
  [840] = {.entry = {.count = 1, .reusable = true}}, SHIFT(163),
  [842] = {.entry = {.count = 1, .reusable = false}}, SHIFT(395),
  [844] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_healthcheck_instruction_repeat1, 2, 0, 0),
  [846] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_healthcheck_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(426),
  [849] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_healthcheck_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(185),
  [852] = {.entry = {.count = 1, .reusable = false}}, SHIFT(187),
  [854] = {.entry = {.count = 1, .reusable = false}}, SHIFT(106),
  [856] = {.entry = {.count = 1, .reusable = true}}, SHIFT(187),
  [858] = {.entry = {.count = 1, .reusable = false}}, SHIFT(134),
  [860] = {.entry = {.count = 1, .reusable = false}}, SHIFT(174),
  [862] = {.entry = {.count = 1, .reusable = false}}, SHIFT(261),
  [864] = {.entry = {.count = 1, .reusable = true}}, SHIFT(174),
  [866] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_image_tag_repeat1, 1, 0, 46),
  [868] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_image_tag_repeat1, 1, 0, 46),
  [870] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_immediate_user_name_or_group_fragment, 1, 0, 0),
  [872] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_run_instruction, 3, 0, 8),
  [874] = {.entry = {.count = 1, .reusable = true}}, SHIFT(72),
  [876] = {.entry = {.count = 1, .reusable = true}}, SHIFT(301),
  [878] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_image_spec, 2, 0, 32),
  [880] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_image_spec, 2, 0, 32),
  [882] = {.entry = {.count = 1, .reusable = true}}, SHIFT(309),
  [884] = {.entry = {.count = 1, .reusable = true}}, SHIFT(120),
  [886] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_shell_command, 2, 0, 0),
  [888] = {.entry = {.count = 1, .reusable = false}}, SHIFT(82),
  [890] = {.entry = {.count = 1, .reusable = false}}, SHIFT(335),
  [892] = {.entry = {.count = 1, .reusable = false}}, SHIFT(252),
  [894] = {.entry = {.count = 1, .reusable = true}}, SHIFT(244),
  [896] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_add_instruction, 4, 0, 39),
  [898] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_run_instruction, 2, 0, 8),
  [900] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_image_digest_repeat1, 1, 0, 48),
  [902] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_image_digest_repeat1, 1, 0, 48),
  [904] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_run_instruction_repeat2, 2, 0, 0),
  [906] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_run_instruction_repeat2, 2, 0, 0), SHIFT_REPEAT(248),
  [909] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_single_quoted_string_repeat1, 1, 0, 36),
  [911] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_single_quoted_string_repeat1, 1, 0, 36),
  [913] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_add_instruction, 5, 0, 39),
  [915] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_copy_instruction, 5, 0, 40),
  [917] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_copy_instruction, 4, 0, 40),
  [919] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_shell_command_repeat1, 2, 0, 0),
  [921] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_shell_command_repeat1, 2, 0, 0), SHIFT_REPEAT(82),
  [924] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_run_instruction, 4, 0, 8),
  [926] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_shell_command, 1, 0, 0),
  [928] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expose_port, 1, 0, 0),
  [930] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expose_port, 1, 0, 0),
  [932] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expose_port, 2, 0, 11),
  [934] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_expose_port, 2, 0, 11),
  [936] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_healthcheck_instruction, 2, 0, 27),
  [938] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_healthcheck_instruction_repeat1, 1, 0, 0),
  [940] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_add_instruction_repeat2, 2, 0, 0),
  [942] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_add_instruction_repeat2, 2, 0, 0),
  [944] = {.entry = {.count = 1, .reusable = false}}, SHIFT(318),
  [946] = {.entry = {.count = 1, .reusable = true}}, SHIFT(212),
  [948] = {.entry = {.count = 1, .reusable = true}}, SHIFT(112),
  [950] = {.entry = {.count = 1, .reusable = true}}, SHIFT(305),
  [952] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_immediate_user_name_or_group_fragment, 1, 0, 41),
  [954] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_json_string_repeat1, 2, 0, 0),
  [956] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_json_string_repeat1, 2, 0, 0), SHIFT_REPEAT(252),
  [959] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_json_string_repeat1, 2, 0, 0), SHIFT_REPEAT(244),
  [962] = {.entry = {.count = 1, .reusable = true}}, SHIFT(410),
  [964] = {.entry = {.count = 1, .reusable = true}}, SHIFT(257),
  [966] = {.entry = {.count = 1, .reusable = true}}, SHIFT(160),
  [968] = {.entry = {.count = 1, .reusable = true}}, SHIFT(329),
  [970] = {.entry = {.count = 1, .reusable = true}}, SHIFT(391),
  [972] = {.entry = {.count = 1, .reusable = true}}, SHIFT(342),
  [974] = {.entry = {.count = 1, .reusable = true}}, SHIFT(417),
  [976] = {.entry = {.count = 1, .reusable = true}}, SHIFT(192),
  [978] = {.entry = {.count = 1, .reusable = false}}, SHIFT(93),
  [980] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_volume_instruction, 2, 0, 18),
  [982] = {.entry = {.count = 1, .reusable = true}}, SHIFT(170),
  [984] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_json_string_repeat1, 1, 0, 51),
  [986] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_json_string_repeat1, 1, 0, 51),
  [988] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_shell_command_repeat1, 2, 0, 0), SHIFT_REPEAT(93),
  [991] = {.entry = {.count = 1, .reusable = true}}, SHIFT(338),
  [993] = {.entry = {.count = 1, .reusable = true}}, SHIFT(341),
  [995] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_shell_command_repeat1, 2, 0, 0),
  [997] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_env_pair, 3, 0, 56),
  [999] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_env_pair, 3, 0, 56),
  [1001] = {.entry = {.count = 1, .reusable = true}}, SHIFT(392),
  [1003] = {.entry = {.count = 1, .reusable = true}}, SHIFT(155),
  [1005] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_volume_instruction_repeat1, 2, 0, 0),
  [1007] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_volume_instruction_repeat1, 2, 0, 0), SHIFT_REPEAT(170),
  [1010] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_image_alias_repeat1, 1, 0, 63),
  [1012] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_json_string_array_repeat1, 2, 0, 0), SHIFT_REPEAT(160),
  [1015] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_json_string_array_repeat1, 2, 0, 0),
  [1017] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_heredoc_block_repeat1, 2, 0, 0), SHIFT_REPEAT(391),
  [1020] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_heredoc_block_repeat1, 2, 0, 0),
  [1022] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_volume_instruction, 3, 0, 18),
  [1024] = {.entry = {.count = 1, .reusable = true}}, SHIFT(408),
  [1026] = {.entry = {.count = 1, .reusable = true}}, SHIFT(208),
  [1028] = {.entry = {.count = 1, .reusable = true}}, SHIFT(371),
  [1030] = {.entry = {.count = 1, .reusable = true}}, SHIFT(352),
  [1032] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_stopsignal_value_repeat1, 1, 0, 42),
  [1034] = {.entry = {.count = 1, .reusable = true}}, SHIFT(412),
  [1036] = {.entry = {.count = 1, .reusable = true}}, SHIFT(195),
  [1038] = {.entry = {.count = 1, .reusable = true}}, SHIFT(414),
  [1040] = {.entry = {.count = 1, .reusable = true}}, SHIFT(202),
  [1042] = {.entry = {.count = 1, .reusable = true}}, SHIFT(415),
  [1044] = {.entry = {.count = 1, .reusable = true}}, SHIFT(272),
  [1046] = {.entry = {.count = 1, .reusable = true}}, SHIFT(416),
  [1048] = {.entry = {.count = 1, .reusable = true}}, SHIFT(277),
  [1050] = {.entry = {.count = 1, .reusable = true}}, SHIFT(420),
  [1052] = {.entry = {.count = 1, .reusable = true}}, SHIFT(181),
  [1054] = {.entry = {.count = 1, .reusable = true}}, SHIFT(418),
  [1056] = {.entry = {.count = 1, .reusable = true}}, SHIFT(179),
  [1058] = {.entry = {.count = 1, .reusable = true}}, SHIFT(421),
  [1060] = {.entry = {.count = 1, .reusable = true}}, SHIFT(222),
  [1062] = {.entry = {.count = 1, .reusable = true}}, SHIFT(419),
  [1064] = {.entry = {.count = 1, .reusable = true}}, SHIFT(285),
  [1066] = {.entry = {.count = 1, .reusable = true}}, SHIFT(422),
  [1068] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [1070] = {.entry = {.count = 1, .reusable = true}}, SHIFT(423),
  [1072] = {.entry = {.count = 1, .reusable = true}}, SHIFT(149),
  [1074] = {.entry = {.count = 1, .reusable = true}}, SHIFT(424),
  [1076] = {.entry = {.count = 1, .reusable = true}}, SHIFT(232),
  [1078] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_env_pair, 1, 0, 0),
  [1080] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_env_pair, 1, 0, 0),
  [1082] = {.entry = {.count = 1, .reusable = false}}, SHIFT(405),
  [1084] = {.entry = {.count = 1, .reusable = false}}, SHIFT(409),
  [1086] = {.entry = {.count = 1, .reusable = false}}, SHIFT(397),
  [1088] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_json_string, 2, 0, 0),
  [1090] = {.entry = {.count = 1, .reusable = true}}, SHIFT(322),
  [1092] = {.entry = {.count = 1, .reusable = true}}, SHIFT(343),
  [1094] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_from_instruction, 2, 0, 2),
  [1096] = {.entry = {.count = 1, .reusable = false}}, SHIFT(237),
  [1098] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_instruction, 2, 0, 22),
  [1100] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [1102] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_array_element, 1, 0, 0),
  [1104] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_image_spec, 2, 0, 33),
  [1106] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_image_spec, 2, 0, 33),
  [1108] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_from_instruction, 3, 0, 2),
  [1110] = {.entry = {.count = 1, .reusable = false}}, SHIFT(207),
  [1112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_env_key, 1, 0, 13),
  [1114] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_user_instruction, 2, 0, 20),
  [1116] = {.entry = {.count = 1, .reusable = true}}, SHIFT(77),
  [1118] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_json_string_array, 3, 0, 0),
  [1120] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_volume_instruction_repeat1, 2, 0, 0),
  [1122] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [1124] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [1126] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_json_string_array, 2, 0, 0),
  [1128] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_json_string, 3, 0, 0),
  [1130] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_json_string_array, 4, 0, 0),
  [1132] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_image_spec, 3, 0, 50),
  [1134] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_image_spec, 3, 0, 50),
  [1136] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_heredoc_block, 3, 0, 0),
  [1138] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_heredoc_block, 2, 0, 0),
  [1140] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_instruction, 2, 0, 23),
  [1142] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [1144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(381),
  [1146] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_spaced_env_pair, 3, 0, 57),
  [1148] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_volume_instruction, 2, 0, 18),
  [1150] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_workdir_instruction, 2, 0, 21),
  [1152] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_shell_instruction, 2, 0, 28),
  [1154] = {.entry = {.count = 1, .reusable = true}}, SHIFT(356),
  [1156] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [1158] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_maintainer_instruction, 2, 0, 29),
  [1160] = {.entry = {.count = 1, .reusable = true}}, SHIFT(357),
  [1162] = {.entry = {.count = 1, .reusable = false}}, SHIFT(328),
  [1164] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_healthcheck_instruction, 3, 0, 27),
  [1166] = {.entry = {.count = 1, .reusable = true}}, SHIFT(92),
  [1168] = {.entry = {.count = 1, .reusable = false}}, SHIFT(38),
  [1170] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_entrypoint_instruction, 2, 0, 17),
  [1172] = {.entry = {.count = 1, .reusable = false}}, SHIFT(355),
  [1174] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_instruction, 1, 0, 0),
  [1176] = {.entry = {.count = 1, .reusable = false}}, SHIFT(379),
  [1178] = {.entry = {.count = 1, .reusable = true}}, SHIFT(234),
  [1180] = {.entry = {.count = 1, .reusable = false}}, SHIFT(66),
  [1182] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_from_instruction, 4, 0, 45),
  [1184] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_cmd_instruction, 2, 0, 9),
  [1186] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_onbuild_instruction, 2, 0, 24),
  [1188] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [1190] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [1192] = {.entry = {.count = 1, .reusable = true}}, SHIFT(266),
  [1194] = {.entry = {.count = 1, .reusable = false}}, SHIFT(171),
  [1196] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_user_instruction, 4, 0, 58),
  [1198] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_cross_build_instruction, 2, 0, 30),
  [1200] = {.entry = {.count = 1, .reusable = true}}, SHIFT(201),
  [1202] = {.entry = {.count = 1, .reusable = false}}, SHIFT(269),
  [1204] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_instruction, 4, 0, 59),
  [1206] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_arg_instruction, 4, 0, 60),
  [1208] = {.entry = {.count = 1, .reusable = true}}, SHIFT(206),
  [1210] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [1212] = {.entry = {.count = 1, .reusable = true}}, SHIFT(276),
  [1214] = {.entry = {.count = 1, .reusable = true}}, SHIFT(281),
  [1216] = {.entry = {.count = 1, .reusable = true}}, SHIFT(340),
  [1218] = {.entry = {.count = 1, .reusable = false}}, SHIFT(427),
  [1220] = {.entry = {.count = 1, .reusable = true}}, SHIFT(178),
  [1222] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_stopsignal_instruction, 2, 0, 26),
  [1224] = {.entry = {.count = 1, .reusable = true}}, SHIFT(180),
  [1226] = {.entry = {.count = 1, .reusable = true}}, SHIFT(316),
  [1228] = {.entry = {.count = 1, .reusable = true}}, SHIFT(286),
  [1230] = {.entry = {.count = 1, .reusable = true}}, SHIFT(158),
  [1232] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [1234] = {.entry = {.count = 1, .reusable = true}}, SHIFT(225),
  [1236] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_from_instruction, 5, 0, 64),
  [1238] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [1240] = {.entry = {.count = 1, .reusable = true}}, SHIFT(361),
  [1242] = {.entry = {.count = 1, .reusable = true}}, SHIFT(150),
  [1244] = {.entry = {.count = 1, .reusable = true}}, SHIFT(233),
  [1246] = {.entry = {.count = 1, .reusable = false}}, SHIFT(367),
  [1248] = {.entry = {.count = 1, .reusable = true}}, SHIFT(368),
  [1250] = {.entry = {.count = 1, .reusable = false}}, SHIFT(376),
  [1252] = {.entry = {.count = 1, .reusable = true}}, SHIFT(377),
  [1254] = {.entry = {.count = 1, .reusable = false}}, SHIFT(380),
  [1256] = {.entry = {.count = 1, .reusable = false}}, SHIFT(384),
  [1258] = {.entry = {.count = 1, .reusable = false}}, SHIFT(387),
  [1260] = {.entry = {.count = 1, .reusable = false}}, SHIFT(390),
  [1262] = {.entry = {.count = 1, .reusable = false}}, SHIFT(393),
  [1264] = {.entry = {.count = 1, .reusable = false}}, SHIFT(396),
  [1266] = {.entry = {.count = 1, .reusable = false}}, SHIFT(398),
  [1268] = {.entry = {.count = 1, .reusable = false}}, SHIFT(400),
  [1270] = {.entry = {.count = 1, .reusable = false}}, SHIFT(402),
  [1272] = {.entry = {.count = 1, .reusable = false}}, SHIFT(404),
  [1274] = {.entry = {.count = 1, .reusable = false}}, SHIFT(406),
  [1276] = {.entry = {.count = 1, .reusable = false}}, SHIFT(407),
  [1278] = {.entry = {.count = 1, .reusable = true}}, SHIFT(411),
  [1280] = {.entry = {.count = 1, .reusable = true}}, SHIFT(344),
  [1282] = {.entry = {.count = 1, .reusable = true}}, SHIFT(137),
};

enum ts_external_scanner_symbol_identifiers {
  ts_external_token_heredoc_marker = 0,
  ts_external_token_heredoc_line = 1,
  ts_external_token_heredoc_end = 2,
  ts_external_token_heredoc_nl = 3,
  ts_external_token_error_sentinel = 4,
};

static const TSSymbol ts_external_scanner_symbol_map[EXTERNAL_TOKEN_COUNT] = {
  [ts_external_token_heredoc_marker] = sym_heredoc_marker,
  [ts_external_token_heredoc_line] = sym_heredoc_line,
  [ts_external_token_heredoc_end] = sym_heredoc_end,
  [ts_external_token_heredoc_nl] = sym_heredoc_nl,
  [ts_external_token_error_sentinel] = sym_error_sentinel,
};

static const bool ts_external_scanner_states[6][EXTERNAL_TOKEN_COUNT] = {
  [1] = {
    [ts_external_token_heredoc_marker] = true,
    [ts_external_token_heredoc_line] = true,
    [ts_external_token_heredoc_end] = true,
    [ts_external_token_heredoc_nl] = true,
    [ts_external_token_error_sentinel] = true,
  },
  [2] = {
    [ts_external_token_heredoc_marker] = true,
  },
  [3] = {
    [ts_external_token_heredoc_marker] = true,
    [ts_external_token_heredoc_nl] = true,
  },
  [4] = {
    [ts_external_token_heredoc_nl] = true,
  },
  [5] = {
    [ts_external_token_heredoc_line] = true,
    [ts_external_token_heredoc_end] = true,
  },
};

#ifdef __cplusplus
extern "C" {
#endif
void *tree_sitter_dockerfile_external_scanner_create(void);
void tree_sitter_dockerfile_external_scanner_destroy(void *);
bool tree_sitter_dockerfile_external_scanner_scan(void *, TSLexer *, const bool *);
unsigned tree_sitter_dockerfile_external_scanner_serialize(void *, char *);
void tree_sitter_dockerfile_external_scanner_deserialize(void *, const char *, unsigned);

#ifdef TREE_SITTER_HIDE_SYMBOLS
#define TS_PUBLIC
#elif defined(_WIN32)
#define TS_PUBLIC __declspec(dllexport)
#else
#define TS_PUBLIC __attribute__((visibility("default")))
#endif

TS_PUBLIC const TSLanguage *tree_sitter_dockerfile(void) {
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
    .external_scanner = {
      &ts_external_scanner_states[0][0],
      ts_external_scanner_symbol_map,
      tree_sitter_dockerfile_external_scanner_create,
      tree_sitter_dockerfile_external_scanner_destroy,
      tree_sitter_dockerfile_external_scanner_scan,
      tree_sitter_dockerfile_external_scanner_serialize,
      tree_sitter_dockerfile_external_scanner_deserialize,
    },
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
