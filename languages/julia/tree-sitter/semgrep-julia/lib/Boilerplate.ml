(**
   Boilerplate to be used as a template when mapping the julia CST
   to another type of tree.
*)

module R = Tree_sitter_run.Raw_tree

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (tok : Tree_sitter_run.Token.t) =
  R.Token tok

let blank (env : env) () =
  R.Tuple []

let map_pat_f66a72a (env : env) (tok : CST.pat_f66a72a) =
  (* pattern #= *) token env tok

let map_end_str (env : env) (tok : CST.end_str) =
  (* end_str *) token env tok

let map_pat_509ec78 (env : env) (tok : CST.pat_509ec78) =
  (* pattern \r?\n *) token env tok

let map_pipe_right_operator (env : env) (tok : CST.pipe_right_operator) =
  (* pipe_right_operator *) token env tok

let map_imm_tok_colon (env : env) (tok : CST.imm_tok_colon) =
  (* ":" *) token env tok

let map_content_str_1 (env : env) (tok : CST.content_str_1) =
  (* content_str_1 *) token env tok

let map_content_str_1_raw (env : env) (tok : CST.content_str_1_raw) =
  (* content_str_1_raw *) token env tok

let map_imm_tok_dot (env : env) (tok : CST.imm_tok_dot) =
  (* "." *) token env tok

let map_content_cmd_3 (env : env) (tok : CST.content_cmd_3) =
  (* content_cmd_3 *) token env tok

let map_tok_choice_dot_choice_pluseq (env : env) (tok : CST.tok_choice_dot_choice_pluseq) =
  (* tok_choice_dot_choice_pluseq *) token env tok

let map_character_literal (env : env) (tok : CST.character_literal) =
  (* character_literal *) token env tok

let map_pair_operator (env : env) (tok : CST.pair_operator) =
  (* pair_operator *) token env tok

let map_power_operator (env : env) (tok : CST.power_operator) =
  (* power_operator *) token env tok

let map_arrow_operator (env : env) (tok : CST.arrow_operator) =
  (* arrow_operator *) token env tok

let map_ellipsis_operator (env : env) (tok : CST.ellipsis_operator) =
  (* ellipsis_operator *) token env tok

let map_tok_dot_pat_1c66fba_choice_pat_55159f5 (env : env) (tok : CST.tok_dot_pat_1c66fba_choice_pat_55159f5) =
  (* tok_dot_pat_1c66fba_choice_pat_55159f5 *) token env tok

let map_bitshift_operator (env : env) (tok : CST.bitshift_operator) =
  (* bitshift_operator *) token env tok

let map_content_str_3 (env : env) (tok : CST.content_str_3) =
  (* content_str_3 *) token env tok

let map_end_cmd (env : env) (tok : CST.end_cmd) =
  (* end_cmd *) token env tok

let map_immediate_bracket (env : env) (tok : CST.immediate_bracket) =
  (* immediate_bracket *) token env tok

let map_word_identifier (env : env) (tok : CST.word_identifier) =
  (* pattern "[_\\p{XID_Start}\\p{Emoji}\194\176\226\136\128-\226\136\135\226\136\142-\226\136\145\226\136\171-\226\136\179&&[^0-9#*]][^\"'`\\s\\.\\-\\[\\]#$,:;@~(){}+==*=/=//=\\\\=^=%=<<=>>=>>>=|=&=\226\136\146=\195\183=\226\138\187=\226\137\148\226\169\180\226\137\149<><>\226\134\144\226\134\146\226\134\148\226\134\154\226\134\155\226\134\158\226\134\160\226\134\162\226\134\163\226\134\166\226\134\164\226\134\174\226\135\142\226\135\141\226\135\143\226\135\144\226\135\146\226\135\148\226\135\180\226\135\182\226\135\183\226\135\184\226\135\185\226\135\186\226\135\187\226\135\188\226\135\189\226\135\190\226\135\191\226\159\181\226\159\182\226\159\183\226\159\185\226\159\186\226\159\187\226\159\188\226\159\189\226\159\190\226\159\191\226\164\128\226\164\129\226\164\130\226\164\131\226\164\132\226\164\133\226\164\134\226\164\135\226\164\140\226\164\141\226\164\142\226\164\143\226\164\144\226\164\145\226\164\148\226\164\149\226\164\150\226\164\151\226\164\152\226\164\157\226\164\158\226\164\159\226\164\160\226\165\132\226\165\133\226\165\134\226\165\135\226\165\136\226\165\138\226\165\139\226\165\142\226\165\144\226\165\146\226\165\147\226\165\150\226\165\151\226\165\154\226\165\155\226\165\158\226\165\159\226\165\162\226\165\164\226\165\166\226\165\167\226\165\168\226\165\169\226\165\170\226\165\171\226\165\172\226\165\173\226\165\176\226\167\180\226\172\177\226\172\176\226\172\178\226\172\179\226\172\180\226\172\181\226\172\182\226\172\183\226\172\184\226\172\185\226\172\186\226\172\187\226\172\188\226\172\189\226\172\190\226\172\191\226\173\128\226\173\129\226\173\130\226\173\131\226\165\183\226\173\132\226\165\186\226\173\135\226\173\136\226\173\137\226\173\138\226\173\139\226\173\140\239\191\169\239\191\171\226\135\156\226\135\157\226\134\156\226\134\157\226\134\169\226\134\170\226\134\171\226\134\172\226\134\188\226\134\189\226\135\128\226\135\129\226\135\132\226\135\134\226\135\135\226\135\137\226\135\139\226\135\140\226\135\154\226\135\155\226\135\160\226\135\162\226\134\183\226\134\182\226\134\186\226\134\187><>=<=========\226\137\165\226\137\164\226\137\161\226\137\160\226\137\162\226\136\136\226\136\137\226\136\139\226\136\140\226\138\134\226\138\136\226\138\130\226\138\132\226\138\138\226\136\157\226\136\138\226\136\141\226\136\165\226\136\166\226\136\183\226\136\186\226\136\187\226\136\189\226\136\190\226\137\129\226\137\131\226\137\130\226\137\132\226\137\133\226\137\134\226\137\135\226\137\136\226\137\137\226\137\138\226\137\139\226\137\140\226\137\141\226\137\142\226\137\144\226\137\145\226\137\146\226\137\147\226\137\150\226\137\151\226\137\152\226\137\153\226\137\154\226\137\155\226\137\156\226\137\157\226\137\158\226\137\159\226\137\163\226\137\166\226\137\167\226\137\168\226\137\169\226\137\170\226\137\171\226\137\172\226\137\173\226\137\174\226\137\175\226\137\176\226\137\177\226\137\178\226\137\179\226\137\180\226\137\181\226\137\182\226\137\183\226\137\184\226\137\185\226\137\186\226\137\187\226\137\188\226\137\189\226\137\190\226\137\191\226\138\128\226\138\129\226\138\131\226\138\133\226\138\135\226\138\137\226\138\139\226\138\143\226\138\144\226\138\145\226\138\146\226\138\156\226\138\169\226\138\172\226\138\174\226\138\176\226\138\177\226\138\178\226\138\179\226\138\180\226\138\181\226\138\182\226\138\183\226\139\141\226\139\144\226\139\145\226\139\149\226\139\150\226\139\151\226\139\152\226\139\153\226\139\154\226\139\155\226\139\156\226\139\157\226\139\158\226\139\159\226\139\160\226\139\161\226\139\162\226\139\163\226\139\164\226\139\165\226\139\166\226\139\167\226\139\168\226\139\169\226\139\170\226\139\171\226\139\172\226\139\173\226\139\178\226\139\179\226\139\180\226\139\181\226\139\182\226\139\183\226\139\184\226\139\185\226\139\186\226\139\187\226\139\188\226\139\189\226\139\190\226\139\191\226\159\136\226\159\137\226\159\146\226\166\183\226\167\128\226\167\129\226\167\161\226\167\163\226\167\164\226\167\165\226\169\166\226\169\167\226\169\170\226\169\171\226\169\172\226\169\173\226\169\174\226\169\175\226\169\176\226\169\177\226\169\178\226\169\179\226\169\181\226\169\182\226\169\183\226\169\184\226\169\185\226\169\186\226\169\187\226\169\188\226\169\189\226\169\190\226\169\191\226\170\128\226\170\129\226\170\130\226\170\131\226\170\132\226\170\133\226\170\134\226\170\135\226\170\136\226\170\137\226\170\138\226\170\139\226\170\140\226\170\141\226\170\142\226\170\143\226\170\144\226\170\145\226\170\146\226\170\147\226\170\148\226\170\149\226\170\150\226\170\151\226\170\152\226\170\153\226\170\154\226\170\155\226\170\156\226\170\157\226\170\158\226\170\159\226\170\160\226\170\161\226\170\162\226\170\163\226\170\164\226\170\165\226\170\166\226\170\167\226\170\168\226\170\169\226\170\170\226\170\171\226\170\172\226\170\173\226\170\174\226\170\175\226\170\176\226\170\177\226\170\178\226\170\179\226\170\180\226\170\181\226\170\182\226\170\183\226\170\184\226\170\185\226\170\186\226\170\187\226\170\188\226\170\189\226\170\190\226\170\191\226\171\128\226\171\129\226\171\130\226\171\131\226\171\132\226\171\133\226\171\134\226\171\135\226\171\136\226\171\137\226\171\138\226\171\139\226\171\140\226\171\141\226\171\142\226\171\143\226\171\144\226\171\145\226\171\146\226\171\147\226\171\148\226\171\149\226\171\150\226\171\151\226\171\152\226\171\153\226\171\183\226\171\184\226\171\185\226\171\186\226\138\162\226\138\163\226\159\130\226\171\170\226\171\171\226\128\166\226\129\157\226\139\174\226\139\177\226\139\176\226\139\175++|\226\136\146\194\166\226\138\149\226\138\150\226\138\158\226\138\159\226\136\170\226\136\168\226\138\148\226\136\148\226\136\184\226\137\143\226\138\142\226\138\187\226\138\189\226\139\142\226\139\147\226\159\135\226\167\186\226\167\187\226\168\136\226\168\162\226\168\163\226\168\164\226\168\165\226\168\166\226\168\167\226\168\168\226\168\169\226\168\170\226\168\171\226\168\172\226\168\173\226\168\174\226\168\185\226\168\186\226\169\129\226\169\130\226\169\133\226\169\138\226\169\140\226\169\143\226\169\144\226\169\146\226\169\148\226\169\150\226\169\151\226\169\155\226\169\157\226\169\161\226\169\162\226\169\163*/%&\\\\\226\140\191\195\183\194\183\194\183\226\139\133\226\136\152\195\151\226\136\169\226\136\167\226\138\151\226\138\152\226\138\153\226\138\154\226\138\155\226\138\160\226\138\161\226\138\147\226\136\151\226\136\153\226\136\164\226\133\139\226\137\128\226\138\188\226\139\132\226\139\134\226\139\135\226\139\137\226\139\138\226\139\139\226\139\140\226\139\143\226\139\146\226\159\145\226\166\184\226\166\188\226\166\190\226\166\191\226\167\182\226\167\183\226\168\135\226\168\176\226\168\177\226\168\178\226\168\179\226\168\180\226\168\181\226\168\182\226\168\183\226\168\184\226\168\187\226\168\188\226\168\189\226\169\128\226\169\131\226\169\132\226\169\139\226\169\141\226\169\142\226\169\145\226\169\147\226\169\149\226\169\152\226\169\154\226\169\156\226\169\158\226\169\159\226\169\160\226\171\155\226\138\141\226\150\183\226\168\157\226\159\149\226\159\150\226\159\151\226\168\159<<>>>>>^\226\134\145\226\134\147\226\135\181\226\159\176\226\159\177\226\164\136\226\164\137\226\164\138\226\164\139\226\164\146\226\164\147\226\165\137\226\165\140\226\165\141\226\165\143\226\165\145\226\165\148\226\165\149\226\165\152\226\165\153\226\165\156\226\165\157\226\165\160\226\165\161\226\165\163\226\165\165\226\165\174\226\165\175\239\191\170\239\191\172\194\172\226\136\154\226\136\155\226\136\156+\194\177\226\136\147]*" *) token env tok

let map_rational_operator (env : env) (tok : CST.rational_operator) =
  (* rational_operator *) token env tok

let map_tok_choice_0x_pat_8c03647_choice_dot_choice_pat_8c03647_pat_dd04cb4 (env : env) (tok : CST.tok_choice_0x_pat_8c03647_choice_dot_choice_pat_8c03647_pat_dd04cb4) =
  (* tok_choice_0x_pat_8c03647_choice_dot_choice_pat_8c03647_pat_dd04cb4 *) token env tok

let map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let map_content_cmd_1_raw (env : env) (tok : CST.content_cmd_1_raw) =
  (* content_cmd_1_raw *) token env tok

let map_comparison_operator (env : env) (tok : CST.comparison_operator) =
  (* comparison_operator *) token env tok

let map_content_cmd_3_raw (env : env) (tok : CST.content_cmd_3_raw) =
  (* content_cmd_3_raw *) token env tok

let map_tilde_operator (env : env) (tok : CST.tilde_operator) =
  (* tilde_operator *) token env tok

let map_pipe_left_operator (env : env) (tok : CST.pipe_left_operator) =
  (* pipe_left_operator *) token env tok

let map_imm_tok_dot_choice_pat_1c66fba_choice_pat_55159f5 (env : env) (tok : CST.imm_tok_dot_choice_pat_1c66fba_choice_pat_55159f5) =
  (* imm_tok_dot_choice_pat_1c66fba_choice_pat_55159f5 *) token env tok

let map_content_str_3_raw (env : env) (tok : CST.content_str_3_raw) =
  (* content_str_3_raw *) token env tok

let map_tok_0x_pat_8c03647 (env : env) (tok : CST.tok_0x_pat_8c03647) =
  (* tok_0x_pat_8c03647 *) token env tok

let map_semgrep_extended_metavariable (env : env) (tok : CST.semgrep_extended_metavariable) =
  (* pattern \$[A-Z_][a-zA-Z_0-9]* *) token env tok

let map_boolean_literal (env : env) (x : CST.boolean_literal) =
  (match x with
  | `True tok -> R.Case ("True",
      (* "true" *) token env tok
    )
  | `False tok -> R.Case ("False",
      (* "false" *) token env tok
    )
  )

let map_immediate_paren (env : env) (tok : CST.immediate_paren) =
  (* immediate_paren *) token env tok

let map_syntactic_operator (env : env) (x : CST.syntactic_operator) =
  (match x with
  | `DOLLAR tok -> R.Case ("DOLLAR",
      (* "$" *) token env tok
    )
  | `DOT tok -> R.Case ("DOT",
      (* "." *) token env tok
    )
  | `DOTDOTDOT tok -> R.Case ("DOTDOTDOT",
      (* "..." *) token env tok
    )
  | `DASHGT tok -> R.Case ("DASHGT",
      (* "->" *) token env tok
    )
  | `QMARK tok -> R.Case ("QMARK",
      (* "?" *) token env tok
    )
  )

let map_imm_tok_squot (env : env) (tok : CST.imm_tok_squot) =
  (* "'" *) token env tok

let map_unary_operator (env : env) (tok : CST.unary_operator) =
  (* unary_operator *) token env tok

let map_unary_plus_operator (env : env) (tok : CST.unary_plus_operator) =
  (* unary_plus_operator *) token env tok

let map_pat_4fd4a56 (env : env) (tok : CST.pat_4fd4a56) =
  (* pattern .* *) token env tok

let map_tok_pat_1c66fba_pat_55159f5 (env : env) (tok : CST.tok_pat_1c66fba_pat_55159f5) =
  (* tok_pat_1c66fba_pat_55159f5 *) token env tok

let map_plus_operator (env : env) (tok : CST.plus_operator) =
  (* plus_operator *) token env tok

let map_tok_0b_pat_f202b54 (env : env) (tok : CST.tok_0b_pat_f202b54) =
  (* tok_0b_pat_f202b54 *) token env tok

let map_pat_1c66fba (env : env) (tok : CST.pat_1c66fba) =
  (* pattern [0-9]([0-9]|_[0-9])* *) token env tok

let map_type_order_operator (env : env) (tok : CST.type_order_operator) =
  (* type_order_operator *) token env tok

let map_block_comment_rest (env : env) (tok : CST.block_comment_rest) =
  (* block_comment_rest *) token env tok

let map_tok_rep1_dot (env : env) (tok : CST.tok_rep1_dot) =
  (* tok_rep1_dot *) token env tok

let map_imm_tok_semi (env : env) (tok : CST.imm_tok_semi) =
  (* ";" *) token env tok

let map_immediate_command_start (env : env) (tok : CST.immediate_command_start) =
  (* immediate_command_start *) token env tok

let map_times_operator (env : env) (tok : CST.times_operator) =
  (* times_operator *) token env tok

let map_lazy_and_operator (env : env) (tok : CST.lazy_and_operator) =
  (* lazy_and_operator *) token env tok

let map_content_cmd_1 (env : env) (tok : CST.content_cmd_1) =
  (* content_cmd_1 *) token env tok

let map_lazy_or_operator (env : env) (tok : CST.lazy_or_operator) =
  (* lazy_or_operator *) token env tok

let map_pat_01abfc7 (env : env) (tok : CST.pat_01abfc7) =
  (* pattern # *) token env tok

let map_immediate_string_start (env : env) (tok : CST.immediate_string_start) =
  (* immediate_string_start *) token env tok

let map_tok_0o_pat_659066a (env : env) (tok : CST.tok_0o_pat_659066a) =
  (* tok_0o_pat_659066a *) token env tok

let map_immediate_brace (env : env) (tok : CST.immediate_brace) =
  (* immediate_brace *) token env tok

let map_imm_tok_choice_bare (env : env) (tok : CST.imm_tok_choice_bare) =
  (* imm_tok_choice_bare *) token env tok

let map_assignment_operator (env : env) (x : CST.assignment_operator) =
  (match x with
  | `COLONEQ tok -> R.Case ("COLONEQ",
      (* ":=" *) token env tok
    )
  | `DOLLAREQ tok -> R.Case ("DOLLAREQ",
      (* "$=" *) token env tok
    )
  | `DOTEQ tok -> R.Case ("DOTEQ",
      (* ".=" *) token env tok
    )
  | `Tok_choice_dot_choice_pluseq x -> R.Case ("Tok_choice_dot_choice_pluseq",
      map_tok_choice_dot_choice_pluseq env x
    )
  )

let map_identifier (env : env) (x : CST.identifier) =
  (match x with
  | `Word_id tok -> R.Case ("Word_id",
      (* pattern "[_\\p{XID_Start}\\p{Emoji}\194\176\226\136\128-\226\136\135\226\136\142-\226\136\145\226\136\171-\226\136\179&&[^0-9#*]][^\"'`\\s\\.\\-\\[\\]#$,:;@~(){}+==*=/=//=\\\\=^=%=<<=>>=>>>=|=&=\226\136\146=\195\183=\226\138\187=\226\137\148\226\169\180\226\137\149<><>\226\134\144\226\134\146\226\134\148\226\134\154\226\134\155\226\134\158\226\134\160\226\134\162\226\134\163\226\134\166\226\134\164\226\134\174\226\135\142\226\135\141\226\135\143\226\135\144\226\135\146\226\135\148\226\135\180\226\135\182\226\135\183\226\135\184\226\135\185\226\135\186\226\135\187\226\135\188\226\135\189\226\135\190\226\135\191\226\159\181\226\159\182\226\159\183\226\159\185\226\159\186\226\159\187\226\159\188\226\159\189\226\159\190\226\159\191\226\164\128\226\164\129\226\164\130\226\164\131\226\164\132\226\164\133\226\164\134\226\164\135\226\164\140\226\164\141\226\164\142\226\164\143\226\164\144\226\164\145\226\164\148\226\164\149\226\164\150\226\164\151\226\164\152\226\164\157\226\164\158\226\164\159\226\164\160\226\165\132\226\165\133\226\165\134\226\165\135\226\165\136\226\165\138\226\165\139\226\165\142\226\165\144\226\165\146\226\165\147\226\165\150\226\165\151\226\165\154\226\165\155\226\165\158\226\165\159\226\165\162\226\165\164\226\165\166\226\165\167\226\165\168\226\165\169\226\165\170\226\165\171\226\165\172\226\165\173\226\165\176\226\167\180\226\172\177\226\172\176\226\172\178\226\172\179\226\172\180\226\172\181\226\172\182\226\172\183\226\172\184\226\172\185\226\172\186\226\172\187\226\172\188\226\172\189\226\172\190\226\172\191\226\173\128\226\173\129\226\173\130\226\173\131\226\165\183\226\173\132\226\165\186\226\173\135\226\173\136\226\173\137\226\173\138\226\173\139\226\173\140\239\191\169\239\191\171\226\135\156\226\135\157\226\134\156\226\134\157\226\134\169\226\134\170\226\134\171\226\134\172\226\134\188\226\134\189\226\135\128\226\135\129\226\135\132\226\135\134\226\135\135\226\135\137\226\135\139\226\135\140\226\135\154\226\135\155\226\135\160\226\135\162\226\134\183\226\134\182\226\134\186\226\134\187><>=<=========\226\137\165\226\137\164\226\137\161\226\137\160\226\137\162\226\136\136\226\136\137\226\136\139\226\136\140\226\138\134\226\138\136\226\138\130\226\138\132\226\138\138\226\136\157\226\136\138\226\136\141\226\136\165\226\136\166\226\136\183\226\136\186\226\136\187\226\136\189\226\136\190\226\137\129\226\137\131\226\137\130\226\137\132\226\137\133\226\137\134\226\137\135\226\137\136\226\137\137\226\137\138\226\137\139\226\137\140\226\137\141\226\137\142\226\137\144\226\137\145\226\137\146\226\137\147\226\137\150\226\137\151\226\137\152\226\137\153\226\137\154\226\137\155\226\137\156\226\137\157\226\137\158\226\137\159\226\137\163\226\137\166\226\137\167\226\137\168\226\137\169\226\137\170\226\137\171\226\137\172\226\137\173\226\137\174\226\137\175\226\137\176\226\137\177\226\137\178\226\137\179\226\137\180\226\137\181\226\137\182\226\137\183\226\137\184\226\137\185\226\137\186\226\137\187\226\137\188\226\137\189\226\137\190\226\137\191\226\138\128\226\138\129\226\138\131\226\138\133\226\138\135\226\138\137\226\138\139\226\138\143\226\138\144\226\138\145\226\138\146\226\138\156\226\138\169\226\138\172\226\138\174\226\138\176\226\138\177\226\138\178\226\138\179\226\138\180\226\138\181\226\138\182\226\138\183\226\139\141\226\139\144\226\139\145\226\139\149\226\139\150\226\139\151\226\139\152\226\139\153\226\139\154\226\139\155\226\139\156\226\139\157\226\139\158\226\139\159\226\139\160\226\139\161\226\139\162\226\139\163\226\139\164\226\139\165\226\139\166\226\139\167\226\139\168\226\139\169\226\139\170\226\139\171\226\139\172\226\139\173\226\139\178\226\139\179\226\139\180\226\139\181\226\139\182\226\139\183\226\139\184\226\139\185\226\139\186\226\139\187\226\139\188\226\139\189\226\139\190\226\139\191\226\159\136\226\159\137\226\159\146\226\166\183\226\167\128\226\167\129\226\167\161\226\167\163\226\167\164\226\167\165\226\169\166\226\169\167\226\169\170\226\169\171\226\169\172\226\169\173\226\169\174\226\169\175\226\169\176\226\169\177\226\169\178\226\169\179\226\169\181\226\169\182\226\169\183\226\169\184\226\169\185\226\169\186\226\169\187\226\169\188\226\169\189\226\169\190\226\169\191\226\170\128\226\170\129\226\170\130\226\170\131\226\170\132\226\170\133\226\170\134\226\170\135\226\170\136\226\170\137\226\170\138\226\170\139\226\170\140\226\170\141\226\170\142\226\170\143\226\170\144\226\170\145\226\170\146\226\170\147\226\170\148\226\170\149\226\170\150\226\170\151\226\170\152\226\170\153\226\170\154\226\170\155\226\170\156\226\170\157\226\170\158\226\170\159\226\170\160\226\170\161\226\170\162\226\170\163\226\170\164\226\170\165\226\170\166\226\170\167\226\170\168\226\170\169\226\170\170\226\170\171\226\170\172\226\170\173\226\170\174\226\170\175\226\170\176\226\170\177\226\170\178\226\170\179\226\170\180\226\170\181\226\170\182\226\170\183\226\170\184\226\170\185\226\170\186\226\170\187\226\170\188\226\170\189\226\170\190\226\170\191\226\171\128\226\171\129\226\171\130\226\171\131\226\171\132\226\171\133\226\171\134\226\171\135\226\171\136\226\171\137\226\171\138\226\171\139\226\171\140\226\171\141\226\171\142\226\171\143\226\171\144\226\171\145\226\171\146\226\171\147\226\171\148\226\171\149\226\171\150\226\171\151\226\171\152\226\171\153\226\171\183\226\171\184\226\171\185\226\171\186\226\138\162\226\138\163\226\159\130\226\171\170\226\171\171\226\128\166\226\129\157\226\139\174\226\139\177\226\139\176\226\139\175++|\226\136\146\194\166\226\138\149\226\138\150\226\138\158\226\138\159\226\136\170\226\136\168\226\138\148\226\136\148\226\136\184\226\137\143\226\138\142\226\138\187\226\138\189\226\139\142\226\139\147\226\159\135\226\167\186\226\167\187\226\168\136\226\168\162\226\168\163\226\168\164\226\168\165\226\168\166\226\168\167\226\168\168\226\168\169\226\168\170\226\168\171\226\168\172\226\168\173\226\168\174\226\168\185\226\168\186\226\169\129\226\169\130\226\169\133\226\169\138\226\169\140\226\169\143\226\169\144\226\169\146\226\169\148\226\169\150\226\169\151\226\169\155\226\169\157\226\169\161\226\169\162\226\169\163*/%&\\\\\226\140\191\195\183\194\183\194\183\226\139\133\226\136\152\195\151\226\136\169\226\136\167\226\138\151\226\138\152\226\138\153\226\138\154\226\138\155\226\138\160\226\138\161\226\138\147\226\136\151\226\136\153\226\136\164\226\133\139\226\137\128\226\138\188\226\139\132\226\139\134\226\139\135\226\139\137\226\139\138\226\139\139\226\139\140\226\139\143\226\139\146\226\159\145\226\166\184\226\166\188\226\166\190\226\166\191\226\167\182\226\167\183\226\168\135\226\168\176\226\168\177\226\168\178\226\168\179\226\168\180\226\168\181\226\168\182\226\168\183\226\168\184\226\168\187\226\168\188\226\168\189\226\169\128\226\169\131\226\169\132\226\169\139\226\169\141\226\169\142\226\169\145\226\169\147\226\169\149\226\169\152\226\169\154\226\169\156\226\169\158\226\169\159\226\169\160\226\171\155\226\138\141\226\150\183\226\168\157\226\159\149\226\159\150\226\159\151\226\168\159<<>>>>>^\226\134\145\226\134\147\226\135\181\226\159\176\226\159\177\226\164\136\226\164\137\226\164\138\226\164\139\226\164\146\226\164\147\226\165\137\226\165\140\226\165\141\226\165\143\226\165\145\226\165\148\226\165\149\226\165\152\226\165\153\226\165\156\226\165\157\226\165\160\226\165\161\226\165\163\226\165\165\226\165\174\226\165\175\239\191\170\239\191\172\194\172\226\136\154\226\136\155\226\136\156+\194\177\226\136\147]*" *) token env tok
    )
  | `Public tok -> R.Case ("Public",
      (* "public" *) token env tok
    )
  | `Outer tok -> R.Case ("Outer",
      (* "outer" *) token env tok
    )
  | `Semg_exte_meta tok -> R.Case ("Semg_exte_meta",
      (* pattern \$[A-Z_][a-zA-Z_0-9]* *) token env tok
    )
  )

let map_float_literal (env : env) (x : CST.float_literal) =
  (match x with
  | `Tok_dot_pat_1c66fba_choice_pat_55159f5 x -> R.Case ("Tok_dot_pat_1c66fba_choice_pat_55159f5",
      map_tok_dot_pat_1c66fba_choice_pat_55159f5 env x
    )
  | `Pat_1c66fba_imm_tok_dot_choice_pat_1c66fba_choice_pat_55159f5 (v1, v2) -> R.Case ("Pat_1c66fba_imm_tok_dot_choice_pat_1c66fba_choice_pat_55159f5",
      let v1 = map_pat_1c66fba env v1 in
      let v2 =
        map_imm_tok_dot_choice_pat_1c66fba_choice_pat_55159f5 env v2
      in
      R.Tuple [v1; v2]
    )
  | `Tok_pat_1c66fba_pat_55159f5 x -> R.Case ("Tok_pat_1c66fba_pat_55159f5",
      map_tok_pat_1c66fba_pat_55159f5 env x
    )
  | `Tok_choice_0x_pat_8c03647_choice_dot_choice_pat_8c03647_pat_dd04cb4 x -> R.Case ("Tok_choice_0x_pat_8c03647_choice_dot_choice_pat_8c03647_pat_dd04cb4",
      map_tok_choice_0x_pat_8c03647_choice_dot_choice_pat_8c03647_pat_dd04cb4 env x
    )
  )

let map_semicolon (env : env) ((v1, v2) : CST.semicolon) =
  let v1 = (* ";" *) token env v1 in
  let v2 =
    R.List (List.map (fun x ->
      map_imm_tok_semi env x
    ) v2)
  in
  R.Tuple [v1; v2]

let map_operator (env : env) (x : CST.operator) =
  (match x with
  | `Pair_op tok -> R.Case ("Pair_op",
      (* pair_operator *) token env tok
    )
  | `Arrow_op tok -> R.Case ("Arrow_op",
      (* arrow_operator *) token env tok
    )
  | `Comp_op tok -> R.Case ("Comp_op",
      (* comparison_operator *) token env tok
    )
  | `Pipe_left_op tok -> R.Case ("Pipe_left_op",
      (* pipe_left_operator *) token env tok
    )
  | `Pipe_right_op tok -> R.Case ("Pipe_right_op",
      (* pipe_right_operator *) token env tok
    )
  | `Ellips_op tok -> R.Case ("Ellips_op",
      (* ellipsis_operator *) token env tok
    )
  | `COLON tok -> R.Case ("COLON",
      (* ":" *) token env tok
    )
  | `Plus_op tok -> R.Case ("Plus_op",
      (* plus_operator *) token env tok
    )
  | `Times_op tok -> R.Case ("Times_op",
      (* times_operator *) token env tok
    )
  | `Rati_op tok -> R.Case ("Rati_op",
      (* rational_operator *) token env tok
    )
  | `Bits_op tok -> R.Case ("Bits_op",
      (* bitshift_operator *) token env tok
    )
  | `Power_op tok -> R.Case ("Power_op",
      (* power_operator *) token env tok
    )
  | `Tilde_op tok -> R.Case ("Tilde_op",
      (* tilde_operator *) token env tok
    )
  | `Type_order_op tok -> R.Case ("Type_order_op",
      (* type_order_operator *) token env tok
    )
  | `Un_op tok -> R.Case ("Un_op",
      (* unary_operator *) token env tok
    )
  | `Un_plus_op tok -> R.Case ("Un_plus_op",
      (* unary_plus_operator *) token env tok
    )
  )

let map_integer_literal (env : env) (x : CST.integer_literal) =
  (match x with
  | `Tok_0b_pat_f202b54 x -> R.Case ("Tok_0b_pat_f202b54",
      map_tok_0b_pat_f202b54 env x
    )
  | `Tok_0o_pat_659066a x -> R.Case ("Tok_0o_pat_659066a",
      map_tok_0o_pat_659066a env x
    )
  | `Tok_0x_pat_8c03647 x -> R.Case ("Tok_0x_pat_8c03647",
      map_tok_0x_pat_8c03647 env x
    )
  | `Pat_1c66fba x -> R.Case ("Pat_1c66fba",
      map_pat_1c66fba env x
    )
  )

let map_terminator (env : env) (x : CST.terminator) =
  (match x with
  | `Pat_509ec78 x -> R.Case ("Pat_509ec78",
      map_pat_509ec78 env x
    )
  | `Semi x -> R.Case ("Semi",
      map_semicolon env x
    )
  )

let rec map_adjoint_expression (env : env) ((v1, v2) : CST.adjoint_expression) =
  let v1 = map_primary_expression env v1 in
  let v2 = map_imm_tok_squot env v2 in
  R.Tuple [v1; v2]

and map_anon_brac_form_rep_COMMA_brac_form_a30483f (env : env) ((v1, v2) : CST.anon_brac_form_rep_COMMA_brac_form_a30483f) =
  let v1 = map_bracket_form env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_bracket_form env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_anon_choice_brac_form_e7dc26c (env : env) (x : CST.anon_choice_brac_form_e7dc26c) =
  (match x with
  | `Brac_form x -> R.Case ("Brac_form",
      map_bracket_form env x
    )
  | `Gene (v1, v2, v3) -> R.Case ("Gene",
      let v1 = map_bracket_form env v1 in
      let v2 = map_for_clause env v2 in
      let v3 =
        R.List (List.map (map_anon_choice_for_clause_4e31839 env) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_for_clause_4e31839 (env : env) (x : CST.anon_choice_for_clause_4e31839) =
  (match x with
  | `For_clause x -> R.Case ("For_clause",
      map_for_clause env x
    )
  | `If_clause (v1, v2) -> R.Case ("If_clause",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_anon_choice_id_00cc266 (env : env) (x : CST.anon_choice_id_00cc266) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Interp_exp x -> R.Case ("Interp_exp",
      map_interpolation_expression env x
    )
  )

and map_anon_choice_impo_a542259 (env : env) (x : CST.anon_choice_impo_a542259) =
  (match x with
  | `Impo x -> R.Case ("Impo",
      map_importable env x
    )
  | `Import_alias (v1, v2, v3) -> R.Case ("Import_alias",
      let v1 = map_importable env v1 in
      let v2 = (* "as" *) token env v2 in
      let v3 = map_exportable env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_anon_choice_import_list_449435c (env : env) (x : CST.anon_choice_import_list_449435c) =
  (match x with
  | `Import_list x -> R.Case ("Import_list",
      map_import_list env x
    )
  | `Sele_import (v1, v2, v3) -> R.Case ("Sele_import",
      let v1 = map_importable env v1 in
      let v2 = map_imm_tok_colon env v2 in
      let v3 = map_import_list env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_array_ (env : env) (x : CST.array_) =
  (match x with
  | `Comp_exp (v1, v2, v3, v4, v5, v6) -> R.Case ("Comp_exp",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_bracket_form env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_for_clause env v4 in
      let v5 =
        R.List (List.map (map_anon_choice_for_clause_4e31839 env) v5)
      in
      let v6 = (* "]" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Matrix_exp (v1, v2, v3, v4, v5) -> R.Case ("Matrix_exp",
      let v1 = (* "[" *) token env v1 in
      let v2 = map_matrix_row env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = map_terminator env v1 in
          let v2 = map_matrix_row env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v5 = (* "]" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Vec_exp (v1, v2, v3, v4) -> R.Case ("Vec_exp",
      let v1 = (* "[" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_brac_form_rep_COMMA_brac_form_a30483f env x
          ))
        | None -> R.Option None)
      in
      let v3 =
        (match v3 with
        | Some tok -> R.Option (Some (
            (* "," *) token env tok
          ))
        | None -> R.Option None)
      in
      let v4 = (* "]" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_arrow_function_expression (env : env) ((v1, v2, v3) : CST.arrow_function_expression) =
  let v1 =
    (match v1 with
    | `Id x -> R.Case ("Id",
        map_identifier env x
      )
    | `Tuple_exp x -> R.Case ("Tuple_exp",
        map_tuple_expression env x
      )
    | `Typed_exp x -> R.Case ("Typed_exp",
        map_typed_expression env x
      )
    )
  in
  let v2 = (* "->" *) token env v2 in
  let v3 = map_bracket_form env v3 in
  R.Tuple [v1; v2; v3]

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_pair_op_exp (v1, v2, v3) -> R.Case ("Exp_pair_op_exp",
      let v1 = map_expression env v1 in
      let v2 = (* pair_operator *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_arrow_op_exp (v1, v2, v3) -> R.Case ("Exp_arrow_op_exp",
      let v1 = map_expression env v1 in
      let v2 = (* arrow_operator *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_lazy_or_op_exp (v1, v2, v3) -> R.Case ("Exp_lazy_or_op_exp",
      let v1 = map_expression env v1 in
      let v2 = (* lazy_or_operator *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_lazy_and_op_exp (v1, v2, v3) -> R.Case ("Exp_lazy_and_op_exp",
      let v1 = map_expression env v1 in
      let v2 = (* lazy_and_operator *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_in_exp (v1, v2, v3) -> R.Case ("Exp_choice_in_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `In tok -> R.Case ("In",
            (* "in" *) token env tok
          )
        | `Isa tok -> R.Case ("Isa",
            (* "isa" *) token env tok
          )
        | `Comp_op tok -> R.Case ("Comp_op",
            (* comparison_operator *) token env tok
          )
        | `Type_order_op tok -> R.Case ("Type_order_op",
            (* type_order_operator *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_pipe_left_op_exp (v1, v2, v3) -> R.Case ("Exp_pipe_left_op_exp",
      let v1 = map_expression env v1 in
      let v2 = (* pipe_left_operator *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_pipe_right_op_exp (v1, v2, v3) -> R.Case ("Exp_pipe_right_op_exp",
      let v1 = map_expression env v1 in
      let v2 = (* pipe_right_operator *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_ellips_op_exp (v1, v2, v3) -> R.Case ("Exp_ellips_op_exp",
      let v1 = map_expression env v1 in
      let v2 = (* ellipsis_operator *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_choice_un_plus_op_exp (v1, v2, v3) -> R.Case ("Exp_choice_un_plus_op_exp",
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `Un_plus_op tok -> R.Case ("Un_plus_op",
            (* unary_plus_operator *) token env tok
          )
        | `Plus_op tok -> R.Case ("Plus_op",
            (* plus_operator *) token env tok
          )
        )
      in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_times_op_exp (v1, v2, v3) -> R.Case ("Exp_times_op_exp",
      let v1 = map_expression env v1 in
      let v2 = (* times_operator *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_rati_op_exp (v1, v2, v3) -> R.Case ("Exp_rati_op_exp",
      let v1 = map_expression env v1 in
      let v2 = (* rational_operator *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_bits_op_exp (v1, v2, v3) -> R.Case ("Exp_bits_op_exp",
      let v1 = map_expression env v1 in
      let v2 = (* bitshift_operator *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Exp_power_op_exp (v1, v2, v3) -> R.Case ("Exp_power_op_exp",
      let v1 = map_expression env v1 in
      let v2 = (* power_operator *) token env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_block (env : env) ((v1, v2, v3) : CST.block) =
  let v1 = map_block_form env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_terminator env v1 in
      let v2 = map_block_form env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3]

and map_block_form (env : env) (x : CST.block_form) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Assign (v1, v2, v3) -> R.Case ("Assign",
      let v1 =
        (match v1 with
        | `Prim_exp x -> R.Case ("Prim_exp",
            map_primary_expression env x
          )
        | `Open_tuple x -> R.Case ("Open_tuple",
            map_open_tuple env x
          )
        | `Choice_un_exp x -> R.Case ("Choice_un_exp",
            map_operation env x
          )
        | `Op x -> R.Case ("Op",
            map_operator env x
          )
        )
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_block_form env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Open_tuple x -> R.Case ("Open_tuple",
      map_open_tuple env x
    )
  )

and map_bracket_form (env : env) (x : CST.bracket_form) =
  (match x with
  | `Exp x -> R.Case ("Exp",
      map_expression env x
    )
  | `Closed_assign (v1, v2, v3) -> R.Case ("Closed_assign",
      let v1 =
        (match v1 with
        | `Prim_exp x -> R.Case ("Prim_exp",
            map_primary_expression env x
          )
        | `Choice_un_exp x -> R.Case ("Choice_un_exp",
            map_operation env x
          )
        | `Op x -> R.Case ("Op",
            map_operator env x
          )
        )
      in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_bracket_form env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_call_expression (env : env) ((v1, v2, v3, v4) : CST.call_expression) =
  let v1 =
    (match v1 with
    | `Prim_exp x -> R.Case ("Prim_exp",
        map_primary_expression env x
      )
    | `Op x -> R.Case ("Op",
        map_operator env x
      )
    )
  in
  let v2 = (* immediate_paren *) token env v2 in
  let v3 = map_tuple_expression env v3 in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_do_clause env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2; v3; v4]

and map_catch_clause (env : env) ((v1, v2, v3, v4) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        (match x with
        | `Id x -> R.Case ("Id",
            map_identifier env x
          )
        | `Semg_ellips tok -> R.Case ("Semg_ellips",
            (* "..." *) token env tok
          )
        )
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_source_file env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_command_literal (env : env) (x : CST.command_literal) =
  (match x with
  | `Deli_cmd_1_rep_choice_content_cmd_1_end_cmd (v1, v2, v3) -> R.Case ("Deli_cmd_1_rep_choice_content_cmd_1_end_cmd",
      let v1 = (* "`" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Content_cmd_1 tok -> R.Case ("Content_cmd_1",
              (* content_cmd_1 *) token env tok
            )
          | `Str_interp x -> R.Case ("Str_interp",
              map_string_interpolation env x
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* end_cmd *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Deli_cmd_3_rep_choice_content_cmd_3_end_cmd (v1, v2, v3) -> R.Case ("Deli_cmd_3_rep_choice_content_cmd_3_end_cmd",
      let v1 = (* "```" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Content_cmd_3 tok -> R.Case ("Content_cmd_3",
              (* content_cmd_3 *) token env tok
            )
          | `Str_interp x -> R.Case ("Str_interp",
              map_string_interpolation env x
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* end_cmd *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_compound_assignment_expression (env : env) ((v1, v2, v3) : CST.compound_assignment_expression) =
  let v1 = map_primary_expression env v1 in
  let v2 =
    (match v2 with
    | `Assign_op x -> R.Case ("Assign_op",
        map_assignment_operator env x
      )
    | `Tilde_op tok -> R.Case ("Tilde_op",
        (* tilde_operator *) token env tok
      )
    )
  in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_curly_expression (env : env) ((v1, v2, v3, v4) : CST.curly_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_brac_form_rep_COMMA_brac_form_a30483f env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v4 = (* "}" *) token env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_definition (env : env) (x : CST.definition) =
  (match x with
  | `Module_defi (v1, v2, v3, v4, v5) -> R.Case ("Module_defi",
      let v1 =
        (match v1 with
        | `Module tok -> R.Case ("Module",
            (* "module" *) token env tok
          )
        | `Bare tok -> R.Case ("Bare",
            (* "baremodule" *) token env tok
          )
        )
      in
      let v2 = map_anon_choice_id_00cc266 env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_source_file env v4 in
      let v5 = (* "end" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Abst_defi (v1, v2, v3, v4) -> R.Case ("Abst_defi",
      let v1 = (* "abstract" *) token env v1 in
      let v2 = (* "type" *) token env v2 in
      let v3 = map_type_head env v3 in
      let v4 = (* "end" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Prim_defi (v1, v2, v3, v4, v5) -> R.Case ("Prim_defi",
      let v1 = (* "primitive" *) token env v1 in
      let v2 = (* "type" *) token env v2 in
      let v3 = map_type_head env v3 in
      let v4 = map_integer_literal env v4 in
      let v5 = (* "end" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Struct_defi (v1, v2, v3, v4, v5, v6) -> R.Case ("Struct_defi",
      let v1 =
        (match v1 with
        | Some tok -> R.Option (Some (
            (* "mutable" *) token env tok
          ))
        | None -> R.Option None)
      in
      let v2 = (* "struct" *) token env v2 in
      let v3 = map_type_head env v3 in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_source_file env v5 in
      let v6 = (* "end" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `Func_defi (v1, v2, v3, v4, v5) -> R.Case ("Func_defi",
      let v1 = (* "function" *) token env v1 in
      let v2 = map_signature env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_source_file env v4 in
      let v5 = (* "end" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Macro_defi (v1, v2, v3, v4, v5) -> R.Case ("Macro_defi",
      let v1 = (* "macro" *) token env v1 in
      let v2 = map_signature env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_source_file env v4 in
      let v5 = (* "end" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  )

and map_do_clause (env : env) ((v1, v2, v3, v4, v5) : CST.do_clause) =
  let v1 = (* "do" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_anon_brac_form_rep_COMMA_brac_form_a30483f env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_terminator env v3 in
  let v4 = map_source_file env v4 in
  let v5 = (* "end" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_else_clause (env : env) ((v1, v2, v3) : CST.else_clause) =
  let v1 = (* "else" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_source_file env v3 in
  R.Tuple [v1; v2; v3]

and map_elseif_clause (env : env) ((v1, v2, v3, v4) : CST.elseif_clause) =
  let v1 = (* "elseif" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 =
    (match v3 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v4 = map_source_file env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_exportable (env : env) (x : CST.exportable) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Macro_id x -> R.Case ("Macro_id",
      map_macro_identifier env x
    )
  | `Op x -> R.Case ("Op",
      map_operator env x
    )
  | `Interp_exp x -> R.Case ("Interp_exp",
      map_interpolation_expression env x
    )
  | `LPAR_expo_RPAR (v1, v2, v3) -> R.Case ("LPAR_expo_RPAR",
      let v1 = (* "(" *) token env v1 in
      let v2 = map_exportable env v2 in
      let v3 = (* ")" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_choice_module_defi x -> R.Case ("Choice_choice_module_defi",
      (match x with
      | `Choice_module_defi x -> R.Case ("Choice_module_defi",
          map_definition env x
        )
      | `Choice_comp_stmt x -> R.Case ("Choice_comp_stmt",
          map_statement env x
        )
      | `Prim_exp x -> R.Case ("Prim_exp",
          map_primary_expression env x
        )
      | `Choice_un_exp x -> R.Case ("Choice_un_exp",
          map_operation env x
        )
      | `Comp_assign_exp x -> R.Case ("Comp_assign_exp",
          map_compound_assignment_expression env x
        )
      | `Macr_exp x -> R.Case ("Macr_exp",
          map_macrocall_expression env x
        )
      | `Arrow_func_exp x -> R.Case ("Arrow_func_exp",
          map_arrow_function_expression env x
        )
      | `Juxt_exp x -> R.Case ("Juxt_exp",
          map_juxtaposition_expression env x
        )
      | `Tern_exp x -> R.Case ("Tern_exp",
          map_ternary_expression env x
        )
      | `Op x -> R.Case ("Op",
          map_operator env x
        )
      | `Int_lit x -> R.Case ("Int_lit",
          map_integer_literal env x
        )
      | `Float_lit x -> R.Case ("Float_lit",
          map_float_literal env x
        )
      | `Begin tok -> R.Case ("Begin",
          (* "begin" *) token env tok
        )
      )
    )
  | `Semg_ellips tok -> R.Case ("Semg_ellips",
      (* "..." *) token env tok
    )
  | `Deep_exp (v1, v2, v3) -> R.Case ("Deep_exp",
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_finally_clause (env : env) ((v1, v2, v3) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_terminator env x
      ))
    | None -> R.Option None)
  in
  let v3 = map_source_file env v3 in
  R.Tuple [v1; v2; v3]

and map_for_binding (env : env) ((v1, v2, v3, v4) : CST.for_binding) =
  let v1 =
    (match v1 with
    | Some tok -> R.Option (Some (
        (* "outer" *) token env tok
      ))
    | None -> R.Option None)
  in
  let v2 =
    (match v2 with
    | `Id x -> R.Case ("Id",
        map_identifier env x
      )
    | `Tuple_exp x -> R.Case ("Tuple_exp",
        map_tuple_expression env x
      )
    | `Typed_exp x -> R.Case ("Typed_exp",
        map_typed_expression env x
      )
    | `Interp_exp x -> R.Case ("Interp_exp",
        map_interpolation_expression env x
      )
    )
  in
  let v3 =
    (match v3 with
    | `In tok -> R.Case ("In",
        (* "in" *) token env tok
      )
    | `EQ tok -> R.Case ("EQ",
        (* "=" *) token env tok
      )
    | `UNKUNKUNK tok -> R.Case ("UNKUNKUNK",
        (* "\226\136\136" *) token env tok
      )
    )
  in
  let v4 = map_expression env v4 in
  R.Tuple [v1; v2; v3; v4]

and map_for_clause (env : env) ((v1, v2, v3) : CST.for_clause) =
  let v1 = (* "for" *) token env v1 in
  let v2 = map_for_binding env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_for_binding env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  R.Tuple [v1; v2; v3]

and map_import_list (env : env) ((v1, v2) : CST.import_list) =
  let v1 = map_anon_choice_impo_a542259 env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_impo_a542259 env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_importable (env : env) (x : CST.importable) =
  (match x with
  | `Expo x -> R.Case ("Expo",
      map_exportable env x
    )
  | `Scoped_id x -> R.Case ("Scoped_id",
      map_scoped_identifier env x
    )
  | `Import_path (v1, v2) -> R.Case ("Import_path",
      let v1 = map_tok_rep1_dot env v1 in
      let v2 =
        (match v2 with
        | `Id x -> R.Case ("Id",
            map_identifier env x
          )
        | `Scoped_id x -> R.Case ("Scoped_id",
            map_scoped_identifier env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  )

and map_interpolation_expression (env : env) (x : CST.interpolation_expression) =
  (match x with
  | `DOLLAR_choice_int_lit (v1, v2) -> R.Case ("DOLLAR_choice_int_lit",
      let v1 = (* "$" *) token env v1 in
      let v2 =
        (match v2 with
        | `Int_lit x -> R.Case ("Int_lit",
            map_integer_literal env x
          )
        | `Float_lit x -> R.Case ("Float_lit",
            map_float_literal env x
          )
        | `Id x -> R.Case ("Id",
            map_identifier env x
          )
        | `Curl_exp x -> R.Case ("Curl_exp",
            map_curly_expression env x
          )
        | `Paren_exp x -> R.Case ("Paren_exp",
            map_parenthesized_expression env x
          )
        | `Tuple_exp x -> R.Case ("Tuple_exp",
            map_tuple_expression env x
          )
        | `Array x -> R.Case ("Array",
            map_array_ env x
          )
        | `Str x -> R.Case ("Str",
            map_string_ env x
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Semg_exte_meta tok -> R.Case ("Semg_exte_meta",
      (* pattern \$[A-Z_][a-zA-Z_0-9]* *) token env tok
    )
  )

and map_juxtaposition_expression (env : env) ((v1, v2) : CST.juxtaposition_expression) =
  let v1 =
    (match v1 with
    | `Int_lit x -> R.Case ("Int_lit",
        map_integer_literal env x
      )
    | `Float_lit x -> R.Case ("Float_lit",
        map_float_literal env x
      )
    | `Adjo_exp x -> R.Case ("Adjo_exp",
        map_adjoint_expression env x
      )
    )
  in
  let v2 = map_primary_expression env v2 in
  R.Tuple [v1; v2]

and map_macro_argument_list (env : env) (xs : CST.macro_argument_list) =
  R.List (List.map (map_block_form env) xs)

and map_macro_head (env : env) (x : CST.macro_head) =
  (match x with
  | `Qual_macro_id (v1, v2, v3) -> R.Case ("Qual_macro_id",
      let v1 = map_primary_expression env v1 in
      let v2 = map_imm_tok_dot env v2 in
      let v3 = map_macro_identifier env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Macro_id x -> R.Case ("Macro_id",
      map_macro_identifier env x
    )
  )

and map_macro_identifier (env : env) ((v1, v2) : CST.macro_identifier) =
  let v1 = (* "@" *) token env v1 in
  let v2 =
    (match v2 with
    | `Id x -> R.Case ("Id",
        map_identifier env x
      )
    | `Op x -> R.Case ("Op",
        map_operator env x
      )
    | `Synt_op x -> R.Case ("Synt_op",
        map_syntactic_operator env x
      )
    | `Scoped_id x -> R.Case ("Scoped_id",
        map_scoped_identifier env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_macrocall_expression (env : env) ((v1, v2) : CST.macrocall_expression) =
  let v1 = map_macro_head env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_macro_argument_list env x
      ))
    | None -> R.Option None)
  in
  R.Tuple [v1; v2]

and map_matrix_row (env : env) (xs : CST.matrix_row) =
  R.List (List.map (map_bracket_form env) xs)

and map_open_tuple (env : env) ((v1, v2) : CST.open_tuple) =
  let v1 = map_expression env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_operation (env : env) (x : CST.operation) =
  (match x with
  | `Un_exp (v1, v2) -> R.Case ("Un_exp",
      let v1 =
        (match v1 with
        | `Tilde_op tok -> R.Case ("Tilde_op",
            (* tilde_operator *) token env tok
          )
        | `Type_order_op tok -> R.Case ("Type_order_op",
            (* type_order_operator *) token env tok
          )
        | `Un_op tok -> R.Case ("Un_op",
            (* unary_operator *) token env tok
          )
        | `Un_plus_op tok -> R.Case ("Un_plus_op",
            (* unary_plus_operator *) token env tok
          )
        )
      in
      let v2 = map_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  | `Range_exp (v1, v2, v3) -> R.Case ("Range_exp",
      let v1 = map_expression env v1 in
      let v2 = map_imm_tok_colon env v2 in
      let v3 = map_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Splat_exp (v1, v2) -> R.Case ("Splat_exp",
      let v1 = map_expression env v1 in
      let v2 = (* "..." *) token env v2 in
      R.Tuple [v1; v2]
    )
  | `Typed_exp x -> R.Case ("Typed_exp",
      map_typed_expression env x
    )
  | `Un_typed_exp (v1, v2) -> R.Case ("Un_typed_exp",
      let v1 = (* "::" *) token env v1 in
      let v2 = map_primary_expression env v2 in
      R.Tuple [v1; v2]
    )
  | `Where_exp x -> R.Case ("Where_exp",
      map_where_expression env x
    )
  )

and map_parenthesized_expression (env : env) ((v1, v2, v3, v4, v5) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_anon_choice_brac_form_e7dc26c env v2 in
  let v3 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_semicolon env v1 in
      let v2 = map_anon_choice_brac_form_e7dc26c env v2 in
      R.Tuple [v1; v2]
    ) v3)
  in
  let v4 =
    (match v4 with
    | Some x -> R.Option (Some (
        map_semicolon env x
      ))
    | None -> R.Option None)
  in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_primary_expression (env : env) (x : CST.primary_expression) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Bool_lit x -> R.Case ("Bool_lit",
      map_boolean_literal env x
    )
  | `Curl_exp x -> R.Case ("Curl_exp",
      map_curly_expression env x
    )
  | `Paren_exp x -> R.Case ("Paren_exp",
      map_parenthesized_expression env x
    )
  | `Tuple_exp x -> R.Case ("Tuple_exp",
      map_tuple_expression env x
    )
  | `Array x -> R.Case ("Array",
      map_array_ env x
    )
  | `Str x -> R.Case ("Str",
      map_string_ env x
    )
  | `Adjo_exp x -> R.Case ("Adjo_exp",
      map_adjoint_expression env x
    )
  | `Broa_call_exp (v1, v2, v3, v4, v5) -> R.Case ("Broa_call_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = map_imm_tok_dot env v2 in
      let v3 = (* immediate_paren *) token env v3 in
      let v4 = map_tuple_expression env v4 in
      let v5 =
        (match v5 with
        | Some x -> R.Option (Some (
            map_do_clause env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Call_exp x -> R.Case ("Call_exp",
      map_call_expression env x
    )
  | `Closed_macr_exp (v1, v2) -> R.Case ("Closed_macr_exp",
      let v1 = map_macro_head env v1 in
      let v2 =
        (match v2 with
        | `Imme_brace_curl_exp (v1, v2) -> R.Case ("Imme_brace_curl_exp",
            let v1 = (* immediate_brace *) token env v1 in
            let v2 = map_curly_expression env v2 in
            R.Tuple [v1; v2]
          )
        | `Imme_brac_array (v1, v2) -> R.Case ("Imme_brac_array",
            let v1 = (* immediate_bracket *) token env v1 in
            let v2 = map_array_ env v2 in
            R.Tuple [v1; v2]
          )
        | `Imme_paren_tuple_exp_opt_do_clause (v1, v2, v3) -> R.Case ("Imme_paren_tuple_exp_opt_do_clause",
            let v1 = (* immediate_paren *) token env v1 in
            let v2 = map_tuple_expression env v2 in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_do_clause env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      R.Tuple [v1; v2]
    )
  | `Para_type_exp (v1, v2, v3) -> R.Case ("Para_type_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* immediate_brace *) token env v2 in
      let v3 = map_curly_expression env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Field_exp (v1, v2, v3) -> R.Case ("Field_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = map_imm_tok_dot env v2 in
      let v3 =
        (match v3 with
        | `Id x -> R.Case ("Id",
            map_identifier env x
          )
        | `Interp_exp x -> R.Case ("Interp_exp",
            map_interpolation_expression env x
          )
        | `Quote_exp x -> R.Case ("Quote_exp",
            map_quote_expression env x
          )
        | `Str x -> R.Case ("Str",
            map_string_ env x
          )
        )
      in
      R.Tuple [v1; v2; v3]
    )
  | `Index_exp (v1, v2, v3) -> R.Case ("Index_exp",
      let v1 = map_primary_expression env v1 in
      let v2 = (* immediate_bracket *) token env v2 in
      let v3 = map_array_ env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Interp_exp x -> R.Case ("Interp_exp",
      map_interpolation_expression env x
    )
  | `Quote_exp x -> R.Case ("Quote_exp",
      map_quote_expression env x
    )
  )

and map_quote_expression (env : env) ((v1, v2) : CST.quote_expression) =
  let v1 = (* ":" *) token env v1 in
  let v2 =
    (match v2 with
    | `Int_lit x -> R.Case ("Int_lit",
        map_integer_literal env x
      )
    | `Float_lit x -> R.Case ("Float_lit",
        map_float_literal env x
      )
    | `Str x -> R.Case ("Str",
        map_string_ env x
      )
    | `Id x -> R.Case ("Id",
        map_identifier env x
      )
    | `Op x -> R.Case ("Op",
        map_operator env x
      )
    | `Imme_brace_curl_exp (v1, v2) -> R.Case ("Imme_brace_curl_exp",
        let v1 = (* immediate_brace *) token env v1 in
        let v2 = map_curly_expression env v2 in
        R.Tuple [v1; v2]
      )
    | `Imme_brac_array (v1, v2) -> R.Case ("Imme_brac_array",
        let v1 = (* immediate_bracket *) token env v1 in
        let v2 = map_array_ env v2 in
        R.Tuple [v1; v2]
      )
    | `Imme_paren_choice_paren_exp (v1, v2) -> R.Case ("Imme_paren_choice_paren_exp",
        let v1 = (* immediate_paren *) token env v1 in
        let v2 =
          (match v2 with
          | `Paren_exp x -> R.Case ("Paren_exp",
              map_parenthesized_expression env x
            )
          | `Tuple_exp x -> R.Case ("Tuple_exp",
              map_tuple_expression env x
            )
          | `LPAR_choice_COLONCOLON_RPAR (v1, v2, v3) -> R.Case ("LPAR_choice_COLONCOLON_RPAR",
              let v1 = (* "(" *) token env v1 in
              let v2 =
                (match v2 with
                | `COLONCOLON tok -> R.Case ("COLONCOLON",
                    (* "::" *) token env tok
                  )
                | `COLONEQ tok -> R.Case ("COLONEQ",
                    (* ":=" *) token env tok
                  )
                | `DOTEQ tok -> R.Case ("DOTEQ",
                    (* ".=" *) token env tok
                  )
                | `EQ tok -> R.Case ("EQ",
                    (* "=" *) token env tok
                  )
                | `Assign_op x -> R.Case ("Assign_op",
                    map_assignment_operator env x
                  )
                | `Lazy_or_op tok -> R.Case ("Lazy_or_op",
                    (* lazy_or_operator *) token env tok
                  )
                | `Lazy_and_op tok -> R.Case ("Lazy_and_op",
                    (* lazy_and_operator *) token env tok
                  )
                | `Synt_op x -> R.Case ("Synt_op",
                    map_syntactic_operator env x
                  )
                )
              in
              let v3 = (* ")" *) token env v3 in
              R.Tuple [v1; v2; v3]
            )
          )
        in
        R.Tuple [v1; v2]
      )
    | `Choice_assign_op x -> R.Case ("Choice_assign_op",
        (match x with
        | `Assign_op x -> R.Case ("Assign_op",
            map_assignment_operator env x
          )
        | `Lazy_or_op tok -> R.Case ("Lazy_or_op",
            (* lazy_or_operator *) token env tok
          )
        | `Lazy_and_op tok -> R.Case ("Lazy_and_op",
            (* lazy_and_operator *) token env tok
          )
        | `Synt_op x -> R.Case ("Synt_op",
            map_syntactic_operator env x
          )
        )
      )
    | `Imm_tok_choice_bare x -> R.Case ("Imm_tok_choice_bare",
        map_imm_tok_choice_bare env x
      )
    )
  in
  R.Tuple [v1; v2]

and map_scoped_identifier (env : env) ((v1, v2) : CST.scoped_identifier) =
  let v1 = map_anon_choice_id_00cc266 env v1 in
  let v2 =
    R.List (List.map (fun (v1, v2) ->
      let v1 = map_imm_tok_dot env v1 in
      let v2 = map_anon_choice_id_00cc266 env v2 in
      R.Tuple [v1; v2]
    ) v2)
  in
  R.Tuple [v1; v2]

and map_signature (env : env) (x : CST.signature) =
  (match x with
  | `Id x -> R.Case ("Id",
      map_identifier env x
    )
  | `Call_exp x -> R.Case ("Call_exp",
      map_call_expression env x
    )
  | `Tuple_exp x -> R.Case ("Tuple_exp",
      map_tuple_expression env x
    )
  | `Typed_exp x -> R.Case ("Typed_exp",
      map_typed_expression env x
    )
  | `Where_exp x -> R.Case ("Where_exp",
      map_where_expression env x
    )
  )

and map_source_file (env : env) (opt : CST.source_file) =
  (match opt with
  | Some x -> R.Option (Some (
      map_block env x
    ))
  | None -> R.Option None)

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Comp_stmt (v1, v2, v3, v4) -> R.Case ("Comp_stmt",
      let v1 = (* "begin" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_source_file env v3 in
      let v4 = (* "end" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Quote_stmt (v1, v2, v3, v4) -> R.Case ("Quote_stmt",
      let v1 = (* "quote" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_source_file env v3 in
      let v4 = (* "end" *) token env v4 in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Let_stmt (v1, v2, v3, v4, v5) -> R.Case ("Let_stmt",
      let v1 = (* "let" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_anon_brac_form_rep_COMMA_brac_form_a30483f env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_terminator env v3 in
      let v4 = map_source_file env v4 in
      let v5 = (* "end" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `If_stmt (v1, v2, v3, v4, v5, v6, v7) -> R.Case ("If_stmt",
      let v1 = (* "if" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_source_file env v4 in
      let v5 = R.List (List.map (map_elseif_clause env) v5) in
      let v6 =
        (match v6 with
        | Some x -> R.Option (Some (
            map_else_clause env x
          ))
        | None -> R.Option None)
      in
      let v7 = (* "end" *) token env v7 in
      R.Tuple [v1; v2; v3; v4; v5; v6; v7]
    )
  | `Try_stmt (v1, v2, v3, v4, v5) -> R.Case ("Try_stmt",
      let v1 = (* "try" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v3 = map_source_file env v3 in
      let v4 =
        (match v4 with
        | `Catch_clause_opt_else_clause_opt_fina_clause (v1, v2, v3) -> R.Case ("Catch_clause_opt_else_clause_opt_fina_clause",
            let v1 = map_catch_clause env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_else_clause env x
                ))
              | None -> R.Option None)
            in
            let v3 =
              (match v3 with
              | Some x -> R.Option (Some (
                  map_finally_clause env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2; v3]
          )
        | `Fina_clause_opt_catch_clause (v1, v2) -> R.Case ("Fina_clause_opt_catch_clause",
            let v1 = map_finally_clause env v1 in
            let v2 =
              (match v2 with
              | Some x -> R.Option (Some (
                  map_catch_clause env x
                ))
              | None -> R.Option None)
            in
            R.Tuple [v1; v2]
          )
        )
      in
      let v5 = (* "end" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `For_stmt (v1, v2, v3, v4, v5, v6) -> R.Case ("For_stmt",
      let v1 = (* "for" *) token env v1 in
      let v2 = map_for_binding env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_for_binding env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v5 = map_source_file env v5 in
      let v6 = (* "end" *) token env v6 in
      R.Tuple [v1; v2; v3; v4; v5; v6]
    )
  | `While_stmt (v1, v2, v3, v4, v5) -> R.Case ("While_stmt",
      let v1 = (* "while" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        (match v3 with
        | Some x -> R.Option (Some (
            map_terminator env x
          ))
        | None -> R.Option None)
      in
      let v4 = map_source_file env v4 in
      let v5 = (* "end" *) token env v5 in
      R.Tuple [v1; v2; v3; v4; v5]
    )
  | `Brk_stmt tok -> R.Case ("Brk_stmt",
      (* "break" *) token env tok
    )
  | `Cont_stmt tok -> R.Case ("Cont_stmt",
      (* "continue" *) token env tok
    )
  | `Ret_stmt (v1, v2) -> R.Case ("Ret_stmt",
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> R.Option (Some (
            map_block_form env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2]
    )
  | `Const_stmt (v1, v2) -> R.Case ("Const_stmt",
      let v1 = (* "const" *) token env v1 in
      let v2 = map_block_form env v2 in
      R.Tuple [v1; v2]
    )
  | `Global_stmt (v1, v2) -> R.Case ("Global_stmt",
      let v1 = (* "global" *) token env v1 in
      let v2 = map_block_form env v2 in
      R.Tuple [v1; v2]
    )
  | `Local_stmt (v1, v2) -> R.Case ("Local_stmt",
      let v1 = (* "local" *) token env v1 in
      let v2 = map_block_form env v2 in
      R.Tuple [v1; v2]
    )
  | `Export_stmt (v1, v2, v3) -> R.Case ("Export_stmt",
      let v1 = (* "export" *) token env v1 in
      let v2 = map_exportable env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_exportable env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Import_stmt (v1, v2) -> R.Case ("Import_stmt",
      let v1 = (* "import" *) token env v1 in
      let v2 = map_anon_choice_import_list_449435c env v2 in
      R.Tuple [v1; v2]
    )
  | `Public_stmt (v1, v2, v3) -> R.Case ("Public_stmt",
      let v1 = (* "public" *) token env v1 in
      let v2 = map_exportable env v2 in
      let v3 =
        R.List (List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_exportable env v2 in
          R.Tuple [v1; v2]
        ) v3)
      in
      R.Tuple [v1; v2; v3]
    )
  | `Using_stmt (v1, v2) -> R.Case ("Using_stmt",
      let v1 = (* "using" *) token env v1 in
      let v2 = map_anon_choice_import_list_449435c env v2 in
      R.Tuple [v1; v2]
    )
  )

and map_string_ (env : env) (x : CST.string_) =
  (match x with
  | `Char_lit tok -> R.Case ("Char_lit",
      (* character_literal *) token env tok
    )
  | `Str_lit x -> R.Case ("Str_lit",
      map_string_literal env x
    )
  | `Cmd_lit x -> R.Case ("Cmd_lit",
      map_command_literal env x
    )
  | `Pref_str_lit (v1, v2, v3, v4) -> R.Case ("Pref_str_lit",
      let v1 = map_identifier env v1 in
      let v2 = (* immediate_string_start *) token env v2 in
      let v3 =
        (match v3 with
        | `Deli_str_1_rep_choice_content_str_1_raw_end_str (v1, v2, v3) -> R.Case ("Deli_str_1_rep_choice_content_str_1_raw_end_str",
            let v1 = (* "\"" *) token env v1 in
            let v2 =
              R.List (List.map (fun x ->
                (match x with
                | `Content_str_1_raw tok -> R.Case ("Content_str_1_raw",
                    (* content_str_1_raw *) token env tok
                  )
                | `Esc_seq tok -> R.Case ("Esc_seq",
                    (* escape_sequence *) token env tok
                  )
                )
              ) v2)
            in
            let v3 = (* end_str *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `Deli_str_3_rep_choice_content_str_3_raw_end_str (v1, v2, v3) -> R.Case ("Deli_str_3_rep_choice_content_str_3_raw_end_str",
            let v1 = (* "\"\"\"" *) token env v1 in
            let v2 =
              R.List (List.map (fun x ->
                (match x with
                | `Content_str_3_raw tok -> R.Case ("Content_str_3_raw",
                    (* content_str_3_raw *) token env tok
                  )
                | `Esc_seq tok -> R.Case ("Esc_seq",
                    (* escape_sequence *) token env tok
                  )
                )
              ) v2)
            in
            let v3 = (* end_str *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_identifier env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  | `Pref_cmd_lit (v1, v2, v3, v4) -> R.Case ("Pref_cmd_lit",
      let v1 = map_identifier env v1 in
      let v2 = (* immediate_command_start *) token env v2 in
      let v3 =
        (match v3 with
        | `Deli_cmd_1_rep_choice_content_cmd_1_raw_end_cmd (v1, v2, v3) -> R.Case ("Deli_cmd_1_rep_choice_content_cmd_1_raw_end_cmd",
            let v1 = (* "`" *) token env v1 in
            let v2 =
              R.List (List.map (fun x ->
                (match x with
                | `Content_cmd_1_raw tok -> R.Case ("Content_cmd_1_raw",
                    (* content_cmd_1_raw *) token env tok
                  )
                | `Esc_seq tok -> R.Case ("Esc_seq",
                    (* escape_sequence *) token env tok
                  )
                )
              ) v2)
            in
            let v3 = (* end_cmd *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        | `Deli_cmd_3_rep_choice_content_cmd_3_raw_end_cmd (v1, v2, v3) -> R.Case ("Deli_cmd_3_rep_choice_content_cmd_3_raw_end_cmd",
            let v1 = (* "```" *) token env v1 in
            let v2 =
              R.List (List.map (fun x ->
                (match x with
                | `Content_cmd_3_raw tok -> R.Case ("Content_cmd_3_raw",
                    (* content_cmd_3_raw *) token env tok
                  )
                | `Esc_seq tok -> R.Case ("Esc_seq",
                    (* escape_sequence *) token env tok
                  )
                )
              ) v2)
            in
            let v3 = (* end_cmd *) token env v3 in
            R.Tuple [v1; v2; v3]
          )
        )
      in
      let v4 =
        (match v4 with
        | Some x -> R.Option (Some (
            map_identifier env x
          ))
        | None -> R.Option None)
      in
      R.Tuple [v1; v2; v3; v4]
    )
  )

and map_string_interpolation (env : env) ((v1, v2) : CST.string_interpolation) =
  let v1 = (* "$" *) token env v1 in
  let v2 =
    (match v2 with
    | `Id x -> R.Case ("Id",
        map_identifier env x
      )
    | `Imme_paren_LPAR_brac_form_RPAR (v1, v2, v3, v4) -> R.Case ("Imme_paren_LPAR_brac_form_RPAR",
        let v1 = (* immediate_paren *) token env v1 in
        let v2 = (* "(" *) token env v2 in
        let v3 = map_bracket_form env v3 in
        let v4 = (* ")" *) token env v4 in
        R.Tuple [v1; v2; v3; v4]
      )
    )
  in
  R.Tuple [v1; v2]

and map_string_literal (env : env) (x : CST.string_literal) =
  (match x with
  | `Deli_str_1_rep_choice_content_str_1_end_str (v1, v2, v3) -> R.Case ("Deli_str_1_rep_choice_content_str_1_end_str",
      let v1 = (* "\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Content_str_1 tok -> R.Case ("Content_str_1",
              (* content_str_1 *) token env tok
            )
          | `Str_interp x -> R.Case ("Str_interp",
              map_string_interpolation env x
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* end_str *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  | `Deli_str_3_rep_choice_content_str_3_end_str (v1, v2, v3) -> R.Case ("Deli_str_3_rep_choice_content_str_3_end_str",
      let v1 = (* "\"\"\"" *) token env v1 in
      let v2 =
        R.List (List.map (fun x ->
          (match x with
          | `Content_str_3 tok -> R.Case ("Content_str_3",
              (* content_str_3 *) token env tok
            )
          | `Str_interp x -> R.Case ("Str_interp",
              map_string_interpolation env x
            )
          | `Esc_seq tok -> R.Case ("Esc_seq",
              (* escape_sequence *) token env tok
            )
          )
        ) v2)
      in
      let v3 = (* end_str *) token env v3 in
      R.Tuple [v1; v2; v3]
    )
  )

and map_ternary_expression (env : env) ((v1, v2, v3, v4, v5) : CST.ternary_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "?" *) token env v2 in
  let v3 = map_bracket_form env v3 in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_bracket_form env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_tuple_expression (env : env) ((v1, v2, v3, v4, v5) : CST.tuple_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x -> R.Option (Some (
        map_semicolon env x
      ))
    | None -> R.Option None)
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) -> R.Option (Some (
        let v1 = map_anon_choice_brac_form_e7dc26c env v1 in
        let v2 =
          R.List (List.map (fun (v1, v2) ->
            let v1 =
              (match v1 with
              | `COMMA tok -> R.Case ("COMMA",
                  (* "," *) token env tok
                )
              | `Semi x -> R.Case ("Semi",
                  map_semicolon env x
                )
              )
            in
            let v2 = map_anon_choice_brac_form_e7dc26c env v2 in
            R.Tuple [v1; v2]
          ) v2)
        in
        R.Tuple [v1; v2]
      ))
    | None -> R.Option None)
  in
  let v4 =
    (match v4 with
    | Some tok -> R.Option (Some (
        (* "," *) token env tok
      ))
    | None -> R.Option None)
  in
  let v5 = (* ")" *) token env v5 in
  R.Tuple [v1; v2; v3; v4; v5]

and map_type_head (env : env) (x : CST.type_head) =
  (match x with
  | `Prim_exp x -> R.Case ("Prim_exp",
      map_primary_expression env x
    )
  | `Bin_exp x -> R.Case ("Bin_exp",
      map_binary_expression env x
    )
  )

and map_typed_expression (env : env) ((v1, v2, v3) : CST.typed_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 = map_primary_expression env v3 in
  R.Tuple [v1; v2; v3]

and map_where_expression (env : env) ((v1, v2, v3) : CST.where_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "where" *) token env v2 in
  let v3 = map_expression env v3 in
  R.Tuple [v1; v2; v3]

let map_block_comment (env : env) ((v1, v2) : CST.block_comment) =
  let v1 = map_pat_f66a72a env v1 in
  let v2 = (* block_comment_rest *) token env v2 in
  R.Tuple [v1; v2]

let map_line_comment (env : env) ((v1, v2) : CST.line_comment) =
  let v1 = map_pat_01abfc7 env v1 in
  let v2 = map_pat_4fd4a56 env v2 in
  R.Tuple [v1; v2]

let dump_tree root =
  map_source_file () root
  |> Tree_sitter_run.Raw_tree.to_channel stdout

let map_extra (env : env) (x : CST.extra) =
  match x with
  | `Line_comment (_loc, x) -> ("line_comment", "line_comment", map_line_comment env x)
  | `Block_comment (_loc, x) -> ("block_comment", "block_comment", map_block_comment env x)

let dump_extras (extras : CST.extras) =
  List.iter (fun extra ->
    let ts_rule_name, ocaml_type_name, raw_tree = map_extra () extra in
    let details =
      if ocaml_type_name <> ts_rule_name then
        Printf.sprintf " (OCaml type '%s')" ocaml_type_name
      else
        ""
    in
    Printf.printf "%s%s:\n" ts_rule_name details;
    Tree_sitter_run.Raw_tree.to_channel stdout raw_tree
  ) extras
