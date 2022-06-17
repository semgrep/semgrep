(* Yoann Padioleau
 *
 * Copyright (c) 2022 R2C
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

module G = AST_generic
module H = Parse_tree_sitter_helpers
module CST = Tree_sitter_elixir.CST

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Elixir parser using tree-sitter-lang/semgrep-elixir and converting
 * directly to AST_generic.ml
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-elixir/Boilerplate.ml *)

let todo (env : env) _ = failwith "not implemented"

let _map_quoted_content_single (env : env) (tok : CST.quoted_content_single) =
  (* quoted_content_single *) token env tok

let _map_newline_before_do (env : env) (tok : CST.newline_before_do) =
  (* newline_before_do *) token env tok

let _map_quoted_content_heredoc_double (env : env)
    (tok : CST.quoted_content_heredoc_double) =
  (* quoted_content_heredoc_double *) token env tok

let _map_alias (env : env) (tok : CST.alias) = (* alias *) token env tok

let _map_quoted_content_angle (env : env) (tok : CST.quoted_content_angle) =
  (* quoted_content_angle *) token env tok

let _map_pat_509ec78 (env : env) (tok : CST.pat_509ec78) =
  (* pattern \r?\n *) token env tok

let _map_quoted_content_i_parenthesis (env : env)
    (tok : CST.quoted_content_i_parenthesis) =
  (* quoted_content_i_parenthesis *) token env tok

let _map_quoted_content_square (env : env) (tok : CST.quoted_content_square) =
  (* quoted_content_square *) token env tok

let _map_quoted_content_parenthesis (env : env)
    (tok : CST.quoted_content_parenthesis) =
  (* quoted_content_parenthesis *) token env tok

let _map_before_unary_op (env : env) (tok : CST.before_unary_op) =
  (* before_unary_op *) token env tok

let map_boolean (env : env) (x : CST.boolean) =
  match x with
  | `True tok -> (* "true" *) token env tok
  | `False tok -> (* "false" *) token env tok

let _map_quoted_content_curly (env : env) (tok : CST.quoted_content_curly) =
  (* quoted_content_curly *) token env tok

let _map_atom_ (env : env) (tok : CST.atom_) = (* atom_ *) token env tok

let _map_keyword_ (env : env) (tok : CST.keyword_) =
  (* keyword_ *) token env tok

let _map_pat_cf9c6c3 (env : env) (tok : CST.pat_cf9c6c3) =
  (* pattern [_\p{Ll}\p{Lm}\p{Lo}\p{Nl}\u1885\u1886\u2118\u212E\u309B\u309C][\p{ID_Continue}]*[?!]? *)
  token env tok

let _map_imm_tok_pat_8f9e87e (env : env) (tok : CST.imm_tok_pat_8f9e87e) =
  (* pattern [a-zA-Z0-9]+ *) token env tok

let _map_not_in (env : env) (tok : CST.not_in) = (* not_in *) token env tok

let _map_quoted_content_heredoc_single (env : env)
    (tok : CST.quoted_content_heredoc_single) =
  (* quoted_content_heredoc_single *) token env tok

let _map_quoted_content_i_double (env : env) (tok : CST.quoted_content_i_double)
    =
  (* quoted_content_i_double *) token env tok

let _map_quoted_content_i_curly (env : env) (tok : CST.quoted_content_i_curly) =
  (* quoted_content_i_curly *) token env tok

let _map_char (env : env) (tok : CST.char) =
  (* pattern \?(.|\\.) *) token env tok

let _map_integer (env : env) (tok : CST.integer) = (* integer *) token env tok

let _map_quoted_content_i_single (env : env) (tok : CST.quoted_content_i_single)
    =
  (* quoted_content_i_single *) token env tok

let _map_quoted_atom_start (env : env) (tok : CST.quoted_atom_start) =
  (* quoted_atom_start *) token env tok

let _map_quoted_content_i_heredoc_double (env : env)
    (tok : CST.quoted_content_i_heredoc_double) =
  (* quoted_content_i_heredoc_double *) token env tok

let _map_quoted_content_i_slash (env : env) (tok : CST.quoted_content_i_slash) =
  (* quoted_content_i_slash *) token env tok

let _map_imm_tok_pat_0db2d54 (env : env) (tok : CST.imm_tok_pat_0db2d54) =
  (* pattern [a-z] *) token env tok

let _map_quoted_content_double (env : env) (tok : CST.quoted_content_double) =
  (* quoted_content_double *) token env tok

let _map_float_ (env : env) (tok : CST.float_) = (* float *) token env tok

let _map_imm_tok_pat_562b724 (env : env) (tok : CST.imm_tok_pat_562b724) =
  (* pattern [A-Z] *) token env tok

let _map_quoted_content_slash (env : env) (tok : CST.quoted_content_slash) =
  (* quoted_content_slash *) token env tok

let _map_imm_tok_pat_5eb9c21 (env : env) (tok : CST.imm_tok_pat_5eb9c21) =
  (* pattern :\s *) token env tok

let _map_quoted_content_i_heredoc_single (env : env)
    (tok : CST.quoted_content_i_heredoc_single) =
  (* quoted_content_i_heredoc_single *) token env tok

let _map_quoted_content_i_angle (env : env) (tok : CST.quoted_content_i_angle) =
  (* quoted_content_i_angle *) token env tok

let _map_escape_sequence (env : env) (tok : CST.escape_sequence) =
  (* escape_sequence *) token env tok

let _map_quoted_content_i_bar (env : env) (tok : CST.quoted_content_i_bar) =
  (* quoted_content_i_bar *) token env tok

let _map_quoted_content_bar (env : env) (tok : CST.quoted_content_bar) =
  (* quoted_content_bar *) token env tok

let _map_quoted_content_i_square (env : env) (tok : CST.quoted_content_i_square)
    =
  (* quoted_content_i_square *) token env tok

let map_anon_choice_PLUS_8019319 (env : env) (x : CST.anon_choice_PLUS_8019319)
    =
  match x with
  | `PLUS tok -> (* "+" *) token env tok
  | `DASH tok -> (* "-" *) token env tok
  | `BANG tok -> (* "!" *) token env tok
  | `HAT tok -> (* "^" *) token env tok
  | `TILDETILDETILDE tok -> (* "~~~" *) token env tok
  | `Not tok -> (* "not" *) token env tok

let map_terminator (env : env) (x : CST.terminator) =
  match x with
  | `Rep_pat_509ec78_SEMI (v1, v2) ->
      let v1 = Common.map (token env (* pattern \r?\n *)) v1 in
      let v2 = (* ";" *) token env v2 in
      todo env (v1, v2)
  | `Rep1_pat_509ec78 xs -> Common.map (token env (* pattern \r?\n *)) xs

let map_identifier (env : env) (x : CST.identifier) =
  match x with
  | `Pat_cf9c6c3 tok ->
      (* pattern [_\p{Ll}\p{Lm}\p{Lo}\p{Nl}\u1885\u1886\u2118\u212E\u309B\u309C][\p{ID_Continue}]*[?!]? *)
      token env tok
  | `DOTDOTDOT tok -> (* "..." *) token env tok

let map_quoted_slash (env : env) ((v1, v2, v3) : CST.quoted_slash) =
  let v1 = (* "/" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_slash tok -> (* quoted_content_slash *) token env tok
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "/" *) token env v3 in
  todo env (v1, v2, v3)

let map_quoted_heredoc_double (env : env)
    ((v1, v2, v3) : CST.quoted_heredoc_double) =
  let v1 = (* "\"\"\"" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_here_double tok ->
            (* quoted_content_heredoc_double *) token env tok
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "\"\"\"" *) token env v3 in
  todo env (v1, v2, v3)

let map_quoted_single (env : env) ((v1, v2, v3) : CST.quoted_single) =
  let v1 = (* "'" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_single tok ->
            (* quoted_content_single *) token env tok
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "'" *) token env v3 in
  todo env (v1, v2, v3)

let map_quoted_angle (env : env) ((v1, v2, v3) : CST.quoted_angle) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_angle tok -> (* quoted_content_angle *) token env tok
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* ">" *) token env v3 in
  todo env (v1, v2, v3)

let map_quoted_double (env : env) ((v1, v2, v3) : CST.quoted_double) =
  let v1 = (* "\"" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_double tok ->
            (* quoted_content_double *) token env tok
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "\"" *) token env v3 in
  todo env (v1, v2, v3)

let map_quoted_parenthesis (env : env) ((v1, v2, v3) : CST.quoted_parenthesis) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_paren tok ->
            (* quoted_content_parenthesis *) token env tok
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

let map_quoted_square (env : env) ((v1, v2, v3) : CST.quoted_square) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_square tok ->
            (* quoted_content_square *) token env tok
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "]" *) token env v3 in
  todo env (v1, v2, v3)

let map_quoted_heredoc_single (env : env)
    ((v1, v2, v3) : CST.quoted_heredoc_single) =
  let v1 = (* "'''" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_here_single tok ->
            (* quoted_content_heredoc_single *) token env tok
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "'''" *) token env v3 in
  todo env (v1, v2, v3)

let map_quoted_curly (env : env) ((v1, v2, v3) : CST.quoted_curly) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_curl tok -> (* quoted_content_curly *) token env tok
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

let map_quoted_bar (env : env) ((v1, v2, v3) : CST.quoted_bar) =
  let v1 = (* "|" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_bar tok -> (* quoted_content_bar *) token env tok
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "|" *) token env v3 in
  todo env (v1, v2, v3)

let map_operator_identifier (env : env) (x : CST.operator_identifier) =
  match x with
  | `AMP tok -> (* "&" *) token env tok
  | `Choice_PLUS x -> map_anon_choice_PLUS_8019319 env x
  | `AT tok -> (* "@" *) token env tok
  | `LTDASH tok -> (* "<-" *) token env tok
  | `BSLASHBSLASH tok -> (* "\\\\" *) token env tok
  | `When tok -> (* "when" *) token env tok
  | `COLONCOLON tok -> (* "::" *) token env tok
  | `BAR tok -> (* "|" *) token env tok
  | `EQ tok -> (* "=" *) token env tok
  | `BARBAR tok -> (* "||" *) token env tok
  | `BARBARBAR tok -> (* "|||" *) token env tok
  | `Or tok -> (* "or" *) token env tok
  | `AMPAMP tok -> (* "&&" *) token env tok
  | `AMPAMPAMP tok -> (* "&&&" *) token env tok
  | `And tok -> (* "and" *) token env tok
  | `EQEQ tok -> (* "==" *) token env tok
  | `BANGEQ tok -> (* "!=" *) token env tok
  | `EQTILDE tok -> (* "=~" *) token env tok
  | `EQEQEQ tok -> (* "===" *) token env tok
  | `BANGEQEQ tok -> (* "!==" *) token env tok
  | `LT tok -> (* "<" *) token env tok
  | `GT tok -> (* ">" *) token env tok
  | `LTEQ tok -> (* "<=" *) token env tok
  | `GTEQ tok -> (* ">=" *) token env tok
  | `BARGT tok -> (* "|>" *) token env tok
  | `LTLTLT tok -> (* "<<<" *) token env tok
  | `GTGTGT tok -> (* ">>>" *) token env tok
  | `LTLTTILDE tok -> (* "<<~" *) token env tok
  | `TILDEGTGT tok -> (* "~>>" *) token env tok
  | `LTTILDE tok -> (* "<~" *) token env tok
  | `TILDEGT tok -> (* "~>" *) token env tok
  | `LTTILDEGT tok -> (* "<~>" *) token env tok
  | `LTBARGT tok -> (* "<|>" *) token env tok
  | `In tok -> (* "in" *) token env tok
  | `Not_in tok -> (* not_in *) token env tok
  | `HATHAT tok -> (* "^^" *) token env tok
  | `PLUSPLUS tok -> (* "++" *) token env tok
  | `DASHDASH tok -> (* "--" *) token env tok
  | `PLUSPLUSPLUS tok -> (* "+++" *) token env tok
  | `DASHDASHDASH tok -> (* "---" *) token env tok
  | `DOTDOT tok -> (* ".." *) token env tok
  | `LTGT tok -> (* "<>" *) token env tok
  | `STAR tok -> (* "*" *) token env tok
  | `SLASH tok -> (* "/" *) token env tok
  | `STARSTAR tok -> (* "**" *) token env tok
  | `DASHGT tok -> (* "->" *) token env tok
  | `DOT tok -> (* "." *) token env tok

let rec map_after_block (env : env) ((v1, v2, v3) : CST.after_block) =
  let v1 = (* "after" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_terminator env x
    | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
    (env : env)
    (x : CST.anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119)
    =
  match x with
  | `Choice_stab_clause_rep_term_choice_stab_clause (v1, v2) ->
      let v1 =
        match v1 with
        | `Stab_clause x -> map_stab_clause env x
      in
      let v2 = Common.map (map_anon_term_choice_stab_clause_70647b7 env) v2 in
      todo env (v1, v2)
  | `Choice_exp_rep_term_choice_exp_opt_term (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Exp x -> map_expression env x
      in
      let v2 = Common.map (map_anon_term_choice_exp_996111b env) v2 in
      let v3 =
        match v3 with
        | Some x -> map_terminator env x
        | None -> todo env ()
      in
      todo env (v1, v2, v3)

and map_anon_choice_exp_0094635 (env : env) (x : CST.anon_choice_exp_0094635) =
  match x with
  | `Exp x -> map_expression env x
  | `Keywos x -> map_keywords env x

and map_anon_choice_quoted_i_double_d7d5f65 (env : env)
    (x : CST.anon_choice_quoted_i_double_d7d5f65) =
  match x with
  | `Quoted_i_double x -> map_quoted_i_double env x
  | `Quoted_i_single x -> map_quoted_i_single env x

and map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e (env : env)
    ((v1, v2, v3) : CST.anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e) =
  let v1 = map_expression env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_keywords env v2 in
        todo env (v1, v2)
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_anon_opt_opt_nl_before_do_do_blk_3eff85f (env : env)
    (opt : CST.anon_opt_opt_nl_before_do_do_blk_3eff85f) =
  match opt with
  | Some (v1, v2) ->
      let v1 =
        match v1 with
        | Some tok -> (* newline_before_do *) token env tok
        | None -> todo env ()
      in
      let v2 = map_do_block env v2 in
      todo env (v1, v2)
  | None -> todo env ()

and map_anon_term_choice_exp_996111b (env : env)
    ((v1, v2) : CST.anon_term_choice_exp_996111b) =
  let v1 = map_terminator env v1 in
  let v2 =
    match v2 with
    | `Exp x -> map_expression env x
  in
  todo env (v1, v2)

and map_anon_term_choice_stab_clause_70647b7 (env : env)
    ((v1, v2) : CST.anon_term_choice_stab_clause_70647b7) =
  let v1 = map_terminator env v1 in
  let v2 =
    match v2 with
    | `Stab_clause x -> map_stab_clause env x
  in
  todo env (v1, v2)

and map_anonymous_call (env : env) ((v1, v2) : CST.anonymous_call) =
  let v1 = map_anonymous_dot env v1 in
  let v2 = map_call_arguments_with_parentheses env v2 in
  todo env (v1, v2)

and map_anonymous_dot (env : env) ((v1, v2) : CST.anonymous_dot) =
  let v1 = map_expression env v1 in
  let v2 = (* "." *) token env v2 in
  todo env (v1, v2)

and map_atom (env : env) (x : CST.atom) =
  match x with
  | `Atom_ tok -> (* atom_ *) token env tok
  | `Quoted_atom (v1, v2) ->
      let v1 = (* quoted_atom_start *) token env v1 in
      let v2 = map_anon_choice_quoted_i_double_d7d5f65 env v2 in
      todo env (v1, v2)

and map_binary_operator (env : env) (x : CST.binary_operator) =
  match x with
  | `Exp_choice_LTDASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `LTDASH tok -> (* "<-" *) token env tok
        | `BSLASHBSLASH tok -> (* "\\\\" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_when_choice_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "when" *) token env v2 in
      let v3 = map_anon_choice_exp_0094635 env v3 in
      todo env (v1, v2, v3)
  | `Exp_COLONCOLON_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "::" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAR_choice_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_anon_choice_exp_0094635 env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `BARBAR tok -> (* "||" *) token env tok
        | `BARBARBAR tok -> (* "|||" *) token env tok
        | `Or tok -> (* "or" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `AMPAMP tok -> (* "&&" *) token env tok
        | `AMPAMPAMP tok -> (* "&&&" *) token env tok
        | `And tok -> (* "and" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `EQEQ tok -> (* "==" *) token env tok
        | `BANGEQ tok -> (* "!=" *) token env tok
        | `EQTILDE tok -> (* "=~" *) token env tok
        | `EQEQEQ tok -> (* "===" *) token env tok
        | `BANGEQEQ tok -> (* "!==" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `LT tok -> (* "<" *) token env tok
        | `GT tok -> (* ">" *) token env tok
        | `LTEQ tok -> (* "<=" *) token env tok
        | `GTEQ tok -> (* ">=" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_BARGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `BARGT tok -> (* "|>" *) token env tok
        | `LTLTLT tok -> (* "<<<" *) token env tok
        | `GTGTGT tok -> (* ">>>" *) token env tok
        | `LTLTTILDE tok -> (* "<<~" *) token env tok
        | `TILDEGTGT tok -> (* "~>>" *) token env tok
        | `LTTILDE tok -> (* "<~" *) token env tok
        | `TILDEGT tok -> (* "~>" *) token env tok
        | `LTTILDEGT tok -> (* "<~>" *) token env tok
        | `LTBARGT tok -> (* "<|>" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_in_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `In tok -> (* "in" *) token env tok
        | `Not_in tok -> (* not_in *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HATHATHAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^^^" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_SLASHSLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "//" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_PLUSPLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `PLUSPLUS tok -> (* "++" *) token env tok
        | `DASHDASH tok -> (* "--" *) token env tok
        | `PLUSPLUSPLUS tok -> (* "+++" *) token env tok
        | `DASHDASHDASH tok -> (* "---" *) token env tok
        | `DOTDOT tok -> (* ".." *) token env tok
        | `LTGT tok -> (* "<>" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `PLUS tok -> (* "+" *) token env tok
        | `DASH tok -> (* "-" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_choice_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `STAR tok -> (* "*" *) token env tok
        | `SLASH tok -> (* "/" *) token env tok
      in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Op_id_SLASH_int (v1, v2, v3) ->
      let v1 = map_operator_identifier env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = (* integer *) token env v3 in
      todo env (v1, v2, v3)

and map_body (env : env) ((v1, v2, v3, v4) : CST.body) =
  let v1 =
    match v1 with
    | Some x -> map_terminator env x
    | None -> todo env ()
  in
  let v2 = map_expression env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let v1 = map_terminator env v1 in
        let v2 = map_expression env v2 in
        todo env (v1, v2))
      v3
  in
  let v4 =
    match v4 with
    | Some x -> map_terminator env x
    | None -> todo env ()
  in
  todo env (v1, v2, v3, v4)

and map_call (env : env) (x : CST.call) =
  match x with
  | `Call_with_parens_b98484c x -> map_call_without_parentheses env x
  | `Call_with_parens_403315d x -> map_call_with_parentheses env x

and map_call_arguments_with_parentheses (env : env)
    ((v1, v2, v3) : CST.call_arguments_with_parentheses) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_call_arguments_with_trailing_separator env x
    | None -> todo env ()
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_call_arguments_with_parentheses_immediate (env : env)
    ((v1, v2, v3) : CST.call_arguments_with_parentheses_immediate) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_call_arguments_with_trailing_separator env x
    | None -> todo env ()
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_call_arguments_with_trailing_separator (env : env)
    (x : CST.call_arguments_with_trailing_separator) =
  match x with
  | `Exp_rep_COMMA_exp_opt_COMMA_keywos_with_trai_sepa (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2))
          v2
      in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_keywords_with_trailing_separator env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3)
  | `Keywos_with_trai_sepa x -> map_keywords_with_trailing_separator env x

and map_call_arguments_without_parentheses (env : env)
    (x : CST.call_arguments_without_parentheses) =
  match x with
  | `Exp_rep_COMMA_exp_opt_COMMA_keywos x ->
      map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e env x
  | `Keywos x -> map_keywords env x

and map_call_with_parentheses (env : env) (x : CST.call_with_parentheses) =
  match x with
  | `Local_call_with_parens x -> map_local_call_with_parentheses env x
  | `Remote_call_with_parens x -> map_remote_call_with_parentheses env x
  | `Anon_call x -> map_anonymous_call env x
  | `Double_call (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Local_call_with_parens x -> map_local_call_with_parentheses env x
        | `Remote_call_with_parens x -> map_remote_call_with_parentheses env x
        | `Anon_call x -> map_anonymous_call env x
      in
      let v2 = map_call_arguments_with_parentheses env v2 in
      let v3 = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
      todo env (v1, v2, v3)

and map_call_without_parentheses (env : env) (x : CST.call_without_parentheses)
    =
  match x with
  | `Local_call_with_parens (v1, v2, v3) ->
      let v1 = map_identifier env v1 in
      let v2 = map_call_arguments_without_parentheses env v2 in
      let v3 = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
      todo env (v1, v2, v3)
  | `Local_call_just_do_blk (v1, v2) ->
      let v1 = map_identifier env v1 in
      let v2 = map_do_block env v2 in
      todo env (v1, v2)
  | `Remote_call_with_parens (v1, v2, v3) ->
      let v1 = map_remote_dot env v1 in
      let v2 =
        match v2 with
        | Some x -> map_call_arguments_without_parentheses env x
        | None -> todo env ()
      in
      let v3 = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
      todo env (v1, v2, v3)

and map_capture_expression (env : env) (x : CST.capture_expression) =
  match x with
  | `LPAR_exp_RPAR (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      todo env (v1, v2, v3)
  | `Exp x -> map_expression env x

and map_catch_block (env : env) ((v1, v2, v3) : CST.catch_block) =
  let v1 = (* "catch" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_terminator env x
    | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_charlist (env : env) (x : CST.charlist) =
  match x with
  | `Quoted_i_single x -> map_quoted_i_single env x
  | `Quoted_i_here_single x -> map_quoted_i_heredoc_single env x

and map_do_block (env : env) ((v1, v2, v3, v4, v5) : CST.do_block) =
  let v1 = (* "do" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_terminator env x
    | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> todo env ()
  in
  let v4 =
    Common.map
      (fun x ->
        match x with
        | `After_blk x -> map_after_block env x
        | `Rescue_blk x -> map_rescue_block env x
        | `Catch_blk x -> map_catch_block env x
        | `Else_blk x -> map_else_block env x)
      v4
  in
  let v5 = (* "end" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_dot (env : env) ((v1, v2, v3) : CST.dot) =
  let v1 = map_expression env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 =
    match v3 with
    | `Alias tok -> (* alias *) token env tok
    | `Tuple x -> map_tuple env x
  in
  todo env (v1, v2, v3)

and map_else_block (env : env) ((v1, v2, v3) : CST.else_block) =
  let v1 = (* "else" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_terminator env x
    | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_expression (env : env) (x : CST.expression) =
  match x with
  | `Blk (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_terminator env x
        | None -> todo env ()
      in
      let v3 =
        match v3 with
        | Some x ->
            map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
              env x
        | None -> todo env ()
      in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Id x -> map_identifier env x
  | `Alias tok -> (* alias *) token env tok
  | `Int tok -> (* integer *) token env tok
  | `Float tok -> (* float *) token env tok
  | `Char_a87deb0 tok -> (* pattern \?(.|\\.) *) token env tok
  | `Bool x -> map_boolean env x
  | `Nil tok -> (* "nil" *) token env tok
  | `Atom x -> map_atom env x
  | `Str x -> map_string_ env x
  | `Char_a593f90 x -> map_charlist env x
  | `Sigil (v1, v2, v3) ->
      let v1 = (* "~" *) token env v1 in
      let v2 =
        match v2 with
        | `Imm_tok_pat_0db2d54_choice_quoted_i_double (v1, v2) ->
            let v1 = (* pattern [a-z] *) token env v1 in
            let v2 =
              match v2 with
              | `Quoted_i_double x -> map_quoted_i_double env x
              | `Quoted_i_single x -> map_quoted_i_single env x
              | `Quoted_i_here_single x -> map_quoted_i_heredoc_single env x
              | `Quoted_i_here_double x -> map_quoted_i_heredoc_double env x
              | `Quoted_i_paren x -> map_quoted_i_parenthesis env x
              | `Quoted_i_curl x -> map_quoted_i_curly env x
              | `Quoted_i_square x -> map_quoted_i_square env x
              | `Quoted_i_angle x -> map_quoted_i_angle env x
              | `Quoted_i_bar x -> map_quoted_i_bar env x
              | `Quoted_i_slash x -> map_quoted_i_slash env x
            in
            todo env (v1, v2)
        | `Imm_tok_pat_562b724_choice_quoted_double (v1, v2) ->
            let v1 = (* pattern [A-Z] *) token env v1 in
            let v2 =
              match v2 with
              | `Quoted_double x -> map_quoted_double env x
              | `Quoted_single x -> map_quoted_single env x
              | `Quoted_here_single x -> map_quoted_heredoc_single env x
              | `Quoted_here_double x -> map_quoted_heredoc_double env x
              | `Quoted_paren x -> map_quoted_parenthesis env x
              | `Quoted_curl x -> map_quoted_curly env x
              | `Quoted_square x -> map_quoted_square env x
              | `Quoted_angle x -> map_quoted_angle env x
              | `Quoted_bar x -> map_quoted_bar env x
              | `Quoted_slash x -> map_quoted_slash env x
            in
            todo env (v1, v2)
      in
      let v3 =
        match v3 with
        | Some tok -> (* pattern [a-zA-Z0-9]+ *) token env tok
        | None -> todo env ()
      in
      todo env (v1, v2, v3)
  | `List (v1, v2, v3) ->
      let v1 = (* "[" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_items_with_trailing_separator env x
        | None -> todo env ()
      in
      let v3 = (* "]" *) token env v3 in
      todo env (v1, v2, v3)
  | `Tuple x -> map_tuple env x
  | `Bits (v1, v2, v3) ->
      let v1 = (* "<<" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_items_with_trailing_separator env x
        | None -> todo env ()
      in
      let v3 = (* ">>" *) token env v3 in
      todo env (v1, v2, v3)
  | `Map (v1, v2, v3, v4, v5) ->
      let v1 = (* "%" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_struct_ env x
        | None -> todo env ()
      in
      let v3 = (* "{" *) token env v3 in
      let v4 =
        match v4 with
        | Some x -> map_items_with_trailing_separator env x
        | None -> todo env ()
      in
      let v5 = (* "}" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Un_op x -> map_unary_operator env x
  | `Bin_op x -> map_binary_operator env x
  | `Dot x -> map_dot env x
  | `Call x -> map_call env x
  | `Access_call (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Anon_func (v1, v2, v3, v4, v5) ->
      let v1 = (* "fn" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_terminator env x
        | None -> todo env ()
      in
      let v3 = map_stab_clause env v3 in
      let v4 =
        Common.map
          (fun (v1, v2) ->
            let v1 = map_terminator env v1 in
            let v2 = map_stab_clause env v2 in
            todo env (v1, v2))
          v4
      in
      let v5 = (* "end" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)

and map_interpolation (env : env) ((v1, v2, v3) : CST.interpolation) =
  let v1 = (* "#{" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_items_with_trailing_separator (env : env)
    (v1 : CST.items_with_trailing_separator) =
  match v1 with
  | `Exp_rep_COMMA_exp_opt_COMMA (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        Common.map
          (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2))
          v2
      in
      let v3 =
        match v3 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ()
      in
      todo env (v1, v2, v3)
  | `Opt_exp_rep_COMMA_exp_COMMA_keywos_with_trai_sepa (v1, v2) ->
      let v1 =
        match v1 with
        | Some (v1, v2, v3) ->
            let v1 = map_expression env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let v1 = (* "," *) token env v1 in
                  let v2 = map_expression env v2 in
                  todo env (v1, v2))
                v2
            in
            let v3 = (* "," *) token env v3 in
            todo env (v1, v2, v3)
        | None -> todo env ()
      in
      let v2 = map_keywords_with_trailing_separator env v2 in
      todo env (v1, v2)

and map_keyword (env : env) (x : CST.keyword) =
  match x with
  | `Kw_ tok -> (* keyword_ *) token env tok
  | `Quoted_kw (v1, v2) ->
      let v1 = map_anon_choice_quoted_i_double_d7d5f65 env v1 in
      let v2 = (* pattern :\s *) token env v2 in
      todo env (v1, v2)

and map_keywords (env : env) ((v1, v2) : CST.keywords) =
  let v1 = map_pair env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_pair env v2 in
        todo env (v1, v2))
      v2
  in
  todo env (v1, v2)

and map_keywords_with_trailing_separator (env : env)
    ((v1, v2, v3) : CST.keywords_with_trailing_separator) =
  let v1 = map_pair env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let v1 = (* "," *) token env v1 in
        let v2 = map_pair env v2 in
        todo env (v1, v2))
      v2
  in
  let v3 =
    match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_local_call_with_parentheses (env : env)
    ((v1, v2, v3) : CST.local_call_with_parentheses) =
  let v1 = map_identifier env v1 in
  let v2 = map_call_arguments_with_parentheses_immediate env v2 in
  let v3 = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
  todo env (v1, v2, v3)

and map_pair (env : env) ((v1, v2) : CST.pair) =
  let v1 = map_keyword env v1 in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_quoted_i_angle (env : env) ((v1, v2, v3) : CST.quoted_i_angle) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_i_angle tok ->
            (* quoted_content_i_angle *) token env tok
        | `Interp x -> map_interpolation env x
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* ">" *) token env v3 in
  todo env (v1, v2, v3)

and map_quoted_i_bar (env : env) ((v1, v2, v3) : CST.quoted_i_bar) =
  let v1 = (* "|" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_i_bar tok -> (* quoted_content_i_bar *) token env tok
        | `Interp x -> map_interpolation env x
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "|" *) token env v3 in
  todo env (v1, v2, v3)

and map_quoted_i_curly (env : env) ((v1, v2, v3) : CST.quoted_i_curly) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_i_curl tok ->
            (* quoted_content_i_curly *) token env tok
        | `Interp x -> map_interpolation env x
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_quoted_i_double (env : env) ((v1, v2, v3) : CST.quoted_i_double) =
  let v1 = (* "\"" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_i_double tok ->
            (* quoted_content_i_double *) token env tok
        | `Interp x -> map_interpolation env x
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "\"" *) token env v3 in
  todo env (v1, v2, v3)

and map_quoted_i_heredoc_double (env : env)
    ((v1, v2, v3) : CST.quoted_i_heredoc_double) =
  let v1 = (* "\"\"\"" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_i_here_double tok ->
            (* quoted_content_i_heredoc_double *) token env tok
        | `Interp x -> map_interpolation env x
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "\"\"\"" *) token env v3 in
  todo env (v1, v2, v3)

and map_quoted_i_heredoc_single (env : env)
    ((v1, v2, v3) : CST.quoted_i_heredoc_single) =
  let v1 = (* "'''" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_i_here_single tok ->
            (* quoted_content_i_heredoc_single *) token env tok
        | `Interp x -> map_interpolation env x
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "'''" *) token env v3 in
  todo env (v1, v2, v3)

and map_quoted_i_parenthesis (env : env)
    ((v1, v2, v3) : CST.quoted_i_parenthesis) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_i_paren tok ->
            (* quoted_content_i_parenthesis *) token env tok
        | `Interp x -> map_interpolation env x
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_quoted_i_single (env : env) ((v1, v2, v3) : CST.quoted_i_single) =
  let v1 = (* "'" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_i_single tok ->
            (* quoted_content_i_single *) token env tok
        | `Interp x -> map_interpolation env x
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "'" *) token env v3 in
  todo env (v1, v2, v3)

and map_quoted_i_slash (env : env) ((v1, v2, v3) : CST.quoted_i_slash) =
  let v1 = (* "/" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_i_slash tok ->
            (* quoted_content_i_slash *) token env tok
        | `Interp x -> map_interpolation env x
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "/" *) token env v3 in
  todo env (v1, v2, v3)

and map_quoted_i_square (env : env) ((v1, v2, v3) : CST.quoted_i_square) =
  let v1 = (* "[" *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_i_square tok ->
            (* quoted_content_i_square *) token env tok
        | `Interp x -> map_interpolation env x
        | `Esc_seq tok -> (* escape_sequence *) token env tok)
      v2
  in
  let v3 = (* "]" *) token env v3 in
  todo env (v1, v2, v3)

and map_remote_call_with_parentheses (env : env)
    ((v1, v2, v3) : CST.remote_call_with_parentheses) =
  let v1 = map_remote_dot env v1 in
  let v2 = map_call_arguments_with_parentheses_immediate env v2 in
  let v3 = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
  todo env (v1, v2, v3)

and map_remote_dot (env : env) ((v1, v2, v3) : CST.remote_dot) =
  let v1 = map_expression env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 =
    match v3 with
    | `Id x -> map_identifier env x
    | `Choice_and x -> (
        match x with
        | `And tok -> (* "and" *) token env tok
        | `In tok -> (* "in" *) token env tok
        | `Not tok -> (* "not" *) token env tok
        | `Or tok -> (* "or" *) token env tok
        | `When tok -> (* "when" *) token env tok
        | `True tok -> (* "true" *) token env tok
        | `False tok -> (* "false" *) token env tok
        | `Nil tok -> (* "nil" *) token env tok
        | `After tok -> (* "after" *) token env tok
        | `Catch tok -> (* "catch" *) token env tok
        | `Do tok -> (* "do" *) token env tok
        | `Else tok -> (* "else" *) token env tok
        | `End tok -> (* "end" *) token env tok
        | `Fn tok -> (* "fn" *) token env tok
        | `Rescue tok -> (* "rescue" *) token env tok)
    | `Op_id x -> map_operator_identifier env x
    | `Quoted_i_double x -> map_quoted_i_double env x
    | `Quoted_i_single x -> map_quoted_i_single env x
  in
  todo env (v1, v2, v3)

and map_rescue_block (env : env) ((v1, v2, v3) : CST.rescue_block) =
  let v1 = (* "rescue" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_terminator env x
    | None -> todo env ()
  in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_stab_clause (env : env) ((v1, v2, v3) : CST.stab_clause) =
  let v1 =
    match v1 with
    | Some x -> map_stab_clause_left env x
    | None -> todo env ()
  in
  let v2 = (* "->" *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> map_body env x
    | None -> todo env ()
  in
  todo env (v1, v2, v3)

and map_stab_clause_arguments_with_parentheses (env : env)
    ((v1, v2, v3) : CST.stab_clause_arguments_with_parentheses) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_stab_clause_arguments_without_parentheses env x
    | None -> todo env ()
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_stab_clause_arguments_without_parentheses (env : env)
    (x : CST.stab_clause_arguments_without_parentheses) =
  match x with
  | `Exp_rep_COMMA_exp_opt_COMMA_keywos x ->
      map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e env x
  | `Keywos x -> map_keywords env x

and map_stab_clause_left (env : env) (x : CST.stab_clause_left) =
  match x with
  | `Stab_clause_args_with_parens_bf4a580 x ->
      map_stab_clause_arguments_with_parentheses env x
  | `Stab_clause_args_with_parens_with_guard_9d9f341 (v1, v2, v3) ->
      let v1 = map_stab_clause_arguments_with_parentheses env v1 in
      let v2 = (* "when" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Stab_clause_args_with_parens_a52ef95 x ->
      map_stab_clause_arguments_without_parentheses env x
  | `Stab_clause_args_with_parens_with_guard_cfbae3b (v1, v2, v3) ->
      let v1 = map_stab_clause_arguments_without_parentheses env v1 in
      let v2 = (* "when" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)

and map_string_ (env : env) (x : CST.string_) =
  match x with
  | `Quoted_i_double x -> map_quoted_i_double env x
  | `Quoted_i_here_double x -> map_quoted_i_heredoc_double env x

and map_struct_ (env : env) (x : CST.struct_) =
  match x with
  | `Alias tok -> (* alias *) token env tok
  | `Atom x -> map_atom env x
  | `Id x -> map_identifier env x
  | `Un_op x -> map_unary_operator env x
  | `Dot x -> map_dot env x
  | `Call_with_parens x -> map_call_with_parentheses env x

and map_tuple (env : env) ((v1, v2, v3) : CST.tuple) =
  let v1 = (* "{" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_items_with_trailing_separator env x
    | None -> todo env ()
  in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_unary_operator (env : env) (x : CST.unary_operator) =
  match x with
  | `Opt_before_un_op_AMP_capt_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some tok -> (* before_unary_op *) token env tok
        | None -> todo env ()
      in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_capture_expression env v3 in
      todo env (v1, v2, v3)
  | `Opt_before_un_op_choice_PLUS_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some tok -> (* before_unary_op *) token env tok
        | None -> todo env ()
      in
      let v2 = map_anon_choice_PLUS_8019319 env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Opt_before_un_op_AT_exp (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some tok -> (* before_unary_op *) token env tok
        | None -> todo env ()
      in
      let v2 = (* "@" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Opt_before_un_op_AMP_int (v1, v2, v3) ->
      let v1 =
        match v1 with
        | Some tok -> (* before_unary_op *) token env tok
        | None -> todo env ()
      in
      let v2 = (* "&" *) token env v2 in
      let v3 = (* integer *) token env v3 in
      todo env (v1, v2, v3)

let map_source (env : env) ((v1, v2) : CST.source) =
  let v1 =
    match v1 with
    | Some x -> map_terminator env x
    | None -> todo env ()
  in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_expression env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let v1 = map_terminator env v1 in
              let v2 = map_expression env v2 in
              todo env (v1, v2))
            v2
        in
        let v3 =
          match v3 with
          | Some x -> map_terminator env x
          | None -> todo env ()
        in
        todo env (v1, v2, v3)
    | None -> todo env ()
  in
  todo env (v1, v2)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_elixir.Parse.file file)
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      (* TODO: remove this try once todo() is not needed anymore *)
      try
        match map_source env cst with
        | G.Pr xs -> xs
        | _ -> failwith "not a program"
      with
      | Failure "not implemented" as exn ->
          let e = Exception.catch exn in
          H.debug_sexp_cst_after_error (CST.sexp_of_source cst);
          Exception.reraise e)

let parse_pattern str =
  H.wrap_parser
    (fun () ->
      (*TODO parse_expression_or_source_file*)
      Tree_sitter_elixir.Parse.string str)
    (fun cst ->
      let file = "<pattern>" in
      let env = { H.file; conv = Hashtbl.create 0; extra = () } in
      match map_source env cst with
      | G.Pr [ x ] -> G.S x
      | G.Pr xs -> G.Ss xs
      | x -> x)
