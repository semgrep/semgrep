(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
open Common
(**
   Boilerplate to be used as a template when mapping the bash CST
   to another type of tree.
*)

open Fpath_.Operators
module AST = AST_bash
module CST = Tree_sitter_bash.CST
open AST_bash
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type ts_tok = Tree_sitter_run.Token.t

(* The 'extra' field indicates if we're parsing a pattern or a program. *)
type env = AST_bash.input_kind H.env

let token = H.token
let str = H.str

(* This is used where we incorrectly support an ellipsis instead of
   just a metavariable. *)
let _fresh_metavariable_name =
  (* TODO: this is a false positive in the rule.  Why? *)
  (* nosemgrep: no-ref-declarations-at-top-scope *)
  let counter = ref 0 in
  let get () =
    let n =
      incr counter;
      !counter
    in
    spf "$__SEMGREP_METAVAR%i" n
  in
  get

(*
   Replace the last element of a list.
   This is usually a sign that something wasn't done right.
*)
let _map_last l f =
  match List.rev l with
  | [] -> []
  | last :: other -> List.rev (f last :: other)

(*
   The 'statement' rule returns one of 2 possible levels of constructs:
   - pipeline = list of commands
   - command
*)
type tmp_stmt =
  | Tmp_pipeline of pipeline
  | Tmp_command of (cmd_redir * unary_control_operator wrap option)

let rec is_empty_blist (blist : blist) =
  match blist with
  | Seq (_loc, a, b) -> is_empty_blist a && is_empty_blist b
  | Pipelines (_loc, []) -> true
  | Pipelines (_loc, _ :: _) -> false
  | Empty _loc -> true

(*
   A generic list function for inspecting and replacing the last element.
   Does nothing is the list is empty.
*)
let replace_last l replace =
  match List.rev l with
  | last :: rest -> List.rev_append (replace last) rest |> List.rev
  | [] -> []

(* Add a terminator e.g. '&' to the last pipeline of the blist. *)
let add_terminator_to_blist (blist : blist) (term : unary_control_operator wrap)
    =
  let term_loc = AST_bash_loc.wrap_loc term in
  let rec add_to blist : blist =
    match blist with
    | Seq (_loc, a, b) -> if is_empty_blist b then add_to a else blist
    | Pipelines (loc, pipelines) ->
        let pipelines =
          replace_last pipelines (fun pip ->
              let loc =
                Tok_range.range (AST_bash_loc.pipeline_loc pip) term_loc
              in
              [ Control_operator (loc, pip, term) ])
        in
        let loc = Tok_range.range loc term_loc in
        Pipelines (loc, pipelines)
    | Empty _ as blist -> blist
  in
  add_to blist

let pipeline_of_tmp_stmt (x : tmp_stmt) : pipeline =
  match x with
  | Tmp_pipeline x -> x
  | Tmp_command (cmd_r, control_op) -> (
      let loc = AST_bash_loc.cmd_redir_loc cmd_r in
      let cmd = Command cmd_r in
      match control_op with
      | None -> cmd
      | Some ((_, tok) as op) ->
          Control_operator (Tok_range.extend loc tok, cmd, op))

(* Extend the pipeline 'left' with the commands of the pipeline 'right'
   e.g. for 'cat <<EOF | grep x'. *)
let rec append_pipeline (left : pipeline) (bar : pipeline_bar wrap)
    (right : pipeline) : pipeline =
  let range pip loc = Tok_range.range (AST_bash_loc.pipeline_loc pip) loc in
  match right with
  | Command cmd_r -> Pipeline (range left cmd_r.loc, left, bar, cmd_r)
  | Pipeline (_loc, pip, bar2, cmd_r) ->
      let pip = append_pipeline left bar pip in
      Pipeline (range pip cmd_r.loc, pip, bar2, cmd_r)
  | Control_operator (loc, pip, op) ->
      let pip = append_pipeline left bar pip in
      Control_operator (range pip loc, pip, op)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)

let unary_test_operator (env : env) (tok : ts_tok) : unary_test_operator wrap =
  let s, tok = str env tok in
  let op =
    match s with
    | "-t" -> FD_refers_to_terminal
    | "-o" -> Is_shell_option_enabled
    | "-v" -> Is_shell_variable_set
    | "-R" -> Is_shell_variable_a_name_ref
    | "-z" -> Is_empty_string
    | "-n" -> Is_nonempty_string
    | "-a"
    | "-e " ->
        File_exists
    | "-b" -> Is_block_special_file
    | "-c" -> Is_character_special_file
    | "-d" -> Is_directory
    | "-f" -> Is_regular_file
    | "-g" -> Has_SGID_bit
    | "-h"
    | "-L" ->
        Is_symlink
    | "-k" -> Has_sticky_bit
    | "-p" -> Is_named_pipe
    | "-r" -> Is_readable
    | "-s" -> Is_nonempty_file
    | "-u" -> Has_SUID_bit
    | "-w" -> Is_writable
    | "-x" -> Is_executable
    | "-G" -> Is_owned_by_effective_group_id
    | "-N" -> Was_modified_since_last_read
    | "-O" -> Is_owned_by_effective_user_id
    | "-S" -> Is_socket
    | _other -> Other_unary_test_operator
  in
  (op, tok)

let _binary_operator (env : env) (tok : ts_tok) : binary_test_operator wrap =
  let s, tok = str env tok in
  let op =
    match s with
    | "-ef" -> Same_physical_file
    | "-nt" -> File_newer_than
    | "-ot" -> File_older_than
    | "=" -> String_equal
    | "==" -> String_pattern_matching
    | "!=" -> String_not_equal
    | "<" -> String_lesser_than
    | ">" -> String_greater_than
    | "-eq" -> Int_equal
    | "-ne" -> Int_not_equal
    | "-lt" -> Int_lesser_than
    | "-le" -> Int_lesser_equal
    | "-gt" -> Int_greater_than
    | "-ge" -> Int_greater_equal
    | _other -> Other_binary_test_operator
  in
  (op, tok)

(* The token of any operator, whichever CST choice type it comes from. *)
let op_tok
    (x :
      [< `PLUSEQ of ts_tok
      | `DASHEQ of ts_tok
      | `STAREQ of ts_tok
      | `SLASHEQ of ts_tok
      | `PERCEQ of ts_tok
      | `STARSTAREQ of ts_tok
      | `LTLTEQ of ts_tok
      | `GTGTEQ of ts_tok
      | `AMPEQ of ts_tok
      | `HATEQ of ts_tok
      | `BAREQ of ts_tok
      | `EQ of ts_tok
      | `EQTILDE of ts_tok
      | `EQEQ of ts_tok
      | `BANGEQ of ts_tok
      | `LT of ts_tok
      | `GT of ts_tok
      | `LTEQ of ts_tok
      | `GTEQ of ts_tok
      | `LTLT of ts_tok
      | `GTGT of ts_tok
      | `PLUS of ts_tok
      | `DASH of ts_tok
      | `STAR of ts_tok
      | `SLASH of ts_tok
      | `PERC of ts_tok
      | `PLUSPLUS of ts_tok
      | `DASHDASH of ts_tok
      | `BARBAR of ts_tok
      | `DASHo of ts_tok
      | `AMPAMP of ts_tok
      | `DASHa of ts_tok
      | `Tok_prec_p1_plusps of ts_tok
      | `Tok_prec_p1_dash of ts_tok
      | `Tok_prec_p1_plus of ts_tok
      | `Tok_prec_p1_tilde of ts_tok ]) : ts_tok =
  match x with
  | `PLUSEQ tok
  | `DASHEQ tok
  | `STAREQ tok
  | `SLASHEQ tok
  | `PERCEQ tok
  | `STARSTAREQ tok
  | `LTLTEQ tok
  | `GTGTEQ tok
  | `AMPEQ tok
  | `HATEQ tok
  | `BAREQ tok
  | `EQ tok
  | `EQTILDE tok
  | `EQEQ tok
  | `BANGEQ tok
  | `LT tok
  | `GT tok
  | `LTEQ tok
  | `GTEQ tok
  | `LTLT tok
  | `GTGT tok
  | `PLUS tok
  | `DASH tok
  | `STAR tok
  | `SLASH tok
  | `PERC tok
  | `PLUSPLUS tok
  | `DASHDASH tok
  | `BARBAR tok
  | `DASHo tok
  | `AMPAMP tok
  | `DASHa tok
  | `Tok_prec_p1_plusps tok
  | `Tok_prec_p1_dash tok
  | `Tok_prec_p1_plus tok
  | `Tok_prec_p1_tilde tok ->
      tok

(* Operands and operator of a binary test expression. *)
let binary_parts
    (x :
      [ `Exp_choice_PLUSEQ_exp of
        CST.expression * CST.anon_choice_PLUSEQ_110f722 * CST.expression
      | `Exp_choice_EQ_exp of
        CST.expression * CST.anon_choice_EQ_2bc102b * CST.expression
      | `Exp_BARBAR_exp of CST.expression * ts_tok * CST.expression
      | `Exp_AMPAMP_exp of CST.expression * ts_tok * CST.expression
      | `Exp_BAR_exp of CST.expression * ts_tok * CST.expression
      | `Exp_HAT_exp of CST.expression * ts_tok * CST.expression
      | `Exp_AMP_exp of CST.expression * ts_tok * CST.expression
      | `Exp_choice_EQEQ_exp of
        CST.expression * CST.anon_choice_EQEQ_8fff75a * CST.expression
      | `Exp_choice_LT_exp of
        CST.expression * CST.anon_choice_LT_995091f * CST.expression
      | `Exp_test_op_exp of CST.expression * ts_tok * CST.expression
      | `Exp_choice_LTLT_exp of
        CST.expression * CST.anon_choice_LTLT_5d21964 * CST.expression
      | `Exp_choice_PLUS_exp of
        CST.expression * CST.anon_choice_PLUS_da42005 * CST.expression
      | `Exp_choice_STAR_exp of
        CST.expression * CST.anon_choice_STAR_6fc9218 * CST.expression
      | `Exp_STARSTAR_exp of CST.expression * ts_tok * CST.expression ]) =
  match x with
  | `Exp_choice_PLUSEQ_exp (a, op, b) -> (a, op_tok op, b)
  | `Exp_choice_EQ_exp (a, op, b) -> (a, op_tok op, b)
  | `Exp_choice_EQEQ_exp (a, op, b) -> (a, op_tok op, b)
  | `Exp_choice_LT_exp (a, op, b) -> (a, op_tok op, b)
  | `Exp_choice_LTLT_exp (a, op, b) -> (a, op_tok op, b)
  | `Exp_choice_PLUS_exp (a, op, b) -> (a, op_tok op, b)
  | `Exp_choice_STAR_exp (a, op, b) -> (a, op_tok op, b)
  | `Exp_BARBAR_exp (a, tok, b)
  | `Exp_AMPAMP_exp (a, tok, b)
  | `Exp_BAR_exp (a, tok, b)
  | `Exp_HAT_exp (a, tok, b)
  | `Exp_AMP_exp (a, tok, b)
  | `Exp_test_op_exp (a, tok, b)
  | `Exp_STARSTAR_exp (a, tok, b) ->
      (a, tok, b)

(* Operands and operator of a binary arithmetic expression. *)
let arithmetic_binary_parts (x : CST.arithmetic_binary_expression) =
  match x with
  | `Arit_exp_choice_PLUSEQ_arit_exp (a, op, b) -> (a, op_tok op, b)
  | `Arit_exp_choice_EQ_arit_exp (a, op, b) -> (a, op_tok op, b)
  | `Arit_exp_choice_EQEQ_arit_exp (a, op, b) -> (a, op_tok op, b)
  | `Arit_exp_choice_LT_arit_exp (a, op, b) -> (a, op_tok op, b)
  | `Arit_exp_choice_LTLT_arit_exp (a, op, b) -> (a, op_tok op, b)
  | `Arit_exp_choice_PLUS_arit_exp (a, op, b) -> (a, op_tok op, b)
  | `Arit_exp_choice_STAR_arit_exp (a, op, b) -> (a, op_tok op, b)
  | `Arit_exp_BARBAR_arit_exp (a, tok, b)
  | `Arit_exp_AMPAMP_arit_exp (a, tok, b)
  | `Arit_exp_BAR_arit_exp (a, tok, b)
  | `Arit_exp_HAT_arit_exp (a, tok, b)
  | `Arit_exp_AMP_arit_exp (a, tok, b)
  | `Arit_exp_STARSTAR_arit_exp (a, tok, b) ->
      (a, tok, b)

let terminator (env : env) (x : CST.terminator) : unary_control_operator wrap =
  match x with
  | `SEMI tok -> (Foreground Fg_semi, token env tok (* ";" *))
  | `SEMISEMI tok -> (Foreground Fg_semisemi, token env tok (* ";;" *))
  | `Pat_1d78758 tok -> (Foreground Fg_newline, token env tok (* "\n" *))
  | `AMP tok -> (Background, token env tok (* "&" *))

let pipe_bar (env : env) (x : CST.anon_choice_BAR_9062646) : pipeline_bar wrap =
  match x with
  | `BAR tok -> (Bar, token env tok (* "|" *))
  | `BARAMP tok -> (Bar_ampersand, token env tok (* "|&" *))

let simple_variable_name (env : env) (x : CST.simple_variable_name) :
    variable_name =
  match x with
  | `Semg_meta tok ->
      (* pattern \$[A-Z_][A-Z_0-9]* *) Var_semgrep_metavar (str env tok)
  | `Pat_42e353e tok -> (* pattern \w+ *) Simple_variable_name (str env tok)

let special_variable_name (env : env) (x : CST.special_variable_name) :
    variable_name =
  Special_variable_name
    (match x with
    | `STAR tok (* "*" *)
    | `AT tok (* "@" *)
    | `QMARK tok (* "?" *)
    | `BANG tok (* "!" *)
    | `HASH tok (* "#" *)
    | `DASH tok (* "-" *)
    | `DOLLAR tok (* "$" *)
    | `X__ tok (* "_" *) ->
        str env tok)

let word (env : env) (tok : CST.word) : expression =
  match str env tok with
  | "...", tok when env.extra =*= Pattern -> Expr_semgrep_ellipsis tok
  | x -> Word x

(* An arithmetic operator, kept as its source text e.g. "+" or "<<=" *)
let arith_op (env : env) (tok : ts_tok) : string wrap = str env tok

(* Split a token such as '$X=' or '$X+=' into the metavariable and the
   operator, whose length is 'op_len'. *)
let split_metavar_op (env : env) (tok : ts_tok) ~op_len : string wrap * tok =
  let tok = token env tok in
  let len = String.length (Tok.content_of_tok tok) in
  let mv_tok, op_tok = Tok.split_tok_at_bytepos (len - op_len) tok in
  ((Tok.content_of_tok mv_tok, mv_tok), op_tok)

(* "$`" is a literal '$' followed by a backquote; split so the '$' is kept. *)
let split_dollar_bquot (env : env) (tok : ts_tok) : string wrap * tok =
  let tok = token env tok in
  let dollar_tok, bquot_tok = Tok.split_tok_at_bytepos 1 tok in
  ((Tok.content_of_tok dollar_tok, dollar_tok), bquot_tok)

(* Function identifier. These can contain some punctuation. '...' is a valid
   function identifier. *)
let extended_word (env : env) (x : CST.extended_word) =
  match x with
  | `Semg_meta tok -> Var_semgrep_metavar (str env tok)
  | `Word tok -> Simple_variable_name (str env tok)

(* '{1..10}' is expanded at run time into words; we keep it as one word. *)
let brace_expression (env : env) ((v1, v2, v3, v4, v5) : CST.brace_expression) :
    expression =
  let first = token env v1 in
  let tok = Tok.combine_toks first (List.map (token env) [ v2; v3; v4; v5 ]) in
  Word (Tok.content_of_tok tok, tok)

let simple_expansion (env : env) (x : CST.simple_expansion) : string_fragment =
  match x with
  | `DOLLAR_choice_orig_simple_var_name (v1, `Choice_STAR (`X__ tok)) ->
      let dollar_tok =
        token env v1
        (* "$" *)
      in
      let name_s, name_tok = (* "_" *) str env tok in
      let mv_s = "$" ^ name_s in
      let mv_tok = Tok.combine_toks dollar_tok [ name_tok ] in
      Frag_semgrep_metavar (mv_s, mv_tok)
  | `DOLLAR_choice_orig_simple_var_name (v1, v2) -> (
      let dollar_tok =
        token env v1
        (* "$" *)
      in
      let var_name =
        match v2 with
        | `Orig_simple_var_name tok ->
            (* pattern \w+ *)
            Simple_variable_name (str env tok)
        | `Choice_STAR x -> special_variable_name env x
        | `BANG tok -> Special_variable_name (str env tok (* "!" *))
        | `HASH tok -> Special_variable_name (str env tok (* "#" *))
      in
      let _name_s, name_tok = variable_name_wrap var_name in
      let loc = (dollar_tok, name_tok) in
      match env.extra with
      | Pattern -> (
          (* Interpret $X as either "metavariable $X" or "expand X" *)
          match var_name with
          | Simple_variable_name (name_s, name_tok)
            when AST_generic.is_metavar_name ("$" ^ name_s) ->
              let mv_s = "$" ^ name_s in
              let mv_tok = Tok.combine_toks dollar_tok [ name_tok ] in
              Frag_semgrep_metavar (mv_s, mv_tok)
          | _ -> Expansion (loc, Simple_expansion (loc, var_name)))
      | Program -> Expansion (loc, Simple_expansion (loc, var_name)))
  | `Semg_named_ellips tok -> Frag_semgrep_named_ellipsis (str env tok)

let simple_expansion_expr (env : env) (x : CST.simple_expansion) : expression =
  let frag = simple_expansion env x in
  String_fragment (AST_bash_loc.string_fragment_loc frag, frag)

(* The first element of a concatenation can't be a comment_word. *)
type concatenation_element =
  [ `Choice_semg_deep_exp of CST.primary_expression
  | `Spec_char of CST.special_character
  | `Comm_word of CST.comment_word
  | `Bare_dollar of CST.bare_dollar ]

let rec concatenation_element (env : env) (x : concatenation_element) :
    expression =
  match x with
  | `Choice_semg_deep_exp x -> primary_expression env x
  | `Spec_char tok -> Special_character (str env tok)
  | `Comm_word tok
  | `Bare_dollar tok ->
      Word (str env tok)

and array_ (env : env) ((v1, v2, v3) : CST.array_) =
  let open_ =
    token env v1
    (* "(" *)
  in
  let elements = List.map (literal env) v2 in
  let close =
    token env v3
    (* ")" *)
  in
  let loc = (open_, close) in
  Array (loc, (open_, elements, close))

and case_pattern (env : env) (x : CST.anon_choice_lit_af2a0ae) : expression =
  match x with
  | `Choice_conc x -> literal env x
  | `Extg_blob x -> extglob_blob env x

(* Shared by case_item and last_case_item. *)
and case_clause (env : env) (v1, v2, v3) ~body ~terminator ~end_tok :
    case_clause =
  let first_pattern = case_pattern env v1 in
  let more_patterns =
    List.map (fun (_bar (* "|" *), pat) -> case_pattern env pat) v2
  in
  let patterns = first_pattern :: more_patterns in
  let loc = (fst (AST_bash_loc.expression_loc first_pattern), end_tok) in
  (loc, patterns, v3, body, terminator)

and case_item (env : env)
    (( `Opt_LPAR_choice_choice_conc_rep_BAR_choice_choice_conc_RPAR
         (_lpar, v1, v2, v3),
       v4,
       v5 ) :
      CST.case_item) : case_clause =
  let paren =
    token env v3
    (* ")" *)
  in
  let body = program ~tok:paren env v4 in
  let terminator, end_tok =
    match v5 with
    | `SEMISEMI tok ->
        let tok =
          token env tok
          (* ";;" *)
        in
        (Break tok, tok)
    | `Choice_SEMIAMP (`SEMIAMP tok) ->
        let tok =
          token env tok
          (* ";&" *)
        in
        (Fallthrough tok, tok)
    | `Choice_SEMIAMP (`SEMISEMIAMP tok) ->
        let tok =
          token env tok
          (* ";;&" *)
        in
        (Try_next tok, tok)
  in
  case_clause env (v1, v2, paren) ~body ~terminator:(Some terminator) ~end_tok

(* This is the same as case_item except for the optional terminator. *)
and last_case_item (env : env)
    ((_lpar, v1, v2, v3, v4, v5) : CST.last_case_item) : case_clause =
  let paren =
    token env v3
    (* ")" *)
  in
  let body = program env ~tok:paren v4 in
  let terminator, end_tok =
    match v5 with
    | Some tok ->
        let tok =
          token env tok
          (* ";;" *)
        in
        (Some (Break tok), tok)
    | None -> (None, snd (AST_bash_loc.blist_loc body))
  in
  case_clause env (v1, v2, paren) ~body ~terminator ~end_tok

and command (env : env) ((v1, v2, v3) : CST.command) : cmd_redir =
  let assignments, prefix_redirects =
    Either_.partition
      (fun x ->
        match x with
        | `Var_assign x -> Left (variable_assignment env x)
        | `Choice_file_redi x -> Right (redirect env x))
      v1
  in
  let prefix_redirects, extra_args = List_.split prefix_redirects in
  let name = command_name env v2 in
  let args, arg_redirects =
    match v3 with
    | `Rep_choice_choice_conc xs ->
        Either_.partition
          (fun x ->
            match x with
            | `Translated_string (_dollar, x) -> Left (String (string_ env x))
            | `Choice_conc x -> Left (literal env x)
            | `Bare_dollar tok -> Left (Word (str env tok (* "$" *)))
            | `Choice_EQTILDE_choice_choice_conc (v1, v2) ->
                (* Not sure why we have this here. Should be only within
                   test commands [[ ... ]]. *)
                let eq =
                  match v1 with
                  | `EQTILDE tok -> (* "=~" *) EQTILDE (token env tok)
                  | `EQEQ tok -> (* "==" *) EQEQ (token env tok)
                in
                let right =
                  match v2 with
                  | `Choice_conc x ->
                      let e = literal env x in
                      Literal (AST_bash_loc.expression_loc e, e)
                  | `Regex tok -> (* regex *) Regexp (str env tok)
                in
                let loc =
                  Tok_range.range
                    (AST_bash_loc.eq_op_loc eq)
                    (AST_bash_loc.right_eq_operand_loc right)
                in
                Left (Equality_test (loc, eq, right))
            | `Here_redi x ->
                Right (Read_herestring (herestring_redirect env x)))
          (merge_translated_strings xs)
    | `Subs x ->
        (* e.g. 'time ( cd tests && make )' *)
        let sub = subshell env x in
        let loc = AST_bash_loc.bracket_loc sub in
        ([ Command_expression (loc, Subshell (loc, sub)) ], [])
  in
  let arguments = List_.flatten extra_args @ (name :: args) in
  let loc =
    let loc1 = Tok_range.of_list AST_bash_loc.assignment_loc assignments in
    let loc2 = Tok_range.of_list AST_bash_loc.expression_loc arguments in
    Tok_range.of_locs [ loc1; loc2 ]
  in
  let command = Simple_command { loc; assignments; arguments } in
  { loc; command; redirects = prefix_redirects @ arg_redirects }

(* The scanner splits a command argument '$"..."' (a string subject to
   translation) into a bare '$' and a string. We rejoin them when they're
   adjacent. *)
and merge_translated_strings xs =
  match xs with
  | `Bare_dollar (((dollar_loc : Tree_sitter_run.Loc.t), _) as dollar)
    :: `Choice_conc
         (`Choice_semg_deep_exp
            (`Choice_word (`Str (((quote_loc, _), _, _, _) as x))))
    :: xs
    when dollar_loc.end_.row =|= quote_loc.start.row
         && dollar_loc.end_.column =|= quote_loc.start.column ->
      `Translated_string (dollar, x) :: merge_translated_strings xs
  | (( `Bare_dollar _ | `Choice_conc _ | `Choice_EQTILDE_choice_choice_conc _
     | `Here_redi _ ) as x)
    :: xs ->
      x :: merge_translated_strings xs
  | [] -> []

and command_name (env : env) (x : CST.command_name) : expression =
  match x with
  | `Conc x ->
      let el = concatenation env x in
      let loc = Tok_range.of_list AST_bash_loc.expression_loc el in
      Concatenation (loc, el)
  | `Choice_semg_deep_exp x -> primary_expression env x
  | `Rep1_spec_char xs ->
      let el = List.map (fun tok -> Special_character (str env tok)) xs in
      let loc = Tok_range.of_list AST_bash_loc.expression_loc el in
      Concatenation (loc, el)

(* Optional leading literal '$' from $`...` (bash keeps it), then the
   substitution bracket. *)
and command_substitution_parts (env : env) (x : CST.command_substitution) :
    string wrap option * blist bracket =
  match x with
  | `DOLLARLPAR_stmts_RPAR (v1, v2, v3) ->
      let open_ =
        token env v1
        (* "$(" *)
      in
      let list = statements env v2 in
      let close =
        token env v3
        (* ")" *)
      in
      (None, (open_, list, close))
  | `DOLLARLPAR_file_redi_RPAR (v1, v2, v3) ->
      let open_ =
        token env v1
        (* "$(" *)
      in
      let _v2 = (* TODO: what's this? *) file_redirect env v2 in
      let close =
        token env v3
        (* ")" *)
      in
      let loc = (open_, close) in
      (None, (open_, Empty loc, close))
  | `BQUOT_stmts_BQUOT (v1, v2, v3) ->
      let open_ =
        token env v1
        (* "`" *)
      in
      let list = statements env v2 in
      let close =
        token env v3
        (* "`" *)
      in
      (None, (open_, list, close))
  | `DOLLARBQUOT_stmts_BQUOT (v1, v2, v3) ->
      let dollar, open_ = split_dollar_bquot env v1 in
      let list = statements env v2 in
      let close =
        token env v3
        (* "`" *)
      in
      (Some dollar, (open_, list, close))

and cmd_subs_expr (env : env) (x : CST.command_substitution) : expression =
  let dollar_opt, bracket = command_substitution_parts env x in
  let frag = Command_substitution bracket in
  let subst = String_fragment (AST_bash_loc.string_fragment_loc frag, frag) in
  match dollar_opt with
  | None -> subst
  | Some dollar ->
      let el = [ Word dollar; subst ] in
      Concatenation (Tok_range.of_list AST_bash_loc.expression_loc el, el)

(* '{ ... }' or '(( ... ))' *)
and compound_statement (env : env) (x : CST.compound_statement) : command =
  match x with
  | `LCURL_opt_choice_rep1_choice_choice_redi_stmt_choice_SEMI_tok_prec_n1_rcurl
      (v1, v2, v3) ->
      let open_ =
        token env v1
        (* "{" *)
      in
      let close =
        token env v3
        (* "}" *)
      in
      let loc = (open_, close) in
      let blist = opt_terminated_statement env loc v2 in
      Command_group (loc, (open_, blist, close))
  | `LPARLPAR_rep_arit_exp_COMMA_arit_exp_RPARRPAR (v1, v2, v3, v4) ->
      let open_ =
        token env v1
        (* "((" *)
      in
      let close =
        token env v4
        (* "))" *)
      in
      (* 'e1, e2, ..., en' where v2 holds the comma-terminated ones *)
      let e =
        match v2 with
        | [] -> arithmetic env v3
        | (e1, comma1) :: more ->
            let acc, last_comma =
              List.fold_left
                (fun (acc, comma) (e, next_comma) ->
                  ( A_binary (acc, arith_op env comma, arithmetic env e),
                    next_comma ))
                (arithmetic env e1, comma1)
                more
            in
            A_binary (acc, arith_op env last_comma, arithmetic env v3)
      in
      Arithmetic_expression ((open_, close), (open_, e, close))

and concatenation (env : env) ((v1, v2, v3) : CST.concatenation) :
    expression list =
  let first_expr = concatenation_element env (v1 :> concatenation_element) in
  let exprs = List.map (fun (_concat, v2) -> concatenation_element env v2) v2 in
  let opt_last_expr =
    match v3 with
    | Some (_concat, v2) -> [ word env v2 (* "$" *) ]
    | None -> []
  in
  (first_expr :: exprs) @ opt_last_expr

and do_group (env : env) ((v1, v2, v3) : CST.do_group) : blist bracket =
  let do_ =
    token env v1
    (* "do" *)
  in
  let done_ =
    token env v3
    (* "done" *)
  in
  (do_, opt_terminated_statement env (do_, done_) v2, done_)

and elif_clause (env : env) ((v1, v2, v3, v4) : CST.elif_clause) : elif =
  let elif =
    token env v1
    (* "elif" *)
  in
  let cond = terminated_statement env v2 in
  let then_ =
    token env v3
    (* "then" *)
  in
  let body = opt_terminated_statement env (then_, then_) v4 in
  let loc = (elif, snd (AST_bash_loc.blist_loc body)) in
  (loc, elif, cond, then_, body)

and else_clause (env : env) ((v1, v2) : CST.else_clause) : else_ =
  let else_ =
    token env v1
    (* "else" *)
  in
  let body = opt_terminated_statement env (else_, else_) v2 in
  let loc = (else_, snd (AST_bash_loc.blist_loc body)) in
  (loc, else_, body)

(* Only the expanded variable is kept; prefixes such as '#' or '!' and
   operations such as ':-default' or '#pattern' are ignored. *)
and expansion (env : env) ((v1, v2, v3) : CST.expansion) :
    complex_expansion bracket =
  let open_ =
    token env v1
    (* "${" *)
  in
  let opt_variable =
    match v2 with
    | None
    | Some (`Rep1_choice_exte_expa_sym_hash _) ->
        None
    | Some (`Opt_imm_tok_bang_choice_var_name_choice_expa_exp (_bang, v, _op))
      -> (
        match v with
        | `Var_name tok -> Some (Simple_variable_name (str env tok))
        | `Choice_semg_meta x -> Some (simple_variable_name env x)
        | `Choice_STAR x -> Some (special_variable_name env x)
        | `Subs _ -> None)
    | Some
        (`Imm_tok_bang_choice_choice_semg_meta_opt_choice_imm_tok_at
           (_bang, v, _suffix)) -> (
        match v with
        | `Choice_semg_meta x -> Some (simple_variable_name env x)
        | `Var_name tok -> Some (Simple_variable_name (str env tok)))
    | Some
        (`Opt_choice_imm_tok_hash_choice_subs_rep_choice_exte_expa_sym_hash
           (_prefix, v, _suffix)) -> (
        match v with
        | `Subs _
        | `Cmd_subs _ ->
            None
        | `Choice_semg_meta x -> Some (simple_variable_name env x)
        | `Choice_STAR x -> Some (special_variable_name env x))
  in
  let close =
    token env v3
    (* "}" *)
  in
  let complex_expansion =
    match opt_variable with
    | Some var ->
        let loc = AST_bash_loc.variable_name_loc var in
        Variable (loc, var)
    | None ->
        let loc = (open_, close) in
        Complex_expansion_TODO loc
  in
  (open_, complex_expansion, close)

and expansion_expr (env : env) (x : CST.expansion) : expression =
  let e = expansion env x in
  let loc = AST_bash_loc.bracket_loc e in
  String_fragment (loc, Expansion (loc, Complex_expansion e))

(* Extended glob patterns such as '*' or '+(a|b)', possibly around a
   string or an expansion. *)
and extglob_blob (env : env) (x : CST.extglob_blob) : expression =
  match x with
  | `Extg_pat tok -> word env tok
  | `Extg_pat_choice_str_opt_extg_pat (v1, v2, v3) ->
      let middle =
        match v2 with
        | `Str x -> String (string_ env x)
        | `Expa x -> expansion_expr env x
        | `Cmd_subs x -> cmd_subs_expr env x
      in
      let el =
        (word env v1 :: [ middle ]) @ Option.to_list (Option.map (word env) v3)
      in
      Concatenation (Tok_range.of_list AST_bash_loc.expression_loc el, el)

(* This covers
   - sh test commands: [ ... ]
   - bash test commands: [[ ... ]]
   Arithmetic, ternary and binary expressions aren't modeled.
*)
and expression (env : env) (x : CST.expression) : test_expression =
  match x with
  | `Choice_conc x ->
      let e = literal env x in
      T_expr (AST_bash_loc.expression_loc e, e)
  | `Un_exp (`BANG_exp (v1, v2)) ->
      let e = expression env v2 in
      let bang = token env v1 in
      let loc = (bang, snd (AST_bash_loc.test_expression_loc e)) in
      T_not (loc, bang, e)
  | `Un_exp (`Test_op_exp (v1, v2)) -> (
      match expression env v2 with
      | T_expr (e_loc, e) ->
          let ((_, op_tok) as op) = unary_test_operator env v1 in
          let loc = (op_tok, snd e_loc) in
          T_unop (loc, op, e)
      | e ->
          (* This doesn't make sense since unary operators like '-e'
             can only be applied to strings. *)
          e)
  | `Un_exp _
  | `Tern_exp _
  | `Bin_exp _
  | `Post_exp _ ->
      T_todo (AST_bash_loc.arithmetic_expression_loc (expression_arith env x))
  | `Paren_exp (_open, e, _close) -> expression env e

(* The arithmetic reading of a test expression, as used in array subscripts
   e.g. 'a[i + 1]' *)
and expression_arith (env : env) (x : CST.expression) : arithmetic_expression =
  let rec_ = expression_arith env in
  match x with
  | `Choice_conc x -> A_operand (literal env x)
  | `Un_exp (`Choice_tok_prec_p1_plusps_exp (o, e)) ->
      A_unary (arith_op env (op_tok o), rec_ e)
  | `Un_exp (`Choice_tok_prec_p1_dash_exp (o, e)) ->
      A_unary (arith_op env (op_tok o), rec_ e)
  | `Un_exp (`BANG_exp (tok, e))
  | `Un_exp (`Test_op_exp (tok, e)) ->
      A_unary (arith_op env tok, rec_ e)
  | `Tern_exp (e1, qmark, e2, colon, e3) ->
      A_ternary (rec_ e1, token env qmark, rec_ e2, token env colon, rec_ e3)
  | `Bin_exp (`Choice_exp_choice_PLUSEQ_exp x) ->
      let e1, tok, e2 = binary_parts x in
      A_binary (rec_ e1, arith_op env tok, rec_ e2)
  | `Bin_exp (`Exp_EQTILDE_regex_no_sp (e, tok, regex)) ->
      A_binary (rec_ e, arith_op env tok, A_operand (Word (str env regex)))
  | `Bin_exp (`Exp_choice_EQEQ_extg_blob (e, o, blob)) ->
      A_binary
        (rec_ e, arith_op env (op_tok o), A_operand (extglob_blob env blob))
  | `Post_exp (e, o) -> A_postfix (rec_ e, arith_op env (op_tok o))
  | `Paren_exp (open_, e, close) ->
      A_paren (token env open_, rec_ e, token env close)

and arithmetic (env : env) (x : CST.arithmetic_expression) :
    arithmetic_expression =
  match x with
  | `Arit_lit x -> arithmetic_literal env x
  | `Arit_un_exp (`Choice_tok_prec_p1_plusps_arit_exp (o, e)) ->
      A_unary (arith_op env (op_tok o), arithmetic env e)
  | `Arit_un_exp (`Choice_tok_prec_p1_dash_arit_exp (o, e)) ->
      A_unary (arith_op env (op_tok o), arithmetic env e)
  | `Arit_un_exp (`BANG_arit_exp (tok, e)) ->
      A_unary (arith_op env tok, arithmetic env e)
  | `Arit_tern_exp (e1, qmark, e2, colon, e3) ->
      A_ternary
        ( arithmetic env e1,
          token env qmark,
          arithmetic env e2,
          token env colon,
          arithmetic env e3 )
  | `Arit_bin_exp x ->
      let e1, tok, e2 = arithmetic_binary_parts x in
      A_binary (arithmetic env e1, arith_op env tok, arithmetic env e2)
  | `Arit_post_exp (e, o) ->
      A_postfix (arithmetic env e, arith_op env (op_tok o))
  | `Arit_paren_exp (open_, e, close) ->
      A_paren (token env open_, arithmetic env e, token env close)
  | `Cmd_subs x -> A_operand (cmd_subs_expr env x)

(* 'e1, e2, ...' where the comma operator is left-associative *)
and arithmetic_comma_list (env : env) (first : arithmetic_expression)
    (rest : (ts_tok * arithmetic_expression) list) : arithmetic_expression =
  List.fold_left
    (fun acc (comma, e) -> A_binary (acc, arith_op env comma, e))
    first rest

and arithmetic_literal (env : env) (x : CST.arithmetic_literal) :
    arithmetic_expression =
  match x with
  | `DOTDOTDOT tok -> A_operand (word env tok)
  | `Semg_deep_exp x -> A_operand (semgrep_deep_expression env x)
  | `Num x -> A_operand (number env x)
  | `Subs x -> array_cell env x
  | `Simple_expa x -> A_operand (simple_expansion_expr env x)
  | `Expa x -> A_operand (expansion_expr env x)
  | `Orig_simple_var_name tok
  | `Var_name tok ->
      (* a bare variable name *)
      A_operand (word env tok)
  | `Str x -> A_operand (String (string_ env x))
  | `Raw_str tok -> A_operand (Raw_string (str env tok))

(* A comma-separated clause of 'for (( ...; ...; ... ))' *)
and c_clause (env : env) ((v1, v2) : CST.anon_c_exp_rep_COMMA_c_exp_f9a26b6) :
    arithmetic_expression =
  arithmetic_comma_list env (c_expression env v1)
    (List.map (fun (comma, e) -> (comma, c_expression env e)) v2)

and c_expression (env : env) (x : CST.c_expression) : arithmetic_expression =
  match x with
  | `C_exp_not_assign x -> c_expression_not_assignment env x
  | `C_var_assign (`Semg_meta_eq_c_exp (v1, v2)) ->
      (* '$X=' *)
      let mv, eq_tok = split_metavar_op env v1 ~op_len:1 in
      let lhs =
        match env.extra with
        | Pattern -> Expr_semgrep_metavar mv
        | Program -> Word mv
      in
      A_binary
        (A_operand lhs, (Tok.content_of_tok eq_tok, eq_tok), c_expression env v2)
  | `C_var_assign (`Pat_2b6adbc_EQ_c_exp (v1, v2, v3)) ->
      A_binary (A_operand (word env v1), arith_op env v2, c_expression env v3)

and c_expression_not_assignment (env : env)
    (x : CST.c_expression_not_assignment) : arithmetic_expression =
  let rec_ = c_expression_not_assignment env in
  match x with
  | `DOTDOTDOT tok -> A_operand (word env tok)
  | `Choice_pat_2b6adbc x -> (
      match x with
      | `Pat_2b6adbc tok -> A_operand (word env tok)
      | `Simple_expa x -> A_operand (simple_expansion_expr env x)
      | `Expa x -> A_operand (expansion_expr env x)
      | `Num x -> A_operand (number env x)
      | `Str x -> A_operand (String (string_ env x))
      | `C_un_exp (o, e) -> A_unary (arith_op env (op_tok o), rec_ e)
      | `C_post_exp (e, o) -> A_postfix (rec_ e, arith_op env (op_tok o))
      | `C_bin_exp x ->
          let e1, tok, e2 =
            match x with
            | `C_exp_not_assign_choice_PLUSEQ_c_exp_not_assign (a, o, b) ->
                (a, op_tok o, b)
            | `C_exp_not_assign_choice_BARBAR_c_exp_not_assign (a, o, b) ->
                (a, op_tok o, b)
            | `C_exp_not_assign_choice_AMPAMP_c_exp_not_assign (a, o, b) ->
                (a, op_tok o, b)
            | `C_exp_not_assign_choice_EQEQ_c_exp_not_assign (a, o, b) ->
                (a, op_tok o, b)
            | `C_exp_not_assign_choice_LT_c_exp_not_assign (a, o, b) ->
                (a, op_tok o, b)
            | `C_exp_not_assign_choice_LTLT_c_exp_not_assign (a, o, b) ->
                (a, op_tok o, b)
            | `C_exp_not_assign_choice_PLUS_c_exp_not_assign (a, o, b) ->
                (a, op_tok o, b)
            | `C_exp_not_assign_choice_STAR_c_exp_not_assign (a, o, b) ->
                (a, op_tok o, b)
            | `C_exp_not_assign_BAR_c_exp_not_assign (a, tok, b)
            | `C_exp_not_assign_HAT_c_exp_not_assign (a, tok, b)
            | `C_exp_not_assign_AMP_c_exp_not_assign (a, tok, b)
            | `C_exp_not_assign_STARSTAR_c_exp_not_assign (a, tok, b) ->
                (a, tok, b)
          in
          A_binary (rec_ e1, arith_op env tok, rec_ e2)
      | `C_paren_exp (open_, e, es, close) ->
          A_paren (token env open_, c_clause env (e, es), token env close)
      | `Cmd_subs x -> A_operand (cmd_subs_expr env x))

(* '$(( ... ))' or the deprecated '$[ ... ]' *)
and arithmetic_expansion (env : env) (x : CST.arithmetic_expansion) :
    string_fragment =
  match x with
  | `DOLLARLPARLPAR_arit_exp_rep_COMMA_arit_exp_RPARRPAR (v1, v2, v3, v4) ->
      let e =
        arithmetic_comma_list env (arithmetic env v2)
          (List.map (fun (comma, e) -> (comma, arithmetic env e)) v3)
      in
      Arithmetic_expansion (token env v1, e, token env v4)
  | `DOLLARLBRACK_arit_exp_RBRACK (v1, v2, v3) ->
      Arithmetic_expansion (token env v1, arithmetic env v2, token env v3)

and file_redir_target (x : expression) : file_redir_target =
  match x with
  | Word ("", tok) -> (* following &> or >& *) Stdout_and_stderr tok
  | Word ("-", tok) -> Close_fd tok
  | Word (s, loc) as e -> (
      match int_of_string_opt s with
      | None -> File e
      | Some fd -> File_descriptor (fd, loc))
  | e -> File e

(* Returns the redirect and the arguments of the command that follow its
   target e.g. 'b' in 'echo a > out b'. *)
and file_redirect (env : env) ((v1, v2) : CST.file_redirect) :
    redirect * expression list =
  let fd_locs, opt_src_fd =
    match v1 with
    | Some tok -> (
        let s, tok = str env tok in
        ( [ (tok, tok) ],
          match int_of_string_opt s with
          | None -> (* bug *) None
          | Some fd -> Some (File_descriptor (fd, tok) : write_redir_src) ))
    | None -> ([], None)
  in
  (* This is for the operators like '>' *)
  let src_fd1 tok : write_redir_src =
    match opt_src_fd with
    | None -> Stdout tok
    | Some x -> x
  in
  (* This is for the operators like '&>' *)
  let src_fd2 tok : write_redir_src =
    match opt_src_fd with
    | None -> Stdout_and_stderr tok
    | Some x -> x
  in
  let tok, file_redir, target_locs, extra_args =
    match v2 with
    | `Choice_LT_rep1_choice_conc (op, v2) ->
        let target_e, extra_args =
          match List.map (literal env) v2 with
          | target_e :: extra_args -> (target_e, extra_args)
          | [] ->
              failwith "Parse_bash_tree_sitter: file redirect without target"
        in
        let target = file_redir_target target_e in
        let tok, file_redir =
          match op with
          | `LT tok ->
              let tok =
                token env tok
                (* "<" *)
              in
              (tok, Read (tok, target))
          | `GT tok ->
              let tok =
                token env tok
                (* ">" *)
              in
              (tok, Write (src_fd1 tok, (Write_truncate, tok), target))
          | `GTGT tok ->
              let tok =
                token env tok
                (* ">>" *)
              in
              (tok, Write (src_fd1 tok, (Write_append, tok), target))
          | `AMPGT tok ->
              let tok =
                token env tok
                (* "&>" *)
              in
              (tok, Write (src_fd2 tok, (Write_truncate, tok), target))
          | `AMPGTGT tok ->
              let tok =
                token env tok
                (* "&>>" *)
              in
              (tok, Write (src_fd2 tok, (Write_append, tok), target))
          | `LTAMP tok ->
              let tok =
                token env tok
                (* "<&" *)
              in
              (tok, Read (tok, target))
          | `GTAMP tok -> (
              let tok =
                token env tok
                (* ">&" *)
              in
              (* ">&" alone is different than ">&1"! *)
              match target with
              | Stdout_and_stderr _ ->
                  (* '>&' *)
                  (tok, Write (src_fd2 tok, (Write_truncate, tok), target))
              | _ ->
                  (* '>&2' etc. *)
                  (tok, Write (src_fd1 tok, (Write_truncate, tok), target)))
          | `GTBAR tok ->
              let tok =
                token env tok
                (* ">|" *)
              in
              (tok, Write (src_fd1 tok, (Write_force_truncate, tok), target))
        in
        (tok, file_redir, [ AST_bash_loc.expression_loc target_e ], extra_args)
    | `Choice_LTAMPDASH_opt_choice_conc (op, v2) ->
        (* Close a file descriptor. What follows is an ordinary argument. *)
        let extra_args = Option.to_list (Option.map (literal env) v2) in
        let tok, file_redir =
          match op with
          | `LTAMPDASH tok ->
              let tok =
                token env tok
                (* "<&-" *)
              in
              (tok, Read (tok, Close_fd tok))
          | `GTAMPDASH tok ->
              let tok =
                token env tok
                (* ">&-" *)
              in
              (tok, Write (src_fd1 tok, (Write_truncate, tok), Close_fd tok))
        in
        (tok, file_redir, [], extra_args)
  in
  let loc = Tok_range.of_locs (((tok, tok) :: fd_locs) @ target_locs) in
  (File_redirect (loc, file_redir), extra_args)

(* Here-documents aren't modeled beyond their location. Whatever follows
   the delimiter on the same line (pipeline, '&&', '||', more redirects,
   arguments) applies to the command being redirected. *)
and heredoc_redirect (env : env) (pip : pipeline)
    ((_fd, v1, _start, v3, _newline, v5) : CST.heredoc_redirect) : pipeline =
  let start_ =
    match v1 with
    | `LTLT tok (* "<<" *)
    | `LTLTDASH tok (* "<<-" *) ->
        token env tok
  in
  let end_ =
    match v5 with
    | `Here_body (_, tok)
    | `Simple_here_body (_, tok) ->
        (* heredoc_end *) token env tok
  in
  let heredoc = Read_heredoc (TODO (start_, end_)) in
  let add ?args pip redirects =
    AST_bash_builder.add_redirects_to_last_command_of_pipeline ?args pip
      redirects
  in
  match v3 with
  | None -> add pip [ heredoc ]
  | Some (`Here_cmd xs) -> add ~args:(List.map (literal env) xs) pip [ heredoc ]
  | Some (`Here_pipe (bar, stmt)) ->
      append_pipeline (add pip [ heredoc ]) (pipe_bar env bar)
        (pipeline_statement env stmt)
  | Some (`Here_exp x) -> heredoc_expression env (add pip [ heredoc ]) x
  | Some (`Rep1_choice_file_redi_opt_here_exp (redirects, opt_exp)) -> (
      let redirects, args = List_.split (List.map (redirect env) redirects) in
      let pip = add ~args:(List_.flatten args) pip (heredoc :: redirects) in
      match opt_exp with
      | None -> pip
      | Some x -> heredoc_expression env pip x)

(* '&& cmd' or '|| cmd' after a heredoc delimiter *)
and heredoc_expression (env : env) (left : pipeline)
    ((v1, v2) : CST.heredoc_expression) : pipeline =
  let right = pipeline_statement env v2 in
  let loc =
    Tok_range.range
      (AST_bash_loc.pipeline_loc left)
      (AST_bash_loc.pipeline_loc right)
  in
  let command =
    match v1 with
    | `AMPAMP tok -> And (loc, left, token env tok, right)
    | `BARBAR tok -> Or (loc, left, token env tok, right)
  in
  Command { loc; command; redirects = [] }

and herestring_redirect (env : env) ((_fd, v1, v2) : CST.herestring_redirect) =
  let op =
    token env v1
    (* "<<<" *)
  in
  let e = literal env v2 in
  let loc = (op, snd (AST_bash_loc.expression_loc e)) in
  TODO loc

and redirect (env : env) (x : CST.redirect) : redirect * expression list =
  match x with
  | `File_redi x -> file_redirect env x
  | `Here_redi x -> (Read_herestring (herestring_redirect env x), [])

and literal (env : env) (x : CST.literal) : expression =
  match x with
  | `Conc x -> (
      let el = concatenation env x in
      let loc = Tok_range.of_list AST_bash_loc.expression_loc el in
      match el with
      | [ e ] -> e
      | _ -> Concatenation (loc, el))
  | `Choice_semg_deep_exp x -> primary_expression env x
  | `Rep1_spec_char xs -> (
      let el = List.map (fun tok -> Special_character (str env tok)) xs in
      let loc = Tok_range.of_list AST_bash_loc.expression_loc el in
      match el with
      | [ e ] -> e
      | _ -> Concatenation (loc, el))

(* A number, possibly with a base e.g. '2#101' or '16#${digits}' *)
and number (env : env) (x : CST.number) : expression =
  match x with
  | `Pat_421f39f tok -> Word (str env tok)
  | `Pat_b978cc7_choice_expa (v1, v2) ->
      let base = Word (str env v1) in
      let digits =
        match v2 with
        | `Expa x -> expansion_expr env x
        | `Cmd_subs x -> cmd_subs_expr env x
      in
      let el = [ base; digits ] in
      Concatenation (Tok_range.of_list AST_bash_loc.expression_loc el, el)

and semgrep_deep_expression (env : env)
    ((v1, v2, v3) : CST.semgrep_deep_expression) : expression =
  let open_ = (* "<..." *) token env v1 in
  let e = literal env v2 in
  let close = (* "...>" *) token env v3 in
  let loc = (open_, close) in
  Expr_semgrep_deep_ellipsis (loc, (open_, e, close))

and primary_expression (env : env) (x : CST.primary_expression) : expression =
  match x with
  | `Semg_deep_exp x -> semgrep_deep_expression env x
  | `DOTDOTDOT tok -> word env tok
  | `Choice_word x -> (
      match x with
      | `Word tok -> word env tok (* word *)
      | `Str x -> String (string_ env x)
      | `Raw_str tok -> Raw_string (str env tok (* pattern "'[^']*'" *))
      | `Ansi_c_str tok ->
          Ansii_c_string (str env tok (* pattern "\\$'([^']|\\\\')*'" *))
      | `Num x -> number env x
      | `Test_op tok -> Word (str env tok)
      | `Tran_str (_dollar, x) ->
          (* $"..." is a string subject to translation by gettext. *)
          String (string_ env x)
      | `Arit_expa x ->
          let frag = arithmetic_expansion env x in
          String_fragment (AST_bash_loc.string_fragment_loc frag, frag)
      | `Expa x -> expansion_expr env x
      | `Simple_expa x -> simple_expansion_expr env x
      | `Cmd_subs x -> cmd_subs_expr env x
      | `Proc_subs (v1, v2, v3) ->
          let open_ =
            match v1 with
            | `LTLPAR tok -> token env tok (* "<(" *)
            | `GTLPAR tok -> (* ">(" *) token env tok
          in
          let body = statements env v2 in
          let close =
            token env v3
            (* ")" *)
          in
          let loc = (open_, close) in
          Process_substitution (loc, (open_, body, close))
      | `Brace_exp x -> brace_expression env x)

(* The token tok is needed to indicate the location of the list of statements
   in case it's empty. *)
and program (env : env) ~tok (opt : CST.program) : blist =
  match opt with
  | Some x -> statements env x
  | None -> Empty (tok, tok)

(*
   Read a statement as a list (of pipelines).
*)
and blist_statement (env : env) (x : CST.statement) : blist =
  match statement env x with
  | Tmp_pipeline x -> Pipelines (AST_bash_loc.pipeline_loc x, [ x ])
  | Tmp_command (cmd_r, control_op) -> (
      let loc = AST_bash_loc.cmd_redir_loc cmd_r in
      let cmd = Command cmd_r in
      match control_op with
      | None -> Pipelines (loc, [ cmd ])
      | Some op ->
          let _, end_ = AST_bash_loc.wrap_loc op in
          let loc2 = (fst loc, end_) in
          Pipelines (loc2, [ Control_operator (loc2, cmd, op) ]))

(*
   Read a statement as a pipeline where a pipeline or a single command
   is expected.
*)
and pipeline_statement (env : env) (x : CST.statement) : pipeline =
  pipeline_of_tmp_stmt (statement env x)

(*
   Do not call this function directly. Instead use one of the
   following depending on which construct is expected:
   - pipeline_statement
   - blist_statement
*)
and statement (env : env) (x : CST.statement) : tmp_stmt =
  match x with
  | `Choice_redi_stmt x -> statement_not_subshell env x
  | `Subs x -> subshell_statement env x

and subshell_statement (env : env) (x : CST.subshell) : tmp_stmt =
  let sub = subshell env x in
  let loc = AST_bash_loc.bracket_loc sub in
  let command = Subshell (loc, sub) in
  Tmp_command ({ loc; command; redirects = [] }, None)

and statement_not_pipeline (env : env) (x : CST.statement_not_pipeline) :
    tmp_stmt =
  match x with
  | `Subs x -> subshell_statement env x
  | ( `Redi_stmt _ | `Var_assign _ | `Var_assigns _ | `Cmd _ | `Decl_cmd _
    | `Unset_cmd _ | `Test_cmd _ | `Nega_cmd _ | `For_stmt _
    | `C_style_for_stmt _ | `While_stmt _ | `If_stmt _ | `Case_stmt _ | `List _
    | `Comp_stmt _ | `Func_defi _ ) as x ->
      statement_not_subshell env x

and redirected_statement (env : env) (x : CST.redirected_statement) : pipeline =
  match x with
  | `Choice_redi_stmt_rep1_choice_file_redi (v1, v2) ->
      (* Arguments may follow a file redirect e.g. 'echo a > /tmp/foo b'. *)
      List.fold_left
        (fun pip x ->
          match x with
          | `File_redi x ->
              let redirect, args = file_redirect env x in
              AST_bash_builder.add_redirects_to_last_command_of_pipeline ~args
                pip [ redirect ]
          | `Here_redi x -> heredoc_redirect env pip x)
        (pipeline_of_tmp_stmt (statement_not_pipeline env v1))
        v2
  | `Choice_if_stmt_here_redi (v1, v2) ->
      let command =
        match v1 with
        | `If_stmt x -> if_statement env x
        | `While_stmt x -> while_statement env x
      in
      let loc = AST_bash_loc.command_loc command in
      Command
        {
          loc;
          command;
          redirects = [ Read_herestring (herestring_redirect env v2) ];
        }
  | `Rep1_choice_file_redi xs ->
      let redirects, args = List_.split (List.map (redirect env) xs) in
      redirects_only env redirects (List_.flatten args)
  | `Here_redi x ->
      redirects_only env [ Read_herestring (herestring_redirect env x) ] []

(* A statement made only of redirects e.g. '> file' *)
and redirects_only (_env : env) (redirects : redirect list)
    (arguments : expression list) : pipeline =
  let loc =
    Tok_range.of_locs
      (List.map AST_bash_loc.redirect_loc redirects
      @ List.map AST_bash_loc.expression_loc arguments)
  in
  Command
    {
      loc;
      command = Simple_command { loc; assignments = []; arguments };
      redirects;
    }

and if_statement (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.if_statement) :
    command =
  let if_ =
    token env v1
    (* "if" *)
  in
  let cond = terminated_statement env v2 in
  let then_ =
    token env v3
    (* "then" *)
  in
  let body = opt_terminated_statement env (then_, then_) v4 in
  let elif_branches = List.map (elif_clause env) v5 in
  let else_branch = Option.map (else_clause env) v6 in
  let fi =
    token env v7
    (* "fi" *)
  in
  let loc = (if_, fi) in
  If (loc, if_, cond, then_, body, elif_branches, else_branch, fi)

and while_statement (env : env) ((v1, v2, v3) : CST.while_statement) : command =
  let cond = terminated_statement env v2 in
  let open_body, body, close_body = do_group env v3 in
  match v1 with
  | `While tok ->
      let while_ = token env tok in
      let loc = (while_, close_body) in
      While_loop (loc, while_, cond, open_body, body, close_body)
  | `Until tok ->
      let until = token env tok in
      let loc = (until, close_body) in
      Until_loop (loc, until, cond, open_body, body, close_body)

and statement_not_subshell (env : env) (x : CST.statement_not_subshell) :
    tmp_stmt =
  let simple command =
    let loc = AST_bash_loc.command_loc command in
    Tmp_command ({ loc; command; redirects = [] }, None)
  in
  match x with
  | `Redi_stmt x -> Tmp_pipeline (redirected_statement env x)
  | `Var_assign x ->
      let a = variable_assignment env x in
      let command = Assignment a in
      Tmp_command
        ({ loc = AST_bash_loc.assignment_loc a; command; redirects = [] }, None)
  | `Var_assigns (v1, v2) ->
      let assignments = List.map (variable_assignment env) (v1 :: v2) in
      let loc = Tok_range.of_list AST_bash_loc.assignment_loc assignments in
      simple (Simple_command { loc; assignments; arguments = [] })
  | `Cmd x -> Tmp_command (command env x, None)
  | `Decl_cmd (v1, v2) ->
      let (attrs1 : declaration_attribute wrap list), tok =
        match v1 with
        | `Decl tok -> (* "declare" *) ([], tok)
        | `Type tok -> (* "typeset" synonym for "declare" *) ([], tok)
        | `Export tok -> (* "export" *) ([ (Export, token env tok) ], tok)
        | `Read tok -> (* "readonly" *) ([ (Readonly, token env tok) ], tok)
        | `Local tok -> (* "local" *) ([ (Local, token env tok) ], tok)
      in
      let first_tok = token env tok in
      let last_tok = ref first_tok in
      let decls = ref [] in
      let assigns = ref [] in
      let attrs2 = ref [] in
      let unknowns = ref [] in
      let push_attr tok attr = Stack_.push (attr, tok) attrs2 in
      v2
      |> List.iter (fun x ->
          match x with
          | `Choice_conc x -> (
              match literal env x with
              | Word (s, tok) as expr ->
                  (match s with
                  | "-a" -> push_attr tok (Array : declaration_attribute)
                  | "-A" -> push_attr tok Associative_array
                  | "-f" -> push_attr tok Function
                  | "-F" -> push_attr tok Function_short
                  | "-g" -> push_attr tok Global
                  | "-i" -> push_attr tok Integer
                  | "-l" -> push_attr tok Lowercase
                  | "-n" -> push_attr tok Nameref
                  | "-p" -> push_attr tok Print
                  | "-r" -> push_attr tok Readonly
                  | "-t" -> push_attr tok Trace
                  | "-u" -> push_attr tok Uppercase
                  | "-x" -> push_attr tok Export
                  | _ -> Stack_.push expr unknowns);
                  last_tok := tok
              | e ->
                  Stack_.push e unknowns;
                  last_tok := snd (AST_bash_loc.expression_loc e))
          | `Choice_semg_meta x ->
              (* x, $X *)
              let var = simple_variable_name env x in
              last_tok := AST_bash_loc.variable_name_loc var |> snd;
              Stack_.push var decls
          | `Var_assign x ->
              (* x=42, $X=42 *)
              let assign = variable_assignment env x in
              last_tok := AST_bash_loc.assignment_loc assign |> snd;
              Stack_.push assign assigns);
      let decls = List.rev !decls in
      let assigns = List.rev !assigns in
      let attrs = attrs1 @ List.rev !attrs2 in
      let unknowns = List.rev !unknowns in
      let loc = (first_tok, !last_tok) in
      let decl : declaration =
        {
          loc;
          declarations = decls;
          assignments = assigns;
          attributes = attrs;
          unknowns;
        }
      in
      let command = Declaration decl in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `Unset_cmd (v1, v2) ->
      let ((_, first_tok) as attr) =
        match v1 with
        | `Unset tok (* "unset" *) ->
            let tok = token env tok in
            (Unset, tok)
        | `Unse tok (* "unsetenv" *) ->
            let tok = token env tok in
            (Unsetenv, tok)
      in
      let decls = ref [] in
      let unknowns = ref [] in
      let last_tok = ref first_tok in
      v2
      |> List.iter (fun x ->
          match x with
          | `Choice_conc x ->
              let e = literal env x in
              Stack_.push e unknowns;
              last_tok := AST_bash_loc.expression_loc e |> snd
          | `Choice_semg_meta tok ->
              let var = simple_variable_name env tok in
              Stack_.push var decls;
              last_tok := variable_name_tok var);
      let loc =
        (first_tok, !last_tok)
        (* TODO *)
      in
      let command =
        Declaration
          {
            loc;
            declarations = List.rev !decls;
            assignments = [];
            attributes = [ attr ];
            unknowns = List.rev !unknowns;
          }
      in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `Test_cmd x -> simple (test_command env x)
  | `Nega_cmd (v1, v2) ->
      let excl =
        token env v1
        (* "!" *)
      in
      let loc, command, redirects =
        match v2 with
        | `Cmd x ->
            let { loc; command; redirects } = command env x in
            (loc, command, redirects)
        | `Var_assign x ->
            let a = variable_assignment env x in
            (a.loc, Assignment a, [])
        | `Test_cmd x ->
            let command = test_command env x in
            let loc = AST_bash_loc.command_loc command in
            (loc, command, [])
        | `Subs x ->
            let sub = subshell env x in
            let loc = AST_bash_loc.bracket_loc sub in
            (loc, Subshell (loc, sub), [])
      in
      let loc = Tok_range.extend loc excl in
      let command = Negated_command (loc, excl, command) in
      Tmp_command ({ loc; command; redirects }, None)
  | `For_stmt (v1, v2, v3, v4, v5) ->
      let for_tok, is_for =
        match v1 with
        | `For tok -> (* "for" *) (token env tok, true)
        | `Select tok -> (* "select" *) (token env tok, false)
      in
      let loop_var = simple_variable_name env v2 in
      let opt_loop_vals =
        match v3 with
        | Some (v1, v2) ->
            let in_ =
              token env v1
              (* "in" *)
            in
            let values = List.map (literal env) v2 in
            Some (in_, values)
        | None ->
            (* iterate over $1, $2, ..., $# *)
            None
      in
      let _semi = terminator env v4 in
      let do_, body, done_ = do_group env v5 in
      let loc = (for_tok, done_) in
      let command =
        if is_for then
          For_loop (loc, for_tok, loop_var, opt_loop_vals, do_, body, done_)
        else Select (loc, for_tok, loop_var, opt_loop_vals, do_, body, done_)
      in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `C_style_for_stmt (v1, _header_open, `For_body v3, _header_close, _semi, v6)
    ->
      let for_ =
        token env v1
        (* "for" *)
      in
      let _body_open, body, body_close =
        match v6 with
        | `Do_group x -> do_group env x
        | `Comp_stmt x -> (
            match compound_statement env x with
            | Command_group (_loc, br) -> br
            | command ->
                let ((open_, close) as loc) =
                  AST_bash_loc.command_loc command
                in
                let cmd = Command { loc; command; redirects = [] } in
                (open_, Pipelines (loc, [ cmd ]), close))
      in
      let loc = (for_, body_close) in
      let init, _semi1, cond, _semi2, next = v3 in
      let clause = Option.map (c_clause env) in
      let header = (clause init, clause cond, clause next) in
      let command = For_loop_c_style (loc, for_, header, body) in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `While_stmt x -> simple (while_statement env x)
  | `If_stmt x -> simple (if_statement env x)
  | `Case_stmt (v1, v2, v3, v4, v5, v6, v7) ->
      let case =
        token env v1
        (* "case" *)
      in
      let subject = literal env v2 in
      let _term = Option.map (terminator env) v3 in
      let in_ =
        token env v4
        (* "in" *)
      in
      let _term = Option.map (terminator env) v5 in
      let case_clauses =
        match v6 with
        | Some (v1, v2) ->
            let cases = List.map (case_item env) v1 in
            let last_case = last_case_item env v2 in
            cases @ [ last_case ]
        | None -> []
      in
      let esac =
        token env v7
        (* "esac" *)
      in
      let loc = (case, esac) in
      let command = Case (loc, case, subject, in_, case_clauses, esac) in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `Pipe (v1, v2) ->
      (* Build the pipeline from left to right, adding one command at a
         time. *)
      let stage x = pipeline_of_tmp_stmt (statement_not_pipeline env x) in
      let pipeline =
        List.fold_left
          (fun left (bar, x) ->
            append_pipeline left (pipe_bar env bar) (stage x))
          (stage v1) v2
      in
      Tmp_pipeline pipeline
  | `List (v1, v2, v3) ->
      let left = pipeline_statement env v1 in
      let right = pipeline_statement env v3 in
      let loc =
        Tok_range.range
          (AST_bash_loc.pipeline_loc left)
          (AST_bash_loc.pipeline_loc right)
      in
      let command =
        match v2 with
        | `AMPAMP tok -> And (loc, left, token env tok, right)
        | `BARBAR tok -> Or (loc, left, token env tok, right)
      in
      Tmp_command ({ loc; command; redirects = [] }, None)
  | `Comp_stmt x -> simple (compound_statement env x)
  | `Func_defi (v1, v2, v3) ->
      let start, function_, name =
        match v1 with
        | `Func_exte_word_opt_LPAR_RPAR (v1, v2, _empty_parens) ->
            let function_ =
              token env v1
              (* "function" *)
            in
            let name = extended_word env v2 in
            (function_, Some function_, name)
        | `Choice_word_LPAR_RPAR (v1, _lpar, _rpar) ->
            let name =
              match v1 with
              | `Word tok
              | `DOTDOTDOT tok ->
                  str env tok
            in
            (snd name, None, Simple_variable_name name)
      in
      let body =
        match v2 with
        | `Comp_stmt x -> compound_statement env x
        | `Subs x ->
            let br = subshell env x in
            Subshell (AST_bash_loc.bracket_loc br, br)
        | `Test_cmd x -> test_command env x
        | `If_stmt x -> if_statement env x
      in
      (* A redirect applies to each call of the function. Arguments can't
         follow it. *)
      let redirects =
        Option.to_list (Option.map (fun x -> fst (redirect env x)) v3)
      in
      let loc = Tok_range.extend (AST_bash_loc.command_loc body) start in
      let command = Function_definition (loc, { loc; function_; name; body }) in
      Tmp_command ({ loc; command; redirects }, None)

and statements (env : env) ((v1, v2, v3) : CST.statements) : blist =
  let blist = terminated_statements env v1 in
  let last_blist = blist_statement env v2 in
  let last_blist =
    match v3 with
    | None -> last_blist
    | Some term -> add_terminator_to_blist last_blist (terminator env term)
  in
  let loc =
    Tok_range.range
      (AST_bash_loc.blist_loc blist)
      (AST_bash_loc.blist_loc last_blist)
  in
  Seq (loc, blist, last_blist)

(* A sequence of statements, each followed by a terminator. The last one may
   be a lone '...' without a terminator e.g. 'while ...; do ... done'. *)
and terminated_statement (env : env) (x : CST.terminated_statement) : blist =
  match x with
  | `Rep1_choice_choice_redi_stmt_choice_SEMI xs -> terminated_statements env xs
  | `Opt_rep1_choice_choice_redi_stmt_choice_SEMI_ellips_cmd (xs, ellipsis) ->
      let xs = List_.flatten (Option.to_list xs) in
      let name = word env ellipsis in
      let loc = AST_bash_loc.expression_loc name in
      let command =
        Simple_command { loc; assignments = []; arguments = [ name ] }
      in
      let ellipsis_blist =
        Pipelines (loc, [ Command { loc; command; redirects = [] } ])
      in
      AST_bash_builder.concat_blists
        (terminated_blists env xs @ [ ellipsis_blist ])

and terminated_statements (env : env)
    (xs : (CST.statement * CST.terminator) list) : blist =
  AST_bash_builder.concat_blists (terminated_blists env xs)

and terminated_blists (env : env) (xs : (CST.statement * CST.terminator) list) :
    blist list =
  List.map
    (fun (stmt, term) ->
      add_terminator_to_blist (blist_statement env stmt) (terminator env term))
    xs

and opt_terminated_statement (env : env) (empty_loc : loc)
    (opt : CST.terminated_statement option) : blist =
  match opt with
  | Some x -> terminated_statement env x
  | None -> Empty empty_loc

and string_ (env : env) ((v1, v2, v3, v4) : CST.string_) :
    string_fragment list bracket =
  let open_ =
    token env v1
    (* "\"" *)
  in
  let fragments =
    List.concat_map
      (fun (v1, _concat) ->
        match v1 with
        | `Opt_DOLLAR_str_content (v1, v2) ->
            let dollar =
              match v1 with
              | Some tok -> [ String_content (str env tok) (* "$" *) ]
              | None -> []
            in
            let content =
              [ String_content (str env v2) (* string_content *) ]
            in
            dollar @ content
        | `Expa x ->
            let e = expansion env x in
            let loc = AST_bash_loc.bracket_loc e in
            [ Expansion (loc, Complex_expansion e) ]
        | `Simple_expa x -> [ simple_expansion env x ]
        | `Cmd_subs x -> (
            match command_substitution_parts env x with
            | None, bracket -> [ Command_substitution bracket ]
            | Some dollar, bracket ->
                [ String_content dollar; Command_substitution bracket ])
        | `Arit_expa x -> [ arithmetic_expansion env x ])
      v2
  in
  let fragment =
    match v3 with
    | Some tok -> [ String_content (str env tok (* "$" *)) ]
    | None -> []
  in
  let close =
    token env v4
    (* "\"" *)
  in
  (open_, fragments @ fragment, close)

(* Only the array variable is used; the index is ignored. *)
and subscript (env : env) ((v1, _open, _index, _, _close, _) : CST.subscript) :
    string wrap =
  str env v1 (* variable_name *)

(* An array cell in arithmetic e.g. 'a[i + 1]' *)
and array_cell (env : env) ((v1, v2, v3, _concat, v5, _concat2) : CST.subscript)
    : arithmetic_expression =
  let index =
    match v3 with
    | `Choice_conc x -> A_operand (literal env x)
    | `Bin_exp x -> expression_arith env (`Bin_exp x)
    | `Un_exp x -> expression_arith env (`Un_exp x)
    | `Comp_stmt x ->
        let command = compound_statement env x in
        A_operand
          (Command_expression (AST_bash_loc.command_loc command, command))
    | `Subs x ->
        let sub = subshell env x in
        let loc = AST_bash_loc.bracket_loc sub in
        A_operand (Command_expression (loc, Subshell (loc, sub)))
  in
  A_array_cell (Word (str env v1), (token env v2, index, token env v5))

and subshell (env : env) ((v1, v2, v3) : CST.subshell) : blist bracket =
  let open_ =
    token env v1
    (* "(" *)
  in
  let blist = statements env v2 in
  let close =
    token env v3
    (* ")" *)
  in
  (open_, blist, close)

and test_command (env : env) (v1 : CST.test_command) : command =
  match v1 with
  | `LBRACK_opt_choice_exp_RBRACK (v1, v2, v3) ->
      let open_ =
        token env v1
        (* "[" *)
      in
      let close =
        token env v3
        (* "]" *)
      in
      let e =
        match v2 with
        | Some (`Exp x) -> expression env x
        | Some (`Redi_stmt x) ->
            (* e.g. '[ a \> b ]' *)
            T_todo (AST_bash_loc.pipeline_loc (redirected_statement env x))
        | None -> T_todo (open_, close)
      in
      let loc = (open_, close) in
      Sh_test (loc, (open_, e, close))
  | `LBRACKLBRACK_choice_exp_RBRACKRBRACK (v1, v2, v3) ->
      let open_ =
        token env v1
        (* "[[" *)
      in
      let e =
        match v2 with
        | `Exp x -> expression env x
        | `Test_cmd_bin_exp (v1, _eq, regex) ->
            (* e.g. '[[ $x = a* ]]' *)
            let left = expression env v1 in
            T_todo
              (Tok_range.extend
                 (AST_bash_loc.test_expression_loc left)
                 (token env regex))
      in
      let close =
        token env v3
        (* "]]" *)
      in
      let loc = (open_, close) in
      Bash_test (loc, (open_, e, close))

and variable_assignment (env : env) (x : CST.variable_assignment) : assignment =
  let lhs, assign_op, rhs =
    match x with
    | `Choice_semg_meta_eq_choice_choice_conc (v1, v2) ->
        let mv, assign_op =
          match v1 with
          | `Semg_meta_eq tok ->
              (* pattern \$[A-Z_][A-Z_0-9]*= *)
              let mv, eq_tok = split_metavar_op env tok ~op_len:1 in
              (mv, (Set, eq_tok (* "=" *)))
          | `Semg_meta_pluseq tok ->
              (* pattern \$[A-Z_][A-Z_0-9]*\+= *)
              let mv, pluseq_tok = split_metavar_op env tok ~op_len:2 in
              (mv, (Add, pluseq_tok (* "+=" *)))
        in
        (* TODO: this returns a metavariable even when parsing a program
           in which metavariables shouldn't exist.
           In such case, we should not return an assignment but convert
           $X=42 to a variable expansion and concatenation. *)
        (mv, assign_op, v2)
    | `Choice_var_name_choice_EQ_choice_choice_conc (v1, v2, v3) ->
        let var =
          match v1 with
          | `Var_name tok -> str env tok (* variable_name *)
          | `Subs x ->
              (* TODO: assignment to array cell *)
              subscript env x
        in
        let assign_op =
          match v2 with
          | `EQ tok -> (Set, token env tok (* "=" *))
          | `PLUSEQ tok -> (Add, token env tok (* "+=" *))
        in
        (var, assign_op, v3)
  in
  let rhs = right_hand_side env rhs in
  let loc = (snd lhs, snd (AST_bash_loc.expression_loc rhs)) in
  { loc; lhs; assign_op; rhs }

and right_hand_side (env : env) (x : CST.anon_choice_lit_a931c8d) : expression =
  match x with
  | `Choice_conc x -> literal env x
  | `Array x -> array_ env x
  | `Comm_word tok -> Word (str env tok)
  | `Empty_value tok ->
      let empty = token env tok in
      let loc = (empty, empty) in
      Empty_expression loc

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_bash.Parse.file !!file)
    (fun cst _extras ->
      let env =
        { H.file; conv = H.line_col_to_pos file; extra = AST_bash.Program }
      in
      let tok = Tok.first_tok_of_file file in
      program env ~tok cst)

let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_bash.Parse.string str)
    (fun cst _extras ->
      let file = Fpath.v "<pattern>" in
      let env =
        {
          H.file;
          conv = H.line_col_to_pos_pattern str;
          extra = AST_bash.Pattern;
        }
      in
      let tok = Tok.first_tok_of_file file in
      program env ~tok cst)
