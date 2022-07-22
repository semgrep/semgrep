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
open Common
module CST = Tree_sitter_elixir.CST
open AST_generic
module G = AST_generic
module H = Parse_tree_sitter_helpers
module H2 = AST_generic_helpers
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Elixir parser using tree-sitter-lang/semgrep-elixir and converting
 * (mostly directly) to AST_generic.ml
 *
 * alt:
 *  - define an intermediate AST_elixir.ml and convert to that instead
 *    of directly to AST_generic.ml (like we do for Bash, Ruby, etc.).
 *    Elixir is quite an unusual language
 *    with a very flexibly syntax and macro system. For example, there
 *    are no 'definition' or 'declaration' grammar rules. Instead a
 *    definition looks really like a function call. This is a bit
 *    similar to LISP.
 *  - define a few intermediate AST constructs below and convert
 *    eventually them to AST_generic.ml (we do that for a few C#
 *    constructs for example)
 *
 * references:
 * - https://hexdocs.pm/elixir/syntax-reference.html
 * - https://github.com/elixir-lang/tree-sitter-elixir/blob/main/docs/parser.md
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
(* A few intermediate AST constructs or aliases corresponding to
 * Elixir constructs, eventually converted to AST_generic constructs.
 *
 * We try to follow the naming conventions in
 * https://hexdocs.pm/elixir/syntax-reference.html
 *)

(* lowercase ident *)
type ident = G.ident

(* uppercase ident *)
type alias = G.ident

(* exprs separated by terminators (newlines or semicolons) *)
type body = expr list

(* less: restrict with special arg? *)
type call = expr

(* inside calls *)
type arg = G.argument

(* ident followed by ':' TODO remove leading ':' *)
type keyword = ident
type pair = keyword * expr

(* inside containers (list, bits, maps, tuples), separated by commas *)
type item = expr

(* Ideally it should be pattern list * tok * body option, but Elixir
 * is more general and use '->' also for type declarations in typespecs,
 * or for parameters (kind of patterns though).
 *)
type stab_clause = arg list * tok (* '->' *) * expr list
type clauses = stab_clause list

(* in after/catch/do/elserescue/() blocks *)
type body_or_clauses = (body, clauses) either

(* TODO *)
type do_block = expr

(* TODO:
   type block = ... ? or same than body?
   type remote = ...
*)

(*****************************************************************************)
(* Intermediate AST constructs to AST_generic *)
(*****************************************************************************)
let body_to_stmts es = es |> Common.map G.exprstmt

let stab_clauses_to_function_definition (_xs : stab_clause list) :
    function_definition =
  raise Todo

let args_of_exprs_and_keywords (_es : expr list) (_kwds : pair list) : arg list
    =
  raise Todo

let items_of_exprs_and_keywords (_es : expr list) (_kwds : pair list) :
    item list =
  raise Todo

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token
let str = H.str
let todo (_env : env) _ = failwith "not implemented"

(* helper to factorize code *)
let map_trailing_comma env v1 : tok option =
  match v1 with
  | Some tok -> Some ((* "," *) token env tok)
  | None -> None

let map_before_unary_op_opt env v1 =
  match v1 with
  | Some tok -> Some ((* before_unary_op *) token env tok)
  | None -> None

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-elixir/Boilerplate.ml *)

(* less: actually can contain '.' *)
let map_alias (env : env) (x : CST.alias) : alias = str env x

let map_boolean (env : env) (x : CST.boolean) : bool wrap =
  match x with
  | `True tok -> (true, (* "true" *) token env tok)
  | `False tok -> (false, (* "false" *) token env tok)

let map_anon_choice_PLUS_8019319 (env : env) (x : CST.anon_choice_PLUS_8019319)
    : ident =
  match x with
  | `PLUS tok -> (* "+" *) str env tok
  | `DASH tok -> (* "-" *) str env tok
  | `BANG tok -> (* "!" *) str env tok
  | `HAT tok -> (* "^" *) str env tok
  | `TILDETILDETILDE tok -> (* "~~~" *) str env tok
  | `Not tok -> (* "not" *) str env tok

let map_terminator (env : env) (x : CST.terminator) : tok list =
  match x with
  | `Rep_pat_509ec78_SEMI (v1, v2) ->
      let v1 = Common.map (token env (* pattern \r?\n *)) v1 in
      let v2 = (* ";" *) token env v2 in
      v1 @ [ v2 ]
  | `Rep1_pat_509ec78 xs -> Common.map (token env (* pattern \r?\n *)) xs

(* helper to factorize code *)
let map_terminator_opt env v1 : tok list option =
  match v1 with
  | Some x -> Some (map_terminator env x)
  | None -> None

let map_identifier (env : env) (x : CST.identifier) : ident =
  match x with
  | `Pat_cf9c6c3 tok ->
      (* pattern [_\p{Ll}\p{Lm}\p{Lo}\p{Nl}\u1885\u1886\u2118\u212E\u309B\u309C][\p{ID_Continue}]*[?!]? *)
      str env tok
  (* TODO: part of elixir! not a semgrep construct! *)
  | `DOTDOTDOT tok -> (* "..." *) str env tok

let map_quoted_xxx (env : env) (v1, v2, v3) : string wrap =
  let v1 = (* "/" or another one *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_slash tok
        | `Quoted_content_here_double tok
        | `Quoted_content_single tok
        | `Quoted_content_bar tok
        | `Quoted_content_curl tok
        | `Quoted_content_here_single tok
        | `Quoted_content_square tok
        | `Quoted_content_paren tok
        | `Quoted_content_double tok
        | `Quoted_content_angle tok ->
            str env tok
        | `Esc_seq tok -> (* escape_sequence *) str env tok)
      v2
  in
  let v3 = (* "/" or another one *) token env v3 in
  let str = v2 |> Common.map fst |> String.concat "" in
  let toks = (v2 |> Common.map snd) @ [ v3 ] in
  (str, PI.combine_infos v1 toks)

let map_quoted_i_xxx map_interpolation (env : env) (v1, v2, v3) : expr =
  let v1 = (* "<" or another one *) token env v1 in
  let v2 =
    Common.map
      (fun x ->
        match x with
        | `Quoted_content_i_angle tok
        | `Quoted_content_i_bar tok
        | `Quoted_content_i_curl tok
        | `Quoted_content_i_double tok
        | `Quoted_content_i_here_double tok
        | `Quoted_content_i_here_single tok
        | `Quoted_content_i_paren tok
        | `Quoted_content_i_single tok
        | `Quoted_content_i_slash tok
        | `Quoted_content_i_square tok ->
            Left3 (* quoted_content_i_angle *) (str env tok)
        | `Interp x ->
            let l, e, r = map_interpolation env x in
            Right3 (l, Some e, r)
        | `Esc_seq tok -> Left3 ((* escape_sequence *) str env tok))
      v2
  in
  let v3 = (* ">" or another one *) token env v3 in
  G.interpolated (v1, v2, v3)

let map_quoted_slash (env : env) ((v1, v2, v3) : CST.quoted_slash) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_heredoc_double (env : env)
    ((v1, v2, v3) : CST.quoted_heredoc_double) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_single (env : env) ((v1, v2, v3) : CST.quoted_single) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_angle (env : env) ((v1, v2, v3) : CST.quoted_angle) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_double (env : env) ((v1, v2, v3) : CST.quoted_double) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_parenthesis (env : env) ((v1, v2, v3) : CST.quoted_parenthesis) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_square (env : env) ((v1, v2, v3) : CST.quoted_square) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_heredoc_single (env : env)
    ((v1, v2, v3) : CST.quoted_heredoc_single) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_curly (env : env) ((v1, v2, v3) : CST.quoted_curly) =
  map_quoted_xxx env (v1, v2, v3)

let map_quoted_bar (env : env) ((v1, v2, v3) : CST.quoted_bar) =
  map_quoted_xxx env (v1, v2, v3)

let map_operator_identifier (env : env) (x : CST.operator_identifier) : ident =
  match x with
  | `AMP tok -> (* "&" *) str env tok
  | `Choice_PLUS x -> map_anon_choice_PLUS_8019319 env x
  | `AT tok -> (* "@" *) str env tok
  | `LTDASH tok -> (* "<-" *) str env tok
  | `BSLASHBSLASH tok -> (* "\\\\" *) str env tok
  | `When tok -> (* "when" *) str env tok
  | `COLONCOLON tok -> (* "::" *) str env tok
  | `BAR tok -> (* "|" *) str env tok
  | `EQ tok -> (* "=" *) str env tok
  | `BARBAR tok -> (* "||" *) str env tok
  | `BARBARBAR tok -> (* "|||" *) str env tok
  | `Or tok -> (* "or" *) str env tok
  | `AMPAMP tok -> (* "&&" *) str env tok
  | `AMPAMPAMP tok -> (* "&&&" *) str env tok
  | `And tok -> (* "and" *) str env tok
  | `EQEQ tok -> (* "==" *) str env tok
  | `BANGEQ tok -> (* "!=" *) str env tok
  | `EQTILDE tok -> (* "=~" *) str env tok
  | `EQEQEQ tok -> (* "===" *) str env tok
  | `BANGEQEQ tok -> (* "!==" *) str env tok
  | `LT tok -> (* "<" *) str env tok
  | `GT tok -> (* ">" *) str env tok
  | `LTEQ tok -> (* "<=" *) str env tok
  | `GTEQ tok -> (* ">=" *) str env tok
  | `BARGT tok -> (* "|>" *) str env tok
  | `LTLTLT tok -> (* "<<<" *) str env tok
  | `GTGTGT tok -> (* ">>>" *) str env tok
  | `LTLTTILDE tok -> (* "<<~" *) str env tok
  | `TILDEGTGT tok -> (* "~>>" *) str env tok
  | `LTTILDE tok -> (* "<~" *) str env tok
  | `TILDEGT tok -> (* "~>" *) str env tok
  | `LTTILDEGT tok -> (* "<~>" *) str env tok
  | `LTBARGT tok -> (* "<|>" *) str env tok
  | `In tok -> (* "in" *) str env tok
  | `Not_in tok -> (* not_in *) str env tok
  | `HATHAT tok -> (* "^^" *) str env tok
  | `PLUSPLUS tok -> (* "++" *) str env tok
  | `DASHDASH tok -> (* "--" *) str env tok
  | `PLUSPLUSPLUS tok -> (* "+++" *) str env tok
  | `DASHDASHDASH tok -> (* "---" *) str env tok
  | `DOTDOT tok -> (* ".." *) str env tok
  | `LTGT tok -> (* "<>" *) str env tok
  | `STAR tok -> (* "*" *) str env tok
  | `SLASH tok -> (* "/" *) str env tok
  | `STARSTAR tok -> (* "**" *) str env tok
  | `DASHGT tok -> (* "->" *) str env tok
  | `DOT tok -> (* "." *) str env tok

let rec map_after_block (env : env) ((v1, v2, v3) : CST.after_block) =
  let v1 = (* "after" *) token env v1 in
  let _v2 = map_terminator_opt env v2 in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> Left []
  in
  (v1, v3)

and map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
    (env : env)
    (x : CST.anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119)
    : body_or_clauses =
  match x with
  | `Choice_stab_clause_rep_term_choice_stab_clause (v1, v2) ->
      let v1 =
        match v1 with
        | `Stab_clause x -> map_stab_clause env x
      in
      let v2 = Common.map (map_anon_term_choice_stab_clause_70647b7 env) v2 in
      Right (v1 :: v2)
  | `Choice_exp_rep_term_choice_exp_opt_term (v1, v2, v3) ->
      let v1 =
        match v1 with
        | `Exp x -> map_expression env x
      in
      let v2 = Common.map (map_anon_term_choice_exp_996111b env) v2 in
      let _v3 = map_terminator_opt env v3 in
      Left (v1 :: v2)

and map_anon_choice_exp_0094635 (env : env) (x : CST.anon_choice_exp_0094635) =
  match x with
  | `Exp x ->
      let e = map_expression env x in
      Left e
  | `Keywos x ->
      let xs = map_keywords env x in
      Right xs

and map_anon_choice_quoted_i_double_d7d5f65 (env : env)
    (x : CST.anon_choice_quoted_i_double_d7d5f65) : expr =
  match x with
  | `Quoted_i_double x -> map_quoted_i_double env x
  | `Quoted_i_single x -> map_quoted_i_single env x

and map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e (env : env)
    ((v1, v2, v3) : CST.anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e) :
    arg list =
  let v1 = map_expression env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_expression env v2 in
        v2)
      v2
  in
  let v3 =
    match v3 with
    | Some (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_keywords env v2 in
        v2
    | None -> []
  in
  args_of_exprs_and_keywords (v1 :: v2) v3

and map_anon_opt_opt_nl_before_do_do_blk_3eff85f (env : env)
    (opt : CST.anon_opt_opt_nl_before_do_do_blk_3eff85f) : do_block option =
  match opt with
  | Some (v1, v2) ->
      let _v1 =
        match v1 with
        | Some tok -> Some ((* newline_before_do *) token env tok)
        | None -> None
      in
      let v2 = map_do_block env v2 in
      Some v2
  | None -> None

and map_anon_term_choice_exp_996111b (env : env)
    ((v1, v2) : CST.anon_term_choice_exp_996111b) : expr =
  let _v1 = map_terminator env v1 in
  let v2 =
    match v2 with
    | `Exp x -> map_expression env x
  in
  v2

and map_anon_term_choice_stab_clause_70647b7 (env : env)
    ((v1, v2) : CST.anon_term_choice_stab_clause_70647b7) : stab_clause =
  let _v1 = map_terminator env v1 in
  let v2 =
    match v2 with
    | `Stab_clause x -> map_stab_clause env x
  in
  v2

and map_anonymous_call (env : env) ((v1, v2) : CST.anonymous_call) =
  let v1 = map_anonymous_dot env v1 in
  let v2 = map_call_arguments_with_parentheses env v2 in
  todo env (v1, v2)

and map_anonymous_dot (env : env) ((v1, v2) : CST.anonymous_dot) =
  let v1 = map_expression env v1 in
  let v2 = (* "." *) token env v2 in
  (v1, v2)

and map_atom (env : env) (x : CST.atom) : expr =
  match x with
  | `Atom_ tok ->
      (* less: should remove the leading ':' from x *)
      let x = (* atom_ *) str env tok in
      L (Atom (G.fake ":", x)) |> G.e
  | `Quoted_atom (v1, v2) ->
      let t = (* quoted_atom_start *) token env v1 in
      let str = map_anon_choice_quoted_i_double_d7d5f65 env v2 in
      OtherExpr (("AtomExpr", t), [ E str ]) |> G.e

and map_binary_operator (env : env) (x : CST.binary_operator) : expr =
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
        | `PLUS tok -> (Plus, (* "+" *) token env tok)
        | `DASH tok -> (Minus, (* "-" *) token env tok)
      in
      let v3 = map_expression env v3 in
      G.opcall v2 [ v1; v3 ]
  | `Exp_choice_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | `STAR tok -> (Mult, (* "*" *) token env tok)
        | `SLASH tok -> (Div, (* "/" *) token env tok)
      in
      let v3 = map_expression env v3 in
      G.opcall v2 [ v1; v3 ]
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

and map_body (env : env) ((v1, v2, v3, v4) : CST.body) : body =
  let _v1 = map_terminator_opt env v1 in
  let v2 = map_expression env v2 in
  let v3 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = map_terminator env v1 in
        let v2 = map_expression env v2 in
        v2)
      v3
  in
  let _v4 = map_terminator_opt env v4 in
  v2 :: v3

and map_call (env : env) (x : CST.call) : call =
  match x with
  | `Call_with_parens_b98484c x -> map_call_without_parentheses env x
  | `Call_with_parens_403315d x -> map_call_with_parentheses env x

and map_call_arguments_with_parentheses (env : env)
    ((v1, v2, v3) : CST.call_arguments_with_parentheses) : arg list bracket =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_call_arguments_with_trailing_separator env x
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

and map_call_arguments_with_parentheses_immediate (env : env)
    ((v1, v2, v3) : CST.call_arguments_with_parentheses_immediate) :
    arg list bracket =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_call_arguments_with_trailing_separator env x
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

and map_call_arguments_with_trailing_separator (env : env)
    (x : CST.call_arguments_with_trailing_separator) : arg list =
  match x with
  | `Exp_rep_COMMA_exp_opt_COMMA_keywos_with_trai_sepa (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            v2)
          v2
      in
      let v3 =
        match v3 with
        | Some (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_keywords_with_trailing_separator env v2 in
            v2
        | None -> []
      in
      args_of_exprs_and_keywords (v1 :: v2) v3
  | `Keywos_with_trai_sepa x ->
      let xs = map_keywords_with_trailing_separator env x in
      args_of_exprs_and_keywords [] xs

and map_call_arguments_without_parentheses (env : env)
    (x : CST.call_arguments_without_parentheses) : arg list =
  match x with
  | `Exp_rep_COMMA_exp_opt_COMMA_keywos x ->
      map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e env x
  | `Keywos x ->
      let xs = map_keywords env x in
      args_of_exprs_and_keywords [] xs

and map_call_with_parentheses (env : env) (x : CST.call_with_parentheses) : call
    =
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
    : call =
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
        | None -> []
      in
      let v3 = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
      todo env (v1, v2, v3)

and map_capture_expression (env : env) (x : CST.capture_expression) : expr =
  match x with
  | `LPAR_exp_RPAR (v1, v2, v3) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ")" *) token env v3 in
      ParenExpr (v1, v2, v3) |> G.e
  | `Exp x -> map_expression env x

and map_catch_block (env : env) ((v1, v2, v3) : CST.catch_block) =
  let v1 = (* "catch" *) token env v1 in
  let _v2 = map_terminator_opt env v2 in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> Left []
  in
  (v1, v3)

and map_charlist (env : env) (x : CST.charlist) =
  match x with
  | `Quoted_i_single x -> map_quoted_i_single env x
  | `Quoted_i_here_single x -> map_quoted_i_heredoc_single env x

and map_do_block (env : env) ((v1, v2, v3, v4, v5) : CST.do_block) : do_block =
  let v1 = (* "do" *) token env v1 in
  let v2 = map_terminator_opt env v2 in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> Left []
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
    | `Alias tok ->
        let al = map_alias env tok in
        todo env al
    | `Tuple x ->
        let _l, xs, _r = map_tuple env x in
        todo env xs
  in
  todo env (v1, v2, v3)

and map_else_block (env : env) ((v1, v2, v3) : CST.else_block) =
  let v1 = (* "else" *) token env v1 in
  let _v2 = map_terminator_opt env v2 in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> Left []
  in
  (v1, v3)

and map_expression (env : env) (x : CST.expression) : expr =
  match x with
  | `Blk (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_terminator_opt env v2 in
      let v3 =
        match v3 with
        | Some x ->
            map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
              env x
        | None -> Left []
      in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Id x ->
      let id = map_identifier env x in
      N (H2.name_of_id id) |> G.e
  | `Alias tok ->
      let al = map_alias env tok in
      (* less: should return a Constructor instead? *)
      N (H2.name_of_id al) |> G.e
  | `Int tok ->
      let s, t = (* integer *) str env tok in
      L (Int (int_of_string_opt s, t)) |> G.e
  | `Float tok ->
      let s, t = (* float *) str env tok in
      L (Float (float_of_string_opt s, t)) |> G.e
  | `Char_a87deb0 tok ->
      let x = (* pattern \?(.|\\.) *) str env tok in
      L (Char x) |> G.e
  | `Bool x ->
      let b = map_boolean env x in
      L (Bool b) |> G.e
  | `Nil tok ->
      let nil = (* "nil" *) token env tok in
      L (Null nil) |> G.e
  | `Atom x -> map_atom env x
  | `Str x -> map_string_ env x
  | `Char_a593f90 x ->
      let xs = map_charlist env x in
      todo env xs
  | `Sigil (v1, v2, v3) ->
      let v1 = (* "~" *) token env v1 in
      let v2 =
        match v2 with
        | `Imm_tok_pat_0db2d54_choice_quoted_i_double (v1, v2) ->
            let _lowerTODO = (* pattern [a-z] *) str env v1 in
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
            let _upperTODO = (* pattern [A-Z] *) str env v1 in
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
      let idopt_TODO =
        match v3 with
        | Some tok -> Some ((* pattern [a-zA-Z0-9]+ *) str env tok)
        | None -> None
      in
      todo env (v1, v2, idopt_TODO)
  | `List (v1, v2, v3) ->
      let l = (* "[" *) token env v1 in
      let xs =
        match v2 with
        | Some x -> map_items_with_trailing_separator env x
        | None -> []
      in
      let r = (* "]" *) token env v3 in
      Container (List, (l, xs, r)) |> G.e
  | `Tuple x ->
      let l, xs, r = map_tuple env x in
      Container (Tuple, (l, xs, r)) |> G.e
  | `Bits (v1, v2, v3) ->
      let v1 = (* "<<" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> map_items_with_trailing_separator env x
        | None -> []
      in
      let v3 = (* ">>" *) token env v3 in
      todo env (v1, v2, v3)
  | `Map (v1, v2, v3, v4, v5) ->
      let v1 = (* "%" *) token env v1 in
      let v2 =
        match v2 with
        | Some x -> Some (map_struct_ env x)
        | None -> None
      in
      let v3 = (* "{" *) token env v3 in
      let v4 =
        match v4 with
        | Some x -> map_items_with_trailing_separator env x
        | None -> []
      in
      let v5 = (* "}" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Un_op x -> map_unary_operator env x
  | `Bin_op x -> map_binary_operator env x
  | `Dot x -> map_dot env x
  | `Call x -> map_call env x
  (* semantic: transformed in Access.get/2 *)
  | `Access_call (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 = (* "]" *) token env v4 in
      ArrayAccess (v1, (v2, v3, v4)) |> G.e
  | `Anon_func (v1, v2, v3, v4, v5) ->
      let v1 = (* "fn" *) token env v1 in
      let _v2 = map_terminator_opt env v2 in
      let v3 = map_stab_clause env v3 in
      let v4 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = map_terminator env v1 in
            let v2 = map_stab_clause env v2 in
            v2)
          v4
      in
      let _v5TODO = (* "end" *) token env v5 in
      let clauses = v3 :: v4 in
      let fdef = stab_clauses_to_function_definition clauses in
      let fdef = { fdef with fkind = (LambdaKind, v1) } in
      Lambda fdef |> G.e

and map_interpolation (env : env) ((v1, v2, v3) : CST.interpolation) :
    expr bracket =
  let v1 = (* "#{" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "}" *) token env v3 in
  (v1, v2, v3)

and map_items_with_trailing_separator (env : env)
    (v1 : CST.items_with_trailing_separator) : item list =
  match v1 with
  | `Exp_rep_COMMA_exp_opt_COMMA (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        Common.map
          (fun (v1, v2) ->
            let _v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            v2)
          v2
      in
      let _v3 = map_trailing_comma env v3 in
      v1 :: v2
  | `Opt_exp_rep_COMMA_exp_COMMA_keywos_with_trai_sepa (v1, v2) ->
      let v1 =
        match v1 with
        | Some (v1, v2, v3) ->
            let v1 = map_expression env v1 in
            let v2 =
              Common.map
                (fun (v1, v2) ->
                  let _v1 = (* "," *) token env v1 in
                  let v2 = map_expression env v2 in
                  v2)
                v2
            in
            let _v3 = (* "," *) token env v3 in
            v1 :: v2
        | None -> []
      in
      let xs = map_keywords_with_trailing_separator env v2 in
      items_of_exprs_and_keywords v1 xs

and map_keyword (env : env) (x : CST.keyword) : keyword =
  match x with
  | `Kw_ tok ->
      (* todo: remove suffix ':' *)
      let x = (* keyword_ *) str env tok in
      x
  | `Quoted_kw (v1, v2) ->
      let v1 = map_anon_choice_quoted_i_double_d7d5f65 env v1 in
      let v2 = (* pattern :\s *) token env v2 in
      todo env (v1, v2)

and map_keywords (env : env) ((v1, v2) : CST.keywords) : pair list =
  let v1 = map_pair env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_pair env v2 in
        v2)
      v2
  in
  v1 :: v2

and map_keywords_with_trailing_separator (env : env)
    ((v1, v2, v3) : CST.keywords_with_trailing_separator) =
  let v1 = map_pair env v1 in
  let v2 =
    Common.map
      (fun (v1, v2) ->
        let _v1 = (* "," *) token env v1 in
        let v2 = map_pair env v2 in
        v2)
      v2
  in
  let _v3 = map_trailing_comma env v3 in
  v1 :: v2

and map_local_call_with_parentheses (env : env)
    ((v1, v2, v3) : CST.local_call_with_parentheses) =
  let v1 = map_identifier env v1 in
  let v2 = map_call_arguments_with_parentheses_immediate env v2 in
  let v3 = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
  todo env (v1, v2, v3)

and map_pair (env : env) ((v1, v2) : CST.pair) : pair =
  let v1 = map_keyword env v1 in
  let v2 = map_expression env v2 in
  (v1, v2)

and map_quoted_i_angle (env : env) ((v1, v2, v3) : CST.quoted_i_angle) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_bar (env : env) ((v1, v2, v3) : CST.quoted_i_bar) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_curly (env : env) ((v1, v2, v3) : CST.quoted_i_curly) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_double (env : env) ((v1, v2, v3) : CST.quoted_i_double) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_heredoc_double (env : env)
    ((v1, v2, v3) : CST.quoted_i_heredoc_double) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_heredoc_single (env : env)
    ((v1, v2, v3) : CST.quoted_i_heredoc_single) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_parenthesis (env : env)
    ((v1, v2, v3) : CST.quoted_i_parenthesis) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_single (env : env) ((v1, v2, v3) : CST.quoted_i_single) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_slash (env : env) ((v1, v2, v3) : CST.quoted_i_slash) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_quoted_i_square (env : env) ((v1, v2, v3) : CST.quoted_i_square) =
  map_quoted_i_xxx map_interpolation env (v1, v2, v3)

and map_remote_call_with_parentheses (env : env)
    ((v1, v2, v3) : CST.remote_call_with_parentheses) =
  let v1 = map_remote_dot env v1 in
  let v2 = map_call_arguments_with_parentheses_immediate env v2 in
  let v3 = map_anon_opt_opt_nl_before_do_do_blk_3eff85f env v3 in
  todo env (v1, v2, v3)

and map_remote_dot (env : env) ((v1, v2, v3) : CST.remote_dot) =
  let v1 = map_expression env v1 in
  let v2 = (* "." *) token env v2 in
  let v3 : (ident, expr) either =
    match v3 with
    | `Id x ->
        let id = map_identifier env x in
        Left id
    | `Choice_and x ->
        Left
          (match x with
          | `And tok -> (* "and" *) str env tok
          | `In tok -> (* "in" *) str env tok
          | `Not tok -> (* "not" *) str env tok
          | `Or tok -> (* "or" *) str env tok
          | `When tok -> (* "when" *) str env tok
          | `True tok -> (* "true" *) str env tok
          | `False tok -> (* "false" *) str env tok
          | `Nil tok -> (* "nil" *) str env tok
          | `After tok -> (* "after" *) str env tok
          | `Catch tok -> (* "catch" *) str env tok
          | `Do tok -> (* "do" *) str env tok
          | `Else tok -> (* "else" *) str env tok
          | `End tok -> (* "end" *) str env tok
          | `Fn tok -> (* "fn" *) str env tok
          | `Rescue tok -> (* "rescue" *) str env tok)
    | `Op_id x ->
        let id = map_operator_identifier env x in
        Left id
    | `Quoted_i_double x ->
        let x = map_quoted_i_double env x in
        Right x
    | `Quoted_i_single x ->
        let x = map_quoted_i_single env x in
        Right x
  in
  todo env (v1, v2, v3)

and map_rescue_block (env : env) ((v1, v2, v3) : CST.rescue_block) =
  let v1 = (* "rescue" *) token env v1 in
  let _v2 = map_terminator_opt env v2 in
  let v3 =
    match v3 with
    | Some x ->
        map_anon_choice_choice_stab_clause_rep_term_choice_stab_clause_b295119
          env x
    | None -> Left []
  in
  (v1, v3)

and map_stab_clause (env : env) ((v1, v2, v3) : CST.stab_clause) : stab_clause =
  let v1 =
    match v1 with
    | Some x -> map_stab_clause_left env x
    | None -> []
  in
  let v2 = (* "->" *) token env v2 in
  let v3 =
    match v3 with
    | Some x -> map_body env x
    | None -> []
  in
  (v1, v2, v3)

and map_stab_clause_arguments_with_parentheses (env : env)
    ((v1, v2, v3) : CST.stab_clause_arguments_with_parentheses) :
    arg list bracket =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    match v2 with
    | Some x -> map_stab_clause_arguments_without_parentheses env x
    | None -> []
  in
  let v3 = (* ")" *) token env v3 in
  (v1, v2, v3)

and map_stab_clause_arguments_without_parentheses (env : env)
    (x : CST.stab_clause_arguments_without_parentheses) : arg list =
  match x with
  | `Exp_rep_COMMA_exp_opt_COMMA_keywos x ->
      let xs = map_anon_exp_rep_COMMA_exp_opt_COMMA_keywos_041d82e env x in
      xs
  | `Keywos x ->
      let xs = map_keywords env x in
      args_of_exprs_and_keywords [] xs

(* we would prefer pattern list, but elixir is more general *)
and map_stab_clause_left (env : env) (x : CST.stab_clause_left) : arg list =
  match x with
  | `Stab_clause_args_with_parens_bf4a580 x ->
      let _, xs, _ = map_stab_clause_arguments_with_parentheses env x in
      xs
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

and map_string_ (env : env) (x : CST.string_) : expr =
  match x with
  | `Quoted_i_double x -> map_quoted_i_double env x
  | `Quoted_i_here_double x -> map_quoted_i_heredoc_double env x

and map_struct_ (env : env) (x : CST.struct_) =
  match x with
  | `Alias tok ->
      let al = map_alias env tok in
      todo env al
  | `Atom x ->
      let a = map_atom env x in
      todo env a
  | `Id x ->
      let id = map_identifier env x in
      todo env id
  | `Un_op x ->
      let op = map_unary_operator env x in
      todo env op
  | `Dot x ->
      let d = map_dot env x in
      todo env d
  | `Call_with_parens x ->
      let call = map_call_with_parentheses env x in
      todo env call

and map_tuple (env : env) ((v1, v2, v3) : CST.tuple) =
  let l = (* "{" *) token env v1 in
  let xs =
    match v2 with
    | Some x -> map_items_with_trailing_separator env x
    | None -> []
  in
  let r = (* "}" *) token env v3 in
  (l, xs, r)

and map_unary_operator (env : env) (x : CST.unary_operator) : expr =
  match x with
  | `Opt_before_un_op_AMP_capt_exp (v1, v2, v3) ->
      let _v1 = map_before_unary_op_opt env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_capture_expression env v3 in
      todo env (v2, v3)
  | `Opt_before_un_op_choice_PLUS_exp (v1, v2, v3) ->
      let _v1 = map_before_unary_op_opt env v1 in
      let v2 = map_anon_choice_PLUS_8019319 env v2 in
      let v3 = map_expression env v3 in
      todo env (v2, v3)
  | `Opt_before_un_op_AT_exp (v1, v2, v3) ->
      let _v1 = map_before_unary_op_opt env v1 in
      let v2 = (* "@" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v2, v3)
  | `Opt_before_un_op_AMP_int (v1, v2, v3) ->
      let _v1 = map_before_unary_op_opt env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = (* integer *) token env v3 in
      todo env (v2, v3)

let map_source (env : env) ((v1, v2) : CST.source) : body =
  let _v1 = map_terminator_opt env v1 in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_expression env v1 in
        let v2 =
          Common.map
            (fun (v1, v2) ->
              let _v1 = map_terminator env v1 in
              let v2 = map_expression env v2 in
              v2)
            v2
        in
        let _v3 = map_terminator_opt env v3 in
        v1 :: v2
    | None -> []
  in
  v2

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
        | es -> body_to_stmts es
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
      | [ e ] -> G.E e
      | es -> G.Ss (body_to_stmts es))
