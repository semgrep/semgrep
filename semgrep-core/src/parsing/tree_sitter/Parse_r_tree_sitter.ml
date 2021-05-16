(* Ross Nanopoulos (zythosec)
 *
 * Copyright (c) 2020
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common
module CST = Tree_sitter_r.CST
module H = Parse_tree_sitter_helpers

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type env = unit H.env

let token = H.token

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying tree-sitter-lang/semgrep-r/Boilerplate.ml *)

(**
   Boilerplate to be used as a template when mapping the r CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

let blank (env : env) () = failwith "not implemented"

let todo (env : env) _ = failwith "not implemented"

let map_pat_5e7ac5f (env : env) (tok : CST.pat_5e7ac5f) = token env tok

(* pattern [^%\\\n]+|\\\r?\n *)

let map_pat_abe3ef5 (env : env) (tok : CST.pat_abe3ef5) = token env tok

(* pattern [A-Za-z.][A-Za-z0-9_.]* *)

let map_pat_de5d470 (env : env) (tok : CST.pat_de5d470) = token env tok

(* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *)

let map_na (env : env) (x : CST.na) =
  match x with
  | `NA tok -> token env tok (* "NA" *)
  | `NA_char_ tok -> token env tok (* "NA_character_" *)
  | `NA_comp_ tok -> token env tok (* "NA_complex_" *)
  | `NA_int_ tok -> token env tok (* "NA_integer_" *)
  | `NA_real_ tok -> token env tok

(* "NA_real_" *)

let map_pat_4ad362e (env : env) (tok : CST.pat_4ad362e) = token env tok

(* pattern [^`\\\n]+|\\\r?\n *)

let map_float_ (env : env) (tok : CST.float_) = token env tok

(* float *)

let map_integer (env : env) (tok : CST.integer) = token env tok

(* integer *)

let map_escape_sequence (env : env) (tok : CST.escape_sequence) = token env tok

(* escape_sequence *)

let map_pat_3e57880 (env : env) (tok : CST.pat_3e57880) = token env tok

(* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *)

let map_special (env : env) ((v1, v2, v3) : CST.special) =
  let v1 = token env v1 (* "%" *) in
  let v2 =
    List.map
      (fun x ->
        match x with
        | `Pat_5e7ac5f tok -> token env tok (* pattern [^%\\\n]+|\\\r?\n *)
        | `Esc_seq tok -> token env tok
        (* escape_sequence *))
      v2
  in
  let v3 = token env v3 (* "%" *) in
  todo env (v1, v2, v3)

let map_identifier (env : env) (x : CST.identifier) =
  match x with
  | `Pat_abe3ef5 tok -> token env tok (* pattern [A-Za-z.][A-Za-z0-9_.]* *)
  | `BQUOT_rep_choice_pat_4ad362e_BQUOT (v1, v2, v3) ->
      let v1 = token env v1 (* "`" *) in
      let v2 =
        List.map
          (fun x ->
            match x with
            | `Pat_4ad362e tok -> token env tok (* pattern [^`\\\n]+|\\\r?\n *)
            | `Esc_seq tok -> token env tok
            (* escape_sequence *))
          v2
      in
      let v3 = token env v3 (* "`" *) in
      todo env (v1, v2, v3)

let map_string_ (env : env) (x : CST.string_) =
  match x with
  | `DQUOT_rep_choice_pat_de5d470_DQUOT (v1, v2, v3) ->
      let v1 = token env v1 (* "\"" *) in
      let v2 =
        List.map
          (fun x ->
            match x with
            | `Pat_de5d470 tok ->
                token env tok (* pattern "[^\"\\\\\\n]+|\\\\\\r?\\n" *)
            | `Esc_seq tok -> token env tok
            (* escape_sequence *))
          v2
      in
      let v3 = token env v3 (* "\"" *) in
      todo env (v1, v2, v3)
  | `SQUOT_rep_choice_pat_3e57880_SQUOT (v1, v2, v3) ->
      let v1 = token env v1 (* "'" *) in
      let v2 =
        List.map
          (fun x ->
            match x with
            | `Pat_3e57880 tok ->
                token env tok (* pattern "[^'\\\\\\n]+|\\\\\\r?\\n" *)
            | `Esc_seq tok -> token env tok
            (* escape_sequence *))
          v2
      in
      let v3 = token env v3 (* "'" *) in
      todo env (v1, v2, v3)

let map_anon_choice_id_c711a0e (env : env) (x : CST.anon_choice_id_c711a0e) =
  match x with
  | `Id x -> map_identifier env x
  | `Str x -> map_string_ env x
  | `Dots tok -> token env tok

(* "..." *)

let rec map_argument (env : env) (x : CST.argument) =
  match x with
  | `Exp x -> map_expression env x
  | `Choice_id_EQ_opt_exp (v1, v2, v3) ->
      let v1 = map_anon_choice_id_c711a0e env v1 in
      let v2 = token env v2 (* "=" *) in
      let v3 =
        match v3 with Some x -> map_expression env x | None -> todo env ()
      in
      todo env (v1, v2, v3)

and map_arguments (env : env) (xs : CST.arguments) =
  List.map
    (fun x ->
      match x with `Arg x -> map_argument env x | `COMMA tok -> token env tok
      (* "," *))
    xs

and map_assignment (env : env) (x : CST.assignment) =
  match x with
  | `Equals_assign (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Left_assign (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<-" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Left_assign2 (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ":=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Right_assign (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "->" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Super_assign (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<<-" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)

and map_binary (env : env) (x : CST.binary) =
  match x with
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_spec_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = map_special env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_COLON_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ":" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_TILDE_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "~" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)

and map_expression (env : env) (x : CST.expression) =
  match x with
  | `Id x -> map_identifier env x
  | `Int tok -> token env tok (* integer *)
  | `Float tok -> token env tok (* float *)
  | `Comp (v1, v2) ->
      let v1 = token env v1 (* float *) in
      let v2 = token env v2 (* "i" *) in
      todo env (v1, v2)
  | `Str x -> map_string_ env x
  | `Call (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        match v3 with Some x -> map_arguments env x | None -> todo env ()
      in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Func_defi x -> map_function_definition env x
  | `Assign x -> map_assignment env x
  | `Brace_list (v1, v2, v3) ->
      let v1 = token env v1 (* "{" *) in
      let v2 = map_program env v2 in
      let v3 = token env v3 (* "}" *) in
      todo env (v1, v2, v3)
  | `Paren_list (v1, v2, v3) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = List.map (map_expression env) v2 in
      let v3 = token env v3 (* ")" *) in
      todo env (v1, v2, v3)
  | `Bin x -> map_binary env x
  | `Un x -> map_unary env x
  | `Subset (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 =
        match v3 with Some x -> map_arguments env x | None -> todo env ()
      in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `Subset2 (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "[[" *) in
      let v3 =
        match v3 with Some x -> map_arguments env x | None -> todo env ()
      in
      let v4 = token env v4 (* "]]" *) in
      todo env (v1, v2, v3, v4)
  | `Dollar (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "$" *) in
      let v3 =
        match v3 with
        | `Id x -> map_identifier env x
        | `Str x -> map_string_ env x
      in
      todo env (v1, v2, v3)
  | `Slot (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "@" *) in
      let v3 = map_identifier env v3 in
      todo env (v1, v2, v3)
  | `Name_get (v1, v2, v3) ->
      let v1 = map_identifier env v1 in
      let v2 = token env v2 (* "::" *) in
      let v3 = map_identifier env v3 in
      todo env (v1, v2, v3)
  | `Name_get_inte (v1, v2, v3) ->
      let v1 = map_identifier env v1 in
      let v2 = token env v2 (* ":::" *) in
      let v3 = map_identifier env v3 in
      todo env (v1, v2, v3)
  | `If (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "if" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* ")" *) in
      let v5 = map_expression env v5 in
      let v6 =
        match v6 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "else" *) in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | None -> todo env ()
      in
      todo env (v1, v2, v3, v4, v5, v6)
  | `For (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_identifier env v3 in
      let v4 = token env v4 (* "in" *) in
      let v5 = map_expression env v5 in
      let v6 = token env v6 (* ")" *) in
      let v7 = map_expression env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `While (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* ")" *) in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Repeat (v1, v2) ->
      let v1 = token env v1 (* "repeat" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Switch (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* "," *) in
      let v5 = map_arguments env v5 in
      let v6 = token env v6 (* ")" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Brk tok -> token env tok (* "break" *)
  | `Next tok -> token env tok (* "next" *)
  | `True tok -> token env tok (* "TRUE" *)
  | `False tok -> token env tok (* "FALSE" *)
  | `Null tok -> token env tok (* "NULL" *)
  | `Inf tok -> token env tok (* "Inf" *)
  | `Nan tok -> token env tok (* "NaN" *)
  | `Na x -> map_na env x
  | `Dots tok -> token env tok (* "..." *)
  | `SEMI tok -> token env tok

(* ";" *)
and map_formal_parameter (env : env) (x : CST.formal_parameter) =
  match x with
  | `Id x -> map_identifier env x
  | `Choice_id_EQ_exp (v1, v2, v3) ->
      let v1 = map_anon_choice_id_c711a0e env v1 in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Dots tok -> token env tok

(* "..." *)
and map_formal_parameters (env : env) ((v1, v2, v3) : CST.formal_parameters) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_formal_parameter env v1 in
        let v2 =
          List.map
            (fun (v1, v2) ->
              let v1 = token env v1 (* "," *) in
              let v2 = map_formal_parameter env v2 in
              todo env (v1, v2))
            v2
        in
        let v3 =
          match v3 with
          | Some tok -> token env tok (* "," *)
          | None -> todo env ()
        in
        todo env (v1, v2, v3)
    | None -> todo env ()
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_function_definition (env : env) ((v1, v2, v3) : CST.function_definition)
    =
  let v1 = token env v1 (* "function" *) in
  let v2 = map_formal_parameters env v2 in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)

and map_program (env : env) (xs : CST.program) =
  List.map
    (fun (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 =
        match v2 with
        | Some x -> (
            match x with
            | `LF tok -> token env tok (* "\n" *)
            | `SEMI tok -> token env tok (* ";" *) )
        | None -> todo env ()
      in
      todo env (v1, v2))
    xs

and map_unary (env : env) (x : CST.unary) =
  match x with
  | `DASH_exp (v1, v2) ->
      let v1 = token env v1 (* "-" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `PLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "+" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `BANG_exp (v1, v2) ->
      let v1 = token env v1 (* "!" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `TILDE_exp (v1, v2) ->
      let v1 = token env v1 (* "~" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () ->
      Parallel.backtrace_when_exn := false;
      Parallel.invoke Tree_sitter_r.Parse.file file ())
    (fun cst ->
      let env = { H.file; conv = H.line_col_to_pos file; extra = () } in
      try map_program env cst
      with Failure "not implemented" as exn ->
        let s = Printexc.get_backtrace () in
        pr2 "Some constructs are not handled yet";
        pr2 "CST was:";
        CST.dump_tree cst;
        pr2 "Original backtrace:";
        pr2 s;
        raise exn)
