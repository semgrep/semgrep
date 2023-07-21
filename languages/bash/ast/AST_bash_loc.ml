(* Martin Jambon
 *
 * Copyright (C) 2021 Semgrep Inc.
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
open AST_bash

(*****************************************************************************)
(* Extraction of the first token, used for its location info *)
(*****************************************************************************)

let wrap_loc (_, tok) : loc = (tok, tok)
let bracket_loc (start_tok, _, end_tok) : loc = (start_tok, end_tok)
let todo_loc (TODO loc) = loc

let command_loc = function
  | Simple_command x -> x.loc
  | And (loc, _, _, _) -> loc
  | Or (loc, _, _, _) -> loc
  | Subshell (loc, _) -> loc
  | Command_group (loc, _) -> loc
  | Sh_test (loc, _) -> loc
  | Bash_test (loc, _) -> loc
  | Arithmetic_expression (loc, _) -> loc
  | For_loop (loc, _, _, _, _, _, _) -> loc
  | For_loop_c_style (loc, _) -> loc
  | Select (loc, _, _, _, _, _, _) -> loc
  | Case (loc, _, _, _, _, _) -> loc
  | If (loc, _, _, _, _, _, _, _) -> loc
  | While_loop (loc, _, _, _, _, _) -> loc
  | Until_loop (loc, _, _, _, _, _) -> loc
  | Coprocess (loc, _, _) -> loc
  | Assignment x -> x.loc
  | Declaration x -> x.loc
  | Negated_command (loc, _, _) -> loc
  | Function_definition (loc, _) -> loc

let pipeline_loc = function
  | Command x -> x.loc
  | Pipeline (loc, _, _, _) -> loc
  | Control_operator (loc, _, _) -> loc

let blist_loc = function
  | Seq (loc, _, _) -> loc
  | Pipelines (loc, _) -> loc
  | Empty loc -> loc

let _sh_test_loc (x : sh_test) =
  let open_, _, close = x in
  (open_, close)

let _bash_test_loc (x : bash_test) =
  let open_, _, close = x in
  (open_, close)

let _arithmetic_expression_loc (x : arithmetic_expression) =
  let open_, _, close = x in
  (open_, close)

let _case_clause_loc ((loc, _, _, _, _) : case_clause) = loc

let case_clause_terminator_tok = function
  | Break tok
  | Fallthrough tok
  | Try_next tok ->
      tok

let _case_clause_terminator_loc x =
  let tok = case_clause_terminator_tok x in
  (tok, tok)

let _elif_loc (x : elif) =
  let loc, _elif, _cond, _then, _body = x in
  loc

let _else_loc (x : else_) =
  let loc, _else, _body = x in
  loc

let assignment_loc (x : assignment) = x.loc

let _assignment_list_loc (x : assignment list) =
  Tok_range.of_list assignment_loc x

let _declaration_loc (x : declaration) = x.loc

let expression_loc = function
  | Word x -> wrap_loc x
  | String x -> bracket_loc x
  | String_fragment (loc, _) -> loc
  | Raw_string x -> wrap_loc x
  | Ansii_c_string x -> wrap_loc x
  | Special_character x -> wrap_loc x
  | Concatenation (loc, _) -> loc
  | Expr_semgrep_ellipsis tok -> (tok, tok)
  | Expr_semgrep_deep_ellipsis (loc, _) -> loc
  | Expr_semgrep_metavar x -> wrap_loc x
  | Equality_test (loc, _, _) -> loc
  | Empty_expression loc -> loc
  | Array (loc, _) -> loc
  | Process_substitution (loc, _) -> loc

let string_fragment_loc = function
  | String_content x -> wrap_loc x
  | Expansion (loc, _) -> loc
  | Command_substitution x -> bracket_loc x
  | Frag_semgrep_metavar x -> wrap_loc x
  | Frag_semgrep_named_ellipsis x -> wrap_loc x

let _expansion_loc = function
  | Simple_expansion (loc, _) -> loc
  | Complex_expansion x -> bracket_loc x

let variable_name_loc x = variable_name_wrap x |> wrap_loc

let _complex_expansion_loc = function
  | Variable (loc, _) -> loc
  | Complex_expansion_TODO loc -> loc

let _command_substitution_loc x = bracket_loc x

let eq_op_loc = function
  | EQTILDE (* "=~" *) tok -> (tok, tok)
  | EQEQ (* "==" *) tok -> (tok, tok)

let right_eq_operand_loc = function
  | Literal (loc, _) -> loc
  | Regexp x -> wrap_loc x

let test_expression_loc = function
  | T_expr (loc, _)
  | T_unop (loc, _, _)
  | T_binop (loc, _, _, _)
  | T_not (loc, _, _)
  | T_and (loc, _, _, _)
  | T_or (loc, _, _, _)
  | T_todo loc ->
      loc

let _write_redir_src_loc (x : write_redir_src) =
  match x with
  | Stdout tok
  | Stdout_and_stderr tok
  | File_descriptor (_, tok) ->
      (tok, tok)

let _file_redir_target_loc (x : file_redir_target) =
  match x with
  | File e -> expression_loc e
  | File_descriptor w -> wrap_loc w
  | Stdout_and_stderr tok -> (tok, tok)
  | Close_fd tok -> (tok, tok)

let redirect_loc (x : redirect) =
  match x with
  | File_redirect (loc, _) -> loc
  | Read_heredoc x -> todo_loc x
  | Read_herestring x -> todo_loc x

let cmd_redir_loc { loc; command = _; redirects = _ } = loc
