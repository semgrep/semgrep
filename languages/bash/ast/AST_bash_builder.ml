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
(* Prelude *)
(*****************************************************************************)
(* Helpers to build or convert AST_bash constructs *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let concat_blists (x : blist list) : blist =
  match List.rev x with
  | [] ->
      (* TODO: use actual location in the program rather than completely
         fake location *)
      Empty Tok_range.unsafe_fake_loc
  | last_blist :: blists ->
      let end_ = AST_bash_loc.blist_loc last_blist in
      List.fold_left
        (fun acc blist ->
          let start = AST_bash_loc.blist_loc blist in
          let loc = Tok_range.range start end_ in
          Seq (loc, blist, acc))
        last_blist blists

let add_redirects_to_command (cmd_r : cmd_redir) (redirects : redirect list) :
    cmd_redir =
  let all_locs = cmd_r.loc :: List_.map AST_bash_loc.redirect_loc redirects in
  let loc = Tok_range.of_list (fun loc -> loc) all_locs in
  { cmd_r with loc; redirects = cmd_r.redirects @ redirects }

let rec add_redirects_to_last_command_of_pipeline pip redirects : pipeline =
  match pip with
  | Command cmd_r -> Command (add_redirects_to_command cmd_r redirects)
  | Pipeline (loc, pip, bar, cmd_r) ->
      let cmd_r = add_redirects_to_command cmd_r redirects in
      let loc = Tok_range.range loc cmd_r.loc in
      Pipeline (loc, pip, bar, cmd_r)
  | Control_operator (loc, pip, op) ->
      let pip = add_redirects_to_last_command_of_pipeline pip redirects in
      let loc = Tok_range.range loc (AST_bash_loc.pipeline_loc pip) in
      Control_operator (loc, pip, op)

(*
   We use this only to analyze and simplify a pattern. This loses the location
   information if the list is empty.
*)
let flatten_blist blist : pipeline list =
  let rec flatten acc blist =
    match blist with
    | Seq (_loc, a, b) ->
        let acc = flatten acc a in
        flatten acc b
    | Pipelines (_loc, pips) -> List.rev_append pips acc
    | Empty _loc -> acc
  in
  flatten [] blist |> List.rev

(*
   Simple expressions returned by this function:

     foo
     $foo
     ""

   Not simple expressions:

     foo bar
     foo;
     foo > bar
     foo &
     foo | bar
*)
let rec pipeline_as_expression pip : expression option =
  match pip with
  | Command cmd_r -> (
      match (cmd_r.redirects, cmd_r.command) with
      | [], Simple_command cmd -> (
          match (cmd.assignments, cmd.arguments) with
          | [], [ arg0 ] -> Some arg0
          | _ -> None)
      | _ -> None)
  | Pipeline _ -> None
  | Control_operator (_loc, pip, (op, _tok)) -> (
      match op with
      | Foreground Fg_newline -> pipeline_as_expression pip
      | Foreground (Fg_semi | Fg_semisemi)
      | Background ->
          None)

(*
   This is necessary to that a pattern 'foo' is not translated into to a
   function/command call.
*)
let blist_as_expression blist : expression option =
  match flatten_blist blist with
  | [ pip ] -> pipeline_as_expression pip
  | _ -> None
