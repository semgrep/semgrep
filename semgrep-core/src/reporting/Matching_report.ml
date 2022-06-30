(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
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
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type match_format =
  (* ex: tests/misc/foo4.php:3
   *  foo(
   *   1,
   *   2);
   *)
  | Normal
  (* ex: tests/misc/foo4.php:3: foo( *)
  | Emacs
  (* ex: tests/misc/foo4.php:3: foo(1,2) *)
  | OneLine

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* When we print in the OneLine format we want to normalize the matched
 * expression or code and so only print the tokens in the AST (and not
 * the extra whitespace, newlines or comments). It's not enough though
 * to just List.map str_of_info because some PHP expressions such as
 * '$x = print FOO' would then be transformed into $x=printFOO, hence
 * this function
 *)
let rec join_with_space_if_needed xs =
  match xs with
  | [] -> ""
  | [ x ] -> x
  | x :: y :: xs ->
      if x =~ ".*[a-zA-Z0-9_]$" && y =~ "^[a-zA-Z0-9_]" then
        x ^ " " ^ join_with_space_if_needed (y :: xs)
      else x ^ join_with_space_if_needed (y :: xs)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let print_match ?(format = Normal) ?(str = "") ?(spaces = 0) ii =
  try
    let mini, maxi = PI.min_max_ii_by_pos ii in
    let file, line = (PI.file_of_info mini, PI.line_of_info mini) in
    let prefix = spf "%s:%d" file line in
    let arr = Common2.cat_array file in
    let lines = Common2.enum (PI.line_of_info mini) (PI.line_of_info maxi) in

    match format with
    | Normal ->
        let prefix = if str = "" then prefix else prefix ^ " " ^ str in
        let spaces_string = String.init spaces (fun _ -> ' ') in
        pr (spaces_string ^ prefix);
        (* todo? some context too ? *)
        lines
        |> Common.map (fun i -> arr.(i))
        |> List.iter (fun s -> pr (spaces_string ^ " " ^ s))
    (* bugfix: do not add extra space after ':', otherwise M-x wgrep will not work *)
    | Emacs -> pr (prefix ^ ":" ^ arr.(List.hd lines))
    | OneLine ->
        pr
          (prefix ^ ": "
          ^ (ii |> Common.map PI.str_of_info |> join_with_space_if_needed))
  with
  | Failure "get_pos: Ab or FakeTok" ->
      pr "<could not locate match, FakeTok or AbstractTok>"
