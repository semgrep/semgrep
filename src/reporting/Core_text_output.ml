(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
 * Copyright (C) 2020-2023 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This file is deprecated. You should use osemgrep text output *)

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
[@@deriving show]

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
    let mini, maxi = Tok_range.min_max_toks_by_pos ii in
    let end_line, _, _ = Tok.end_pos_of_loc (Tok.unsafe_loc_of_tok maxi) in
    let file, line = (Tok.file_of_tok mini, Tok.line_of_tok mini) in
    let prefix = spf "%s:%d" file line in
    let lines_str =
      File.lines_of_file (Tok.line_of_tok mini, end_line) (Fpath.v file)
    in
    match format with
    | Normal ->
        let prefix = if str = "" then prefix else prefix ^ " " ^ str in
        let spaces_string = String.init spaces (fun _ -> ' ') in
        pr (spaces_string ^ prefix);
        (* todo? some context too ? *)
        lines_str |> List.iter (fun s -> pr (spaces_string ^ " " ^ s))
    (* bugfix: do not add extra space after ':', otherwise M-x wgrep will not work *)
    | Emacs ->
        pr (prefix ^ ":" ^ Common.hd_exn "unexpected empty list" lines_str)
    | OneLine ->
        pr
          (prefix ^ ": "
          ^ (ii |> Common.map Tok.content_of_tok |> join_with_space_if_needed))
  with
  | Failure "get_pos: Ab or FakeTok" ->
      pr "<could not locate match, FakeTok or AbstractTok>"
