(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
 * Copyright (C) 2020-2024 Semgrep Inc.
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
(* This file is mostly deprecated. You should use osemgrep text output instead
 * and stop using directly semgrep-core.
 *
 * it is unfortunately still useful to use the text output of semgrep-core,
 * even when combined with the -rules and -targets passed from pysemgrep,
 * to better see what is going on when debugging.
 *)

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
(* Print_aux *)
(*****************************************************************************)

let print_match_toks ?(str = "") ?(spaces = 0) (caps : < Cap.stdout >)
    (ii : Tok.t list) : unit =
  let print = CapConsole.print caps#stdout in
  try
    let mini, maxi = Tok_range.min_max_toks_by_pos ii in
    let end_line, _, _ = Tok.end_pos_of_loc (Tok.unsafe_loc_of_tok maxi) in
    let file, line = (Tok.file_of_tok mini, Tok.line_of_tok mini) in
    let prefix = spf "%s:%d" file line in
    let lines_str =
      UFile.lines_of_file (Tok.line_of_tok mini, end_line) (Fpath.v file)
    in
    let prefix = if str = "" then prefix else prefix ^ " " ^ str in
    let spaces_string = String.init spaces (fun _ -> ' ') in
    print (spaces_string ^ prefix);
    (* todo? some context too ? *)
    lines_str |> List.iter (fun s -> print (spaces_string ^ " " ^ s))
  with
  | Failure "get_pos: Ab or FakeTok" ->
      print "<could not locate match, FakeTok or AbstractTok>"

let print_intermediate_vars ~spaces (caps : < Cap.stdout >) (toks : Tok.t list)
    : unit =
  let spaces_string = String.init spaces (fun _ -> ' ') in
  let print str = CapConsole.print caps#stdout (spaces_string ^ str) in
  let rec loop_print curr_file = function
    | [] -> ()
    | tok :: toks -> (
        match Tok.loc_of_tok tok with
        | Error _ ->
            (* The toks are supposed to be real toks, so this should not happen, but
             * it would be better to have a list of locs then. *)
            ()
        | Ok loc ->
            let pos : Pos.t = loc.pos in
            if pos.file <> curr_file then print pos.file;
            print (spf "- %s @l.%d" loc.str pos.line);
            loop_print pos.file toks)
  in
  loop_print "<NO FILE>" toks

let rec print_taint_call_trace (caps : < Cap.stdout >) ~spaces = function
  | Pattern_match.Toks toks -> print_match_toks caps ~spaces toks
  | Call { call_toks; intermediate_vars; call_trace } ->
      let print = CapConsole.print caps#stdout in
      let spaces_string = String.init spaces (fun _ -> ' ') in
      print (spaces_string ^ "call to");
      print_match_toks caps ~spaces call_toks;
      if intermediate_vars <> [] then (
        print (spf "%sthese intermediate values are tainted:" spaces_string);
        print_intermediate_vars caps ~spaces:(spaces + 2) intermediate_vars);
      print (spaces_string ^ "then");
      print_taint_call_trace caps ~spaces:(spaces + 2) call_trace

let print_taint_trace (caps : < Cap.stdout >) taint_trace =
  taint_trace |> Lazy.force
  |> List.iteri (fun idx { Pattern_match.source_trace; tokens; sink_trace } ->
         let print = CapConsole.print caps#stdout in
         if idx =*= 0 then print "  * Taint may come from this source:"
         else print "  * Taint may also come from this source:";
         print_taint_call_trace caps ~spaces:4 source_trace;
         if tokens <> [] then (
           print "  * These intermediate values are tainted:";
           print_intermediate_vars caps ~spaces:4 tokens);
         print "  * This is how taint reaches the sink:";
         print_taint_call_trace caps ~spaces:4 sink_trace)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let print_match (caps : < Cap.stdout >) (match_ : Pattern_match.t)
    (mvars : Metavariable.mvar list) : unit =
  let print = CapConsole.print caps#stdout in
  let Pattern_match.
        { env; tokens = (lazy tokens_matched_code); taint_trace; dependency; _ }
      =
    match_
  in
  let str = spf "with rule %s" (Rule_ID.to_string match_.rule_id.id) in
  (* there are a few fake tokens in the generic ASTs now (e.g.,
   * for DotAccess generated outside the grammar) *)
  let toks = tokens_matched_code |> List.filter Tok.is_origintok in
  let dep_toks_and_version =
    (* Only print the extra data if it was a reachable finding *)
    (* TODO: special printing for lockfile-only findings *)
    match dependency with
    | Some (CodeAndLockfileMatch (dmatched, _)) ->
        Some
          ( dmatched.toks |> List.filter Tok.is_origintok,
            dmatched.package_version_string )
    | _ -> None
  in
  (if mvars =*= [] then print_match_toks caps ~str toks
   else
     (* similar to the code of Lib_matcher.print_match, maybe could
      * factorize code a bit.
      *)
     let mini, _maxi = Tok_range.min_max_toks_by_pos toks in
     let file, line = (Tok.file_of_tok mini, Tok.line_of_tok mini) in

     let strings_metavars =
       mvars
       |> List_.map (fun x ->
              match Common2.assoc_opt x env with
              | Some any ->
                  any |> Metavariable.ii_of_mval
                  |> List.filter Tok.is_origintok
                  |> List_.map Tok.content_of_tok
                  |> join_with_space_if_needed
              | None -> failwith (spf "the metavariable '%s' was not bound" x))
     in
     print (spf "%s:%d: %s" file line (String.concat ":" strings_metavars));
     ());
  dep_toks_and_version
  |> Option.iter (fun (toks, version) ->
         print ("with dependency match at version " ^ version);
         print_match_toks caps toks);
  taint_trace |> Option.iter (print_taint_trace caps)
