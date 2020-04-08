(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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

module PI = Parse_info
module R = Rule
module E = Error_code
module J = Json_type

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  rule: Rule.t;
  file: Common.filename;
  code: Ast_generic.any;
  env: Metavars_generic.metavars_binding;
}

(*****************************************************************************)
(* JSON *)
(*****************************************************************************)

let json_range min max =
  (* pfff (and Emacs) have the first column at index 0, but not r2c *)
  let adjust_column x = x + 1 in

  let min = PI.token_location_of_info min in
  let max = PI.token_location_of_info max in
  let len_max = String.length max.PI.str in
  J.Object [
    "line", J.Int min.PI.line;
    "col", J.Int (adjust_column min.PI.column);
    "offset", J.Int min.PI.charpos;
  ],
  J.Object [
    "line", J.Int max.PI.line;
    "col", J.Int (adjust_column (max.PI.column + len_max));
    "offset", J.Int (max.PI.charpos + len_max);
  ]

let range_of_any any = 
  let ii = Lib_ast.ii_of_any any in
  let ii = ii |> List.filter PI.is_origintok in
  let (min, max) = PI.min_max_ii_by_pos ii in
  let (startp, endp) = json_range min max in
  startp, endp

let json_metavar x startp (s, any) =
  let (startp, endp) = 
    try 
      range_of_any any 
    with Parse_info.NoTokenLocation exn ->
     failwith (spf 
      "NoTokenLocation %s exn while processing %s for rule %s, with metavar %s, close location = %s"
        exn x.file x.rule.R.id  s (Json_io.string_of_json startp))
  in
  s, J.Object [
  "start", startp;
  "end", endp;
  "abstract_content", J.String (
      any
      |> Lib_ast.ii_of_any |> List.filter PI.is_origintok
      |> List.sort Parse_info.compare_pos
      |> List.map PI.str_of_info 
      |> Matching_report.join_with_space_if_needed
    )
  ]
  

(* similar to pfff/h_program-lang/r2c.ml *)
let match_to_json x =
  let (startp, endp) = range_of_any x.code in

  J.Object [
    "check_id", J.String x.rule.R.id;
    "path", J.String x.file;
    "start", startp;
    "end", endp;
    "extra", J.Object [
       "message", J.String x.rule.R.message;
       "metavars", J.Object (x.env |> List.map (json_metavar x startp));
     ]
  ]

(*****************************************************************************)
(* Error *)
(*****************************************************************************)
(* this is used only in the testing code, to reuse the 
 * Error_code.compare_actual_to_expected
 *)
let error tok rule =
  match rule.R.severity with
  | R.Error ->
      E.error tok (E.SgrepLint (rule.R.id, rule.R.message))
  | R.Warning ->
      E.warning tok (E.SgrepLint (rule.R.id, rule.R.message))

let match_to_error x = 
  let toks = Lib_ast.ii_of_any x.code |> List.filter PI.is_origintok in
  let tok = List.hd toks in
  error tok x.rule
