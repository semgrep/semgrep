(*s: semgrep/reporting/json_report.ml *)
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
open Ast_generic

module PI = Parse_info
module R = Rule
module E = Error_code
module J = Json_type

open Match_result

(*****************************************************************************)
(* Unique ID *)
(*****************************************************************************)
(*s: function [[Json_report.string_of_resolved]] *)
let string_of_resolved = function
  | Global -> "Global"
  | Local -> "Local"
  | Param -> "Param"
  | EnclosedVar -> "EnclosedVar"
  | ImportedEntity _ -> "ImportedEntity"
  | ImportedModule _ -> "ImportedModule"
  | TypeName -> "TypeName"
  | Macro -> "Macro"
  | EnumConstant -> "EnumConstant"
(*e: function [[Json_report.string_of_resolved]] *)

(*s: function [[Json_report.unique_id]] *)
(* Returning scoping-aware information about a metavariable, so that
 * the callers of sgrep (sgrep-lint) can check if multiple metavariables
 * reference the same entity, or reference exactly the same code.
 * See pfff/.../naming_ast.ml for more information.
 *)
let unique_id any =
  match any with
  | E (Id (id, { id_resolved = {contents = Some (resolved, sid)}; _})) ->
      J.Object [
        "type", J.String "id";
        "value", J.String (Ast_generic.str_of_ident id);
        "kind", J.String (string_of_resolved resolved);
        (* single unique id *)
        "sid", J.Int sid;
      ]
  (* not an Id, return a md5sum of its AST as a "single unique id" *)
  | _ ->

     (* todo? note that if the any use a parameter, or a local,
      * as in foo(x): return complex(x), then they will have different
      * md5sum because the parameter will be different! We may
      * want to abstract also the resolved information in those cases.
      *)
     let any = Lib_ast.abstract_position_info_any any in
     (* alt: Using the AST dumper should work also.
      * let v = Meta_ast.vof_any any in
      * let s = Ocaml.string_of_v v in
      *)
     let s = Marshal.to_string any [] in
     let md5 = Digest.string s in
     J.Object [
      "type", J.String "AST";
      "md5sum", J.String (Digest.to_hex md5);
     ]
(*e: function [[Json_report.unique_id]] *)


(*****************************************************************************)
(* JSON *)
(*****************************************************************************)

(*s: function [[Json_report.json_range]] *)
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
(*e: function [[Json_report.json_range]] *)

(*s: function [[Json_report.range_of_any]] *)
let range_of_any any = 
  let ii = Lib_ast.ii_of_any any in
  let ii = ii |> List.filter PI.is_origintok in
  let (min, max) = PI.min_max_ii_by_pos ii in
  let (startp, endp) = json_range min max in
  startp, endp
(*e: function [[Json_report.range_of_any]] *)

(*s: function [[Json_report.json_metavar]] *)
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
    );
  "unique_id", unique_id any
  ]
(*e: function [[Json_report.json_metavar]] *)
  

(*s: function [[Json_report.match_to_json]] *)
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
(*e: function [[Json_report.match_to_json]] *)

(*****************************************************************************)
(* Error *)
(*****************************************************************************)
(*s: function [[Json_report.error]] *)
(* this is used only in the testing code, to reuse the 
 * Error_code.compare_actual_to_expected
 *)
let error tok rule =
  match rule.R.severity with
  | R.Error ->
      E.error tok (E.SgrepLint (rule.R.id, rule.R.message))
  | R.Warning ->
      E.warning tok (E.SgrepLint (rule.R.id, rule.R.message))
  | R.Info ->
      E.info tok (E.SgrepLint (rule.R.id, rule.R.message))
(*e: function [[Json_report.error]] *)

(*s: function [[Json_report.match_to_error]] *)
let match_to_error x = 
  let toks = Lib_ast.ii_of_any x.code |> List.filter PI.is_origintok in
  let tok = List.hd toks in
  error tok x.rule
(*e: function [[Json_report.match_to_error]] *)
(*e: semgrep/reporting/json_report.ml *)
