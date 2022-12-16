(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
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

open Ast_web
open Ast_html

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type program2 = Ast_web.web_document * Ast_web.token list

exception Parse_error of Parse_info.t

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse2 filename =
  let (ast, _toks) = Parse_html.parse filename in

  let js = ref [] in
  let css = ref [] in

  let tokens = ref [] in

  let rec visit = function
    | Element ((Tag (s_tag, _tok_t)), _attrs, xs) ->
        (match s_tag, xs with
         | "script", [Data (s, tok)] ->
             let tmpfile = Common.new_temp_file "web" ".js" in
             Common.write_file ~file:tmpfile s;
             let ast = Parse_js.parse_program tmpfile in
             Common.push (tok, ast) js;

         | "style", [Data (s, tok)] ->
             let tmpfile = Common.new_temp_file "web" ".css" in
             Common.write_file ~file:tmpfile s;
             let (ast, _toks) = Parse_css.parse tmpfile in
             Common.push (tok, ast) css;

         | ("script" | "style"), _ ->
             failwith "wrong script/style tag"
         | _ -> ()
        );
        xs |> List.iter visit;
    | Data _ ->
        ()
  in
  visit ast;
  {
    html = ast;
    (* TODO *)
    js = !js;
    css = !css;
    (* TODO *)
    stuff_in_js = [];
  }, !tokens


let parse a =
  Common.profile_code "Parse_web.parse" (fun () -> parse2 a)
