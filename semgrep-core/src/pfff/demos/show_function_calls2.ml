(*s: show_function_calls2.ml *)

(*s: basic pfff modules open *)
open Common
open Cst_php
(*e: basic pfff modules open *)
module V = Visitor_php

(*s: show_function_calls v2 *)
let show_function_calls file =
  let ast = Parse_php.parse_program file in

  (*s: create visitor *)
  let visitor = V.mk_visitor { V.default_visitor with
                               V.kexpr = (fun (k, _) e ->

                                 match e with
                                 | Call (Id funcname, args) ->
                                     (*s: print funcname *)
                                     let s = Cst_php.str_of_name funcname in
                                     let info = Cst_php.info_of_name funcname in
                                     let line = Parse_info.line_of_info info in
                                     pr2 (spf "Call to %s at line %d" s line);
                                     (* to handle calls inside calls *)
                                     (*e: print funcname *)
                                     k e

                                 | _ ->
                                     (*s: visitor recurse using k *)
                                     k e
                                     (*e: visitor recurse using k *)
                               );
                             }
  in
  (*e: create visitor *)
  (*s: iter on asts using visitor *)
  visitor (Program  ast)
(*e: iter on asts using visitor *)
(*e: show_function_calls v2 *)

let main =
  show_function_calls Sys.argv.(1)
(*e: show_function_calls2.ml *)
