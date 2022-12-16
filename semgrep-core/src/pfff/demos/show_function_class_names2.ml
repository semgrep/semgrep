open Common
open Cst_php

module V = Visitor_php

let pr_func_def func_def =
  let s = Cst_php.str_of_ident func_def.f_name in
  let info = Cst_php.info_of_ident func_def.f_name in
  let line = Parse_info.line_of_info info in
  pr2 (spf "Define function %s at line %d" s line)

let pr_class_def class_def =
  let s = Cst_php.str_of_ident class_def.c_name in
  let info = Cst_php.info_of_ident class_def.c_name in
  let line = Parse_info.line_of_info info in
  pr2 (spf "Define class %s at line %d" s line)

let show_function_calls file =
  let ast = Parse_php.parse_program file in

  (*s: create visitor *)
  let visitor = V.mk_visitor { V.default_visitor with
                               V.kfunc_def = (fun (k, _) e ->
                                 pr_func_def e; k e
                               );
                               (*V.kmethod_def = (fun (k, _) e ->
                                 pr_func_def e; k e
                                 );*)
                               V.kclass_def = (fun (k, _) e ->
                                 pr_class_def e; k e
                               );
                             }
  in
  visitor (Program  ast)

let main =
  show_function_calls Sys.argv.(1)
