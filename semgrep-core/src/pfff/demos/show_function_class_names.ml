open Common
open Cst_php

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

  ast |> List.iter (fun toplevel ->
    match toplevel with
    | FuncDef func_def ->
        pr_func_def func_def
    | ClassDef class_def ->
        pr_class_def class_def;
        (unbrace class_def.c_body) |> List.iter (fun class_stmt ->
          match class_stmt with
          | Method func_def->
              pr_func_def func_def
          | _ -> ()
        )
    | _ -> ()
  )

let main =
  show_function_calls Sys.argv.(1)
