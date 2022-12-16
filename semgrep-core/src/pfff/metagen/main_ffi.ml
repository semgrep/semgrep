(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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


(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(*
ocamlc -I ../commons unix.cma str.cma ../commons/lib.cma gen_boilerplate.ml
 *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action file =
  let (ast2, _stat) = Parse_php.parse file in
  let ast = Parse_php.program_of_program2 ast2 in

  Export_ast_php.show_info := true;
  let s = Export_ast_php.sexp_string_of_program ast in
  pr2 s;
  pr2 "";

  let v = Meta_ast_php.vof_program ast in
  let s = OCaml.string_sexp_of_v v in
  pr2 s;
  pr2 "";

  ()


(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)
let test_python_gen s =
  raise Todo
(*
  let t = OCaml.get_type s in
  Python_php.generate_classes (s, t)
*)

let test_python_all () =
  raise Todo
(*
  pr (Python_php.prelude);

  let all_types = [

    "parse_info";

    "exprbis";
    "variablebis";
    "stmt_and_def";
    "stmt";


    "pinfo";
    "bracket";
    "brace";
    "paren";
    "wrap";
    "tok";
    "info";

    "fully_qualified_class_name";
    "qualifier";
    "dname";
    "name";
    "ptype";
    "w_variable";
    "r_variable";
    "rw_variable";
    "obj_dim";
    "obj_property";
    "obj_access";
    "argument";
    "indirect";
    "var_info";
    "variable";
    "obj_prop_access";
    "class_name_reference";
    "array_pair";
    "list_assign";
    "castOp";
    "unaryOp";
    "assignOp";
    "logicalOp";
    "arithOp";
    "binaryOp";
    "fixOp";
    "encaps";
    "cpp_directive";
    "constant";
    "scalar";
    "exp_info";
    "expr";
    "program";
    "toplevel";
    "static_array_pair";
    "static_scalar_affect";
    "static_scalar";
    "static_var";
    "global_var";
    "method_body";
    "modifier";
    "method_def";
    "class_var_modifier";
    "class_variable";
    "class_constant";
    "class_stmt";
    "interface_def";
    "interface";
    "extend";
    "class_type";
    "class_def";
    "is_ref";
    "hint_type";
    "parameter";
    "func_def";
    "new_else";
    "new_elseif";
    "colon_stmt";
    "declare";
    "use_filename";
    "catch";
    "foreach_variable";
    "foreach_arrow";
    "for_expr";
    "case";
    "switch_case_list";
  ]
  in
  all_types +> List.iter test_python_gen;
  ()
*)

let ffi_extra_actions () = [
  "-python_gen", "<type>",
  Common.mk_action_1_arg test_python_gen;
  "-python_gen_all", "",
  Common.mk_action_0_arg test_python_all;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () =
  ffi_extra_actions() ++
  Test_meta_php.actions () ++
  []

let options () =
  [
    "-verbose", Arg.Set verbose,
    " ";
  ] ++
  Common.options_of_actions action (all_actions()) ++
  Common.cmdline_flags_devel () ++
  Common.cmdline_flags_verbose () ++
  Common.cmdline_flags_other () ++
  [
(*
  "-version",   Arg.Unit (fun () ->
    pr2 (spf "XXX version: %s" Config.version);
    exit 0;
  ),
    "  guess what";
*)

    (* this can not be factorized in Common *)
    "-date",   Arg.Unit (fun () ->
      pr2 "version: $Date: 2008/10/26 00:44:57 $";
      raise (Common.UnixExit 0)
    ),
    "   guess what";
  ] ++
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () =
  let usage_msg =
    "Usage: " ^ Common.basename Sys.argv.(0) ^
    " [options] <file or dir> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () ->

    (match args with

     (* --------------------------------------------------------- *)
     (* actions, useful to debug subpart *)
     (* --------------------------------------------------------- *)
     | xs when List.mem !action (Common.action_list (all_actions())) ->
         Common.do_action !action xs (all_actions())

     | [] when !action = "-yyy" ->
         pr2 "yyy"

     | _ when not (Common.null_string !action) ->
         failwith ("unrecognized action or wrong params: " ^ !action)

     (* --------------------------------------------------------- *)
     (* main entry *)
     (* --------------------------------------------------------- *)
     | x::xs ->
         main_action x

     (* --------------------------------------------------------- *)
     (* empty entry *)
     (* --------------------------------------------------------- *)
     | [] ->
         Common.usage usage_msg (options());
         failwith "too few arguments"
    )
  )



(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () ->
    main ();
  )
