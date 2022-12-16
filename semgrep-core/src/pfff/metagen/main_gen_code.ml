open Common

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

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

let main_action _file =
  raise Todo


(*****************************************************************************)
(* Extra actions *)
(*****************************************************************************)
let test_matcher_gen s =
  let t = OCaml.get_type s in
  Gen_ml_code.gen_matcher (s, t)

let test_matcher_all () =

  let all_types = [
    "tok";
    "wrap";
    "name";
    "dotted_name";
    "qualified_name";
    "module_name";
    "resolved_name";

    "constructor_definition";
    "action";
    "case_and_body";
    "any";
    "program";
    "item";
    "other_directive_operator";
    "alias";
    "directive";
    "class_kind";
    "class_definition";
    "other_type_definition_operator";
    "other_type_kind_operator";
    "type_definition_kind";
    "type_definition";
    "field";
    "variable_definition";
    "other_parameter_operator";
    "parameter_classic";
    "parameter";
    "parameters";
    "function_definition";
    "type_parameter_constraint";
    "type_parameter_constraints";
    "type_parameter";
    "definition_kind";
    "entity";
    "definition";
    "other_pattern_operator";
    "pattern";
    "other_stmt_operator";
    "for_var_or_expr";
    "for_header";
    "label";
    "finally";
    "catch";
    "case";
    "stmt";
    "other_attribute_operator";
    "attribute";
    "other_type_operator";
    "other_type_argument_operator";
    "type_argument";
    "type_arguments";
    "type_";
    "other_expr_operator";
    "other_argument_operator";
    "argument";
    "arguments";
    "arithmetic_operator";
    "special";
    "id_info";
    "container_operator";
    "literal";
    "expr";
    "module_definition";
    "module_definition_kind";
    "macro_definition";
    "xml";
    "field_pattern";
  ] in


  let _all_types_js = [

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
  all_types |> List.iter (fun t ->
    try
      test_matcher_gen t
    with
    | Todo ->
        pr2 ("PB TODO with : " ^ t);
    | Not_found ->
        pr2 ("PB Not_found with : " ^ t);
  );
  ()

let ffi_extra_actions () = [
  "-matcher_gen", "<type>",
  Common.mk_action_1_arg test_matcher_gen;
  "-matcher_gen_all", "",
  Common.mk_action_0_arg test_matcher_all;
]

(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () =
  ffi_extra_actions() @
  []

let options () =
  [
    "-verbose", Arg.Set verbose,
    " ";
  ] @
  Common.options_of_actions action (all_actions()) @
  (*  Common.cmdline_flags_devel () @
      Common.cmdline_flags_verbose () @
      Common.cmdline_flags_other () @
  *)
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
  ] @
  []

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () =
  let usage_msg =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^
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
     | x::_xs ->
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
