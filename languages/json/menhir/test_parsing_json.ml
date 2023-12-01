open Common

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_parse _xs = raise Todo
let test_dump _file = raise Todo

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () =
  [
    ("-parse_json", "   <file or dir>", Arg_.mk_action_n_arg test_parse);
    (* -dump_json uses the json-wheel pretty printer *)
    ("-dump_ast_json", "   <file>", Arg_.mk_action_1_arg test_dump);
  ]
