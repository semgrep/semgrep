(*
   Create a list of tests for regression testing
*)
val make_tests :
  ?fail_callback:
    ((* default to Alcotest.fail msg *)
     int (* num errors *) ->
    string (* msg *) ->
    unit) ->
  (* default to Test_engine.single_xlang_from_rules *)
  ?get_xlang:(Fpath.t -> Rule.rules -> Xlang.t) ->
  (* default to false *)
  ?prepend_lang:bool ->
  Fpath.t list ->
  Alcotest_ext.test list

(* helpers used in Test_subcommand.ml *)
val find_target_of_yaml_file_opt : Fpath.t -> Fpath.t option
val xlangs_of_rules : Rule.t list -> Xlang.t list
val first_xlang_of_rules : Rule.t list -> Xlang.t
val xtarget_of_file : Xlang.t -> Fpath.t -> Xtarget.t
