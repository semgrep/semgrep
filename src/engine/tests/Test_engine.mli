(*
   Create a list of tests for regression testing
*)
val make_tests :
  ?fail_callback:
    ((* default to Alcotest.fail msg *)
     int (* num errors *) ->
    string (* msg *) ->
    unit) ->
  (* default to Test_engine.single_analyzer_from_rules *)
  ?get_analyzer:(Fpath.t -> Rule.rules -> Analyzer.t) ->
  (* default to false *)
  ?prepend_lang:bool ->
  Fpath.t list ->
  Testo.t list

(* For Pro tests *)
val collect_tests :
  ?get_analyzer:(Fpath.t -> Rule.rules -> Analyzer.t) ->
  Fpath.t list (* roots *) ->
  (Fpath.t (* rule file *) * Fpath.t (* target file *) * Analyzer.t) list

(* helpers used in Test_subcommand.ml
 * TODO? Move in Rule_tests.mli?
 *)
val find_target_of_yaml_file_opt : Fpath.t -> Fpath.t option
val analyzers_of_rules : Rule.t list -> Analyzer.t list
val first_analyzer_of_rules : Rule.t list -> Analyzer.t
val xtarget_of_file : Analyzer.t -> Fpath.t -> Xtarget.t
