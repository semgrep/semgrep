(*
   Create a list of tests for regression testing
*)
val make_tests :
  ?fail_callback:
    ((* default to Alcotest.fail msg *)
     int (* num errors *) ->
    string (* msg *) ->
    unit) ->
  ?get_xlang:(Fpath.t -> Rule.rules -> Xlang.t) option ->
  ?prepend_lang:bool ->
  Fpath.t list ->
  Alcotest_ext.test list

(* [test_rules dirs] run the tests discovered under [dirs]
 * and print a summary.
 * This is what 'semgrep-core -test_rules' run.
 *)
val test_rules : Fpath.t list -> unit
