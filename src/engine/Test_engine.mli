(*
   Create a list of tests for unit testing and a function to print
   a summary once each test ran once.
*)
val make_tests :
  ?unit_testing:bool ->
  ?get_xlang:(Common.filename -> Rule.rules -> Xlang.t) option ->
  Common.filename list ->
  (string * (unit -> unit)) list * (unit -> unit)

(* Run the tests and print a summary. *)
val test_rules : ?unit_testing:bool -> Common.filename list -> unit
