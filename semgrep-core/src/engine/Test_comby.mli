(* [test_comby str file] will run the Comby matching engine on [file]
 * looking for the pattern [str] and report matches on stdout.
 *
 * This function is called by the -test_comby command-line action.
 *)
val test_comby : string -> Common.filename -> unit
