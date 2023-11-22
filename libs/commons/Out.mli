(* Print on the standard output the string and print a newline after
 * (alias for Common.pr when the output is not redirected)
 *)
val put : string -> unit

(* formatter on the standard output
 * (alias for Format.std_formatter when the output is not redirected)
 *)
val formatter : unit -> Format.formatter

(* TODO:
   with_mock_output ()
   with_redirect_output ()
*)
