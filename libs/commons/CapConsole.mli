(* Print a string, print a newline, and flush the stdout channel. *)
val print : Cap.Console.stdout -> string -> unit

val ocolor_format_printf :
  Cap.Console.stdout -> ('b, Format.formatter, unit) format -> 'b
