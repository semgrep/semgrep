val put : Cap.Console.stdout -> string -> unit

val ocolor_format_printf :
  Cap.Console.stdout -> ('b, Format.formatter, unit) format -> 'b
