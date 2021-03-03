(* range as "start row:start col-end row:end col" -> filename -> (label * pattern) list *)
val synthesize_patterns: string -> string -> (string * string) list

val generate_pattern_choices: string list -> string list
