(* range as "start row:start col-end row:end col" -> filename -> (label * pattern) list *)
(* Sythesize possible patterns from a given target range *)
val synthesize_patterns : string -> string -> (string * string) list

(* "start row:start col-end row:end col"+ file ((range list)@[file]) -> pattern list *)
(* Generates a pattern from several target ranges that matches each target *)
val generate_pattern_choices : string list -> string list
