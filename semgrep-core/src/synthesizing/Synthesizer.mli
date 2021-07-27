(* range as "start row:start col-end row:end col" -> filename -> (label * pattern) list *)
(* Sythesize possible patterns from a given target range *)
val synthesize_patterns :
  Config_semgrep.t -> string -> string -> (string * string) list

(* "start row:start col-end row:end col"+ file ((range list)@[file]) -> pattern list *)
(* Generates a pattern from several target ranges that matches each target.
  Returns the detected language, the list of targest, and the Pattern list.
*)

val generate_pattern_from_targets :
  Config_semgrep.t -> string list -> Lang.t * AST_generic.any list * Pattern.t

(* "start row:start col-end row:end col"+ file ((range list)@[file]) -> pattern list *)
(* Prints the pattern created by generate_patter_from_targets. *)
val print_pattern_from_targets : Config_semgrep.t -> string list -> string list
