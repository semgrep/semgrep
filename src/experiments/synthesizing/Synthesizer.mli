(* TODO: please explain what this whole module is about *)

(* range as "start row:start col-end row:end col" -> filename -> (label * pattern) list *)
(* Sythesize possible patterns from a given target range *)
val synthesize_patterns :
  Rule_options.t -> string -> Fpath.t -> (string * string) list

(* "start row:start col-end row:end col"+ file ((range list)@[file]) -> pattern list *)
(* Generates a pattern from several target ranges that matches each target.
   Returns the detected language, the list of targest, and the Pattern list.
*)

val generate_pattern_from_targets :
  Rule_options.t -> string list -> Lang.t * AST_generic.any list * Pattern.t

(* diff_files -> cve_results (see interfaces/) *)
(* For each file, given the location of a patch for that file,
   identify the functions that were patched.
   This helps autogenerate SCA rules from vulnerability reports *)
val locate_patched_functions : Fpath.t -> string

(* "start row:start col-end row:end col"+ file ((range list)@[file]) -> pattern list *)
(* Prints the pattern created by generate_patter_from_targets. *)
val print_pattern_from_targets : Rule_options.t -> string list -> string list
