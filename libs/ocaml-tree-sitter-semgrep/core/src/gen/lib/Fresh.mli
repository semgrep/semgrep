(*
   Generate unique names with a given prefix.
*)

(* Registry of all names. *)
type scope

val create_scope : unit -> scope

(* Initialize a scope with a list of unique names. Returns the list
   of duplicates in case of Error. *)
val init_scope : string list -> (scope, string list) result

(* Return a unique name within the scope, with the given prefix.

   For example, a first call to [create_name "foo"] returns ["foo"].
   A later call to [create_name "foo"] must return something different,
   which may be ["foo2"].
*)
val create_name : scope -> string -> string
