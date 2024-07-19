(* a metavariable name (e.g. "$FOO") *)
type t = string [@@deriving show, eq, hash]

(* return whether a string could be a metavariable name (e.g., "$FOO", but not
 * "FOO"). This mostly check for the regexp $[A-Z_][A-Z_0-9]* but
 * also handles special variables like $_GET in PHP which are actually
 * not metavariables.
 *)
val is_metavar_name : t -> bool

(* example: "$...FOO" is a metavariable ellipsis *)
val is_metavar_ellipsis : t -> bool

(* metavariables like $_ *)
val is_anonymous_metavar : t -> bool

(* example: "$1" *)
val is_metavar_for_capture_group : string -> bool

(* ??? *)
val mvars_of_regexp_string : string -> t list
