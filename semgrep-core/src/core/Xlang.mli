(*
   Extended languages: everything from Lang.t + spacegrep (generic) and regex.
*)

type t =
  (* For "real" semgrep.
     The first language is used to parse the pattern and targets.
     The other languages are extra target languages that can use the
     same pattern. *)
  | L of Lang.t * Lang.t list
  (* for pattern-regex (referred as 'regex' or 'none' in languages:) *)
  | LRegex
  (* for spacegrep *)
  | LGeneric
[@@deriving show, eq]

exception InternalInvalidLanguage of string (* rule id *) * string (* msg *)

val of_lang : Lang.t -> t

(* raises an exception with error message *)
val to_lang : t -> Lang.t

(* raises an exception with error message *)
val lang_of_opt_xlang : t option -> Lang.t

(* map from valid extended language names to unique xlang ID *)
val assoc : (string * t) list

(* efficient map from valid extended language names to unique xlang ID *)
val map : (string, t) Hashtbl.t

(* list of valid names for extended languages, sorted alphabetically *)
val keys : string list

(* comma-separated list of the supported languages *)
val supported_xlangs : string

(*
   Convert from a string or raise an exception with an error message.
   TODO: explain the 'id' option
*)
val of_string : ?id:string option -> string -> t
val to_string : t -> string
