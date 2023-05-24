(*
   The type of a language analyzer.

   In most cases, the language analyzer is the name of the programming
   language (Lang.t). These languages are regular Semgrep languages
   using the generic AST. Other analyzers exist, though, and they are
   included in this type.

   See also Lang.mli.

   eXtended languages: everything from Lang.t + spacegrep (generic) and regex.
*)

type t =
  (* For "real" semgrep.
     The first language is used to parse the pattern and targets.
     The other languages are extra target languages that can use the
     same pattern. *)
  | L of Lang.t * Lang.t list
  (* for pattern-regex (referred as 'regex' or 'none' in languages:) *)
  | LRegex
  (* semgrep-like pattern matching *)
  | LSpacegrep
  | LAliengrep
[@@deriving show, eq, hash]

exception InternalInvalidLanguage of string (* rule id *) * string (* msg *)

val of_lang : Lang.t -> t

(* raises an exception with error message *)
val to_lang_exn : t -> Lang.t

(* Does not raise, but returns empty list for all but the L variant *)
val to_langs : t -> Lang.t list

(* raises an exception with error message *)
val lang_of_opt_xlang_exn : t option -> Lang.t

(*
   Convert an object that represent a pattern's multiple languages into
   the list of target languages of the form 'L (lang, [])'.
*)
val flatten : t -> t list

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
*)
val of_string : ?rule_id:string -> string -> t
val to_string : t -> string
val is_proprietary : t -> bool

(* of_string/to_string for ATD e.g.
   type xlang = string wrap <ocaml module="Xlang">
*)
val wrap : string -> t
val unwrap : t -> string
