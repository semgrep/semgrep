(*
   A rule ID is essentially a string. This creates a dedicated type to
   clarify interfaces and error messages.
*)
type t [@@deriving show, eq]

exception Malformed_rule_ID of string

(* conversion functions *)
val to_string : t -> string

(* may raise Malformed_rule_ID *)
val of_string_exn : string -> t
val of_string_opt : string -> t option

(* there are a few places where we need to convert list of rule IDs *)
val to_string_list : t list -> string list
val of_string_list : string list -> t list
val compare : t -> t -> int

(* Validation function called by of_string.
   Checks for the format [a-zA-Z0-9._-]* *)
val validate : string -> bool

(* Remove any forbidden characters to produce a valid rule ID fragment. *)
val sanitize_string : string -> string

(*
   Rule ids are prepended with the `path.to.the.rules.file.`, so
   when comparing a rule (r) with the rule to be included or excluded,
   allow for a preceding path

     "path.to.foo.bar" ~suffix:"foo.bar" -> true
     "foo.bar" ~suffix:"foo.bar" -> true
     "xfoo.bar" ~suffix:"foo.bar" -> false
*)
val ends_with : t -> suffix:t -> bool

(* "path.to.foo.bar" -> Some "bar" *)
val last_elt_opt : t -> string option

(* for -e *)
val dash_e : t

(* for ATD string wrap in semgrep_output_v1.atd *)
val unwrap : t -> string
val wrap : string -> t
