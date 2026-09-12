(*
   Maintain a 1:1 map of translations from 'src' to 'dst', ensuring 'dst'
   is not reserved or already taken by a different translation.
*)

(* Translation map. *)
type t

(*
   Initialize a translation map. 'reserved_dst' specifies forbidden 'dst'
   elements. It guarantees that no 'src' string will be translated to
   to one of these reserved elements.

   Additionally, any 'src' in the 'reserved_dst' list is set to be translated
   with a single underscore appended.

   Example:

     let map = create ~reserved_dst:["let"] () in
     assert (translate map "let" = "let_")
*)
val create : ?reserved_dst:string list -> unit -> t

(* Translate a string 'src' to a string 'dst', ensuring that
   'dst' is as close as possible to 'preferred_dst' and that
   nothing else already translates to that 'dst'.
   'preferred_dst' defaults to 'src'.

   This translation is remembered, with the consequence that calling this
   function later with the same arguments is guaranteed to return the same
   result.
*)
val add_translation : ?preferred_dst:string -> t -> string -> string

(* Check for an existing translation. *)
val translate : t -> string -> string option

(* Lowercase identifiers that are keywords in OCaml. *)
val ocaml_keywords : string list

(* Lowercase identifiers that are built-in type names in OCaml. *)
val ocaml_builtin_types : string list

(* Type names reserved for use by the code generator. *)
val reserved_type_names : string list

(* Union of ocaml_keywords, ocaml_builtin_types, reserved_type_names *)
val ocaml_reserved : string list
