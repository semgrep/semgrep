
(* This path type uses private variants:
   https://v2.ocaml.org/manual/privatetypes.html#ss%3Aprivate-types-variant

   A private variant constructor is one which behaves like a regular constructor,
   but it cannot be freely used to construct values of the given type.

   This means that a `path_special` can only be constructed via `Path.of_string` (below),
   but it can be freely destructed via pattern-matching without needing to call `Path.to_string`.
   This ensures that all values of the type `path_special` must go through `of_string`, and in
   particular, ensures that they all must be validated by `Unix.realpath`.

   Importantly, due to this reliance on `Unix.realpath`, this means that all values of this type
   must be valid paths to existent files or directories. You cannot use this for something which
   does not yet exist. See some examples of this in `Unit_commons.test_path_conversion()`.
*)
type path = private Path of string [@@deriving show, eq]
type t = path [@@deriving show, eq]


val of_string : string -> t
val to_string : t -> string
val canonical : string -> string

val (/) : t -> string -> t
val concat : t -> string -> t
val apply : f:(string -> 'a) -> t -> 'a

val cat : t -> string list

val write_file : file:t -> string -> unit

val read_file : ?max_len:int -> t -> string
val file_exists : t -> bool

val is_directory : t -> bool
val basename : t -> string
val dirname : t -> t
val extension : t -> string
