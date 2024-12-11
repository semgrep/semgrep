(*
   A dedicated type for Semgrep scanning roots to avoid confusion with
   discovered target files.
*)

(*
   The type of a scanning root.

   A scanning root is a file or a folder that exists. It may be absolute
   or relative to the current working directory. It may be a symbolic
   link, in which case it will be dereferenced when scanning for target files.

   Note that resolving symlinks may be done only after figuring out the
   project root, otherwise the project root would be inferred incorrectly.

   NOTE: This is distinguished type from the below
*)
type t = private Fpath.t [@@deriving show]

(* Conversions from/to fpaths are no-ops. *)
val of_fpath : Fpath.t -> t
val to_fpath : t -> Fpath.t
val of_string : string -> t
val to_string : t -> string
