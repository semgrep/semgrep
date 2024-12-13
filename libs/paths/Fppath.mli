(*
   Path information for a file in a project, which includes:
   - a path in the file system (fpath)
   - the path relative to the project root (ppath)

   Both are useful in different context (e.g., ppath is usually better
   in error messages while fpath for actual file content access).
*)

type t = { fpath : Fpath.t; ppath : Ppath.t } [@@deriving show]

(* Compare based on the original fpath *)
val compare : t -> t -> int

(* Append a relative fpath to an existing root path.
   Raise Invalid_argument is the provided fpath isn't relative. *)
val append_relative_fpath : t -> Fpath.t -> t
