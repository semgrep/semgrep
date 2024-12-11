(*
   Realpath + original user-friendly path.

   A file path associated with its normalized path (absolute, physical path)
   that can be used for deduplication purposes.

   Unlike Fpath.t, this type requires the file to exist (relative to
   the current directory if the path is relative).

   Obtaining the physical path is in general costly. This allows caching it.
*)

type t = private { fpath : Fpath.t; rpath : Rpath.t; cwd : Rpath.t }
[@@deriving show, eq]

(*
   'Error msg' is returned if the physical path can't be determined.
   This is the case if the file doesn't exist at all, its resolution
   leads a broken symlink, or insufficient permissions.
*)
val of_string : string -> (t, string) Result.t
val of_fpath : Fpath.t -> (t, string) Result.t

(* Same but raise a best-effort exception if the path resolution fails.
   Use with care. *)
val of_string_exn : string -> t
val of_fpath_exn : Fpath.t -> t

(* Convert a list of paths.
   A result is a pair (valid paths, invalid paths with error). *)
val of_strings : string list -> t list * (string * string) list
val of_fpaths : Fpath.t list -> t list * (Fpath.t * string) list

(* Ignore missing or inaccessible files but log the errors in the hope
   someone sees them. *)
val of_strings_with_warnings : string list -> t list
val of_fpaths_with_warnings : Fpath.t list -> t list

(* Return the original path *)
val to_fpath : t -> Fpath.t

(* Return the realpath *)
val to_rpath : t -> Rpath.t

(* Return the current directory "." *)
val getcwd : unit -> t

(*
   Check whether the physical path associated with a relative path
   is still valid. Invalidation happens when the current working directory
   (cwd) changes. This is intended for debugging.
*)
val is_valid : t -> bool

(*
   Return the parent folder of the physical path.
*)
val parent : t -> t
