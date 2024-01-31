(*
   Realpath + original user-friendly path

   A file path associated with its normalized path (absolute, physical path)
   that can be used for deduplication purposes.

   Obtaining the physical path is in general costly. This allows caching it.
*)

type t = private { fpath : Fpath.t; rpath : Rpath.t } [@@deriving show, eq]

val of_string : string -> t
val of_fpath : Fpath.t -> t

(* Return the original path *)
val to_fpath : t -> Fpath.t

(* Return the realpath *)
val to_rpath : t -> Rpath.t

(* Return the current directory "." *)
val getcwd : unit -> t
