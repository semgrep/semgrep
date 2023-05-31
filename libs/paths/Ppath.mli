(*
   Abstract type for a file path within a project.

   This avoids issues of Unix-style vs. Windows-style paths; All
   paths use '/' as a separator.

   The name of the module imitates Fpath.ml, but use Ppath.ml for
   Project path (instead of File path).
*)

(* All file paths are *absolute* and *normalized* *)
type t

(*
   Returns an absolute, normalized path relative to the project root.
   This is purely syntactic.
   It is recommended to work on physical paths as returned by 'realpath()'
   to ensure that both paths below share the longest common prefix.

   Example of use:

     let* ppath = in_project ~root:(Fpath.v "/a") (Fpath.v "/a/b/c") in
     Ok (segments ppath, to_string ppath)

   equals to

     Ok ([""; "b"; "c"], "/b/c" )
*)
val in_project : root:Fpath.t -> Fpath.t -> (t, string) result

(* Creates a ppath from a list of segments. Segments may not contain
   slashes. The first segment must be "" because ppath must be
   absolute paths.

   Fail if the resulting path is absolute but refers to a file above the
   root e.g. '/..' or '/../..'.
   Example of syntactic path normalization:
   foo/../bar -> bar  (even if 'foo/' doesn't exist)

   Example of use: from_segments ["";"a";"b";"..";"c"] should
   return a ppath whose final segments are ["";"a";"c"].
*)
val from_segments : string list -> (t, string) result

(* Convert back to a system path. *)
val to_fpath : root:Fpath.t -> t -> Fpath.t

(* /, useful as a folding starting point *)
val root : t

(* Deprecated: you should use to_fpath (and then Fpath.to_string if needed) *)
val to_string : t -> string

(* The first element returned will always be "", because ppaths are always
 * absolute paths
 *)
val segments : t -> string list

(* Append a segment to a path. *)
val add_seg : t -> string -> t

(* Imitate File.Operators in libs/commons/ *)
module Operators : sig
  (* Same as append *)
  val ( / ) : t -> string -> t
end

(* ------------- TO DELETE *)

val of_fpath : Fpath.t -> t

(* Create a path from the list of segments. Segments may not contain
   slashes. *)
val create : string list -> t

(* Turn foo/bar into /foo/bar *)
val make_absolute : t -> t
val normalize_ppath : t -> (t, string) result

(* internals *)

(* A slash-separated path. *)
val of_string_for_tests : string -> t
