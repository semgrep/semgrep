(*
   Abstract type for a file path within a project.

   This avoids issues of Unix-style vs. Windows-style paths; All
   paths use '/' as a separator.

   The name of the module imitates Fpath.ml, but use Ppath.ml for
   Project path (instead of File path).
*)

(* All project paths are *absolute* and *normalized* *)
type t [@@deriving show]

(*
   Returns an absolute, normalized path relative to the project root.

   For instance, when given the input path "/a/b/c" and the project
   root "/a," the function would return "/b/c." Similarly, for the
   input path "a/b" and the project root "a/b," the function would
   return "/".

   This is purely syntactic.
   It is required to work on physical paths as returned by 'realpath()'
   to ensure that both paths below share the longest common prefix.

   Example of use:

     let* ppath = in_project ~root:(Fpath.v "/a") (Fpath.v "/a/b/c") in
     Ok (segments ppath, to_string ppath)

   equals to

     Ok ([""; "b"; "c"], "/b/c" )

   Note that the input path may be a symbolic link.
*)
val in_project : root:Rfpath.t -> Rfpath.t -> (t, string) result

(* Both arguments must be physical paths. Use only in tests. *)
val in_project_unsafe : phys_root:Fpath.t -> Fpath.t -> (t, string) result

(* Creates a ppath from a list of segments. Segments may not contain
   slashes. The first segment must be "" because ppath must be
   absolute paths.

   Fail if the resulting path is absolute but refers to a file above the
   root e.g. '/..' or '/../..'.
   Example of syntactic path normalization:
   foo/../bar -> bar  (even if 'foo/' doesn't exist)

   Example of use: from_segments ["";"a";"b";"..";"c"] should
   return a ppath whose final segments are ["";"a";"c"].

   Raises Invalid_argument if the segments can't be normalized into a
   valid ppath.
*)
val create : string list -> t

(*
   Convert back to a system path.
   If no root is given, a relative path is returned.
*)
val to_fpath : ?root:Fpath.t -> t -> Fpath.t

(* Convert to the string-based representation for regexp-matching purposes.
   The resulting path uses '/' as a separator and starts with a '/'.
   Do not use function this for other purposes regexp matching.
   - for testing and logging, use 'to_string_for_tests';
   - for conversion to system paths, use 'to_fpath'.
*)
val to_string_fast : t -> string

(* /, useful as a folding starting point *)
val root : t

(* The first element returned will always be "", because ppaths are always
 * absolute paths
 *)
val segments : t -> string list

(* Same as 'segments' but remove the leading empty segment corresponding
   to the leading '/'. *)
val relative_segments : t -> string list

(* Append a segment to a path.
   ".." and "." are not supported by this operation.
   Raises Invalid_argument.
*)
val add_seg : t -> string -> t

(* Append segments to a path.
   ".." and "." are not supported by this operation.
   Raises Invalid_argument. *)
val add_segs : t -> string list -> t

(* Append a relative fpath.
   Raises Invalid_argument if the fpath is absolute. *)
val append_fpath : t -> Fpath.t -> t

(* Create a ppath from a relative fpath.
   Raises Invalid_argument if the input is not a relative path. *)
val of_relative_fpath : Fpath.t -> t

(* Express a ppath relatively to another. The result is relative fpath.
   It can't be a ppath because ppaths can't be relative to one another.

   For example, in string notation, 'relativize ~root:"/a/b" "/a/b/c/d"'
   gives "c/d".

   The 'root' must be a prefix of the other path. No '..' or '.' are
   supported.
*)
val relativize : root:t -> t -> Fpath.t

(* Imitate File.Operators in libs/commons/ *)
module Operators : sig
  (* Same as append *)
  val ( / ) : t -> string -> t
end

(* internals *)

(* A slash-separated path. Must start with a slash. *)
val of_string_for_tests : string -> t

(* Produce a path that starts with '/', matching the internal
   representation.
   Outside of tests, use 'to_fpath' to produce an absolute or a relative
   path in the file system.
*)
val to_string_for_tests : t -> string
