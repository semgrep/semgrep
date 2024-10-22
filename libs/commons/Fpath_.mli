(*
   Extended version of Fpath.

   Provides operations on file system paths only, without any access
   to the file system.
*)

(*
  Extra utilities to convert between lists of files between
  string and Fpath.t without having to write
  'List_.map Fpath.v ...' every time.

  For converting a single path, use Fpath.v and Fpath.to_string directly.

  of_strings, like Fpath.v which it uses, will raise an exception
  in case of a malformed path such as "" or "foo\000bar".

  Performance notes:
  - these operations involve creating a new list.
  - converting a path to a string is assumed to be cheap since Fpath.t
    internally is a string.
  - converting a string to a path involves validating the path syntax,
    which is more expensive.
 *)
val of_strings : string list -> Fpath.t list
val to_strings : Fpath.t list -> string list
val to_yojson : Fpath.t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (Fpath.t, string) result

(* alias but with derived available *)
type t = Fpath.t [@@deriving show, eq, ord, sexp]

(*
   Take a nonempty list of path segments and turn them in to relative path.
   Only the last segment may by empty, representing a trailing slash.
   For example, ["a"; "b"; ""] becomes "/a/b/" and
   ["a"; "b"] becomes "a/b".
   Raises Invalid_argument.
*)
val of_relative_segments : string list -> Fpath.t

(* Fpath.to_string. Like for the other operators, we recommend using it
   with 'open File.Operators'. *)
val ( !! ) : Fpath.t -> string

(* Same as Fpath.append or Fpath.(//) but if the first argument is ".",
   the second argument is returned as-is.
   For example, 'append_no_dot (Fpath.v ".") (Fpath.v "a")'
   equals 'Fpath.v "a"' rather than 'Fpath.v "./a"'. *)
val append_no_dot : Fpath.t -> Fpath.t -> Fpath.t

(*
   Operators on files or file paths or anything related to files.
   This is module is meant to be opened:

   Usage:

     open File.Operators
*)
module Operators : sig
  (* Fpath.add_seg = Fpath.(/) *)
  val ( / ) : Fpath.t -> string -> Fpath.t

  (* Fpath.append = Fpath.(//) *)
  val ( // ) : Fpath.t -> Fpath.t -> Fpath.t

  (* File.Path.(!!) = Fpath.to_string *)
  val ( !! ) : Fpath.t -> string

  (* TODO? also add this one? or use ++ a bit like we have !! to
   * avoid collision with known operators?
   *)
  (*
  val ( + ) : Fpath.t -> Fpath.ext -> Fpath.t
  *)
end

val readable : root:Fpath.t -> Fpath.t -> Fpath.t

(* Fpath.v "." *)
val current_dir : Fpath.t

(* exts (Fpath.v "foo.tar.gz") = ["tar";"gz"] *)
val exts : Fpath.t -> string list

(* split_ext ~multi:true (Fpath.v "a/foo.tar.gz") = Fpath.v "a/foo", ".tar.gz" *)
val split_ext : ?multi:bool -> Fpath.t -> Fpath.t * string

(* DO NOT USE THIS *)
val fake_file : Fpath.t
val is_fake_file : Fpath.t -> bool
