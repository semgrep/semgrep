(*
   Extended version of Fpath.

   Provides operations on file system paths only, without any access
   to the file system.
*)

(*
  Extra utilities to convert between lists of files between
  string and Fpath.t without having to write
  'Common.map Fpath.v ...' every time.

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

(* Fpath.to_string. Like for the other operators, we recommend using it
   with 'open File.Operators'. *)
val ( !! ) : Fpath.t -> string

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

(* Returns a string where if the last element is a directory there
   will not be a trailing slash to signify this. *)
val to_string_no_trailing_slash : Fpath.t -> string
  
