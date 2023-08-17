(*
   Operations on files in the general sense (regular file, folder, etc.).

   Migration note:
   - step 1: here, we expose the same functions as in Common but using the
             Fpath.t type.
   - step 2: move the original implementation from Common.ml to here
             and stop using 'string' for file paths.
*)

(*
   Extended version of Fpath.

   Provides operations on file system paths only, without any access
   to the file system.
*)
module Path : sig
  include module type of Fpath

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
end

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

(* For realpath, use Unix.realpath in OCaml >= 4.13, or Rpath.mli *)
(*
   Check that the file exists and produce a valid absolute path for the file.
*)
val fullpath : Fpath.t -> Fpath.t
val readable : root:Fpath.t -> Fpath.t -> Fpath.t

(* use the command 'find' internally and tries to skip files in
 * version control system (vcs) (e.g., .git, _darcs, etc.).
 * Deprecated?
 *)
val files_of_dirs_or_files_no_vcs_nofilter : Fpath.t list -> Fpath.t list

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(* Inputs a line of text in a platform-agnostic way. Should be preferred over
   `input_line`, especially when dealing with Windows.
   More info can be found in `Common.ml`.
   This in-channel should be opened in binary mode.
*)
val input_text_line : in_channel -> string

(*
   Return the lines of a file. Both Windows-style and Unix-style line endings
   are recognized and removed from the end of the line.
*)
val cat : Fpath.t -> string list
val write_file : Fpath.t -> string -> unit

(* Read the contents of file.

   This implementation works even with Linux files like /dev/fd/63
   created by bash when using "process substitution"* e.g.

     my-ocaml-program <(echo contents)

   * https://www.gnu.org/software/bash/manual/html_node/Process-Substitution.html

   If max_len is specified, at most that many bytes are read from the file.
*)
val read_file : ?max_len:int -> Fpath.t -> string

(* Scheme-inspired combinators that automatically close the file
 * once the function callback is done. Here is an example of use:
 *   with_open_outfile "/tmp/foo.txt" (fun (pr, _chan) ->
 *     pr "this goes in foo.txt"
 *   )
 *)
val with_open_outfile : Fpath.t -> ((string -> unit) * out_channel -> 'a) -> 'a
val with_open_infile : Fpath.t -> (in_channel -> 'a) -> 'a

(* creation of /tmp files, a la gcc
 * ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c"
 *)
val new_temp_file : string (* prefix *) -> string (* suffix *) -> Fpath.t
val erase_temp_files : unit -> unit
val erase_this_temp_file : Fpath.t -> unit

val find_first_match_with_whole_line :
  Fpath.t -> ?split:char -> string -> string option
(** [find_first_match_with_whole_line path ~split term] opens [path], split it
    by the given [split] character (defaults to ['\n']) and tries to return the
    {b first} full element which contains the given [term].

    For instance, you can search the first line which contains ["semgrep"]:

    {[
      find_first_match_with_whole_line my_file "semgrep"
    ]}

    It will returns the first {b full} line which contains the ["semgrep"]
    occurrence. *)

(*****************************************************************************)
(* File properties *)
(*****************************************************************************)
val is_executable : Fpath.t -> bool
val filesize : Fpath.t -> int
val filemtime : Fpath.t -> float

(* [lines_of_file (start_line, end_line) file] returns
 * the list of lines from start_line to end_line included.
 *
 * Note that the returned lines do not contain \n.
 *
 * This function is slow, you should not use it!
 *)
val lines_of_file : int * int -> Fpath.t -> string list
