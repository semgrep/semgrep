(*
   Operations on files in the general sense (regular file, folder, etc.).

   Migration note:
   - step 1: here, we expose the same functions as in Common but using the
             Fpath.t type.
   - step 2: move the original implementation from Common.ml to here
             and stop using 'string' for file paths.
*)

(* TODO: call this type 't'? *)
type fpath = Fpath.t [@@deriving show, eq]
(* This type exists only to be used with ppx deriving because Fpath.t
   doesn't provide 'show' and 'eq' functions.

   The deriving above will define those functions below, which
   are needed if one use 'deriving eq, show' on other types
   using internally 'filename'
   (e.g., 'type foo = Foo of File.t [@@deriving show]')

   val pp_fpath: Format.formatter -> File.t -> unit
   val equal_fpath: File.t -> File.t -> bool
*)

(* Usage:

     open File.Operators
*)
module Operators : sig
  (* Fpath.add_seg = Fpath.(/) *)
  val ( / ) : Fpath.t -> string -> Fpath.t

  (* Fpath.append = Fpath.(//) *)
  val ( // ) : Fpath.t -> Fpath.t -> Fpath.t

  (* Fpath.to_string *)
  val ( !! ) : Fpath.t -> string
end

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

(* for realpath, use Unix.realpath in ocaml >= 4.13 *)
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
