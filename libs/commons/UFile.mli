(*
   Operations on files in the general sense (regular file, folder, etc.).

   As opposed to Fpath.ml, which is purely syntactical, the functions below
   actually relies on the filesystem.

   TODO: you should use the capability-aware functions in CapFS.ml
   instead of the functions in this unsafe (hence the U prefix) module.
*)

(*****************************************************************************)
(* Paths *)
(*****************************************************************************)

(* ugly: internal flag for files_of_dir_or_files_no_vcs_nofilter *)
val follow_symlinks : bool ref

(* use the command 'find' internally and tries to skip files in
 * version control system (vcs) (e.g., .git, _darcs, etc.).
 * Deprecated?
 *)
val files_of_dirs_or_files_no_vcs_nofilter : Fpath.t list -> Fpath.t list

(*****************************************************************************)
(* IO *)
(*****************************************************************************)

(*
   Return the lines of a file. Both Windows-style and Unix-style line endings
   are recognized and removed from the end of the line.
*)
val cat : Fpath.t -> string list

(* this is 1-based access, line 1 is at res.[1]
 * (res.[0] is a fake empty string)
 *)
val cat_array : Fpath.t -> string array
val write_file : file:Fpath.t -> string -> unit

(* [lines_of_file (start_line, end_line) file] returns
 * the list of lines from start_line to end_line included
 * or an error message (for example in case of out of bounds access).
 *
 * Note that the returned lines do not contain \n.
 * Note also that line numbers are 1-based.
 *
 * This function is slow, you should not use it!
 *)
val lines_of_file : int * int -> Fpath.t -> (string list, string) result

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
val with_open_out : Fpath.t -> ((string -> unit) * out_channel -> 'a) -> 'a
val with_open_in : Fpath.t -> (in_channel -> 'a) -> 'a

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

(* Check if the file is executable by others or by the group.
   If the file is only executable by the user owning the file ('u'),
   this function reports it as not executable.
   For example, the following commands create a file that's executable by its
   owner (and by root) on which is_executable fails:

     echo > foo
     chmod 700 foo
     ./foo && echo 'success'

   TODO: is this intentional? Please explain.
*)
val is_executable : Fpath.t -> bool
val filesize : Fpath.t -> int
val filemtime : Fpath.t -> float

(*
   Functions for testing whether a file exists and is of the expected kind,
   without raising exceptions.

   The goal is to deal with the 3 common file types (dir, reg, lnk)
   and focus only on files that are usable. If a file is not usable
   due for example to missing permissions, all these functions will return a
   negative answer ('false') rather than raising an exception.
   A design principle is "make common tasks easy and uncommon tasks possible".
   Here, we're focusing on the former.

   dir = directory = folder
   reg = regular files
   lnk = symbolic link

   The functions whose name contains 'lnk' never follow symlinks.

   For more exotic file kinds or for classifying files by kind,
   use UUnix.stat or UUnix.lstat directly.
*)
val is_dir : follow_symlinks:bool -> Fpath.t -> bool
val is_reg : follow_symlinks:bool -> Fpath.t -> bool
val is_lnk : Fpath.t -> bool
val is_dir_or_reg : follow_symlinks:bool -> Fpath.t -> bool
val is_dir_or_lnk : Fpath.t -> bool
val is_lnk_or_reg : Fpath.t -> bool
val is_dir_or_lnk_or_reg : Fpath.t -> bool

(* Turn a file kind into a JSON string node and vice-versa *)
val file_kind_to_yojson : Unix.file_kind -> Yojson.Safe.t
val file_kind_of_yojson : Yojson.Safe.t -> (Unix.file_kind, string) result

(*****************************************************************************)
(* Filesystem manipulation *)
(*****************************************************************************)
(* Makes the given directory as well as its parent directories.
 * Raises Unix_error if A non-directory object with the same name exists.
 *)
val make_directories : Fpath.t -> unit

(*****************************************************************************)
(* Legacy API using 'string' for filenames instead of Fpath.t *)
(*****************************************************************************)

(* Deprecated! *)
module Legacy : sig
  val files_of_dirs_or_files_no_vcs_nofilter :
    string (* root *) list -> string (* filename *) list

  val cat : string (* filename *) -> string list
  val write_file : file:string (* filename *) -> string -> unit
  val read_file : ?max_len:int -> string (* filename *) -> string

  val with_open_outfile :
    string (* filename *) -> ((string -> unit) * out_channel -> 'a) -> 'a

  val with_open_infile : string (* filename *) -> (in_channel -> 'a) -> 'a

  (* NOT IN MAIN API *)
  val dir_contents : string (* filename *) -> string (* filename *) list
  (** [dir_contents dir] will return a recursive list of all files in a dir *)
end
