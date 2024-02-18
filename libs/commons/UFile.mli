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

(* Check that the file exists and produce a valid absolute path for the file.
 * Deprecated: use Unix.realpath in OCaml >= 4.13, or Rpath.mli
 *)
val fullpath : Fpath.t -> Fpath.t

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

(* this is 1-based access, line 1 is at res.[1] *)
val cat_array : Fpath.t -> string array
val write_file : file:Fpath.t -> string -> unit

(* [lines_of_file (start_line, end_line) file] returns
 * the list of lines from start_line to end_line included.
 *
 * Note that the returned lines do not contain \n.
 *
 * This function is slow, you should not use it!
 *)
val lines_of_file : int * int -> Fpath.t -> string list

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
val is_executable : Fpath.t -> bool
val filesize : Fpath.t -> int
val filemtime : Fpath.t -> float

(* raise Unix_error if the directory does not exist *)
val is_directory : Fpath.t -> bool

(* raise Unix_error if the file does not exist *)
val is_file : Fpath.t -> bool
val is_symlink : Fpath.t -> bool
val lfile_exists : Fpath.t -> bool

(* no raised Unix_error if the directory does not exist *)
val dir_exists : Fpath.t -> bool

(*****************************************************************************)
(* Legacy API using 'string' for filenames instead of Fpath.t *)
(*****************************************************************************)

(* Deprecated! *)
module Legacy : sig
  val fullpath : string (* filename *) -> string (* filename *)

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
