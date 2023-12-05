(*
   Operations on files in the general sense (regular file, folder, etc.).

   Migration note:
   - step 1: here, we expose the same functions as in Common but using the
             Fpath.t type.
   - step 2: move the original implementation from Common.ml to here
             and stop using 'string' for file paths.
*)

(* For realpath, use Unix.realpath in OCaml >= 4.13, or Rpath.mli *)
(*
   Check that the file exists and produce a valid absolute path for the file.
*)
val fullpath : Fpath.t -> Fpath.t

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
val write_file : Fpath.t -> string -> unit

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

(* If the file is a named pipe (e.g., created with <(echo 'foo')), copy it
   into a temporary regular file (with prefix [prefix]) and return the path
   of that temporary file. This allows multiple reads on the file and
   avoids illegal seeks when reporting match results or parsing errors.
   The temporary file is deleted at_exit.
*)
val replace_named_pipe_by_regular_file_if_needed :
  ?prefix:string -> Fpath.t -> Fpath.t

(* Scheme-inspired combinators that automatically close the file
 * once the function callback is done. Here is an example of use:
 *   with_open_outfile "/tmp/foo.txt" (fun (pr, _chan) ->
 *     pr "this goes in foo.txt"
 *   )
 *)
val with_open_out : Fpath.t -> ((string -> unit) * out_channel -> 'a) -> 'a
val with_open_in : Fpath.t -> (in_channel -> 'a) -> 'a

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
