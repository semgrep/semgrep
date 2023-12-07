(* Same as print_endline: print the string and a newline, then flush stdout.
 * Just shorter. *)
val pr : string -> unit

(*****************************************************************************)
(* Real file paths - deprecated, use File.mli *)
(*****************************************************************************)
(* Deprecated!

   Migration in progress: File.ml reproduces the functions below and uses
   Fpath.t instead of strings to represent file/directory paths.
*)

(*
   Check that the file exists and produce a valid absolute path for the file.
   Deprecated: use the Rpath module instead!
*)
val fullpath : string (* filename *) -> string (* filename *)

val dir_contents : string (* filename *) -> string (* filename *) list
(** [dir_contents dir] will return a recursive list of all files in a directory *)

(* use the command 'find' internally and tries to skip files in
 * version control system (vcs) (e.g., .git, _darcs, etc.).
 * Deprecated?
 *)
val files_of_dir_or_files_no_vcs_nofilter :
  string list -> string (* filename *) list

(* ugly: internal flag for files_of_dir_or_files_no_vcs_nofilter *)
val follow_symlinks : bool ref

(*****************************************************************************)
(* IO - deprecated, use File.mli *)
(*****************************************************************************)

(*
   Return the lines of a file. Both Windows-style and Unix-style line endings
   are recognized and removed from the end of the line.
*)
val cat : string (* filename *) -> string list
val write_file : file:string (* filename *) -> string -> unit

(* Read the contents of file.

   This implementation works even with Linux files like /dev/fd/63
   created by bash when using "process substitution"* e.g.

     my-ocaml-program <(echo contents)

   * https://www.gnu.org/software/bash/manual/html_node/Process-Substitution.html

   If max_len is specified, at most that many bytes are read from the file.
*)
val read_file : ?max_len:int -> string (* filename *) -> string

(* Scheme-inspired combinators that automatically close the file
 * once the function callback is done. Here is an example of use:
 *   with_open_outfile "/tmp/foo.txt" (fun (pr, _chan) ->
 *     pr "this goes in foo.txt"
 *   )
 *)
val with_open_outfile :
  string (* filename *) -> ((string -> unit) * out_channel -> 'a) -> 'a

val with_open_infile : string (* filename *) -> (in_channel -> 'a) -> 'a

(*****************************************************************************)
(* Tmp files *)
(*****************************************************************************)

(* creation of /tmp files, a la gcc
 * ex: new_temp_file "cocci" ".c" will give "/tmp/cocci-3252-434465.c"
 *)
val new_temp_file :
  string (* prefix *) -> string (* suffix *) -> string (* filename *)

(* ??? *)
val _temp_files_created : (string, unit) Hashtbl.t
val save_tmp_files : bool ref
val erase_temp_files : unit -> unit
val erase_this_temp_file : string (* filename *) -> unit

(*****************************************************************************)
(* Profiling *)
(*****************************************************************************)

(*
   Run a function and print how long it took to return or to raise an
   exception. pr_time prints to stdout.
*)
val pr_time : string -> (unit -> 'a) -> 'a

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* run by main_boilerplate below at its finalize step before exiting.
 * Can be used for example to display some profiling information
 * (see Profiling.ml as an example)
 *)
val before_exit : (unit -> unit) list ref

(* do some finalize, signal handling, unix exit conversion, etc *)
val main_boilerplate : (unit -> unit) -> unit
