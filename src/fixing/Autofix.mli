(* Apply the fix for the list of matches to the given file, returning the
 * resulting file contents. Currently used only for tests.
 *)
val produce_autofixes :
  Core_result.processed_match list -> Core_result.processed_match list

val apply_fixes : ?dryrun:bool -> Textedit.t list -> unit

(* for use by osemgrep *)
val apply_fixes_of_core_matches :
  ?dryrun:bool -> Semgrep_output_v1_t.core_match list -> unit

(* Apply the fix for the list of matches to the given file, returning the
   resulting file contents. Currently used only for tests.

   Raises an exception in case of a conflict.
*)
val apply_fixes_to_file_exn : Fpath.t -> Textedit.t list -> string

(* See make_fixed_lines for an explanation *)
type fixed_lines_env

(* See make_fixed_lines for an explanation *)
val make_fixed_lines_env : unit -> fixed_lines_env

(* Does a dry-run of applying the given Textedit.t and returns the affected
 * lines after the fix has been applied, if any. The mutable env is used to
 * track whether or not we have previously applied a Textedit.t to the location
 * in question. If we have, we do not apply the fix and instead return None.
 *
 * This could potentially go into Textedit.ml but it's somewhat peculiar
 * business logic which I (nmote) do not think belongs in a general-purpose
 * library. *)
val make_fixed_lines : fixed_lines_env -> Textedit.t -> string list option

(* Like the above but uses the given contents for the file instead of reading
 * from the filesystem. Exposed for testing but might be useful in other
 * contexts. *)
val make_fixed_lines_of_string :
  fixed_lines_env ->
  (* file contents *) string ->
  Textedit.t ->
  string list option
