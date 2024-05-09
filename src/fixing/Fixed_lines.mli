(* See make_fixed_lines for an explanation *)
type env

(* See make_fixed_lines for an explanation *)
val mk_env : unit -> env

(* Does a dry-run of applying the given Textedit.t and returns the affected
 * lines after the fix has been applied, if any. The mutable env is used to
 * track whether or not we have previously applied a Textedit.t to the location
 * in question. If we have, we do not apply the fix and instead return None.
 *
 * This could potentially go into Textedit.ml but it's somewhat peculiar
 * business logic which I (nmote) do not think belongs in a general-purpose
 * library. *)
val make_fixed_lines : env -> Textedit.t -> string list option

(* Like the above but uses the given contents for the file instead of reading
 * from the filesystem. Exposed for testing but might be useful in other
 * contexts. *)
val make_fixed_lines_of_string :
  env -> (* file contents *) string -> Textedit.t -> string list option
