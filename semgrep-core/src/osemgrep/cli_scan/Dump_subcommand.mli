(* There is currently no 'semgrep dump' subcommand. Dumps are run via
 * 'semgrep scan --dump-ast ...' but internally it's quite similar to
 * a subcommand.
 *)

type conf = { language : string; target : target_kind; json : bool }

and target_kind = Pattern of string | File of Common.filename
[@@deriving show]

val run : conf -> Exit_code.t
