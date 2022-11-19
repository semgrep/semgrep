(* There is currently no 'semgrep dump' subcommand. Dumps are run via
 * 'semgrep scan --dump-ast ...' but internally it's quite similar to
 * a subcommand.
 *)

type conf = { language : Lang.t; json : bool; target : target_kind }

and target_kind = Pattern of string | File of Common.filename
[@@deriving show]

val run : conf -> Exit_code.t
