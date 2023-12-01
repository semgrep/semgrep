(* Define a flag that can be negated e.g. --foo and --no-foo.
   It's not supported out-of-the-box by cmdliner but we want it for
   backward compatibility with the Python CLI.
*)
val negatable_flag :
  ?default:bool ->
  neg_options:string list ->
  doc:string ->
  string list ->
  bool Cmdliner.Term.t

(* Define a flag that can be negated e.g. --foo and --no-foo, and being able to
   specify an environment variable.
   It's not supported out-of-the-box by cmdliner but we want it for
   backward compatibility with the Python CLI.
*)
val negatable_flag_with_env :
  ?default:bool ->
  ?env:Cmdliner.Cmd.Env.info ->
  neg_options:string list ->
  doc:string ->
  string list ->
  bool Cmdliner.Term.t

(* Parse command-line arguments representing a number of bytes, such as
 * '5 mb' or '3.2GiB'
 *)
val number_of_bytes_converter : int Cmdliner.Arg.conv

val uri : Uri.t Cmdliner.Arg.conv
(** A simple [Uri.t] cmdliner's converter. *)

val sha1 : Digestif.SHA1.t Cmdliner.Arg.conv
(** A simple [Digestif.SHA1.t] cmdliner's converter. *)
