(*
   Install semgrep (in CI) for a given repository
*)

val main : string array -> Exit_code.t
val run : Install_CLI.conf -> Exit_code.t
