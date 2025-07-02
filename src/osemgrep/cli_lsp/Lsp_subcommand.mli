type caps =
  < Core_scan.caps ; Cap.random ; Cap.network ; Cap.tmp ; Cap.readdir >

val hook_pro_language_server :
  (caps -> Eio_unix.Stdenv.base -> unit) option Hook.t

(*
   Parse a semgrep-lsp command, execute it and exit.

   Usage: main [| "semgrep-lsp"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : < caps ; .. > -> Eio_unix.Stdenv.base -> string array -> Exit_code.t

(* internal *)
val run_conf :
  < caps ; .. > -> Eio_unix.Stdenv.base -> Lsp_CLI.conf -> Exit_code.t
