(*
   Copyright (c) 2022-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val hook_pro_language_server :
  (Eio_unix.Stdenv.base -> Lsp_CLI.conf -> unit) option Hook.t

(*
   Parse a semgrep-lsp command, execute it and exit.

   Usage: main [| "semgrep-lsp"; ... |]

   This function returns an exit code to be passed to the 'exit' function.
*)
val main : string array -> Exit_code.t

(* internal *)
val run_conf : Lsp_CLI.conf -> Exit_code.t
