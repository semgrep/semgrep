(*
   Copyright (c) 2021-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val tests : Login_subcommand.caps -> Testo.t list

(* to be reused in other tests *)

val with_fake_deployment_response : string -> (unit -> 'a) -> 'a
val with_semgrep_logged_in : (unit -> 'a) -> 'a
val fake_token : string
val fake_deployment : string
