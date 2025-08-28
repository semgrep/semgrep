(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Show the current identity of the user running the command.
*)

type identity_kind = Identity | Deployment

(* TODO: actually it's using Logs.app which prints on stderr *)
val print : < Cap.network ; Cap.stdout > -> identity_kind -> Exit_code.t
