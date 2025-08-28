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
(* Code to interact with the metrics.semgrep.dev endpoint *)

(* will also print Logs *)
val send_async : < Cap.network ; .. > -> unit Lwt.t
val send : < Cap.network ; .. > -> unit
