(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Gather and show system resources.
*)

open Common

(* TODO: available memory, details on the host platform, ... *)
type t = { cpu : Num_jobs.t } [@@deriving yojson]

let resources = { cpu = Num_jobs.get () }

let show () =
  spf {|host CPUs: %i
available CPUs: %i
default number of parallel jobs: %i|}
    resources.cpu.host_cpus resources.cpu.available_cpus
    resources.cpu.recommended_parmap_jobs

let to_json () = Yojson.Safe.pretty_to_string (to_yojson resources)
