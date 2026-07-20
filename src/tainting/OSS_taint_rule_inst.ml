(*
   Copyright (c) 2026 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)

type mutable_state = {
  java_props_cache : Taint_rule_inst.java_props_cache;
  timeouts : Taint_rule_inst.file_timeout_stats;
}

type t = mutable_state Taint_rule_inst.t

let fresh_muts () : mutable_state =
  { java_props_cache = Hashtbl.create 30; timeouts = Hashtbl.create 2 }

let of_config = Taint_rule_inst.of_config
