(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type t = {
  configuration : string list;
  exclude : string list;
  include_ : string list;
  jobs : int;
  max_memory : int;
  max_target_bytes : int;
  timeout : int;
  timeout_threshold : int;
  only_git_dirty : bool;
  ci : bool;
  do_hover : bool;
  pro_intrafile : bool;
}

val default : t
val t_of_yojson : Yojson.Safe.t -> (t, string) result
val yojson_of_t : t -> Yojson.Safe.t
val pp : Format.formatter -> t -> unit
val find_targets_conf_of_t : t -> Find_targets.conf
val core_runner_conf_of_t : t -> Core_runner.conf
