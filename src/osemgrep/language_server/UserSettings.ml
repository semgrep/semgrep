(* Austin Theriault
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(* Commentary *)
(* User facing settings. Should match all applicable scan settings in *)
(* package.json of the VSCode extension *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Code *)
(*****************************************************************************)
type t = {
  configuration : string list; [@default []]
  exclude : string list; [@default []]
  include_ : string list; [@key "include"] [@default []]
  jobs : int; [@default 1]
  max_memory : int; [@key "maxMemory"] [@default 0]
  max_target_bytes : int; [@key "maxTargetBytes"] [@default 1000000]
  timeout : int; [@default 30]
  timeout_threshold : int; [@key "timeoutThreshold"] [@default 3]
  only_git_dirty : bool; [@key "onlyGitDirty"] [@default true]
}
[@@deriving yojson]

let default =
  {
    configuration = [];
    exclude = [];
    include_ = [];
    jobs = 1;
    max_memory = 0;
    max_target_bytes = 1000000;
    timeout = 30;
    timeout_threshold = 3;
    only_git_dirty = true;
  }

let t_of_yojson json = of_yojson json
let yojson_of_t settings = to_yojson settings

let find_targets_conf_of_t settings =
  let include_ =
    if List.length settings.include_ > 0 then Some settings.include_ else None
  in
  Find_targets.
    {
      exclude = settings.exclude;
      include_;
      max_target_bytes = settings.max_target_bytes;
      respect_git_ignore = true;
      baseline_commit = None;
      scan_unknown_extensions = true;
      project_root = None;
    }

let core_runner_conf_of_t settings =
  Core_runner.
    {
      num_jobs = settings.jobs;
      (* This breaks things for some reason if true*)
      optimizations = false;
      max_memory_mb = settings.max_memory;
      timeout = float_of_int settings.timeout;
      timeout_threshold = settings.timeout_threshold;
      ast_caching = true;
    }
