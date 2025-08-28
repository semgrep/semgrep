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
open Common
module C = Rules_config

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Gather Semgrep registry related code.
 *
 * See also Rule_fetching.ml for the actual code fetching the rules from
 * the registry.
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(* TODO: instead of this Uri.to_string and Uri.of_string, we should
 * use Uri.with_path to adjust the path (a la Filename.concat)
 *)
let url_of_registry_config_kind rkind =
  (* we go through the CURL interface for now (c/).
   * TODO: don't use the curl interface which seems slower than what
   * pysemgrep is doing.
   *)
  let prefix = Uri.to_string !Semgrep_envvars.v.semgrep_url ^ "/c" in
  let url =
    match rkind with
    | C.Registry s -> spf "%s/r/%s" prefix s
    | C.Pack s -> spf "%s/p/%s" prefix s
    | C.Snippet s -> spf "%s/s/%s" prefix s
    (* The code below was commented at some point because handling those
     * shortcuts led to a 50s slowdown in make osemgrep-e2e; too many tests
     * are relying on those configs which take a long time to download.
     * TODO: Those tests should be optimized and use local configs instead.
     *)
    | C.Auto -> spf "%s/p/default" prefix
    | C.R2c -> spf "%s/p/r2c" prefix
  in
  Uri.of_string url
