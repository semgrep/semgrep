(* Emma Jin
 *
 * Copyright (C) 2024 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers to prepare data for Opentelemetry tracing *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type analysis_flags = {
  secrets_validators : bool;
  allow_all_origins : bool;
  historical_scan : bool;
  deep_intra_file : bool;
  deep_inter_file : bool;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Set the descriptor for allowed origins. This is not simply
   a boolean because we will likely include new origins in the
   future *)
let allowed_origins allow_all_origins =
  if allow_all_origins then "all_origins" else "pro_rules_only"

(* Poor man's Git repo detection. Running git repo detection again
   seems wasteful, but checking two env vars is pretty cheap.

   TODO the more we port of semgrep scan and semgrep ci, the more
   of this information will already be in OCaml *)
let repo_name () =
  match Sys.getenv_opt "SEMGREP_REPO_DISPLAY_NAME" with
  | Some name -> name
  | None -> (
      match Sys.getenv_opt "SEMGREP_REPO_NAME" with
      | Some name -> name
      | None -> "<local run>")

(* In case we don't have a repo name, report the base folder where
   semgrep was run. We report only the base name to avoid leaking
   user information they may not have expected us to include. *)
let current_working_folder () = Filename.basename (Sys.getcwd ())

(*****************************************************************************)
(* Shortcuts for Otel tracing *)
(*****************************************************************************)

let no_analysis_features () =
  {
    secrets_validators = false;
    historical_scan = false;
    allow_all_origins = false;
    deep_intra_file = false;
    deep_inter_file = false;
  }

let data_of_languages (languages : Xlang.t list) =
  languages |> List_.map (fun l -> (Xlang.to_string l, `Bool true))

let get_top_level_data jobs version analysis_flags =
  [
    ("version", `String version);
    ("jobs", `Int jobs);
    ("folder", `String (current_working_folder ()));
    ("repo_name", `String (repo_name ()));
    ("pro_secrets_validators", `Bool analysis_flags.secrets_validators);
    ("pro_historical_scanning", `Bool analysis_flags.historical_scan);
    ("pro_deep_intrafile", `Bool analysis_flags.deep_intra_file);
    ("pro_deep_interfile", `Bool analysis_flags.deep_inter_file);
  ]
  @
  if analysis_flags.secrets_validators then
    [
      ( "pro_secrets_allowed_origins",
        `String (allowed_origins analysis_flags.allow_all_origins) );
    ]
  else []
