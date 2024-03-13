(* Austin Theriault
 *
 * Copyright (C) 2019-2024 Semgrep, Inc.
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

(* See libs/tracing/unix/Tracing.ml. This is the virtual module to allow
   JS to build without requiring curl to be installed *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type span = Trace_core.span [@@deriving show]

type analysis_flags = {
  secrets_validators : bool;
  historical_scan : bool;
  allow_all_origins : bool;
  deep_intra_file : bool;
  deep_inter_file : bool;
}
[@@deriving show]

type top_level_data = { version : string; analysis_flags : analysis_flags }
[@@deriving show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let no_analysis_features () =
  {
    secrets_validators = false;
    historical_scan = false;
    allow_all_origins = false;
    deep_intra_file = false;
    deep_inter_file = false;
  }

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let enter_span = Trace_core.enter_span
let exit_span = Trace_core.exit_span
let with_span = Trace_core.with_span

let add_data_to_span (_i : span) (_data : (string * Trace_core.user_data) list)
    =
  ()

let add_data_to_opt_span (_i : span option)
    (_data : (string * Trace_core.user_data) list) =
  ()

(*****************************************************************************)
(* Entry points for setting up tracing *)
(*****************************************************************************)

let configure_tracing (_service_name : string) = ()
let with_setup f = f (Int64.of_int 0)
