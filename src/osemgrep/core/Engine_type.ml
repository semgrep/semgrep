(* Yoann Padioleau
 *
 * Copyright (C) 2023-2025 Semgrep Inc.
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

(****************************************************************************)
(* Prelude *)
(****************************************************************************)
(* Subpart of Scan_CLI.conf to encode engine type related CLI flags.
 *
 * Note that we also have Engine_kind.t, which is an alias to
 * semgrep_output_v1.engine_type with just `OSS | `Pro, and even
 * Semgrep_metrics_t.engine_config. However, the goal of this module is to
 * encode the CLI flags, to configure the engine, whereas Engine_kind.t is more
 * useful to tag findings and Semgrep_metrics_t.engine_config as the name
 * suggests are metrics for us.
 *)

(****************************************************************************)
(* Types *)
(****************************************************************************)

type t =
  (* a.k.a CE for Community Edition *)
  | OSS
  | PRO of pro_config

and pro_config = {
  analysis : analysis_flavor;
  path_sensitive : bool;
  extra_languages : bool;
  (* For Code/Secrets/SCA, None means Disabled *)
  code_config : code_config option;
  secrets_config : secrets_config option;
  sca_config : sca_config option;
}

and analysis_flavor =
  (* a.k.a. OSS scan, CE scan, Core scan.
   * This case may seem redundant with the OSS case above, but in theory
   * we can run a regular Intraprocedural Core_scan but on pro languages
   * or with secrets enabled which requires a pro_config.
   *)
  | Intraprocedural
  (* a.k.a. Pro intrafile, intrafile interprocedural *)
  | Interprocedural
  (* a.k.a. Deep scan, crossfile interprocedural *)
  | Interfile

(* alt: analysis_flavor could be part of the code_config really *)
and code_config = unit

and secrets_config = {
  (* Typically secrets will only run validators from semgrep.dev. The
     allow_all_origins flag bypasses this security check. *)
  allow_all_origins : bool;
  (* This is used by Historical scans to keep only the `Confirmed_valid
   * matches in the postprocess secrets phase.
   *)
  only_validated : bool;
}

and sca_config = unit [@@deriving show]
