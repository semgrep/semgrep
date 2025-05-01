(* Sal Olivares
 *
 * Copyright (c) 2025, Semgrep Inc.
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
(* See Target_kind.mli for documentation of public items. *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = Source_file | Dependency_source_file | Unknown [@@deriving show, eq]

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

let pp_debug fmt t = Format.fprintf fmt "%s" (show t)

(*****************************************************************************)
(* Hooks for handlers defined in Sca_hooks.ml *)
(*****************************************************************************)
let hook_classify_target = ref None

(*****************************************************************************)
(* Helper functions *)
(*****************************************************************************)

let classify_target (path : Fpath.t) : t =
  match !hook_classify_target with
  | Some ct -> ct path
  | None -> Source_file (* Fallback to source file *)

let is_dependency_source_file (path : Fpath.t) : bool =
  classify_target path = Dependency_source_file
