(* Heejong Lee
 *
 * Copyright (C) 2023 Semgrep Inc.
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
(* Config for the differential scan triggered when using --baseline-commit *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* Note that, when the --baseline-commit option is used, `targets` are
 * automatically calculated based on the output from the `git diff` command
 * between the baseline commit and the head commit.
 *)
type t =
  (* the set of changed files *)
  | BaseLine of Fpath.t list (* implies depth 0 *)
  (* pro: deep differential scan. See Deep_diffscan_helpers.ml for more info. *)
  | Depth of Fpath.t list (* starting point *) * int (* depth *)
  (* TODO: alt: remove and use more option in the caller *)
  | WholeScan
[@@deriving show]

let default_depth = 2
