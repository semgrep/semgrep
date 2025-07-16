(* Cooper Pierce
 *
 * Copyright (c) 2023, Semgrep Inc.
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
(* See Origin.mli for top-level documentation of this module. *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t =
  | Target_file of Fppath.t
  | Unfilterable_target_file of Fpath.t
  | Git_blob of {
      sha : Git_wrapper.hash;
      paths : (Git_wrapper.commit * Fppath.t) list;
    }
[@@deriving show, eq, ord]

(*****************************************************************************)
(* to_string *)
(*****************************************************************************)

let to_string (s : t) =
  match s with
  | Unfilterable_target_file path -> Fpath.to_string path
  | Target_file path -> Fpath.to_string path.fpath
  | Git_blob { sha; _ } -> sha |> Git_wrapper.hex_of_hash

let to_string_opt ?(unspecified = "unknown") (s : t option) =
  match s with
  | Some s -> to_string s
  | None -> Printf.sprintf "<%s>" unspecified
