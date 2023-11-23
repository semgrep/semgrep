(* Cooper Pierce, Yoann Padioleau
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
(* Gather types and helper functions to deal with extract rules *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* ex: foo.html *)
type original_target = Original of Fpath.t

(* ex: /tmp/extracted-foo-42.js *)
type extracted_target = Extracted of Fpath.t

let debug_extract_mode = ref false

(* A function which maps a match result from the *extracted* target
 * (e.g., '/tmp/extract-foo.rb') to a match result to the
 * *original* target (e.g., 'src/foo.erb').
 *
 * We could use instead:
 *
 *        (a -> a) -> a Report.match_result -> a Report.match_result
 *
 * although this is a bit less ergonomic for the caller.
 *)
type match_result_location_adjuster =
  Core_result.matches_single_file -> Core_result.matches_single_file

type adjusters = {
  loc_adjuster : (extracted_target, match_result_location_adjuster) Hashtbl.t;
  original_target : (extracted_target, original_target) Hashtbl.t;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

let is_extract_rule (r : Rule.t) : bool =
  match r.mode with
  | `Extract _ -> true
  | `Secrets _
  | `Search _
  | `Taint _
  | `Steps _ ->
      false

(* this does not just filter, this also returns a better type *)
let filter_extract_rules (rules : Rule.t list) : Rule.extract_rule list =
  rules
  |> Common.map_filter (fun (r : Rule.t) ->
         match r.mode with
         | `Extract _ as e -> Some ({ r with mode = e } : Rule.extract_rule)
         | `Search _
         | `Taint _
         | `Steps _
         | `Secrets _ ->
             None)
