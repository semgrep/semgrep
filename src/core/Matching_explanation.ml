(* Yoann Padioleau
 *
 * Copyright (C) 2022 Semgrep Inc.
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
open Common
module Out = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Explanations of how something was matched.
 * This is useful for a step-by-step debugger of the matching process.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* coupling: semgrep_output_v1.atd matching_explanation_extra type *)
type extra = {
  (* only present for And kind *)
  before_negation_matches : Core_match.t list option;
  (* only present in nodes which have children Filter nodes *)
  before_filter_matches : Core_match.t list option;
}
[@@deriving show]

(* coupling: semgrep_output_v1.atd matching_explanation type *)
type t = {
  op : Out.matching_operation;
  children : t list;
  (* resulting ranges *)
  matches : Core_match.t list;
  (* TODO: should be a range loc in the rule file *)
  pos : Rule.tok;
  extra : extra option;
}
[@@deriving show]

(*****************************************************************************)
(* Debug output *)
(*****************************************************************************)

(* less: could also display short info on metavar values *)
let match_to_charpos_range (pm : Core_match.t) : string =
  let min_loc, max_loc = pm.range_loc in
  let startp, endp = Semgrep_output_utils.position_range min_loc max_loc in
  spf "%d-%d" startp.Out.offset endp.Out.offset

(* alt: use Format module *)
let rec print_indent indent { op; children; matches; pos; extra = _ } =
  let s =
    spf "%s op = %s (at %d), matches = %s" (Common2.n_space indent)
      (Out.show_matching_operation op)
      (Tok.bytepos_of_tok pos)
      (matches |> List_.map match_to_charpos_range |> String.concat " ")
  in
  UConsole.print s;
  children |> List.iter (print_indent (indent + 2))

(* used by semgrep-core -matching_explanations in Text mode output *)
let print x =
  UConsole.print " Matching explanations:";
  print_indent 2 x

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let mk_extra ?before_negation_matches ?before_filter_matches () =
  { before_negation_matches; before_filter_matches }
