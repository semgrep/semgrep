(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
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
module OutJ = Semgrep_output_v1_j

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Explanations of how something was matched.
 * This is useful for a step-by-step debugger of the matching process.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* coupling: semgrep_output_v1.atd matching_explanation type *)
type t = {
  op : OutJ.matching_operation;
  children : t list;
  (* resulting ranges *)
  matches : Pattern_match.t list;
  (* TODO: should be a range loc in the rule file *)
  pos : Rule.tok;
}
[@@deriving show]

(*****************************************************************************)
(* Debug output *)
(*****************************************************************************)

(* less: could also display short info on metavar values *)
let match_to_charpos_range (pm : Pattern_match.t) : string =
  let min_loc, max_loc = pm.range_loc in
  let startp, endp = Semgrep_output_utils.position_range min_loc max_loc in
  spf "%d-%d" startp.OutJ.offset endp.OutJ.offset

(* alt: use Format module *)
let rec print_indent indent { op; children; matches; pos } =
  let s =
    spf "%s op = %s (at %d), matches = %s" (Common2.n_space indent)
      (OutJ.show_matching_operation op)
      (Tok.bytepos_of_tok pos)
      (matches |> List_.map match_to_charpos_range |> String.concat " ")
  in
  Out.put s;
  children |> List.iter (print_indent (indent + 2))

(* used by semgrep-core -matching_explanations in Text mode output *)
let print x =
  Out.put " Matching explanations:";
  print_indent 2 x
