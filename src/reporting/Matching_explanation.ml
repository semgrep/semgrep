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
module Out = Semgrep_output_v1_j
module Out2 = Output_from_core_util

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
  op : Out.matching_operation;
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
  let startp, endp = Out2.position_range min_loc max_loc in
  spf "%d-%d" startp.Out.offset endp.Out.offset

(* alt: use Format module *)
let rec print_indent indent { op; children; matches; pos } =
  let s =
    spf "%s op = %s (at %d), matches = %s" (Common2.n_space indent)
      (Out.show_matching_operation op)
      (Tok.bytepos_of_tok pos)
      (matches |> Common.map match_to_charpos_range |> Common.join " ")
  in
  pr s;
  children |> List.iter (print_indent (indent + 2))

(* used by semgrep-core -matching_explanations in Text mode output *)
let print x =
  pr " Matching explanations:";
  print_indent 2 x
