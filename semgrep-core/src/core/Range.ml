(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Basic code range (start/end of code portion).
 *
 * We use this for the pattern-from-code synthesizing project where we need to
 * manipulate code ranges selected by the user.
 *
 * We also now use it to manipulate ranges and apply boolean logic operations
 * on them (for pattern-inside, pattern-not, etc.), now that we also handle
 * the whole rule in OCaml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* charpos is 0-indexed. First char of a file is at charpos:0
 * (unlike in Emacs where point starts at 1).
 *)
type charpos = int [@@deriving show]

(* the range is inclusive, {start = 0; end_ = 4} means [0..4] not [0..4[ *)
type t = { start : charpos; end_ : charpos } [@@deriving show]

(* related: Parse_info.NotTokenLocation *)
exception NotValidRange of string

(*****************************************************************************)
(* Set operations *)
(*****************************************************************************)
(* is r1 included or equal to r2 *)
let ( $<=$ ) r1 r2 = r1.start >= r2.start && r1.end_ <= r2.end_

(* is r1 disjoint of r2 *)
let rec ( $<>$ ) r1 r2 =
  if r1.start <= r2.start then r1.end_ < r2.start else r2 $<>$ r1

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

(* ex: "line1:col1-line2:col2" *)
let range_of_linecol_spec str file =
  if str =~ "\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\):\\([0-9]+\\)" then (
    let a, b, c, d = Common.matched4 str in
    let line1, col1 = (s_to_i a, s_to_i b) in
    let line2, col2 = (s_to_i c, s_to_i d) in
    (* quite inefficient, but should be ok *)
    let trans = Parse_info.full_charpos_to_pos_large file in
    let start = ref (-1) in
    let end_ = ref (-1) in
    for i = 0 to Common2.filesize file do
      let l, c = trans i in
      if (l, c) = (line1, col1) then start := i;
      if (l, c) = (line2, col2) then end_ := i
    done;
    if !start <> -1 && !end_ <> -1 then { start = !start; end_ = !end_ }
    else failwith (spf "could not find range %s in %s" str file) )
  else failwith (spf "wrong format for linecol range spec: %s" str)

let range_of_token_locations (start_loc : PI.token_location)
    (end_loc : PI.token_location) =
  let start = start_loc.charpos in
  let end_ = end_loc.charpos + String.length end_loc.str - 1 in
  { start; end_ }

let range_of_tokens xs =
  try
    let xs = List.filter PI.is_origintok xs in
    let mini, maxi = PI.min_max_ii_by_pos xs in
    let start = PI.pos_of_info mini in
    let end_ =
      PI.pos_of_info maxi + (String.length (PI.str_of_info maxi) - 1)
    in
    Some { start; end_ }
  with PI.NoTokenLocation _ -> None

let hmemo = Hashtbl.create 101

let content_at_range file r =
  let str = Common.memoized hmemo file (fun () -> Common.read_file file) in
  String.sub str r.start (r.end_ - r.start + 1)
