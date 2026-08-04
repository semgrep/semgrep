(* Yoann Padioleau
 *
 * Copyright (C) 2020 Semgrep Inc.
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
(* Comparisons *)
(*****************************************************************************)

let equal r1 r2 =
  let { start = a1; end_ = b1 } = r1 in
  let { start = a2; end_ = b2 } = r2 in
  Int.equal a1 a2 && Int.equal b1 b2

let compare r1 r2 =
  let { start = a1; end_ = b1 } = r1 in
  let { start = a2; end_ = b2 } = r2 in
  match Int.compare a1 a2 with
  | 0 -> Int.compare b1 b2
  | cmp -> cmp

(*****************************************************************************)
(* Set operations *)
(*****************************************************************************)
(* is r1 included or equal to r2 *)
let ( $<=$ ) r1 r2 = r1.start >= r2.start && r1.end_ <= r2.end_

(* is r1 strictly included in r2 *)
let ( $<$ ) r1 r2 =
  (r1.start >= r2.start && r1.end_ < r2.end_)
  || (r1.start > r2.start && r1.end_ <= r2.end_)

(* is r1 disjoint of r2 *)
let rec ( $<>$ ) r1 r2 =
  if r1.start <= r2.start then r1.end_ < r2.start else r2 $<>$ r1

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

let range_of_token_locations (start_loc : Tok.location) (end_loc : Tok.location)
    =
  let start = start_loc.pos.bytepos in
  let end_ = end_loc.pos.bytepos + String.length end_loc.str - 1 in
  { start; end_ }

let rf_cache = SharedMemo.create ()
let read_file_memoed = SharedMemo.make_with_state rf_cache UFile.read_file

let () =
  (* nosemgrep: forbid-tmp *)
  UTmp.register_temp_file_cleanup_hook (fun file ->
      SharedMemo.remove rf_cache file)

let content_at_range file r =
  let str = read_file_memoed file in
  String.sub str r.start (r.end_ - r.start + 1)
