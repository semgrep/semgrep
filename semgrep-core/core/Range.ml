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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Basic code range (start/end of code portion).
 *
 * For semgrep pattern-from-code synthesizing project we need to
 * manipulate code ranges selected by the user.
 * Note that the semgrep python wrapper also needs to manipulate ranges
 * to apply boolean logic operations on them (for pattern-inside, patter-not,
 * etc.), so if one day we decide to port that part to OCaml we will also
 * need the range type of this module.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* this is 0-indexed. First char of a file is at charpos:0
 * (unlike in Emacs where point starts at 1).
 *)
type charpos = int

(* the range is inclusive, [0..4] *)
type t = {
  start: charpos;
  end_: charpos;
}

(* related: Parse_info.NotTokenLocation *)
exception NotValidRange of string

(*****************************************************************************)
(* Set operations *)
(*****************************************************************************)
(* is r1 included or equal to r2 *)
let ($<=$) _r1 _r2 =
  raise Todo

(* is r1 disjoint of r2 *)
let ($<>$) _r1 _r2 =
  raise Todo

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

(* ex: "line1:col1-line2:col2" *)
let range_of_string _str =
  raise Todo

let range_of_tokens _xs =
  raise Todo
