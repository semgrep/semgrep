(* Yoann Padioleau
 *
 * Copyright (C) 2023-2024 Semgrep Inc.
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

(* Channels on (local) files.
 *
 * In many cases, having just the channel is not enough for good error
 * reporting so here we mostly wrap In_channel.t and Out_channel.t
 * (themselves alias for Stdlib.in_channel and Stdlib.out_channel) with
 * the filename attached to it.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* alt: "in", but reserved keyword, and "in_" is ugly
 * alt: use an Origin.t instead of Fpath.t? so can also get Stdin or Network?
 *)

type i = { ic : in_channel; p : Fpath.t }
type o = { oc : out_channel; p : Fpath.t }

(*****************************************************************************)
(* API *)
(*****************************************************************************)
(* mostly a copy of In_channel.ml, Out_channel.ml, and some funcs in File.ml *)
