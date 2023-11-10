(* Martin Jambon
 *
 * Copyright (C) 2021-2023 Semgrep Inc.
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
(* Provide different equality over the generic AST constructs.
 *
 * In theory 'deriving eq' can provide only one equal (that can
 * be customized), but by using globals we can provide multiple equals.
 *
 * TODO: this module uses a global, so this might not play well with
 * OCaml 5.0 at some point.
 *)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

(*
   Trickery to offer different collections of equality functions:

   - structural equality: do two AST nodes have the same structure?
     This is the equality we usually want. Note that we already don't
     care about token location thanks to Tok.t_always_equal.
     This also disregards IDs that could be assigned uniquely to AST nodes
     (old: we don't anymore, but we could).
   - referential equality: are these two AST nodes physically the same?
     This is essentially physical equality but it tolerates copying or even
     some transformation as long as a node ID is preserved.
     (old: was use for stmts matching caching but got removed now).
   - syntactic equality: do two AST nodes represent the same code?
     This disregards the id_info inferred about Id nodes. This means
     that `secret1` in a YAML file is considered to be the same as
     the string `secret1` in a Java file. Syntactic equality is used
     to compare metavariables for multistep rules.

   Comparing two AST nodes must be done via one of the with_equal_* wrappers
   so as to select structural or referential equality.
*)

type busy_with_equal = Not_busy | Structural_equal | Syntactic_equal
(* old:  | Referential_equal *)

(* global state! managed by the with_equal_* functions *)
let busy_with_equal = ref Not_busy

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let equal_id_info equal a b =
  match !busy_with_equal with
  | Not_busy -> failwith "Call AST_utils.with_xxx_equal to avoid this error."
  | Syntactic_equal -> true
  | Structural_equal -> equal a b

(*
   Wrap one of the generated equal_* functions into one that selects
   match_based equality, ignoring node IDs, position information, and
   fields that may be affected by code around the variable (e.g. id_resolved,
   id_type.)
*)
let with_syntactic_equal equal a b =
  match !busy_with_equal with
  | Not_busy ->
      busy_with_equal := Syntactic_equal;
      Common.protect
        ~finally:(fun () -> busy_with_equal := Not_busy)
        (fun () -> equal a b)
  | Syntactic_equal
  | Structural_equal ->
      failwith "an equal is already in progress"

(*
   Wrap one of the generated equal_* functions into one that selects
   structural equality, ignoring node IDs and position information)
*)
let with_structural_equal equal a b =
  match !busy_with_equal with
  | Not_busy ->
      busy_with_equal := Structural_equal;
      Common.protect
        ~finally:(fun () -> busy_with_equal := Not_busy)
        (fun () -> equal a b)
  | Syntactic_equal
  | Structural_equal ->
      failwith "an equal is already in progress"
