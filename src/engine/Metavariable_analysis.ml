(* Martin Jambon
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
module MV = Metavariable
module G = AST_generic
open Match_env

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(*
   Find the metavariable value, convert it back to a string, and
   run it through an analyzer that returns true if there's a "match".
*)
let analyze_metavar env (bindings : MV.bindings) mvar analyzer =
  match List.assoc_opt mvar bindings with
  | None ->
      error env
        (Common.spf
           "metavariable-analysis failed because %s is not in scope, please \
            check your rule"
           mvar);
      false
  | Some mvalue -> analyzer mvalue

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let analyze_string_metavar env bindings mvar (analyzer : string -> bool) =
  analyze_metavar env bindings mvar (function
    (* We don't use Eval_generic.text_of_binding on string literals because
       it returns the quoted string but we want it unquoted. *)
    | E { G.e = G.L (G.String (_, (escaped, _tok), _)); _ } ->
        let esc = escaped |> String_literal.approximate_unescape in
        analyzer esc
    | other_mval -> (
        match Eval_generic.text_of_binding mvar other_mval with
        | Some s -> analyzer s
        | None -> false))
