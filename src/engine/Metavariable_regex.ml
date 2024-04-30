(* Brandon Wu
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

open Common
module MV = Metavariable
module RM = Range_with_metavars
module G = AST_generic
module Log = Log_engine.Log

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let get_metavar_regex_capture_bindings env ~file r (mvar, re_str) =
  let bindings = r.RM.mvars in
  (* If anything goes wrong, we just quit out and fail the condition.
     But, by precondition, this should succeed.
  *)
  match List.assoc_opt mvar bindings with
  | None ->
      Log.warn (fun m ->
          m "Attempted to regex capture on unbound metavar %s" mvar);
      None
  | Some mval -> (
      (* Piggy-back off of the Eval_generic logic so that we can get the
         contents of this metavariable.
      *)
      let fk = Tok.unsafe_fake_tok "" in
      let fki = AST_generic.empty_id_info () in
      let mvar_str_exp =
        let mvar_exp = G.N (G.Id ((mvar, fk), fki)) |> G.e in
        let call_str x =
          G.Call (G.N (G.Id (("str", fk), fki)) |> G.e, (fk, [ G.Arg x ], fk))
          |> G.e
        in
        call_str mvar_exp
      in

      (* actually call the generic evaluation logic! *)
      match Eval_generic.eval_opt env mvar_str_exp with
      | Some (String str) -> (
          (* Here, we would like to know what the offset of the metavariable in
             its source file is.

             This is because we produce positions within the regex matcher, and
             these positions call `line_col_of_charpos` relative to the charpos
             within the metavariable's contents, and not its position within the
             file.

             So for instance, in the program foo(bar) if our metavariable matches
             `bar`, we would try to get the line/col of charpos 0, even though the
             character `b` should have charpos 4.

             So we carry a base offset of the metavariable's start, so that we can
             perform that calculation.
          *)
          let* mast_start_loc =
            (* metavariable-regex doesn't make sense on synthetic ASTs such as
             * those created for resolved names, which are used pervasively by
             * the Pro Engine. It should only try to match when we actually have
             * underlying text to match against. So, if there is no legitimate
             * source location associated with the mevariable, we'll just fail
             * to match here. *)
            let* start, _ =
              mval |> MV.ii_of_mval |> AST_generic_helpers.range_of_tokens
            in
            Some (Tok.unsafe_loc_of_tok start)
          in
          let mval_start_pos = mast_start_loc.pos in

          match
            Eval_generic.eval_regexp_matches ~base_offset:mval_start_pos.bytepos
              ~regexp:re_str ~file str
          with
          | [] -> None
          | matches -> Some (List_.map snd matches))
      | _ ->
          Log.err (fun m ->
              m
                "Somehow got a non-string or exn from str(%s) in generic \
                 evaluation"
                mvar);
          None)
