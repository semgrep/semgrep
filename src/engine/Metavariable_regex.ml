(* Brandon Wu
 *
 * Copyright (C) 2023 r2c
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
module RM = Range_with_metavars
module G = AST_generic

let logger = Logging.get_logger [ __MODULE__ ]

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
      logger#error "Attempted to regex capture on unbound metavar %s" mvar;
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
      match Eval_generic.eval env mvar_str_exp with
      | String str -> (
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
          let mast_start_loc =
            mval |> MV.ii_of_mval |> AST_generic_helpers.range_of_tokens |> fst
            |> Tok.unsafe_loc_of_tok
          in
          let mval_start_pos = mast_start_loc.pos in

          match
            Eval_generic.eval_regexp_matches ~base_offset:mval_start_pos.bytepos
              ~regexp:re_str ~file str
          with
          | [] -> None
          | matches -> Some (Common.map snd matches))
      | _ ->
          logger#error
            "Somehow got a non-string from str(%s) in generic evaluation" mvar;
          None)
