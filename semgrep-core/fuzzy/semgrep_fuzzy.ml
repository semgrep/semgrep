(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)

module V = Lib_ast_fuzzy

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* See https://github.com/facebook/pfff/wiki/Sgrep
*)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

type pattern = Ast_fuzzy.tree list

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let sgrep ~hook pattern ast =

  let len = List.length pattern in

  (* visit AST and try to match pattern on it *)
  let hook =
    { V.default_visitor with
      V.ktrees = (fun (k, _) xs ->
        if List.length xs >= len then begin
          let shorter, rest = Common2.splitAt len xs in

          (* pr (Ocaml.string_of_v (Ast_fuzzy.vof_trees shorter));*)

          let matches_with_env =
            Matching_fuzzy.match_trees_trees pattern shorter
          in
          if matches_with_env = []
          then
            (* recurse on sublists *)
            k xs
          else begin
            (* could also recurse to find nested matching inside
             * the matched code itself
            *)
            let matched_tokens = Lib_ast_fuzzy.toks_of_trees shorter in
            matches_with_env |> List.iter (fun env ->
              hook env matched_tokens
            );
            k rest
          end
        end
        else
          (* at least recurse *)
          k xs
      );
    }
  in
  (V.mk_visitor hook) ast
