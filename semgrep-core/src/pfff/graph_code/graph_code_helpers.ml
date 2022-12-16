(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
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
open Common

module E = Entity_code
module G = Graph_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

let propagate_users_of_functions_globals_types_to_prototype_extern_typedefs g =
  let pred = G.mk_eff_use_pred g in
  g |> G.iter_nodes (fun n ->
    let n_def_opt =
      match n with
      | s, E.Prototype -> Some (s, E.Function)
      | s, E.GlobalExtern -> Some (s, E.Global)
      (* todo: actually should look at env.typedefs because it's not
       * necessaraly T_Xxxx -> S_Xxxx
      *)
      | s, E.Type when s =~ "T__\\(.*\\)$" ->
          Some ("S__" ^(Common.matched1 s), E.Type)
      | _ -> None
    in
    n_def_opt |> Option.iter (fun n_def ->
      let n_decl = n in
      if G.has_node n_def g
      then begin
        (* let's create a link between the def and the decl *)
        g |> G.add_edge (n_def, n_decl) G.Use;
        (* and now the users *)
        let users = pred n_def in
        users |> List.iter (fun user ->
          g |> G.add_edge (user, n_decl) G.Use
        )
      end
    )
  )
