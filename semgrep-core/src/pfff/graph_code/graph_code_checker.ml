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
module Error = Error_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * current checks:
 *  - Deadcode
 *  - UndefinedDefOfDecl
 *  - UnusedExport
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* todo: generalize and mv in file_type.ml? *)
let is_header_file file =
  let (_d,_b,e) = Common2.dbe_of_filename file in
  e = "h"

(*****************************************************************************)
(* Checker *)
(*****************************************************************************)
let check_imperative g =

  let pred = G.mk_eff_use_pred g in

  g |> G.iter_nodes (fun n ->
    G.nodeinfo_opt n g |> Option.iter (fun info ->

      let ps = pred n in
      (* todo: filter nodes that are in boilerplate code *)
      if ps = []
      then
        (match n with
         | s, E.Function when s =$= "main" || s =~ "^main__.*" -> ()
         | _ ->
             Error.warning_loc info.G.pos (Error.Deadcode n);
        );

      (* todo: factorize with graph_code_clang, put in database_code? *)
      let n_def_opt =
        match n with
        | s, E.Prototype -> Some (s, E.Function)
        | s, E.GlobalExtern -> Some (s, E.Global)
        | _ -> None
      in
      n_def_opt |> Option.iter (fun n_def ->
        let n_decl = n in
        if not (G.has_node n_def g)
        then
          (* actually in C we can have things that looks like GlobalExtern
           * e.g. Syscall sysnop; but are actually Prototype, so we must look
           * for E.Function in that case
          *)
          if (G.has_node (fst n_def, E.Function) g)
          then ()
          else
            Error.warning_loc info.G.pos (Error.UndefinedDefOfDecl n_decl)
      );

      let n_decl_opt =
        match n with
        | s, E.Function -> Some (s, E.Prototype)
        | s, E.Global -> Some (s, E.GlobalExtern)
        | _ -> None
      in
      n_decl_opt |> Option.iter (fun n_decl ->
        let n_def = n in
        if (G.has_node n_decl g)
        then begin
          let file_def = G.file_of_node n_def g in
          let file_decl = G.file_of_node n_decl g in
          let info_decl = G.nodeinfo n_decl g in
          let users_outside = ps |> List.filter (fun n ->
            try
              let file_user = G.file_of_node n g in
              file_user <> file_def
            with Not_found -> true
          )
          in
          if users_outside = [] && ps <> [] && is_header_file file_decl
          then Error.warning_loc info_decl.G.pos
              (Error.UnusedExport (n_decl, file_def));

          (* for clang I usually add a Use edge between the def and the decl
           * so the decl would not have been marked as dead without this:
          *)
          if ps = []
          then Error.warning_loc info_decl.G.pos (Error.Deadcode n_decl);
        end
      );
    ))

let check g =
  Common.save_excursion Error.g_errors [] (fun () ->
    check_imperative g;
    !Error.g_errors |> List.rev
  )
