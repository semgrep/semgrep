(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
 * Copyright (C) 2022 r2c
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
open Graph_code_AST_env
open AST_generic
module E = Entity_code
module G = Graph_code
module AST = AST_generic
module T = Type_AST

let logger = Logging.get_logger [ __MODULE__ ]

let ( let* ) = Option.bind

(*****************************************************************************)
(* Debugging helpers *)
(*****************************************************************************)
let string_of_any any =
  let v = Meta_AST.vof_any any in
  let s = OCaml.string_of_v v in
  s

(*****************************************************************************)
(* Defs/Uses control *)
(*****************************************************************************)
let when_defs_phase env f = if env.phase = Defs then f () else ()

let when_uses_phase env f = if env.phase = Uses then f () else ()

let when_uses_phase_or_none env f = if env.phase = Uses then f () else None

(*****************************************************************************)
(* AST generic accessors helpers *)
(*****************************************************************************)

(* When we create a node, we need to qualify it fully, because each
 * node must be unique (no duplicate nodes) *)
let entname_of_dotted_ident xs = xs |> List.map fst |> Common.join "."

(* When we lookup things, we actually care only about the last part
 * of the name as we gradually go down in the graph.
 *)
let dotted_ident_of_entname str = Common.split "\\." str

let dotted_ident_of_dir str = Common.split "/" str

(* see also AST_generic_helpers.name_of_ids *)

let last_ident_of_dotted_ident xs =
  match List.rev xs with
  | [] -> raise Impossible
  | x :: _ -> x

(* For now we handle only sample entities like function/class definitions
 * where the name is a simple identifier.
 *)
let ident_of_entity_opt _env ent =
  match ent.name with
  | EN (Id (id, _)) -> Some id
  | _ -> None

let entity_kind_of_definition _env (ent, defkind) =
  match defkind with
  | FuncDef _ -> E.Function (* less: could be also Method *)
  | ClassDef _ -> E.Class
  (* TODO: look parent node, if already in a function, then it's a local? *)
  | VarDef _ ->
      if
        AST_generic_helpers.has_keyword_attr AST.Final ent.attrs
        || AST_generic_helpers.has_keyword_attr AST.Const ent.attrs
      then E.Constant
      else E.Global
  | _ ->
      logger#error "entity kind not handled yet: %s"
        (string_of_any (Dk defkind));
      E.Other "Todo"

let type_of_module_opt env entname =
  if env.lang = Python then
    (* This is to allow to treat Python modules like classes
     * where you can do mod.function like for a field access.
     * The type of the module is then simply its name,
     * which will allow lookup_type_of_dotted_ident_opt to work.
     *)
    let tk =
      Parse_info.first_loc_of_file env.readable |> Parse_info.mk_info_of_loc
    in
    let xs = dotted_ident_of_entname entname in
    Some (T.N (xs |> List.map (fun s -> (s, tk))))
  else None

let dotted_ident_of_name_opt = function
  | Id (_v1, v2) -> (
      match !(v2.id_resolved) with
      | Some (ImportedEntity xs, _sid)
      | Some (ImportedModule (DottedName xs), _sid) ->
          Some xs
      (* TODO: even if not resolved, could be a local entity! use
       * env.file_qualifier? or class_qualifier?
       *)
      | _ -> None)
  (* TODO *)
  | IdQualified _ -> None

(* This is now used only in xxx-semgrep/.../Run.ml
 * less: move it there?
 *)
let rec dotted_ident_of_exprkind_opt ekind =
  match ekind with
  | N name -> dotted_ident_of_name_opt name
  | DotAccess (v1, _v2, v3) -> (
      let* xs = dotted_ident_of_exprkind_opt v1.e in
      match v3 with
      (* TODO? we could potentially set idinfo.resolved here *)
      | FN (Id (id, _idinfo)) -> Some (xs @ [ id ])
      | _ -> None)
  | _ -> None

let type_of_type_generic_opt _env ty =
  let aux ty =
    match ty.t with
    | TyN n ->
        let* xs = dotted_ident_of_name_opt n in
        Some (T.N xs)
    | _ -> None
  in
  aux ty

let type_of_definition_opt env dotted_ident (_ent, defkind) =
  match defkind with
  (* for a class, its name is his type *)
  | ClassDef _ -> Some (T.N dotted_ident)
  | VarDef { vtype = Some ty; _ } -> type_of_type_generic_opt env ty
  | FuncDef { frettype = Some ty; fparams; _ } ->
      let* ty = type_of_type_generic_opt env ty in
      Some (T.Function (fparams, ty))
  | _ -> None

(*****************************************************************************)
(* Graph builders helpers *)
(*****************************************************************************)

(* quite similar to create_intermediate_directories_if_not_present, but
 * for Packages.
 * java-specific?
 *)
let create_intermediate_packages_if_not_present g root xs =
  let dirs = Common2.inits xs |> List.map (fun xs -> Common.join "." xs) in
  let dirs =
    match dirs with
    | "" :: xs -> xs
    | _ -> raise Impossible
  in

  let rec aux current xs =
    match xs with
    | [] -> ()
    | x :: xs ->
        let entity = (x, E.Package) in
        if G.has_node entity g then aux entity xs
        else (
          g |> G.add_node entity;
          g |> G.add_edge (current, entity) G.Has;
          aux entity xs)
  in
  aux root dirs

let add_use_edge env (name, kind) =
  let src = env.current_parent in
  let dst = (name, kind) in
  logger#info "trying %s --> %s" (G.string_of_node src) (G.string_of_node dst);
  match () with
  | _ when not (G.has_node src env.g) ->
      logger#error "LOOKUP SRC FAIL %s --> %s, src does not exist???"
        (G.string_of_node src) (G.string_of_node dst)
  | _ when G.has_node dst env.g -> G.add_edge (src, dst) G.Use env.g
  | _ -> (
      match kind with
      | _ -> (
          let kind_original = kind in
          let dst = (name, kind_original) in
          let parent_target = G.not_found in
          match kind_original with
          | E.Package ->
              let fake_package =
                Common.split "\\." name |> List.map (fun s -> s ^ "2")
              in
              let dst = (Common.join "." fake_package, kind_original) in
              if not (G.has_node dst env.g) then
                (* disabled for now
                   create_intermediate_packages_if_not_present
                     env.g parent_target
                     (fake_package |> List.map (fun s -> s,()));
                *)
                logger#error "PB: lookup fail on %s (in %s)"
                  (G.string_of_node dst) (G.string_of_node src);
              env.g |> G.add_edge (src, dst) G.Use;
              ()
          | _ ->
              pr2
                (spf "PB: lookup fail on %s (in %s)" (G.string_of_node dst)
                   (G.string_of_node src));
              G.add_node dst env.g;
              env.g |> G.add_edge (parent_target, dst) G.Has;
              env.g |> G.add_edge (src, dst) G.Use))

let nodeinfo_of_id env (_id, tk) =
  let loc = Parse_info.unsafe_token_location_of_info tk in
  let loc = { loc with Parse_info.file = env.readable } in
  { G.pos = loc; props = []; typ = None }

let nodeinfo_of_file readable =
  let loc = Parse_info.first_loc_of_file readable in
  { G.pos = loc; props = []; typ = None }
