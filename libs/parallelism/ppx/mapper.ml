(* Austin Theriault
 *
 * Copyright (C) Semgrep, Inc.
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

(* nosemgrep: no-ref-declarations-at-top-scope *)
let auto = ref false

open Ppxlib
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Location

let ppx_identifier = "maybe_yield"
let mkloc txt loc = { txt; loc }
let mknoloc txt = mkloc txt !Ast_helper.default_loc
let digest x = Digest.to_hex (Digest.string (Marshal.to_string x []))

let error loc code =
  let open Printf in
  let message = function
    | `Too_many_attributes -> "too many attributes"
    | `Expecting_payload l ->
        sprintf "expecting payload in [%s]"
          (* nosemgrep: no-list-map *)
          (String.concat "," (List.map (sprintf "\"%s\"") l))
    | `Payload_not_a_string -> "payload is not a string"
    | `Payload_not_an_expression -> "payload is not an expression"
    | `Provide_a_name -> "this yield annotation requires a name argument"
  in
  Location.Error.raise
    (Location.Error.make ~loc ~sub:[]
       (Printf.sprintf "ppx_yield: %s" (message code)))

let has_name key { attr_name = { txt; _ }; _ } = txt = key
let remove_attribute key = List.filter (fun x -> not (has_name key x))

let has_attribute ?(auto = false) key l =
  if auto || List.exists (has_name key) l then Some (remove_attribute key l)
  else None

type yield = Constant of string | Dynamic of Parsetree.expression

let get_payload key = function
  | {
      attr_name = { txt; _ };
      attr_payload =
        PStr
          [
            {
              pstr_desc =
                Pstr_eval
                  ( { pexp_desc = Pexp_constant (Pconst_string (x, _, None)); _ },
                    _ );
              _;
            };
          ];
      _;
    }
    when txt = key ->
      Some (Some (Constant x))
  | { attr_name = { txt; _ }; attr_payload = PStr []; _ } when txt = key ->
      Some None
  | _ -> None

let get_string_payload key ({ attr_loc; _ } as e) =
  match get_payload key e with
  | Some None -> Some None
  | Some (Some (Constant x)) -> Some (Some x)
  | Some (Some _) -> error attr_loc `Payload_not_a_string
  | None -> None

let has_yield_attribute ?auto = has_attribute ?auto ppx_identifier
let payload_of_string x = PStr [ Str.eval (Exp.constant (Const.string x)) ]
let var x = Exp.ident (mknoloc (Longident.parse x))

let string_of_loc (l : Location.t) =
  let file = l.loc_start.pos_fname in
  let line = l.loc_start.pos_lnum in
  Printf.sprintf "%s:%d" file line

let unit = Exp.construct (mknoloc (Longident.parse "()")) None

let call_yield =
  let maybe_yield = "Concurrent.maybe_yield" in
  Exp.apply (var maybe_yield) [ (Nolabel, unit) ]

let wrap_yield expr = Exp.sequence call_yield expr

let rec arity { pexp_desc; _ } =
  match pexp_desc with
  | Pexp_fun (a, _, _, e) -> a :: arity e
  | Pexp_function cases ->
      let min_list l1 l2 = if List.length l1 < List.length l2 then l1 else l2 in
      Nolabel
      :: List.fold_left
           (fun acc { pc_rhs; _ } -> min_list (arity pc_rhs) acc)
           [] cases
  | Pexp_newtype (_, e) -> arity e
  | Pexp_constraint (e, _) -> arity e
  | Pexp_poly (e, _) -> arity e
  | _ -> []

let rec wrap_yield_method ({ pexp_desc; _ } as expr) =
  match pexp_desc with
  | Pexp_fun (label, def, pat, e) ->
      { expr with pexp_desc = Pexp_fun (label, def, pat, wrap_yield_method e) }
  | Pexp_poly (e, typ) ->
      { expr with pexp_desc = Pexp_poly (wrap_yield_method e, typ) }
  | _ -> wrap_yield expr

let eta_expand f t n =
  (* nosemgrep: no-list-mapi *)
  let vars = List.mapi (fun k x -> (x, Printf.sprintf "__x%d" k)) n in
  let rec app acc = function
    | [] -> acc
    | (l, x) :: tl ->
        app (Exp.apply acc [ (l, Exp.ident (mknoloc (Lident x))) ]) tl
  in
  let rec lam = function
    | [] -> f (app t vars)
    | (l, x) :: tl -> Exp.fun_ l None (Pat.var (mknoloc x)) (lam tl)
  in
  lam vars

let rec not_a_constant expr =
  match expr.pexp_desc with
  | Pexp_constant _
  | Pexp_ident _ ->
      false
  | Pexp_coerce (e, _, _)
  | Pexp_poly (e, _)
  | Pexp_constraint (e, _) ->
      not_a_constant e
  | _ -> true

let rec name_of_pattern pat =
  match pat.ppat_desc with
  | Ppat_var { txt; _ } -> Some txt
  | Ppat_constraint (pat, _) -> name_of_pattern pat
  | _ -> None

let translate_value_bindings value_binding auto vbs =
  let vbs_arity_name =
    (* nosemgrep: no-list-map *)
    List.map
      (fun vb ->
        match (vb, has_yield_attribute ~auto vb.pvb_attributes) with
        | { pvb_expr; pvb_loc; pvb_pat; _ }, Some attr
          when not_a_constant pvb_expr -> (
            let arity = arity pvb_expr in
            let from_names arity fun_name =
              if auto && arity = [] then (vb, None)
              else (vb, Some (arity, fun_name, attr))
            in
            match
              ( name_of_pattern pvb_pat,
                (* nosemgrep: no-list-filter-map *)
                List.filter_map (get_payload ppx_identifier) vb.pvb_attributes
              )
            with
            | Some fun_name, []
            | Some fun_name, [ None ] ->
                from_names arity fun_name
            | _, [ None ] -> from_names [] ""
            | _, [] -> (vb, None)
            | _, [ Some _ ]
            | _, _ :: _ :: _ ->
                error pvb_loc `Too_many_attributes)
        | _, _ -> (vb, None))
      vbs
  in
  let vbs =
    (* nosemgrep: no-list-map *)
    List.map
      (function
        | vb, None -> value_binding vb
        | { pvb_pat; pvb_loc; pvb_expr; _ }, Some (arity, _, attrs) ->
            (* Remove yield attribute: *)
            let vb =
              Vb.mk ~attrs ~loc:pvb_loc pvb_pat pvb_expr |> value_binding
            in
            if arity = [] then { vb with pvb_expr = wrap_yield vb.pvb_expr }
            else vb)
      vbs_arity_name
  in
  let new_vbs =
    (* nosemgrep: no-list-filter-map *)
    List.filter_map
      (function
        | _, Some ((_ :: _ as arity), fun_name, _) ->
            let ident = Exp.ident (mknoloc (Lident fun_name)) in
            let expr = eta_expand wrap_yield ident arity in
            Some (Vb.mk (Pat.var (mknoloc fun_name)) expr)
        | _ -> None)
      vbs_arity_name
  in
  (vbs, new_vbs)

let mapper =
  object (self)
    inherit [bool] Ast_traverse.fold_map as super

    method! module_binding ({ pmb_name; _ } as binding) (auto as acc) =
      let acc =
        match pmb_name.txt with
        | None -> acc
        | Some _ -> auto
      in
      let result, _ = super#module_binding binding acc in
      (result, auto)

    method! structure l auto =
      let _, results =
        List.fold_left
          (fun (auto, acc) expr ->
            match expr with
            | { pstr_desc = Pstr_attribute attr; pstr_loc; _ } as pstr -> (
                match get_string_payload ppx_identifier attr with
                | Some (Some "auto") -> (true, acc)
                | Some (Some "auto-off") -> (false, acc)
                | None -> (auto, pstr :: acc)
                | _ ->
                    error pstr_loc (`Expecting_payload [ "auto"; "auto-off" ]))
            | { pstr_desc = Pstr_value (rec_flag, vbs); pstr_loc } ->
                let value_binding vb = fst (self#value_binding vb auto) in
                let vbs, new_vbs =
                  translate_value_bindings value_binding auto vbs
                in
                let str = Str.value ~loc:pstr_loc rec_flag vbs in
                if new_vbs = [] then (auto, str :: acc)
                else
                  let warning_off =
                    Str.attribute
                      {
                        attr_name = mknoloc "ocaml.warning";
                        attr_payload = payload_of_string "-32";
                        attr_loc = Location.none;
                      }
                  in
                  let include_wrapper =
                    new_vbs |> Str.value Nonrecursive |> fun x ->
                    Mod.structure [ warning_off; x ] |> Incl.mk |> Str.include_
                  in
                  (auto, include_wrapper :: str :: acc)
            | sti ->
                let sti, _ = super#structure_item sti auto in
                (auto, sti :: acc))
          (auto, []) l
      in
      (List.rev results, auto)

    method! class_field class_field (auto as acc) =
      match class_field with
      | {
       pcf_desc = Pcf_method (loc, privat, Cfk_concrete (flag, expr));
       pcf_loc;
       pcf_attributes;
       _;
      } -> begin
          let yield =
            match
              (* nosemgrep: no-list-filter-map *)
              (List.filter_map (get_payload ppx_identifier) pcf_attributes, auto)
            with
            | [ Some yield_name ], _ -> Some yield_name
            | [ None ], _
            | _, true ->
                Some (Constant loc.txt)
            | [], false -> None
            | _ :: _ :: _, _ -> error pcf_loc `Too_many_attributes
          in
          match yield with
          | None -> super#class_field class_field acc
          | Some _ ->
              let expr = wrap_yield (fst (self#expression expr acc)) in
              ( {
                  class_field with
                  pcf_desc = Pcf_method (loc, privat, Cfk_concrete (flag, expr));
                  pcf_attributes =
                    remove_attribute ppx_identifier pcf_attributes;
                },
                acc )
        end
      | _ -> super#class_field class_field acc

    method! class_expr class_expr (_ as acc) =
      match class_expr with
      | { pcl_desc = Pcl_let (rec_flag, vbs, body); _ } ->
          let vbs, new_vbs =
            let value_binding vb = fst (self#value_binding vb acc) in
            translate_value_bindings value_binding false vbs
          in
          let body, _ = self#class_expr body acc in
          let body =
            if new_vbs = [] then body else Cl.let_ Nonrecursive new_vbs body
          in
          ({ class_expr with pcl_desc = Pcl_let (rec_flag, vbs, body) }, acc)
      | _ -> super#class_expr class_expr acc

    method! expression expr (_ as acc) =
      let expr =
        match expr with
        | { pexp_desc = Pexp_let (rec_flag, vbs, body); _ } as expr ->
            let vbs, new_vbs =
              let value_binding vb = fst (self#value_binding vb acc) in
              translate_value_bindings value_binding false vbs
            in
            let body = fst (self#expression body acc) in
            let body =
              if new_vbs = [] then body else Exp.let_ Nonrecursive new_vbs body
            in
            { expr with pexp_desc = Pexp_let (rec_flag, vbs, body) }
        | expr -> fst (super#expression expr acc)
      in
      let { pexp_attributes; pexp_loc; _ } = expr in
      (* nosemgrep: no-list-filter-map *)
      match List.filter_map (get_payload ppx_identifier) pexp_attributes with
      | [ Some _ ] ->
          ( {
              expr with
              pexp_attributes = remove_attribute ppx_identifier pexp_attributes;
            }
            |> wrap_yield,
            acc )
      | [ None ] -> error pexp_loc `Provide_a_name
      | [] -> (expr, acc)
      | _ -> error pexp_loc `Too_many_attributes
  end

let remove_attributes =
  object
    inherit Ast_traverse.map as super

    method! structure l =
      let l =
        List.filter
          (function
            | { pstr_desc = Pstr_attribute attr; _ }
              when has_yield_attribute [ attr ] <> None ->
                false
            | _ -> true)
          l
      in
      super#structure l

    method! attributes attributes =
      super#attributes
        (match has_yield_attribute attributes with
        | Some attrs -> attrs
        | None -> attributes)
  end

let has_disable l =
  let disable = ref false in
  let f = function
    | { pstr_desc = Pstr_attribute attr; pstr_loc; _ } as pstr -> (
        match get_string_payload ppx_identifier attr with
        | Some (Some "disable") ->
            disable := true;
            None
        | Some (Some "auto-off")
        | Some (Some "auto")
        | None ->
            Some pstr
        | _ ->
            error pstr_loc
              (`Expecting_payload [ "auto"; "auto-off"; "disable" ]))
    | i -> Some i
  in
  (* nosemgrep: no-list-filter-map *)
  let res = List.filter_map f l in
  (!disable, res)

let toplevel_mapper auto =
  object
    inherit Ast_traverse.map
    method! signature si = si

    method! structure l =
      match l with
      | [] -> []
      | l ->
          let disable, l = has_disable l in
          if disable then l
          else begin
            let l, _ = mapper#structure l auto in
            l
          end
  end
