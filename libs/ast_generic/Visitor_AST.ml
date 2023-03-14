(* Yoann Padioleau
 *
 * Copyright (C) 2019, 2020 r2c
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
open OCaml
open AST_generic
module G = AST_generic
module H = AST_generic_helpers
module PI = Parse_info

(* Disable warnings against unused variables *)
[@@@warning "-26"]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* hooks *)
type visitor_in = {
  (* those are the one used by semgrep *)
  kexpr : (expr -> unit) * visitor_out -> expr -> unit;
  kstmt : (stmt -> unit) * visitor_out -> stmt -> unit;
  kstmts : (stmt list -> unit) * visitor_out -> stmt list -> unit;
  ktype_ : (type_ -> unit) * visitor_out -> type_ -> unit;
  kpattern : (pattern -> unit) * visitor_out -> pattern -> unit;
  kfield : (field -> unit) * visitor_out -> field -> unit;
  kfields : (field list -> unit) * visitor_out -> field list -> unit;
  kattr : (attribute -> unit) * visitor_out -> attribute -> unit;
  kpartial : (partial -> unit) * visitor_out -> partial -> unit;
  kdef : (definition -> unit) * visitor_out -> definition -> unit;
  kdir : (directive -> unit) * visitor_out -> directive -> unit;
  kparam : (parameter -> unit) * visitor_out -> parameter -> unit;
  ktparam : (type_parameter -> unit) * visitor_out -> type_parameter -> unit;
  kcatch : (catch -> unit) * visitor_out -> catch -> unit;
  kident : (ident -> unit) * visitor_out -> ident -> unit;
  kname : (name -> unit) * visitor_out -> name -> unit;
  kentity : (entity -> unit) * visitor_out -> entity -> unit;
  kfunction_definition :
    (function_definition -> unit) * visitor_out -> function_definition -> unit;
  kclass_definition :
    (class_definition -> unit) * visitor_out -> class_definition -> unit;
  kinfo : (tok -> unit) * visitor_out -> tok -> unit;
  kid_info : (id_info -> unit) * visitor_out -> id_info -> unit;
  ksvalue : (svalue -> unit) * visitor_out -> svalue -> unit;
  kargument : (argument -> unit) * visitor_out -> argument -> unit;
  klit : (literal -> unit) * visitor_out -> literal -> unit;
  ktodo : (todo_kind -> unit) * visitor_out -> todo_kind -> unit;
  kraw : (raw_tree -> unit) * visitor_out -> raw_tree -> unit;
}

and visitor_out = any -> unit

let default_visitor =
  {
    kexpr = (fun (k, _) x -> k x);
    kstmt = (fun (k, _) x -> k x);
    ktype_ = (fun (k, _) x -> k x);
    kpattern = (fun (k, _) x -> k x);
    kfield = (fun (k, _) x -> k x);
    kfields = (fun (k, _) x -> k x);
    kpartial = (fun (k, _) x -> k x);
    kdef = (fun (k, _) x -> k x);
    kdir = (fun (k, _) x -> k x);
    kattr = (fun (k, _) x -> k x);
    kparam = (fun (k, _) x -> k x);
    ktparam = (fun (k, _) x -> k x);
    kcatch = (fun (k, _) x -> k x);
    kident = (fun (k, _) x -> k x);
    kname = (fun (k, _) x -> k x);
    kentity = (fun (k, _) x -> k x);
    kstmts = (fun (k, _) x -> k x);
    kfunction_definition = (fun (k, _) x -> k x);
    kclass_definition = (fun (k, _) x -> k x);
    kinfo = (fun (k, _) x -> k x);
    (* By default, do not visit the refs in id_info *)
    kid_info =
      (fun (_k, _) x ->
        let {
          id_resolved = v_id_resolved;
          id_type = v_id_type;
          id_svalue = _IGNORED;
          id_hidden = _IGNORED2;
          id_info_id = _IGNORED3;
        } =
          x
        in
        let arg = v_ref_do_not_visit (v_option (fun _ -> ())) v_id_resolved in
        let arg = v_ref_do_not_visit (v_option (fun _ -> ())) v_id_type in
        ());
    ksvalue = (fun (k, _) x -> k x);
    kargument = (fun (k, _) x -> k x);
    klit = (fun (k, _) x -> k x);
    ktodo = (fun (k, _) x -> k x);
    kraw = (fun (k, _) x -> k x);
  }

type visitor_env_extra = {
  vin : visitor_in;
  (* This is the same as `v#visit_any env`, but we'd like to avoid the
   * allocations associated with repeated partial applications of v#visit_any.
   * However, we have to assign to this after construction of this object
   * because otherwise we would have a reference to `env` while constructing
   * `env`. *)
  mutable vout : visitor_out;
}

class ['self] visitor =
  object (_self : 'self)
    inherit [_] Matching_visitor.matching_visitor as super

    (**************************************************************************)
    (* Overrides:
     *
     * These are responsible for calling the various k___ functions that the
     * client has provided.
     * *)
    (**************************************************************************)

    method! v_partial ~recurse env x =
      let k = super#v_partial ~recurse env in
      env.extra.vin.kpartial (k, env.extra.vout) x

    method! v_stmts env xs =
      let k = super#v_stmts env in
      env.extra.vin.kstmts (k, env.extra.vout) xs

    method! v_fields env xs =
      let k = super#v_fields env in
      env.extra.vin.kfields (k, env.extra.vout) xs

    method! visit_tok env tok =
      let k = super#visit_tok env in
      env.extra.vin.kinfo (k, env.extra.vout) tok

    method! visit_ident env id =
      let k = super#visit_ident env in
      env.extra.vin.kident (k, env.extra.vout) id

    method! visit_id_info env info =
      let k = super#visit_id_info env in
      env.extra.vin.kid_info (k, env.extra.vout) info

    method! visit_name env name =
      let k = super#visit_name env in
      env.extra.vin.kname (k, env.extra.vout) name

    method! visit_expr env expr =
      let k = super#visit_expr env in
      env.extra.vin.kexpr (k, env.extra.vout) expr

    method! visit_literal env lit =
      let k = super#visit_literal env in
      env.extra.vin.klit (k, env.extra.vout) lit

    method! visit_svalue env svalue =
      let k = super#visit_svalue env in
      env.extra.vin.ksvalue (k, env.extra.vout) svalue

    method! visit_argument env arg =
      let k = super#visit_argument env in
      env.extra.vin.kargument (k, env.extra.vout) arg

    method! visit_type_ env t =
      let k = super#visit_type_ env in
      env.extra.vin.ktype_ (k, env.extra.vout) t

    method! visit_todo_kind env x =
      let k = super#visit_todo_kind env in
      env.extra.vin.ktodo (k, env.extra.vout) x

    method! visit_type_parameter env x =
      let k = super#visit_type_parameter env in
      env.extra.vin.ktparam (k, env.extra.vout) x

    method! visit_attribute env x =
      let k = super#visit_attribute env in
      env.extra.vin.kattr (k, env.extra.vout) x

    method! visit_stmt env x =
      let k = super#visit_stmt env in
      env.extra.vin.kstmt (k, env.extra.vout) x

    method! visit_catch env x =
      let k = super#visit_catch env in
      env.extra.vin.kcatch (k, env.extra.vout) x

    method! visit_pattern env x =
      let k = super#visit_pattern env in
      env.extra.vin.kpattern (k, env.extra.vout) x

    method! visit_definition env x =
      let k = super#visit_definition env in
      env.extra.vin.kdef (k, env.extra.vout) x

    method! visit_entity env x =
      let k = super#visit_entity env in
      env.extra.vin.kentity (k, env.extra.vout) x

    method! visit_function_definition env x =
      let k = super#visit_function_definition env in
      env.extra.vin.kfunction_definition (k, env.extra.vout) x

    method! visit_parameter env x =
      let k = super#visit_parameter env in
      env.extra.vin.kparam (k, env.extra.vout) x

    method! visit_field env x =
      let k = super#visit_field env in
      env.extra.vin.kfield (k, env.extra.vout) x

    method! visit_class_definition env x =
      let k = super#visit_class_definition env in
      env.extra.vin.kclass_definition (k, env.extra.vout) x

    method! visit_directive env x =
      let k = super#visit_directive env in
      env.extra.vin.kdir (k, env.extra.vout) x

    method! visit_raw_tree env x =
      let k (x : raw_tree) = super#visit_raw_tree env x in
      env.extra.vin.kraw (k, env.extra.vout) x
  end

let visitor_instance = lazy (new visitor)

let (mk_visitor :
      ?vardef_assign:bool ->
      ?flddef_assign:bool ->
      ?attr_expr:bool ->
      visitor_in ->
      visitor_out) =
 fun ?vardef_assign ?flddef_assign ?attr_expr vin ->
  let (lazy v) = visitor_instance in
  let visitor_env_extra = { vin; vout = (fun _ -> ()) } in
  let visitor_env =
    Matching_visitor.mk_env ?vardef_assign ?flddef_assign ?attr_expr
      visitor_env_extra
  in
  (* See the declaration of the visitor_env type for an explanation *)
  visitor_env_extra.vout <- v#visit_any visitor_env;
  v#visit_any visitor_env

(*****************************************************************************)
(* Extract tokens *)
(*****************************************************************************)

class ['self] extract_info_visitor =
  object (_self : 'self)
    inherit ['self] AST_generic.iter_no_id_info as super
    method! visit_tok globals tok = Common.push tok globals

    method! visit_expr globals x =
      match x.e with
      (* Ignore the tokens from the expression str is aliased to *)
      | Alias ((_str, t), _e) -> Common.push t globals
      | _ -> super#visit_expr globals x
  end

let ii_of_any any =
  let v = new extract_info_visitor in
  let globals = ref [] in
  v#visit_any globals any;
  List.rev !globals
  [@@profiling]

(*e: function [[Lib_AST.ii_of_any]] *)

let first_info_of_any any =
  let xs = ii_of_any any in
  let xs = List.filter Parse_info.is_origintok xs in
  let min, _max = Parse_info.min_max_ii_by_pos xs in
  min

(*****************************************************************************)
(* Extract ranges *)
(*****************************************************************************)

class ['self] range_visitor =
  let smaller t1 t2 =
    if compare t1.PI.charpos t2.PI.charpos < 0 then t1 else t2
  in
  let larger t1 t2 =
    if compare t1.PI.charpos t2.PI.charpos > 0 then t1 else t2
  in
  let incorporate_tokens ranges (left, right) =
    match !ranges with
    | None -> ranges := Some (left, right)
    | Some (orig_left, orig_right) ->
        ranges := Some (smaller orig_left left, larger orig_right right)
  in
  let incorporate_token ranges tok =
    if PI.is_origintok tok then
      let tok_loc = PI.unsafe_token_location_of_info tok in
      incorporate_tokens ranges (tok_loc, tok_loc)
  in
  object (_self : 'self)
    inherit ['self] AST_generic.iter_no_id_info as super
    method! visit_tok ranges tok = incorporate_token ranges tok

    method! visit_expr ranges expr =
      match expr.e_range with
      | None -> (
          let saved_ranges = !ranges in
          ranges := None;
          super#visit_expr ranges expr;
          expr.e_range <- !ranges;
          match saved_ranges with
          | None -> ()
          | Some r -> incorporate_tokens ranges r)
      | Some range -> incorporate_tokens ranges range

    method! visit_stmt ranges stmt =
      match stmt.s_range with
      | None -> (
          let saved_ranges = !ranges in
          ranges := None;
          super#visit_stmt ranges stmt;
          stmt.s_range <- !ranges;
          match saved_ranges with
          | None -> ()
          | Some r -> incorporate_tokens ranges r)
      | Some range -> incorporate_tokens ranges range
  end

let extract_ranges :
    AST_generic.any -> (PI.token_location * PI.token_location) option =
  let v = new range_visitor in
  let ranges = ref None in
  fun any ->
    v#visit_any ranges any;
    let res = !ranges in
    ranges := None;
    res

let range_of_tokens tokens =
  List.filter PI.is_origintok tokens |> PI.min_max_ii_by_pos
  [@@profiling]

let range_of_any_opt any =
  (* Even if the ranges are cached, calling `extract_ranges` to get them
   * is extremely expensive (due to `mk_visitor`). Testing taint-mode
   * open-redirect rule on Django, we spent ~16 seconds computing range
   * info (despite caching). If we bypass `extract_ranges` as we do here,
   * that time drops to just ~1.5 seconds! *)
  match any with
  | G.E e when Option.is_some e.e_range -> e.e_range
  | G.S s when Option.is_some s.s_range -> s.s_range
  | G.Tk tok -> (
      match Parse_info.token_location_of_info tok with
      | Ok tok_loc -> Some (tok_loc, tok_loc)
      | Error _ -> None)
  | G.Anys [] -> None
  | _ -> extract_ranges any
  [@@profiling]
