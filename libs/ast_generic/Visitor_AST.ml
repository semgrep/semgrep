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

type visitor_env = {
  vin : visitor_in;
  (* This is the same as `v#visit_any env`, but we'd like to avoid the
   * allocations associated with repeated partial applications of v#visit_any.
   * However, we have to assign to this after construction of this object
   * because otherwise we would have a reference to `env` while constructing
   * `env`. *)
  mutable vout : visitor_out;
  vardef_assign : bool;
  flddef_assign : bool;
  attr_expr : bool;
}

(* NOTE: we do a few subtle things at a few places now for semgrep to trigger a
 * few more artificial visits:
 *  - we call vardef_to_assign (if `vardef_assign` is `true`)
 *  - we generate partial defs on the fly and call kpartial
 *  - we call v_expr on nested XmlXml to give the chance for an
 *    Xml pattern to also be matched against nested Xml elements
 *
 * old: We used to apply the VarDef-Assign equivalence by default, but this was
 * error prone because visitors typically do side-effectful things and VarDefs
 * were visited twice (as a VarDef and as an Assign), thus repeating
 * side-effects, leading to surprises.
 *)
class ['self] matching_visitor =
  object (self : 'self)
    inherit ['self] AST_generic.iter as super

    (**************************************************************************)
    (* Helpers *)
    (**************************************************************************)

    (* The recurse argument is subtle. It is needed because we want different
     * behaviors depending on the context:
     * - in some context we want to recurse, for example when we call ii_of_any
     *   (Partial ...), we want to get all the tokens in it
     * - in other context we do not want to recurse, because that would mean we
     *   would visit two times the same function (one with a body, and one
     *   without a body), which can lead some code, e.g., Naming_AST, to
     *   generate intermediate sids which in turn lead to regressions in
     *   end-2-end tests (because the value of sid differ).
     * This is why when we are called from visit_partial (which is called by the
     * generated visit_any), we recurse, but when we are called from e.g.
     * visit_definition, where we construct a new PartialDef, we don't.
     *)
    method v_partial ~recurse env x =
      let k x = if recurse then super#visit_partial env x in
      env.vin.kpartial (k, env.vout) x

    method v_named_attr_as_expr env name args =
      (* A named attribute is essentially a function call, but this is not
       * explicit in Generic so we cannot match expression patterns against
       * attributes. This equivalence enables exactly that, and we can e.g.
       * match `@f(a)` with `f($X)`. *)
      if env.attr_expr then self#visit_expr env (e (Call (e (N name), args)))

    method v_stmts env xs =
      let k xs =
        match xs with
        | [] -> ()
        | x :: xs ->
            self#visit_stmt env x;
            (* we will call the visitor also on subsequences. This is useful
             * for semgrep *)
            self#v_stmts env xs
      in
      env.vin.kstmts (k, env.vout) xs

    method v_fields env xs =
      (* As opposed to kstmts, we don't call the client-supplied visitor for
       * sublists of xs. Indeed, in semgrep, fields are matched in any order so
       * calling the visitor and matcher on the entire list of fields should
       * also work. *)
      let k xs = self#visit_list self#visit_field env xs in
      env.vin.kfields (k, env.vout) xs

    method v_vardef_as_assign_expr env ventity =
      function
      | VarDef ({ vinit = Some _; _ } as vdef) when env.vardef_assign ->
          (* A VarDef is implicitly a declaration followed by an assignment
           * expression, so we should visit the assignment expression as well.
           *
           * Note that we cannot treat this as a simple equivalence later, as
           * expressions are visited separately from statements.
           *
           * This feels a bit hacky here, so let's take a TODO to improve this
           *)
          self#visit_expr env (H.vardef_to_assign (ventity, vdef))
      | _ -> ()

    method v_flddef_as_assign_expr env ventity =
      function
      (* No need to cover the VarDef({vinit = Some _; )} case here. It will
       * be covered by v_vardef_as_assign_expr at some point when v_field
       * below call v_stmt (which itself will call v_def).
       *
       * In certain languages like Javascript, some method definitions look
       * really like assignements, so we would like an expression pattern like
       * '$X = function() { ...}' to also match code like
       * 'class Foo { x = function() { return; } }'.
       *)
      | FuncDef fdef when env.flddef_assign ->
          let resolved = Some (LocalVar, G.SId.unsafe_default) in
          self#visit_expr env (H.funcdef_to_lambda (ventity, fdef) resolved)
      | _ -> ()

    (* WEIRD: not sure why, but using this code below instead of
     * the v_def_as_partial above cause some regressions.
     *
     *  (* calling kpartial with a modified def *)
     *  (match x with
     *  | ent, FuncDef def ->
     *     let partial_def = { def with fbody = empty_fbody } in
     *     v_partial (PartialDef (ent, FuncDef partial_def))
     *  | _ -> ()
     *  )
     *)
    method v_def_as_partial env ent defkind =
      (* calling kpartial with a modified def *)
      match defkind with
      | FuncDef def ->
          let partial_def = { def with fbody = FBNothing } in
          self#v_partial ~recurse:false env
            (PartialDef (ent, FuncDef partial_def))
      | ClassDef def ->
          let partial_def = { def with cbody = empty_body } in
          self#v_partial ~recurse:false env
            (PartialDef (ent, ClassDef partial_def))
      | _ -> ()

    (**************************************************************************)
    (* Overrides:
     *
     * These implement matching-specific behavior such as generating partial
     * nodes and they are also responsible for calling the various k___
     * functions that the client has provided.
     * *)
    (**************************************************************************)

    method! visit_tok env tok =
      let k = super#visit_tok env in
      env.vin.kinfo (k, env.vout) tok

    method! visit_ident env id =
      let k = super#visit_ident env in
      env.vin.kident (k, env.vout) id

    method! visit_id_info env info =
      let k = super#visit_id_info env in
      env.vin.kid_info (k, env.vout) info

    method! visit_xml_body env xml =
      match xml with
      | XmlXml v1 ->
          (* subtle: old: let v1 = v_xml v1 in ()
           * We want a simple Expr (Xml ...) pattern to also be matched
           * against nested XmlXml elements *)
          self#visit_expr env (Xml v1 |> G.e)
      | _else_ -> super#visit_xml_body env xml

    method! visit_name env name =
      let k = super#visit_name env in
      env.vin.kname (k, env.vout) name

    method! visit_expr env expr =
      let k x =
        match x.e with
        | Container (v1, v2) ->
            (match v1 with
            (* less: could factorize with case below by doing List|Dict here and
             * below in Tuple a String|Id
             *)
            | Dict ->
                v2 |> PI.unbracket
                |> List.iter (fun e ->
                       match e.e with
                       | Container
                           ( Tuple,
                             (tok, [ { e = L (String (_, id, _)); _ }; e ], _)
                           ) ->
                           let t = PI.fake_info tok ":" in
                           self#v_partial ~recurse:false env
                             (PartialSingleField (id, t, e))
                       | _ -> ())
            (* for Go where we use List for composite literals.
             * TODO? generate Dict in go_to_generic.ml instead directly?
             *)
            | List ->
                v2 |> PI.unbracket
                |> List.iter (fun e ->
                       match e.e with
                       | Container
                           (Tuple, (tok, [ { e = N (Id (id, _)); _ }; e ], _))
                         ->
                           let t = PI.fake_info tok ":" in
                           self#v_partial ~recurse:false env
                             (PartialSingleField (id, t, e))
                       | _ -> ())
            | _ -> ());
            let v1 = self#visit_container_operator env v1
            and v2 =
              self#visit_bracket (self#visit_list self#visit_expr) env v2
            in
            ()
        | _else -> super#visit_expr_kind env x.e
      in
      env.vin.kexpr (k, env.vout) expr

    method! visit_literal env lit =
      let k = super#visit_literal env in
      env.vin.klit (k, env.vout) lit

    method! visit_svalue env svalue =
      let k = super#visit_svalue env in
      env.vin.ksvalue (k, env.vout) svalue

    method! visit_argument env arg =
      let k x =
        (match x with
        | ArgKwd (v1, v2) ->
            let tok = snd v1 in
            let t = PI.fake_info tok ":" in
            self#v_partial ~recurse:false env (PartialSingleField (v1, t, v2))
        | _else -> ());
        super#visit_argument env x
      in
      env.vin.kargument (k, env.vout) arg

    method! visit_type_ env t =
      let k = super#visit_type_ env in
      env.vin.ktype_ (k, env.vout) t

    method! visit_todo_kind env x =
      (* bugfix: do not call visit_ident here, otherwise code like
       * Analyze_pattern might consider the string for -filter_irrelevant_rules
       *)
      let k x =
        let _str, tok = x in
        self#visit_tok env tok
      in
      env.vin.ktodo (k, env.vout) x

    method! visit_type_parameter env x =
      let k = super#visit_type_parameter env in
      env.vin.ktparam (k, env.vout) x

    method! visit_attribute env x =
      let k x =
        (match x with
        | NamedAttr (_, v1, v2) -> self#v_named_attr_as_expr env v1 v2
        | _else -> ());
        super#visit_attribute env x
      in
      env.vin.kattr (k, env.vout) x

    method! visit_case_and_body env x =
      self#v_partial ~recurse:false env (PartialSwitchCase x);
      super#visit_case_and_body env x

    method! visit_stmt env x =
      let k x =
        (match x.s with
        | If (t, Cond v1, _v2, _v3) ->
            self#v_partial ~recurse:false env (PartialIf (t, v1))
        | Switch (v0, Some (G.Cond v1), _v2) ->
            self#v_partial ~recurse:false env (PartialMatch (v0, v1))
        | Try (t, v1, _v2, _v3) ->
            self#v_partial ~recurse:false env (PartialTry (t, v1))
        | _else -> ());
        (* todo? visit the s_id too? *)
        super#visit_stmt_kind env x.s
      in
      env.vin.kstmt (k, env.vout) x

    method! visit_catch env x =
      let k x =
        self#v_partial ~recurse:false env (PartialCatch x);
        super#visit_catch env x
      in
      env.vin.kcatch (k, env.vout) x

    method! visit_finally env x =
      let t, v = x in
      self#v_partial ~recurse:false env (PartialFinally (t, v));
      super#visit_finally env x

    method! visit_pattern env x =
      let k = super#visit_pattern env in
      env.vin.kpattern (k, env.vout) x

    method! visit_definition env x =
      let k x =
        let v1, v2 = x in
        self#v_vardef_as_assign_expr env v1 v2;
        self#v_def_as_partial env v1 v2;
        super#visit_definition env x
      in
      env.vin.kdef (k, env.vout) x

    method! visit_partial env x = self#v_partial ~recurse:true env x

    method! visit_entity env x =
      let k = super#visit_entity env in
      env.vin.kentity (k, env.vout) x

    method! visit_function_definition env x =
      let k x =
        self#v_partial ~recurse:false env (PartialLambdaOrFuncDef x);
        super#visit_function_definition env x
      in
      env.vin.kfunction_definition (k, env.vout) x

    method! visit_parameter env x =
      let k = super#visit_parameter env in
      env.vin.kparam (k, env.vout) x

    method! visit_field env x =
      let k x =
        match x with
        | F v1 ->
            (match v1.s with
            | DefStmt
                ( { name = EN (Id (id, _)); _ },
                  FieldDefColon { vinit = Some e; _ } ) ->
                let t = PI.fake_info (snd id) ":" in
                self#v_partial ~recurse:false env
                  (PartialSingleField (id, t, e))
            | DefStmt (ent, def) -> self#v_flddef_as_assign_expr env ent def
            | _ -> ());
            self#visit_stmt env v1
      in
      env.vin.kfield (k, env.vout) x

    method! visit_class_definition env x =
      let k { ckind; cextends; cimplements; cmixins; cparams; cbody } =
        (* This is handcoded so that we can call v_fields which calls the
         * client-supplied visitor function *)
        self#visit_wrap self#visit_class_kind env ckind;
        self#visit_list self#visit_class_parent env cextends;
        self#visit_list self#visit_type_ env cimplements;
        self#visit_list self#visit_type_ env cmixins;
        self#visit_parameters env cparams;
        self#visit_bracket self#v_fields env cbody
      in
      env.vin.kclass_definition (k, env.vout) x

    method! visit_directive env x =
      let k = super#visit_directive env in
      env.vin.kdir (k, env.vout) x

    method! visit_raw_tree env x =
      let k (x : raw_tree) = super#visit_raw_tree env x in
      env.vin.kraw (k, env.vout) x

    (* Overrides to call v_fields instead of the autogenerated code which just
     * repeatedly calls visit_field. We could instead define a `fields` type in
     * AST_generic, use it instead of `field list` wherever we want this
     * behavior, and override `visit_fields`. *)
    method! visit_Record env v1 = self#visit_bracket self#v_fields env v1

    method! visit_TyRecordAnon env v0 v1 =
      self#visit_wrap self#visit_class_kind env v0;
      self#visit_bracket self#v_fields env v1

    method! visit_AndType env v1 = self#visit_bracket self#v_fields env v1
    method! visit_Flds env v1 = self#v_fields env v1

    (* Overrides to call v_stmts instead of the autogenerated code which just
     * repeatedly calls visit_stmt. We could instead define a `stmts` type in
     * AST_generic, use it instead of `stmt list` wherever we want this visitor
     * behavior, and override `visit_stmts`. *)

    method! visit_Block env v1 = self#visit_bracket self#v_stmts env v1

    method! visit_ModuleStruct env v1 v2 =
      self#visit_option self#visit_dotted_ident env v1;
      self#v_stmts env v2

    method! visit_program env v1 = self#v_stmts env v1
    method! visit_Ss env v1 = self#v_stmts env v1
  end

let visitor_instance = lazy (new matching_visitor)

let (mk_visitor :
      ?vardef_assign:bool ->
      ?flddef_assign:bool ->
      ?attr_expr:bool ->
      visitor_in ->
      visitor_out) =
 fun ?(vardef_assign = false) ?(flddef_assign = false) ?(attr_expr = false) vin ->
  let (lazy v) = visitor_instance in
  let visitor_env =
    { vin; vout = (fun _ -> ()); vardef_assign; flddef_assign; attr_expr }
  in
  (* See the declaration of the visitor_env type for an explanation *)
  visitor_env.vout <- v#visit_any visitor_env;
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
    if PI.has_origin_loc tok then
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
  List.filter PI.has_origin_loc tokens |> PI.min_max_ii_by_pos
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
