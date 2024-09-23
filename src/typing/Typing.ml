(* Nat Mote
 *
 * Copyright (C) 2019-2023 Semgrep, Inc.
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
module G = AST_generic
module H = AST_generic_helpers

(* hook to allow pro-only type inference for expressions *)
let pro_hook_type_of_expr :
    (Lang.t -> G.expr -> G.name Type.t option) option ref =
  ref None

(* returns possibly the inferred type of the expression,
 * as well as an ident option that can then be used to query LSP to get the
 * type of the ident. *)
let rec type_of_expr lang e : G.name Type.t * G.ident option =
  let pro_type =
    match !pro_hook_type_of_expr with
    | None -> None
    | Some f -> f lang e
  in
  match pro_type with
  | Some ty -> (ty, None)
  | None -> (
      match e.G.e with
      | G.L lit ->
          let t = type_of_lit lang lit in
          (t, None)
      | G.DotAccess
          ( _obj,
            _,
            FN (Id (("length", _), { id_type = { contents = None }; _ })) )
        when lang =*= Lang.Java ->
          (* TODO: Move this to 'guess_type_of_dotaccess', but somehow we need to
           * pass it additional info (e.g. the "expected type" as in bidirectional
           * type checking) so that it can distinguish `length` as a method (e.g. `String`)
           * and `length` as an attribute (arrays). The exact solution needs to be
           * considered more carefully.
           *
           * NOTE that right now the use of `length` as a method is handled by
           * 'guess_type_of_dotaccess' which is invoked by the 'typing_visitor'
           * (see 'check_program'). *)
          (Type.Builtin Type.Int, None)
      | G.N name
      | G.DotAccess (_, _, FN name) ->
          type_of_name lang name
      | G.Cast (t, _, _) -> (type_of_ast_generic_type lang t, None)
      (* TODO? or generate a fake "new" id for LSP to query on tk? *)
      (* We conflate the type of a class with the type of its instance. Maybe at
       * some point we should introduce a `Class` type and unwrap it here upon
       * instantiation. *)
      | G.New (_tk, t, _ii, _) -> (type_of_ast_generic_type lang t, None)
      (* Binary operator *)
      | G.Call ({ e = IdSpecial (Op op, _); _ }, (_l, [ Arg e1; Arg e2 ], _r))
        ->
          let t1, _id = type_of_expr lang e1 in
          let t2, _id = type_of_expr lang e2 in
          let t =
            match (t1, op, t2) with
            | ( Type.(Builtin (Int | Float)),
                (G.Plus | G.Minus (* TODO more *)),
                _ )
            | ( _,
                (G.Plus | G.Minus | G.Mult (* TODO more *)),
                Type.(Builtin (Int | Float)) )
            (* Note that `+` is overloaded in many languages and may also be
             * string concatenation, and unfortunately some languages such
             * as Java and JS/TS have implicit coercions to string. *)
              when lang =*= Lang.Python || lang =*= Lang.Php (* TODO more *) ->
                Type.Builtin Type.Number
            | ( Type.Builtin Type.Int,
                (G.Plus | G.Minus (* TODO more *)),
                Type.Builtin Type.Int ) ->
                Type.Builtin Type.Int
            | ( _,
                ( G.Eq | G.PhysEq | G.NotEq | G.NotPhysEq | G.Lt | G.LtE | G.Gt
                | G.GtE | G.In | G.NotIn | G.Is | G.NotIs | G.And ),
                _ ) ->
                Type.Builtin Type.Bool
            | Type.Builtin Type.Bool, G.Or, _
            | _, G.Or, Type.Builtin Type.Bool ->
                Type.Builtin Type.Bool
            | _, G.Or, _ when lang =*= Lang.Java ->
                (* E.g. in Python you can write `x or ""` to mean `""` in case `x` is `None`.
                 * THINK: Is there a similar idiom involving `and`/`&&` ? *)
                Type.Builtin Type.Bool
            | Type.Builtin Type.Bool, (G.BitOr | G.BitAnd | G.BitXor), _
            | _, (G.BitOr | G.BitAnd | G.BitXor), Type.Builtin Type.Bool
              when lang =*= Lang.Java ->
                (* If the operands to |, &, or ^ are boolean, in Java these are
                 * boolean operators. If we can resolve one operand to a boolean, we
                 * know that in a well-formed program, the other is also a boolean.
                 * *)
                Type.Builtin Type.Bool
            | _else_ -> Type.NoType
          in
          (t, None)
      (* Unary operator *)
      | G.Call ({ e = IdSpecial (Op op, _); _ }, (_l, [ Arg e ], _r)) ->
          let t, _id = type_of_expr lang e in
          let t =
            match (op, t) with
            | G.Not, _ -> Type.Builtin Type.Bool
            | _else_ -> Type.NoType
          in
          (t, None)
      | G.Call (e, _args) ->
          (* If 'e' is of the form `o.f`, then 'check_program' should have filled in the
           * 'id_type' of `f` using 'guess_type_of_dotaccess'. *)
          let t, id = type_of_expr lang e in
          let t =
            match t with
            (* less: in OCaml functions can be curried, so we need to match _params
             * and _args to calculate the resulting type. *)
            | Function (_params, ret) -> ret
            | _else_ -> Type.NoType
          in
          (t, id)
      | G.Conditional (_, e1, e2) ->
          let t1, id1opt = type_of_expr lang e1 in
          let t2, id2opt = type_of_expr lang e2 in
          (* LATER: we could also not enforce to have a type for both branches,
           * but let's go simple for now and enforce both branches have
           * a type and that the types are equal.
           *)
          let t =
            (* LATER: in theory we should look if the types are compatible,
             * and take the lowest upper bound of the two types *)
            let eq = Type.equal G.equal_name in
            if eq t1 t2 then t1 else Type.NoType
          in
          let idopt =
            (* TODO? is there an Option.xxx or Common.xxx function for that? *)
            match (id1opt, id2opt) with
            | Some id1, _ -> Some id1
            | _, Some id2 -> Some id2
            | None, None -> None
          in
          (t, idopt)
      | _else_ -> (Type.NoType, None))

and type_of_lit lang = function
  (* NB: We could infer Type.Number for JS int/float literals, but we can
     * handle that relationship in matching and we can be more precise for
     * now. One actual rule uses `float` for a typed metavariable in JS so
     * let's avoid breaking that for now at least. *)
  | G.Int _ -> Type.Builtin Type.Int
  | G.Float _ -> Type.Builtin Type.Float
  | G.Bool _ -> Type.Builtin Type.Bool
  | G.String (_, (_, t), _) when lang =*= Lang.Cpp ->
      type_of_ast_generic_type lang
        (G.TyPointer
           ( t,
             {
               t = G.TyN (H.name_of_id ("char", t));
               t_attrs = [ KeywordAttr (Const, t) ];
             } )
        |> G.t)
  | G.String _ -> Type.Builtin Type.String
  | _else_ -> Type.NoType

and type_of_name lang = function
  | Id (ident, id_info) ->
      let t = resolved_type_of_id_info lang id_info in
      let t =
        match t with
        (* Even if we can't resolve the type, the name of the ident can still be
         * useful for matching. If we had a Typeof variant of Type.t, it might
         * be more accurate to say this is `Type.Typeof (Type.UnresolvedName
         * ...)`. See also how we conflate `Class<T>` with `T` itself, evident
         * in the way we infer the type for a `new` expression. *)
        | Type.NoType -> Type.UnresolvedName (fst ident, [])
        | _else_ -> t
      in
      (t, Some ident)
  | IdQualified { name_last = ident, None; name_info; _ } ->
      let t = resolved_type_of_id_info lang name_info in
      (* TODO Use UnresolvedName like above when we can't resolve the name? What
       * part of the qualified name should be used? The whole thing? How should
       * it be converted to a string representation? *)
      (t, Some ident)
  | IdQualified { name_last = _, Some _; _ } ->
      (* TODO What to do with type arguments? *)
      (Type.NoType, None)

and resolved_type_of_svalue lang = function
  | Some (G.Lit lit) -> type_of_lit lang lit
  | Some (Cst Cbool) -> Type.Builtin Type.Bool
  | Some (Cst Cint) -> Type.Builtin Type.Int
  | Some (Cst Cstr) -> Type.Builtin Type.String
  | _else_ -> Type.NoType

and resolved_type_of_id_info lang info : G.name Type.t =
  (* First look at the svalue to see if we can get the type of the actual value
   * that this was initialized to, rather than the declared type. This can fail
   * for a variety of reasons, so if we don't get a type from this, fall back to
   * the resolved type. *)
  let svalue_type = resolved_type_of_svalue lang !(info.G.id_svalue) in
  if Type.is_real_type svalue_type then svalue_type
  else
    match !(info.G.id_type) with
    | Some t -> type_of_ast_generic_type lang t
    | None -> Type.NoType

and type_of_ast_generic_type lang t : G.name Type.t =
  match t.G.t with
  (* TODO Check language? Someone could make a user type named `nil` in Java,
   * for example. *)
  | G.TyN (Id ((("null" | "nil" | "NULL"), _), _)) -> Type.Null
  | G.TyN (Id ((str, _), _) as name) -> (
      match Type.builtin_type_of_string lang str with
      | Some t -> Type.Builtin t
      | None -> Type.N ((name, []), []))
  (* Pick up IdQualified as well *)
  | G.TyN name -> Type.N ((name, []), [])
  | G.TyApply ({ G.t = G.TyN name; _ }, (_l, args, _r)) ->
      let args =
        args
        |> List_.map (function
             | G.TA t -> Type.TA (type_of_ast_generic_type lang t)
             | G.TAWildcard (_, None) -> Type.TAWildcard None
             | G.TAWildcard (_, Some ((kind, _), t)) ->
                 let t = type_of_ast_generic_type lang t in
                 let kind =
                   match kind with
                   | false -> Type.TAUpper t
                   | true -> Type.TALower t
                 in
                 Type.TAWildcard (Some kind)
             | _else_ -> Type.OtherTypeArg None)
      in
      Type.N ((name, args), [])
  | G.TyApply _ ->
      (* Should always be a TyN according to the comments in AST_generic.ml *)
      Type.NoType
  | G.TyArray ((_l, size_expr, _r), elem_type) ->
      let size =
        match size_expr with
        | Some { G.e = G.L (G.Int pi); _ } -> Some pi
        | _else_ -> None
      in
      let elem_type = type_of_ast_generic_type lang elem_type in
      Type.Array (size, elem_type)
  | G.TyFun (params, tret) ->
      let params =
        params
        |> List_.map (function
             | G.Param { G.pname; ptype; _ } ->
                 let pident = Option.map fst pname in
                 let ptype =
                   Option.map (type_of_ast_generic_type lang) ptype
                   |> Type.of_opt
                 in
                 Type.Param { Type.pident; ptype }
             | _else_ -> OtherParam None)
      in
      let tret = type_of_ast_generic_type lang tret in
      Type.Function (params, tret)
  | G.TyPointer (_, t) ->
      let t = type_of_ast_generic_type lang t in
      Type.Pointer t
  | G.TyExpr e when Lang.is_js lang ->
      (* TyExpr is a bit of a suspicious construct with a few uses. But in JS/TS
       * it's most often used for `new` where you could write `new (foo())()` to
       * instantiate the class returned by the function `foo()` (or any other
       * arbitrary expression). So, we find the type of that expression and pass
       * it up. *)
      let t, _id = type_of_expr lang e in
      t
  | G.TyExpr
      {
        e =
          ArrayAccess
            ( { e = N (Id (("Union", _), _) as union); _ },
              ( _,
                {
                  e =
                    Container
                      ( Tuple,
                        ( _,
                          [ { e = N (Id _ as name); _ }; { e = L (Null _); _ } ],
                          _ ) );
                  _;
                },
                _ ) );
        _;
      }
  (* Python: Union[X, None] *)
    when lang =*= Lang.Python ->
      Type.N ((union, [ TA (N ((name, []), [])); TA Null ]), [])
  (* TODO: Need to expand Type.ml if we want to represent more *)
  | _else_ -> Type.NoType

(*****************************************************************************)
(* Typing visitor / check a program *)
(*****************************************************************************)

(* Currently, for types created during naming in Semgrep (OSS and Pro Engine),
 * we can't tell the difference between a resolved name and an unresolved name,
 * so we turn them all into `Type.N`s. Some of the names will be fully-qualified
 * resolved names, and some will just be names as written by the user.
 *
 * We should use Type.t for `id_type` to address this ambiguity. In the
 * meantime, we will use this helper function to abstract it away for type
 * guessing purposes. *)
let name_and_targs_of_named_type lang = function
  | Type.N ((G.Id ((str, _), _), targs), _)
  | Type.UnresolvedName (str, targs) ->
      Some (str, targs)
  | Type.N
      ( ( G.IdQualified { G.name_last; name_middle = Some (QDots middle); _ },
          targs ),
        _ ) ->
      let (str_last, _), _ = name_last in
      let middle_strs =
        middle |> List_.map (fun ((str, _info), _targs) -> str)
      in
      let str = String.concat "." (middle_strs @ [ str_last ]) in
      Some (str, targs)
  | Type.Builtin b -> Some (Type.name_of_builtin_type lang b, [])
  | _else_ -> None

(* This function is for guessing the type of an expression, when we can't figure
 * it out using ordinary type inference. This typically happens when some name
 * cannot be resolved because it either came from an external file (in OSS
 * Semgrep) or from the standard library or a third party library (in Pro
 * Engine).
 *
 * For example, in Java we guess that `x.equals(y)` returns a `boolean`, even if
 * we don't know the type of `x`. *)
let guess_type_of_dotaccess lang ty_name_and_targs str =
  (* TODO: The types of the parameters should just be computed from the actuals. *)
  let todo_param =
    (* Param type could be Top if we add that as a type *)
    Type.Param { pident = None; ptype = Type.NoType }
  in
  match (lang, ty_name_and_targs, str) with
  | Lang.Java, _, "isEmpty" -> Type.Function ([], Type.Builtin Type.Bool)
  | Lang.Java, _, ("equals" | "contains" | "containsKey" | "containsValue") ->
      (* Really the return type is all that matters. We could add the parameters
       * later if we need to. *)
      Type.Function ([ todo_param ], Type.Builtin Type.Bool)
  | Lang.Java, _, ("size" | "length") ->
      Type.Function ([], Type.Builtin Type.Int)
  | Lang.Java, _, ("compareTo" | "compareToIgnoreCase") ->
      Type.Function ([], Type.Builtin Type.Int)
  (* For unresolved types with one type parameter, assume that the `get`
   * method's return type is the type parameter (e.g. List<T>). For unresolved
   * types with two type parameters, assume that the `get` method's return type
   * is the second (e.g. Map<K, V>) *)
  | ( Lang.Java,
      Some (_str, ([ _; Type.TA elt_type ] | [ Type.TA elt_type ])),
      "get" ) ->
      Type.Function ([ todo_param ], elt_type)
  | Lang.Java, Some (("String" | "java.lang.String"), _), "matches" ->
      let param =
        Type.Param { pident = None; ptype = Type.Builtin Type.String }
      in
      Type.Function ([ param ], Type.Builtin Type.Bool)
  | ( Lang.Java,
      Some (("Boolean" | "java.lang.Boolean"), _),
      ("valueOf" | "parseBoolean") ) ->
      Type.Function ([ todo_param ], Type.Builtin Type.Bool)
  | ( Lang.Java,
      Some (("Integer" | "java.lang.Integer"), _),
      ("decode" | "valueOf" | "parseInt" | "parseUnsignedInt") ) ->
      Type.Function ([ todo_param ], Type.Builtin Type.Int)
  | ( Lang.Java,
      Some (("Long" | "java.lang.Long"), _),
      ("decode" | "valueOf" | "parseLong" | "parseUnsignedLong") ) ->
      Type.Function ([ todo_param ], Type.Builtin Type.Int)
  | ( Lang.Java,
      Some (("Float" | "java.lang.Float"), _),
      ("valueOf" | "parseFloat") ) ->
      Type.Function ([ todo_param ], Type.Builtin Type.Float)
  | _else_ -> Type.NoType

(* TODO: We could probably add a `Type.t ref` to `Call` nodes without major perf
 * problems, and that together with `id_type`s should allow pre-computing types here. *)
let typing_visitor =
  (* All untyped function ids will share the same type. *)
  let todo_kind = ("TODO", G.fake "TODO") in
  let todo_param : G.parameter = OtherParam (todo_kind, []) in
  let todo_type : G.type_ = { t = OtherType (todo_kind, []); t_attrs = [] } in
  let todo_fun_type : G.type_ =
    { t = TyFun ([ todo_param ], todo_type); t_attrs = [] }
  in
  let some_todo_fun_type = Some todo_fun_type in
  (* Visitor. *)
  object (_self : 'self)
    inherit [_] AST_generic.iter_no_id_info as super

    method! visit_expr_kind lang e =
      (match e with
      | Call
          ( {
              e =
                DotAccess
                  ( obj,
                    _,
                    FN
                      (Id
                        ( (id_str, _),
                          ({ id_type = { contents = None }; _ } as id_info) ))
                  );
              _;
            },
            _ ) -> (
          let obj_ty, _ = type_of_expr lang obj in
          let guessed_type =
            let ty_name_and_targs = name_and_targs_of_named_type lang obj_ty in
            guess_type_of_dotaccess lang ty_name_and_targs id_str
            |> Type.to_ast_generic_type_ lang (fun name _alts -> name)
          in
          match guessed_type with
          | Some _ -> id_info.id_type := guessed_type
          | None ->
              (* Method calls that have no type assigned get a "TODO" function type,
               * this is useful for taint analysis to avoid tracking fields that
               * correspond to methods. *)
              id_info.id_type := some_todo_fun_type)
      | __else__ -> ());
      super#visit_expr_kind lang e
  end

(* This is called right after parsing a target file to fill in the 'id_type's.
 * Right now this pass focuses on giving types to names used as method calls,
 * for what it relies on 'guess_type_of_dotaccess'. *)
let check_program lang prog = typing_visitor#visit_program lang prog
[@@trace_debug]
