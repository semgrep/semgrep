(* Sophia Roshal
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
open Core_jsonnet
open Eval_jsonnet_common
module A = AST_jsonnet
module J = JSON
module V = Value_jsonnet
module H = Eval_jsonnet_common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Jsonnet evaluator following the substitution model from the spec.
 *
 * This is far slower than Eval_jsonnet.ml, but it is also more correct
 * especially for programs using self and super.
 *
 * TODO: lots of duplication with Eval_jsonnet. It would be great to
 * factorize (need to use a functor?)
 *)

(*****************************************************************************)
(* Standard library *)
(*****************************************************************************)

(* Creates std so that we can add it to the environment when we switch back to
 * environment model for handling standard library functions
 *)
let pre_std = lazy (Std_jsonnet.get_std_jsonnet ())

(* This is an arbitrary path, used as a placeholder, since we aren't
 * desugaring from a file *)
let path =
  match Fpath.of_string "../" with
  | Ok p -> p
  | Error _ -> failwith ""

let std = lazy (Desugar_jsonnet.desugar_program path Lazy.(force pre_std))

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let fake_self = IdSpecial (Self, Tok.unsafe_fake_tok "self")
let fake_super = IdSpecial (Super, Tok.unsafe_fake_tok "super")

(* TODO: unused? *)
let _is_imp_std s =
  s = "type" || s = "primitiveEquals" || s = "length" || s = "makeArray"
  || s = "filter" || s = "objectHasEx"

let freshvar =
  let store = ref 0 in
  fun () ->
    incr store;
    ("?tmp" ^ string_of_int !store, fk)

(* Converts field names that have been evaluated into expressions *)
let vfld_name_to_fld_name fld_name =
  let thing_to_make_string_ = Tok.unsafe_fake_bracket fld_name in
  let insideL : A.string_ = A.mk_string_ thing_to_make_string_ in
  FExpr (Tok.unsafe_fake_bracket (L (Str insideL)))

(* Converts objects that have been evaluated back to expressions so that
   we can call plus on them *)
let vobj_to_obj l asserts fields r =
  let new_fields =
    fields
    |> List_.map (fun { V.fld_name; fld_hidden; fld_value } ->
           match fld_value.lv with
           | Val _
           | Closure _ ->
               error (Tok.unsafe_fake_tok "") "shouldn't be a value"
           | Unevaluated e ->
               {
                 fld_name = vfld_name_to_fld_name fld_name;
                 fld_hidden;
                 fld_value = e;
               })
  in
  O (l, Object (asserts, new_fields), r)

let rec bind_list_contains binds id =
  match binds with
  | B ((name, _), _, _) :: t ->
      if id = name then true else bind_list_contains t id
  | [] -> false

let rec parameter_list_contains parameters id =
  match parameters with
  | P ((name, _), _, _) :: t ->
      if id = name then true else parameter_list_contains t id
  | [] -> false

let to_lazy_value _env e : V.lazy_value = { lv = Unevaluated e }

(*****************************************************************************)
(* Subst *)
(*****************************************************************************)

(* This implements substitution for variables *)
let rec substitute id sub expr =
  match expr with
  | L v -> L v
  | Array (l, xs, r) -> Array (l, List_.map (substitute id sub) xs, r)
  | Lambda { f_tok; f_params = lparams, params, rparams; f_body } ->
      if parameter_list_contains params id then
        Lambda { f_tok; f_params = (lparams, params, rparams); f_body }
      else
        let new_params =
          List_.map
            (fun (P (name, tk, e)) -> P (name, tk, substitute id sub e))
            params
        in
        let new_body = substitute id sub f_body in
        Lambda
          {
            f_tok;
            f_params = (lparams, new_params, rparams);
            f_body = new_body;
          }
  | O (l, v, r) -> (
      match v with
      | Object (asserts, fields) ->
          let new_asserts =
            List_.map (fun (tk, expr) -> (tk, substitute id sub expr)) asserts
          in
          let new_fields =
            List_.map
              (fun { fld_name; fld_hidden; fld_value } ->
                match fld_name with
                | FExpr (l, name, r) ->
                    {
                      fld_name = FExpr (l, substitute id sub name, r);
                      fld_hidden;
                      fld_value = substitute id sub fld_value;
                    })
              fields
          in
          O (l, Object (new_asserts, new_fields), r)
      | ObjectComp _ -> O (l, v, r) (* TODO *))
  | Id (s, tk) -> if s = id then sub else Id (s, tk)
  | IdSpecial id -> IdSpecial id
  | Call
      ( ArrayAccess
          ( Id ("std", std_tok),
            ( l1,
              L (Str (None, DoubleQuote, (l2, [ (meth_str, meth_tk) ], r2))),
              r1 ) ),
        (l, args, r) ) ->
      (* Because we use environment model substitution for std library,
         we don't do any substitutions for "std". *)
      let new_args =
        if id = "std" then args
        else
          List_.map
            (fun arg ->
              match arg with
              | Arg e -> Arg (substitute id sub e)
              | NamedArg (ident, tk, e) ->
                  NamedArg (ident, tk, substitute id sub e))
            args
      in
      Call
        ( ArrayAccess
            ( Id ("std", std_tok),
              ( l1,
                L (Str (None, DoubleQuote, (l2, [ (meth_str, meth_tk) ], r2))),
                r1 ) ),
          (l, new_args, r) )
  | Local (_tlocal, binds, _tsemi, e) ->
      if bind_list_contains binds id then Local (_tlocal, binds, _tsemi, e)
      else
        let new_binds =
          List_.map
            (fun (B (name, tk, expr)) -> B (name, tk, substitute id sub expr))
            binds
        in
        Local (_tlocal, new_binds, _tsemi, substitute id sub e)
  | ArrayAccess (v1, (l, v2, r)) ->
      ArrayAccess (substitute id sub v1, (l, substitute id sub v2, r))
  | Call (e0, (l, args, r)) ->
      let new_func = substitute id sub e0 in
      let new_args =
        List_.map
          (fun arg ->
            match arg with
            | Arg e -> Arg (substitute id sub e)
            | NamedArg (ident, tk, e) ->
                NamedArg (ident, tk, substitute id sub e))
          args
      in
      Call (new_func, (l, new_args, r))
  | UnaryOp ((op, tk), e) -> UnaryOp ((op, tk), substitute id sub e)
  | BinaryOp (el, (op, tk), er) ->
      BinaryOp (substitute id sub el, (op, tk), substitute id sub er)
  | If (tif, e1, e2, e3) ->
      If (tif, substitute id sub e1, substitute id sub e2, substitute id sub e3)
  | Error (tk, e) -> Error (tk, substitute id sub e)
  | ExprTodo ((_s, _tk), _ast_expr) -> ExprTodo ((_s, _tk), _ast_expr)

(* This implements substitution for keywords (self/super)*)
let rec substitute_kw kw sub expr =
  match expr with
  | L v -> L v
  | Array (l, xs, r) -> Array (l, List_.map (substitute_kw kw sub) xs, r)
  | Lambda { f_tok; f_params = lparams, params, rparams; f_body } ->
      let new_params =
        List_.map
          (fun (P (id, tok, e)) -> P (id, tok, substitute_kw kw sub e))
          params
      in
      let new_f_body = substitute_kw kw sub f_body in
      Lambda
        {
          f_tok;
          f_params = (lparams, new_params, rparams);
          f_body = new_f_body;
        }
  | O (l, v, r) -> (
      match v with
      | Object (asserts, fields) ->
          let new_fields =
            List_.map
              (fun { fld_name = FExpr (l, e, r); fld_hidden; fld_value } ->
                {
                  fld_name = FExpr (l, substitute_kw kw sub e, r);
                  fld_hidden;
                  fld_value;
                })
              fields
          in
          O (l, Object (asserts, new_fields), r)
      | ObjectComp _ -> O (l, v, r) (* TODO *))
  | Id (s, tk) -> Id (s, tk)
  | IdSpecial (Super, tk) -> (
      match kw with
      | IdSpecial (Super, _) -> sub
      | IdSpecial (Self, _) -> IdSpecial (Super, tk)
      | _ -> error tk "not a keyword")
  | IdSpecial (Self, tk) -> (
      match kw with
      | IdSpecial (Self, _) -> sub
      | IdSpecial (Super, _) -> IdSpecial (Self, tk)
      | _ -> error tk "not a keyword")
  | Call
      ( ArrayAccess
          ( Id ("std", std_tok),
            ( l1,
              L (Str (None, DoubleQuote, (l2, [ (meth_str, meth_tk) ], r2))),
              r1 ) ),
        (l, args, r) ) ->
      let new_args =
        List_.map
          (fun arg ->
            match arg with
            | Arg e -> Arg (substitute_kw kw sub e)
            | NamedArg (ident, tk, e) ->
                NamedArg (ident, tk, substitute_kw kw sub e))
          args
      in
      Call
        ( ArrayAccess
            ( Id ("std", std_tok),
              ( l1,
                L (Str (None, DoubleQuote, (l2, [ (meth_str, meth_tk) ], r2))),
                r1 ) ),
          (l, new_args, r) )
  | Local (_tlocal, binds, _tsemi, e) ->
      let new_binds =
        List_.map
          (fun (B (name, tk, expr)) -> B (name, tk, substitute_kw kw sub expr))
          binds
      in
      Local (_tlocal, new_binds, _tsemi, substitute_kw kw sub e)
  | ArrayAccess (v1, (l, v2, r)) ->
      ArrayAccess (substitute_kw kw sub v1, (l, substitute_kw kw sub v2, r))
  | Call (e0, (l, args, r)) ->
      let new_func = substitute_kw kw sub e0 in
      let new_args =
        List_.map
          (fun arg ->
            match arg with
            | Arg e -> Arg (substitute_kw kw sub e)
            | NamedArg (ident, tk, e) ->
                NamedArg (ident, tk, substitute_kw kw sub e))
          args
      in
      Call (new_func, (l, new_args, r))
  | UnaryOp ((op, tk), e) -> UnaryOp ((op, tk), substitute_kw kw sub e)
  | BinaryOp (el, (op, tk), er) ->
      BinaryOp (substitute_kw kw sub el, (op, tk), substitute_kw kw sub er)
  | If (tif, e1, e2, e3) ->
      If
        ( tif,
          substitute_kw kw sub e1,
          substitute_kw kw sub e2,
          substitute_kw kw sub e3 )
  | Error (tk, e) -> Error (tk, substitute_kw kw sub e)
  | ExprTodo ((_s, _tk), _ast_expr) -> ExprTodo ((_s, _tk), _ast_expr)

(*****************************************************************************)
(* eval_expr *)
(*****************************************************************************)

let rec to_value env (v : V.lazy_value) : V.t =
  match v.lv with
  | Val v -> v
  | Unevaluated e ->
      let finalv = eval_expr env e in
      v.lv <- Val finalv;
      finalv
  | Closure _ -> raise Impossible

(* Note that we pass an environment here, but we just use its depth field
 * for debugging purpose. We do not use its locals field; we use substitution
 * to handle locals, not the environment.
 *)
and eval_expr env expr =
  match expr with
  | L lit -> H.eval_literal env lit
  (* lazy evaluation of Array elements and Lambdas *)
  | Array (l, xs, r) ->
      let elts =
        xs |> List_.map (fun x -> to_lazy_value env x) |> Array.of_list
      in
      V.Array (l, elts, r)
  (* in subst model we don't rely on closure so locals here is empty *)
  | Lambda f -> V.Lambda (f, Map_.empty)
  | O v -> eval_obj_inside env v
  | Id (name, tk) -> error tk ("trying to evaluate just a variable: " ^ name)
  | IdSpecial (_, tk) -> error tk "evaluating just a keyword"
  | Call
      ( (ArrayAccess
           (Id ("std", _), (_, L (Str (None, DoubleQuote, (_, [ meth ], _))), _))
         as e0),
        (l, args, r) ) ->
      H.eval_std_method env e0 meth (l, args, r)
  | Local (_tlocal, binds, _tsemi, e) ->
      let new_e =
        List.fold_left
          (fun e_1 (B ((name, _), _, e')) ->
            substitute name (Local (_tlocal, binds, _tsemi, e')) e_1)
          e binds
      in
      eval_expr env new_e
  | ArrayAccess (v1, v2) -> eval_array_access env v1 v2
  | Call (e0, args) -> H.eval_call env e0 args
  | UnaryOp ((op, tk), e) -> H.eval_unary_op env (op, tk) e
  | BinaryOp (el, (op, tk), er) -> H.eval_binary_op env el (op, tk) er
  | If (tif, e1, e2, e3) -> (
      match eval_expr env e1 with
      | Primitive (Bool (b, _)) ->
          if b then eval_expr env e2 else eval_expr env e3
      | v -> error tif (spf "not a boolean for if: %s" (sv v)))
  | Error (tk, e) -> (
      match eval_expr env e with
      | Primitive (Str (s, tk)) -> error tk (spf "ERROR: %s" s)
      | v -> error tk (spf "ERROR: %s" (tostring v)))
  | ExprTodo ((s, tk), _ast_expr) -> error tk (spf "ERROR: ExprTODO: %s" s)

(* Check if this is a standard library call. If it is, call the environment
 * model evaluation to get more efficiency (for example, on
 * array_comprehensions2.jsonnet, this optimization creates an approx 100x
 * speedup (11 seconds down to 0.1)
 *)
and eval_expr_for_call env e0 =
  match e0 with
  | ArrayAccess
      (Id ("std", _), (_, L (Str (None, DoubleQuote, (_, [ _ ], _))), _)) ->
      (* set locals so that "std" shows up in the environment when evaluating *)
      let e0_and_std =
        Local (fk, [ B (("std", fk), fk, Lazy.force std) ], fk, e0)
      in
      (* !!! Switch to Eval_jsonnet !!! but just to access the code of the
       * function; the function itself is still executed below
       * using the subst model.
       *)
      Eval_jsonnet_envir.eval_program e0_and_std
  | _ -> eval_expr env e0

and eval_array_access env v1 v2 =
  let e = eval_expr env v1 in
  let l, index, _r = (eval_bracket eval_expr) env v2 in
  match (e, index) with
  | Array (_l, arr, _r), Primitive (Double (f, tkf)) ->
      if Float.is_integer f then
        let i = int_of_float f in
        match i with
        | _ when i < 0 ->
            error tkf (spf "negative value for array index: %s" (sv index))
        | _ when i >= 0 && i < Array.length arr ->
            let ei = arr.(i) in
            to_value env ei
        | _else_ ->
            error tkf (spf "Out of bound for array index: %s" (sv index))
      else error tkf (spf "Not an integer: %s" (sv index))
  | Primitive (Str (s, tk)), Primitive (Double (f, tkf)) ->
      let i = int_of_float f in
      if i >= 0 && i < String.length s then
        (* TODO: which token to use for good tracing in case of error? *)
        let s' = String.make 1 (String.get s i) in
        Primitive (Str (s', tk))
      else
        error tkf
          (spf "string bounds error: %d not within [0, %d)" i (String.length s))
  (* Field access! A tricky operation. *)
  | V.Object (_l, (_assertsTODO, fields), _r), Primitive (Str (fld, tk)) -> (
      match
        fields
        |> List.find_opt (fun (field : V.value_field) ->
               fst field.fld_name = fld)
      with
      | None -> error tk (spf "field '%s' not present in %s" fld (sv e))
      | Some fld -> (
          match fld.fld_value.lv with
          | V.Closure _ -> raise Impossible
          | V.Val v -> v
          | V.Unevaluated e ->
              (* Late-bound self.
               * We need to do the self assignment on field access rather
               * than on object creation, because when objects are merged,
               * we need self to reference the new merged object rather
               * than the original. Here's such an example:
               *
               *    ({ name : self.y } + {y : 42})["name"]
               *
               * If we were to do the assignment of self before doing the
               * field access, we would have the following (incorrect)
               * evaluation where o = { name : self.y }
               *       ({name : o.y} + {y : 42})["name"]
               *       o.y
               *       {name : self.y}.y
               *       Error no such field.
               * However, if we only assign self on access, we get the
               * following (correct) evaluation
               *      ({ name : self.y } + {y : 42})["name"]
               *      { name : self.y, y : 42 }["name"]
               *      {name: self.y, y : 42}[y]
               *      42
               *)
              let new_e =
                e |> substitute_kw fake_self v1
                |> substitute_kw fake_super
                     (O
                        ( Tok.unsafe_fake_tok "{",
                          Object ([], []),
                          Tok.unsafe_fake_tok "}" ))
              in
              eval_expr env new_e))
  | _else_ -> error l (spf "Invalid ArrayAccess: %s[%s]" (sv e) (sv index))

and eval_std_filter_element env (tk : tok) (f : function_definition)
    (ei : V.lazy_value) : V.t * V.env =
  match f with
  | { f_params = _l, [ P (_id, _eq, _default) ], _r; _ } ->
      ( (* similar to eval_expr for Local *)
        (* similar to eval_call *)
        (*TODO: Is the environment correct? *)
        (match ei.lv with
        | Val _
        | Closure _ ->
            error (Tok.unsafe_fake_tok "oof") "shouldn't have been evaluated"
        | Unevaluated e -> eval_call env (Lambda f) (_l, [ Arg e ], _r)),
        env )
  | _else_ -> error tk "filter function takes 1 parameter"

and eval_obj_inside env (l, x, r) : V.t =
  match x with
  | Object (assertsTODO, fields) ->
      let hdupes = Hashtbl.create 16 in
      let fields =
        fields
        |> List_.filter_map
             (fun { fld_name = FExpr (tk, ei, _); fld_hidden; fld_value } ->
               match eval_expr env ei with
               | Primitive (Null _) -> None
               | Primitive (Str ((str, _) as fld_name)) ->
                   if Hashtbl.mem hdupes str then
                     error tk (spf "duplicate field name: \"%s\"" str)
                   else Hashtbl.add hdupes str true;
                   Some
                     {
                       V.fld_name;
                       fld_hidden;
                       (* fields are evaluated lazily! Sometimes with an
                        * env adjusted (see eval_plus_object()).
                        * We do not bind Self here! This is done on field
                        * access instead (late bound).
                        *)
                       fld_value = to_lazy_value env fld_value;
                     }
               | v -> error tk (spf "field name was not a string: %s" (sv v)))
      in
      let new_assertsTODO =
        assertsTODO |> List_.map (fun ass -> (ass, empty_env))
      in
      V.Object (l, (new_assertsTODO, fields), r)
  | ObjectComp _x -> error l "TODO: ObjectComp"

and eval_plus_object env _tk objl objr =
  let l, (lassert, lflds), _r = objl in
  let _, (rassert, rflds), r = objr in
  let asserts = lassert @ rassert in
  let hash_of_right_field_names =
    rflds
    |> List_.map (fun { V.fld_name = s, _; _ } -> s)
    |> Hashtbl_.hashset_of_list
  in

  let lflds_no_overlap =
    lflds
    |> List.filter (fun { V.fld_name = s, _; _ } ->
           not (Hashtbl.mem hash_of_right_field_names s))
  in

  let lflds_overlap_hidden =
    lflds
    |> List_.map (fun { V.fld_name = s, _; fld_hidden; _ } -> (s, fld_hidden))
    |> List.to_seq |> Hashtbl.of_seq
  in

  let super = freshvar () in
  let self = freshvar () in

  let new_rh_asserts =
    lassert
    |> List_.map (fun ((tk, e), _) ->
           ( tk,
             e
             |> substitute_kw fake_super (Id super)
             |> substitute_kw fake_self (Id self) ))
  in
  let new_rh_fields =
    lflds
    |> List_.map (fun { V.fld_name; fld_hidden; fld_value } ->
           match fld_value.lv with
           | Val _
           | Closure _ ->
               error (Tok.unsafe_fake_tok "") "shouldn't have been evaluated"
           | Unevaluated e ->
               let new_field_name = vfld_name_to_fld_name fld_name in

               let new_fld_value =
                 e
                 |> substitute_kw fake_super (Id super)
                 |> substitute_kw fake_self (Id self)
               in
               {
                 fld_name = new_field_name;
                 fld_hidden;
                 fld_value = new_fld_value;
               })
  in

  let rh_obj =
    O
      ( Tok.unsafe_fake_tok "{",
        Object (new_rh_asserts, new_rh_fields),
        Tok.unsafe_fake_tok "}" )
  in
  let e_s = BinaryOp (fake_super, (Plus, Tok.unsafe_fake_tok "+"), rh_obj) in
  let new_binds =
    [
      B (super, Tok.unsafe_fake_tok "=", fake_super);
      B (self, Tok.unsafe_fake_tok "=", fake_self);
    ]
  in

  let new_ers =
    rflds
    |> List_.map (fun { V.fld_name; fld_hidden; fld_value } ->
           match fld_value.lv with
           | Val _
           | Closure _ ->
               error (Tok.unsafe_fake_tok "") "shouldn't have been evaluated"
           | Unevaluated e ->
               let new_fld_value =
                 Local
                   ( Tok.unsafe_fake_tok "local",
                     new_binds,
                     Tok.unsafe_sc,
                     substitute_kw fake_super e_s e )
               in
               let name, _ = fld_name in
               (* implements hidden inheritance as defined in spec *)
               let hidden, _ = fld_hidden in
               let new_hidden =
                 if Hashtbl.mem lflds_overlap_hidden name then
                   match hidden with
                   | Visible -> Hashtbl.find lflds_overlap_hidden name
                   | _ -> fld_hidden
                 else fld_hidden
               in

               {
                 V.fld_name;
                 fld_hidden = new_hidden;
                 fld_value = to_lazy_value env new_fld_value;
               })
  in

  let all_fields = new_ers @ lflds_no_overlap in
  (l, (asserts, all_fields), r)

(*****************************************************************************)
(* Manifest *)
(*****************************************************************************)

(* note that this is mutually recursive with eval_expr *)
and manifest_value (v : V.t) : JSON.t =
  let env = empty_env in
  match v with
  | Primitive x -> (
      match x with
      | Null _t -> J.Null
      | Bool (b, _tk) -> J.Bool b
      | Double (f, _tk) -> J.Float f
      | Str (s, _tk) -> J.String s)
  | Lambda ({ f_tok = tk; _ }, _locals) ->
      error tk (spf "Lambda value: %s" (sv v))
  | Array (_, arr, _) ->
      J.Array
        (arr |> Array.to_list
        |> List_.map (fun (entry : V.lazy_value) ->
               manifest_value (to_value env entry)))
  | V.Object (_l, (_assertsTODO, fields), _r) ->
      (* TODO: evaluate asserts *)
      let xs =
        fields
        |> List_.filter_map (fun { V.fld_name; fld_hidden; fld_value } ->
               match fst fld_hidden with
               | A.Hidden -> None
               | A.Visible
               | A.ForcedVisible ->
                   (* similar to what we do in eval_expr on field access *)
                   let _new_assertsTODO =
                     _assertsTODO
                     |> List_.map (fun ((tk, prog), _) -> (tk, prog))
                   in
                   let _new_self = vobj_to_obj _l _new_assertsTODO fields _r in
                   let v =
                     match fld_value.lv with
                     | Closure _ -> raise Impossible
                     | Val v -> v
                     | Unevaluated e ->
                         let new_e =
                           e
                           |> substitute_kw fake_self _new_self
                           |> substitute_kw fake_super
                                (O
                                   ( Tok.unsafe_fake_tok "{",
                                     Object ([], []),
                                     Tok.unsafe_fake_tok "}" ))
                         in
                         eval_expr env new_e
                   in

                   let j = manifest_value v in
                   Some (fst fld_name, j))
      in
      J.Object xs

and tostring (v : V.t) : string =
  let j = manifest_value v in
  JSON.string_of_json j

and empty_env =
  {
    V.empty_env with
    eval_expr;
    eval_std_filter_element;
    to_lazy_value;
    to_value;
    eval_expr_for_call;
    eval_plus_object;
    tostring;
  }

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let eval_program (x : Core_jsonnet.program) : V.t = eval_expr empty_env x
