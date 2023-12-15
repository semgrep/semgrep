(* Yoann Padioleau
 * Sophia Roshal
 *
 * Copyright (C) 2022-2023 Semgrep Inc.
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
module A = AST_jsonnet
module J = JSON
module V = Value_jsonnet
open Eval_jsonnet_common
module H = Eval_jsonnet_common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Core_jsonnet to Value_jsonnet Jsonnet evaluator using
 * an environment-style evaluation and closures instead
 * of lambda substitutions like in the spec.
 *
 * See https://jsonnet.org/ref/spec.html#semantics
 *
 * See Eval_jsonnet_subst for the substitution-based evaluator
 * which is more correct (but far slower).
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Start of big mutually recursive functions *)
let rec lookup (env : V.env) tk local_id =
  let id = string_of_local_id local_id in
  let entry =
    try Map_.find local_id env.locals with
    | Not_found ->
        Logs.debug (fun m ->
            m "lookup fail for %s in env = %s" id (show_env env));
        error tk (spf "could not find '%s' in the environment" id)
  in
  Logs.debug (fun m -> m "found '%s', value = %s" id (show_lazy_value entry));
  to_value entry

and to_value (v : V.lazy_value) : V.t =
  match v with
  | Closure (env, e) -> eval_expr env e
  (* We use Val for self/super to actually store the ref to the V.Object *)
  | Val v -> v
  | Lv _
  | Unevaluated _ ->
      raise Impossible

and to_lazy_value env x : V.lazy_value = V.Closure (env, x)

(*****************************************************************************)
(* eval_expr *)
(*****************************************************************************)

and eval_expr (env : V.env) (e : expr) : V.t =
  match e with
  | L lit -> H.eval_literal env lit
  (* lazy evaluation of Array elements and Lambdas *)
  | Array (l, xs, r) ->
      let elts =
        xs |> List_.map (fun x -> to_lazy_value env x) |> Array.of_list
      in
      Array (l, elts, r)
  | Lambda f -> Lambda f
  | O v -> eval_obj_inside env v
  | Id (s, tk) -> lookup env tk (V.LId s)
  | IdSpecial (Self, tk) -> lookup env tk V.LSelf
  (* TODO: check if super is in the environment and if not error with
   * RUNTIME ERROR: attempt to use super when there is no super class.
   *)
  | IdSpecial (Super, tk) -> lookup env tk V.LSuper
  | Call
      ( (ArrayAccess
           (Id ("std", _), (_, L (Str (None, DoubleQuote, (_, [ meth ], _))), _))
         as e0),
        (l, args, r) ) ->
      H.eval_std_method env e0 meth (l, args, r)
  | Local (_tlocal, binds, _tsemi, e) ->
      let locals =
        binds
        |> List.fold_left
             (fun acc (B (id, _teq, e_i)) ->
               let binding = to_lazy_value env e_i in
               Map_.add (V.LId (fst id)) binding acc)
             env.locals
      in
      eval_expr { env with locals } e
  | ArrayAccess (v1, v2) -> (
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
                (* TODO: Is this the right environment to evaluate in? *)
                to_value ei
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
              (spf "string bounds error: %d not within [0, %d)" i
                 (String.length s))
      (* Field access! A tricky operation. *)
      | ( (V.Object (_l, (_assertsTODO, fields), _r) as obj),
          Primitive (Str (fld, tk)) ) -> (
          match
            fields
            |> List.find_opt (fun (field : V.value_field) ->
                   fst field.fld_name = fld)
          with
          | None -> error tk (spf "field '%s' not present in %s" fld (sv e))
          | Some fld -> (
              match fld.fld_value with
              | V.Lv _
              | V.Unevaluated _ ->
                  raise Impossible
              | V.Val v -> v
              | V.Closure (_env_closure_TODO_maybe, e) ->
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
                  let locals =
                    if !Conf_ojsonnet.implement_self then
                      env.locals |> Map_.add V.LSelf (V.Val obj)
                    else env.locals
                  in
                  eval_expr { env with locals } e))
      | _else_ -> error l (spf "Invalid ArrayAccess: %s[%s]" (sv e) (sv index)))
  | Call (e0, args) -> H.eval_call env e0 args
  | UnaryOp ((op, tk), e) -> (
      match op with
      | UBang -> (
          match eval_expr env e with
          | Primitive (Bool (b, tk)) -> Primitive (Bool (not b, tk))
          | v -> error tk (spf "Not a boolean for unary !: %s" (sv v)))
      | UPlus -> (
          match eval_expr env e with
          | Primitive (Double (f, tk)) -> Primitive (Double (f, tk))
          | v -> error tk (spf "Not a number for unary +: %s" (sv v)))
      | UMinus -> (
          match eval_expr env e with
          | Primitive (Double (f, tk)) -> Primitive (Double (-.f, tk))
          | v -> error tk (spf "Not a number for unary -: %s" (sv v)))
      | UTilde -> (
          match eval_expr env e with
          | Primitive (Double (f, tk)) ->
              let f = f |> Int64.of_float |> Int64.lognot |> Int64.to_float in
              Primitive (Double (f, tk))
          | v -> error tk (spf "Not a number for unary -: %s" (sv v))))
  | BinaryOp (el, (op, tk), er) -> eval_binary_op env el (op, tk) er
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

(* In theory, we should just recursively evaluate f(ei), but
 * ei is actually not an expression but a lazy_value coming from
 * a array, so we can't just call eval_call(). The code below is
 * a specialization of eval_call and eval_expr for Local.
 *)
and eval_std_filter_element (env : V.env) (tk : tok) (f : function_definition)
    (ei : V.lazy_value) : V.t * V.env =
  match f with
  | { f_params = _l, [ P (id, _eq, _default) ], _r; f_body; _ } ->
      (* similar to eval_expr for Local *)
      let locals = Map_.add (V.LId (fst id)) ei env.locals in
      (* similar to eval_call *)
      (*TODO: Is the environment correct? *)
      (eval_expr { env with depth = env.depth + 1; locals } f_body, env)
  | _else_ -> error tk "filter function takes 1 parameter"

and eval_plus_object _env _tk objl objr : V.object_ A.bracket =
  let l, (lassert, lflds), _r = objl in
  let _, (rassert, rflds), r = objr in
  let hobjr =
    rflds
    |> List_.map (fun { V.fld_name = s, _; _ } -> s)
    |> Hashtbl_.hashset_of_list
  in
  (* TODO: this currently just merges the f *)
  let asserts = lassert @ rassert in
  let lflds' =
    lflds
    |> List.filter (fun { V.fld_name = s, _; _ } -> not (Hashtbl.mem hobjr s))
  in
  (* Add Super to the environment of the right fields *)
  let rflds' =
    rflds
    |> List_.map (fun ({ V.fld_value; _ } as fld) ->
           (* TODO: here we bind super to objl, and this works for simple
            * examples (e.g., basic_super1.jsonnet) but failed for
            * more complex examples where the accessed field uses self, as in
            *   { x: 1, w: 1, y: self.x } +
            *   { x: 2, w: 2, y: super.y, z : super.w }
            * which should return { x: 2, w: 2, y : 2, z : 1 }
            * but currently return { x : 2, w : 2, y : 1, z : 1 }
            * because super is bounded just to the left object
            * ({ x: 1, w: 1, y: self.x), and in that context
            * self.x is evaluated to 1 not 2
            * (see also eval_fail/basic_super2.jsonnet)
            *)
           match fld_value with
           | Lv _
           | Unevaluated _
           | Val _ ->
               raise Impossible
           | Closure (env, e) ->
               let locals =
                 env.locals |> Map_.add V.LSuper (V.Val (V.Object objl))
               in
               { fld with fld_value = Closure ({ env with locals }, e) })
  in
  let flds' = lflds' @ rflds' in
  (l, (asserts, flds'), r)

and eval_binary_op env el (op, tk) er =
  match op with
  | Plus -> (
      match (eval_expr env el, eval_expr env er) with
      | Array (l1, arr1, _r1), Array (_l2, arr2, r2) ->
          Array (l1, Array.append arr1 arr2, r2)
      | Primitive (Double (f1, tk)), Primitive (Double (f2, _)) ->
          Primitive (Double (f1 +. f2, tk))
      | Primitive (Str (s1, tk1)), Primitive (Str (s2, _tk2)) ->
          Primitive (Str (s1 ^ s2, tk1))
      | Primitive (Str (s, tk)), v -> Primitive (Str (s ^ tostring v, tk))
      | v, Primitive (Str (s, tk)) -> Primitive (Str (tostring v ^ s, tk))
      | V.Object objl, V.Object objr ->
          let obj = eval_plus_object env tk objl objr in
          V.Object obj
      | v1, v2 ->
          error tk (spf "TODO: Plus (%s, %s) not yet handled" (sv v1) (sv v2)))
  | And -> (
      match eval_expr env el with
      | Primitive (Bool (b, _)) as v -> if b then eval_expr env er else v
      | v -> error tk (spf "Not a boolean for &&: %s" (sv v)))
  | Or -> (
      match eval_expr env el with
      | Primitive (Bool (b, _)) as v -> if b then v else eval_expr env er
      | v -> error tk (spf "Not a boolean for ||: %s" (sv v)))
  | Lt
  | LtE
  | Gt
  | GtE ->
      let cmp = eval_std_cmp env tk el er in
      let bool =
        match (op, cmp) with
        | Lt, Inf -> true
        | Lt, (Eq | Sup) -> false
        | LtE, (Inf | Eq) -> true
        | LtE, Sup -> true
        | Gt, (Inf | Eq) -> false
        | Gt, Sup -> true
        | GtE, Inf -> false
        | GtE, (Eq | Sup) -> true
        | ( ( Plus | Minus | Mult | Div | LSL | LSR | And | Or | BitAnd | BitOr
            | BitXor ),
            _ ) ->
            assert false
      in
      Primitive (Bool (bool, tk))
  | Minus
  | Mult
  | Div -> (
      match (eval_expr env el, eval_expr env er) with
      | Primitive (Double (f1, itk)), Primitive (Double (f2, _)) ->
          let op =
            match op with
            | Minus -> ( -. )
            | Mult -> ( *. )
            | Div -> ( /. )
            | _else_ -> assert false
          in
          Primitive (Double (op f1 f2, itk))
      | v1, v2 ->
          error tk
            (spf "binary operator wrong operands: %s %s %s" (sv v1)
               (Tok.content_of_tok tk) (sv v2)))
  | LSL
  | LSR
  | BitAnd
  | BitOr
  | BitXor -> (
      let v1 = eval_expr env el in
      let v2 = eval_expr env er in
      match (v1, v2) with
      | Primitive (Double (f1, tk1)), Primitive (Double (f2, tk2)) ->
          let i1 = Int64.of_float f1 in
          let i2 = Int64.of_float f2 in
          let i64 =
            match op with
            | LSL ->
                let i2 = Int64.to_int i2 in
                if i2 < 0 then
                  error tk2 (spf "negative number for LSL: %s" (sv v2))
                else Int64.shift_left i1 i2
            | LSR ->
                let i2 = Int64.to_int i2 in
                if i2 < 0 then
                  error tk2 (spf "negative number for LSR: %s" (sv v2))
                else Int64.shift_right i1 i2
            | BitAnd -> Int64.logand i1 i2
            | BitOr -> Int64.logor i1 i2
            | BitXor -> Int64.logxor i1 i2
            | _else_ -> assert false
          in
          Primitive (Double (Int64.to_float i64, tk1))
      | v1, v2 ->
          error tk
            (spf "binary operator wrong operands: %s %s %s" (sv v1)
               (Tok.content_of_tok tk) (sv v2)))

(*****************************************************************************)
(* std.cmp *)
(*****************************************************************************)
(* Seems like std.cmp() is not defined in std.jsonnet nor mentionned in
 * the Jsonnet Standard library spec, so I guess it's a hidden builtin
 * so we dont need to produce a value_ that other code can use; we can
 * return a cmp.
 *)
and eval_std_cmp env tk (el : expr) (er : expr) : cmp =
  let rec eval_std_cmp_value_ (v_el : V.t) (v_er : V.t) : cmp =
    match (v_el, v_er) with
    | V.Array (_, [||], _), V.Array (_, [||], _) -> Eq
    | V.Array (_, [||], _), V.Array (_, _, _) -> Inf
    | V.Array (_, _, _), V.Array (_, [||], _) -> Sup
    | V.Array (al, ax, ar), V.Array (bl, bx, br) -> (
        let a0 = to_value ax.(0) in

        let b0 = to_value bx.(0) in

        match eval_std_cmp_value_ a0 b0 with
        | (Inf | Sup) as r -> r
        | Eq ->
            let a_sub =
              V.Array (al, Array.sub ax 1 (Array.length ax - 1), ar)
            in
            let b_sub =
              V.Array (bl, Array.sub bx 1 (Array.length bx - 1), br)
            in
            eval_std_cmp_value_ a_sub b_sub)
    | Primitive (Double (fl, _)), Primitive (Double (fr, _)) ->
        Float.compare fl fr |> int_to_cmp
    | Primitive (Str (strl, _)), Primitive (Str (strr, _)) ->
        (* TODO? or use unicode? *)
        String.compare strl strr |> int_to_cmp
    (* note that it does not make sense to compare (<, <=, >=, >) 2 booleans
     * or 2 nulls. They are not ordonnable
     *)
    | _else_ ->
        error tk (spf "comparing uncomparable: %s vs %s" (sv v_el) (sv v_er))
  in
  eval_std_cmp_value_ (eval_expr env el) (eval_expr env er)

(*****************************************************************************)
(* eval_obj_inside *)
(*****************************************************************************)

and eval_obj_inside env (l, x, r) : V.t =
  match x with
  | Object (assertsTODO, fields) ->
      let hdupes = Hashtbl.create 16 in
      let fields =
        fields
        |> List_.map_filter
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
      let asserts_with_env = List.map (fun x -> (x, env)) assertsTODO in
      V.Object (l, (asserts_with_env, fields), r)
  | ObjectComp _x -> error l "TODO: ObjectComp"
(*
      let v = eval_obj_comprehension env x in

and eval_obj_comprehension env v =
  (fun env (_fldname, _tk, v3, v4) ->
    let v3 = eval_expr env v3 in
    let v4 = eval_for_comp env v4 in
    ...)
    env v

and eval_for_comp env v =
  (fun env (_tk1, _id, _tk2, v4) ->
    let v4 = eval_expr env v4 in
    ...)
    env v
*)

(*****************************************************************************)
(* Manfestation *)
(*****************************************************************************)
and tostring (v : V.t) : string =
  let j = manifest_value v in
  JSON.string_of_json j

(* After we switched to explicitely representing the environment in
 * Value_jsonnet.ml, this function became mutually recursive with
 * eval_expr() and so need to be defined in the same file.
 *)
and manifest_value (v : V.t) : JSON.t =
  match v with
  | Primitive x -> (
      match x with
      | Null _t -> J.Null
      | Bool (b, _tk) -> J.Bool b
      | Double (f, _tk) -> J.Float f
      | Str (s, _tk) -> J.String s)
  | Lambda { f_tok = tk; _ } -> error tk (spf "Lambda value: %s" (sv v))
  | Array (_, arr, _) ->
      J.Array
        (arr |> Array.to_list
        |> List_.map (fun (entry : V.lazy_value) ->
               manifest_value (to_value entry)))
  | V.Object (_l, (_assertsTODO, fields), _r) as obj ->
      (* TODO: evaluate asserts *)
      let xs =
        fields
        |> List_.map_filter (fun { V.fld_name; fld_hidden; fld_value } ->
               match fst fld_hidden with
               | A.Hidden -> None
               | A.Visible
               | A.ForcedVisible ->
                   let v =
                     match fld_value with
                     | Lv _
                     | Unevaluated _ ->
                         raise Impossible
                     | Val v -> v
                     | Closure (env, e) ->
                         (* similar to what we do in eval_expr on field access *)
                         let locals =
                           if !Conf_ojsonnet.implement_self then
                             env.locals |> Map_.add V.LSelf (V.Val obj)
                           else env.locals
                         in
                         eval_expr { env with locals } e
                   in
                   let j = manifest_value v in
                   Some (fst fld_name, j))
      in
      J.Object xs

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(*Same as eval_expr but with profiling *)
let eval_program_with_env (env : V.env) (e : Core_jsonnet.program) : V.t =
  eval_expr env e
[@@profiling]

let eval_program (e : Core_jsonnet.program) : V.t =
  eval_program_with_env
    {
      V.empty_env with
      eval_expr;
      eval_std_filter_element;
      to_lazy_value;
      eval_expr_for_call = eval_expr;
    }
    e
