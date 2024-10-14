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
(* Core_jsonnet to Value_jsonnet Jsonnet evaluator using an environment-style
 * evaluation and closures instead of lambda substitutions like in the spec.
 *
 * See https://jsonnet.org/ref/spec.html#semantics
 *
 * See Eval_jsonnet_subst for the substitution-based evaluator, which is more
 * correct (but far slower).
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Start of big mutually recursive functions *)
let rec lookup (env : V.env) tk (id : V.local_id) =
  let entry =
    try Map_.find id env.locals with
    | Not_found ->
        error tk
          (spf "could not find '%s' in the environment" (V.show_local_id id))
  in
  to_value entry

and to_value (v : V.lazy_value) : V.t =
  match v.lv with
  | Closure (env, e) ->
      let finalv = eval_expr env e in
      v.lv <- Val finalv;
      finalv
  (* We use Val for self/super to actually store the ref to the V.Object *)
  | Val v -> v
  | Unevaluated _ -> raise Impossible

and to_lazy_value env x : V.lazy_value = { lv = V.Closure (env, x) }

(*****************************************************************************)
(* eval_expr *)
(*****************************************************************************)

and eval_expr (env : V.env) (e : expr) : V.t =
  match e with
  | L lit -> H.eval_literal env lit
  (* lazy evaluation of Array elements *)
  | Array (l, xs, r) ->
      let elts =
        xs |> List_.map (fun x -> to_lazy_value env x) |> Array.of_list
      in
      V.Array (l, elts, r)
  | Lambda f -> V.Lambda (f, env.locals)
  | O v -> eval_obj_inside env v
  | Id (s, tk) -> lookup env tk (V.LId s)
  | IdSpecial (Self, tk) -> lookup env tk V.LSelf
  (* TODO: check if super is in the environment and if not error with
   * RUNTIME ERROR: attempt to use super when there is no super class.
   *)
  | IdSpecial (Super, tk) -> lookup env tk V.LSuper
  | Local (_tlocal, binds, _tsemi, e) ->
      let locals =
        binds
        |> List.fold_left
             (fun acc (B (id, _teq, e_i)) ->
               Map_.add (V.LId (fst id)) (to_lazy_value env e_i) acc)
             env.locals
      in
      eval_expr { env with locals } e
  (* this covers also obj access *)
  | ArrayAccess (v1, v2) -> eval_array_access env v1 v2
  | Call
      ( (ArrayAccess
           (Id ("std", _), (_, L (Str (None, DoubleQuote, (_, [ meth ], _))), _))
         as e0),
        (l, args, r) ) ->
      H.eval_std_method env e0 meth (l, args, r)
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

(* this also covers obj access as those are desugared in array access *)
and eval_array_access env v1 v2 =
  let e = eval_expr env v1 in
  let l, index, _r = (eval_bracket eval_expr) env v2 in
  match (e, index) with
  (* actual array access *)
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
  (* string access *)
  | Primitive (Str (s, tk)), Primitive (Double (f, tkf)) ->
      let i = int_of_float f in
      if i >= 0 && i < String.length s then
        (* TODO: which token to use for good tracing in case of error? *)
        let s' = String.make 1 (String.get s i) in
        Primitive (Str (s', tk))
      else
        error tkf
          (spf "string bounds error: %d not within [0, %d)" i (String.length s))
  (* obj field access! A tricky operation. *)
  | ( (V.Object (_l, (_assertsTODO, fields), _r) as obj),
      Primitive (Str (fld, tk)) ) -> (
      let (fld_opt : V.value_field option) =
        List.find_opt (fun fld2 -> fst fld2.V.fld_name = fld) fields
      in
      match fld_opt with
      | None -> error tk (spf "field '%s' not present in %s" fld (sv e))
      | Some fld -> (
          match fld.fld_value.lv with
          | V.Closure (env_closure, e) ->
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
                (* bugfix: here we want to use env_closure.locals not
                 * env.locals!!
                 *)
                if !Conf_ojsonnet.implement_self then
                  env_closure.locals |> Map_.add V.LSelf V.{ lv = Val obj }
                else env_closure.locals
              in
              let finalv = eval_expr { env with locals } e in
              fld.fld_value.lv <- Val finalv;
              finalv
          | V.Val v -> v
          | V.Unevaluated _ -> raise Impossible))
  | _else_ -> error l (spf "Invalid ArrayAccess: %s[%s]" (sv e) (sv index))

and eval_plus_object _env _tk objl objr : V.object_ A.bracket =
  let l, (lassert, lflds), _ = objl in
  let _, (rassert, rflds), r = objr in
  let hobjr =
    rflds
    |> List_.map (fun { V.fld_name = s, _; _ } -> s)
    |> Hashtbl_.hashset_of_list
  in
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
           match fld_value.lv with
           | Closure (env, e) ->
               let locals =
                 env.locals |> Map_.add V.LSuper V.{ lv = Val (Object objl) }
               in
               {
                 fld with
                 fld_value = V.{ lv = Closure ({ env with locals }, e) };
               }
           | Val _v -> fld
           | Unevaluated _ -> raise Impossible)
  in
  let flds' = lflds' @ rflds' in
  (l, (asserts, flds'), r)

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
      let asserts_with_env = List_.map (fun x -> (x, env)) assertsTODO in
      V.Object (l, (asserts_with_env, fields), r)
  (* big TODO *)
  | ObjectComp _x -> error l "TODO: ObjectComp"

(* This is called from Eval_jsonnet_common.eval_std_method for "std.filter".
 * In theory, we should just recursively evaluate f(ei), but
 * ei is actually not an expression but a lazy_value coming from
 * an array, so we can't just call eval_call(). The code below is
 * a specialization of eval_call and eval_expr for Local.
 * TODO? why we need a special implem? could be moved to Eval_jsonnet_common?
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
  | Lambda ({ f_tok = tk; _ }, _locals) ->
      error tk (spf "Lambda value: %s" (sv v))
  | Array (_, arr, _) ->
      J.Array
        (Array.to_list arr
        |> List_.map (fun (entry : V.lazy_value) ->
               manifest_value (to_value entry)))
  | V.Object (_l, (_assertsTODO, fields), _r) as obj ->
      (* TODO: evaluate asserts *)
      let xs =
        fields
        |> List_.filter_map (fun { V.fld_name; fld_hidden; fld_value } ->
               match fst fld_hidden with
               | A.Hidden -> None
               | A.Visible
               | A.ForcedVisible ->
                   let v =
                     match fld_value.lv with
                     | Closure (env, e) ->
                         (* similar to what we do in eval_expr on fld access *)
                         let locals =
                           if !Conf_ojsonnet.implement_self then
                             env.locals |> Map_.add V.LSelf V.{ lv = Val obj }
                           else env.locals
                         in
                         let finalv = eval_expr { env with locals } e in
                         fld_value.lv <- Val finalv;
                         finalv
                     | Val v -> v
                     | Unevaluated _ -> raise Impossible
                   in
                   let j = manifest_value v in
                   Some (fst fld_name, j))
      in
      J.Object xs

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Same as eval_expr but with profiling *)
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
      to_value = (fun _env x -> to_value x);
      eval_expr_for_call = eval_expr;
      eval_plus_object;
      tostring;
    }
    e
