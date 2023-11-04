(* Yoann Padioleau
 * Sophia Roshal
 *
 * Copyright (C) 2022 Semgrep Inc.
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Core_jsonnet to Value_jsonnet Jsonnet evaluator.
 *
 * See https://jsonnet.org/ref/spec.html#semantics
 *
 * This is using an environment-style evaluation and closures instead
 * of lambda substitutions like in the spec.
 * See Eval_jsonnet_subst for the substitution-based evaluator
 * which is more correct (but far slower).
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let eval_bracket ofa env (v1, v2, v3) =
  let v2 = ofa env v2 in
  (v1, v2, v3)

let string_of_local_id = function
  | V.LSelf -> "self"
  | V.LSuper -> "super"
  | V.LId s -> s

(* Start of big mutually recursive functions *)
let rec lookup (env : V.env) tk local_id =
  let entry =
    try Map_.find local_id env.locals with
    | Not_found ->
        error tk
          (spf "could not find '%s' in the environment"
             (string_of_local_id local_id))
  in
  evaluate_lazy_value_ entry

and evaluate_lazy_value_ (v : V.lazy_value) =
  match v.value with
  | Val v -> v
  | Unevaluated e -> eval_expr v.env e

(*****************************************************************************)
(* eval_expr *)
(*****************************************************************************)

and eval_expr (env : V.env) (e : expr) : V.t =
  match e with
  | L lit ->
      let prim =
        match lit with
        | A.Null tk -> V.Null tk
        | A.Bool (b, tk) -> V.Bool (b, tk)
        | A.Str x -> V.Str (A.string_of_string_ x)
        | A.Number (s, tk) ->
            (* TODO: double check things *)
            let f = float_of_string s in
            V.Double (f, tk)
      in
      V.Primitive prim
  (* lazy evaluation of Array elements and Lambdas *)
  | Array (l, xs, r) ->
      let elts =
        xs
        |> Common.map (fun x -> { V.value = Unevaluated x; env })
        |> Array.of_list
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
      eval_std_method env e0 meth (l, args, r)
  | Local (_tlocal, binds, _tsemi, e) ->
      let locals =
        binds
        |> List.fold_left
             (fun acc (B (id, _teq, e_i)) ->
               let binding = { V.value = Unevaluated e_i; env } in
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
                evaluate_lazy_value_ ei
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
              match fld.fld_value.value with
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
                  let locals =
                    env.locals |> Map_.add V.LSelf { V.value = V.Val obj; env }
                  in
                  eval_expr { env with locals } e))
      | _else_ -> error l (spf "Invalid ArrayAccess: %s[%s]" (sv e) (sv index)))
  | Call (e0, args) -> eval_call env e0 args
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

and eval_std_method env e0 (method_str, tk) (l, args, r) =
  match (method_str, args) with
  | "type", [ Arg e ] ->
      log_call env ("std." ^ method_str) l;
      let v = eval_expr env e in
      let s = std_type v in
      Primitive (Str (s, l))
  (* this method is called in std.jsonnet equals()::, and calls to
   * this equals() are generated in Desugar_jsonnet when
   * desugaring the == operator.
   *)
  | "type", _else_ ->
      error tk
        (spf "Improper #arguments to std.type: expected 1, got %d"
           (List.length args))
  | "primitiveEquals", [ Arg e; Arg e' ] ->
      log_call env ("std." ^ method_str) l;
      let v = eval_expr env e in
      let v' = eval_expr env e' in
      let b = std_primivite_equals v v' in
      Primitive (Bool (b, l))
  | "primitiveEquals", _else_ ->
      error tk
        (spf "Improper #arguments to std.primitiveEquals: expected 2, got %d"
           (List.length args))
  | "length", [ Arg e ] -> (
      log_call env ("std." ^ method_str) l;
      match eval_expr env e with
      | Primitive (Str (s, tk)) ->
          let i = String.length s in
          Primitive (Double (float_of_int i, tk))
      | Array (_, arr, _) ->
          let i = Array.length arr in
          Primitive (Double (float_of_int i, tk))
      | V.Object (_, (_asserts, flds), _) ->
          let i = List.length flds in
          (* TODO: in the spec they use std.objectFieldsEx *)
          Primitive (Double (float_of_int i, tk))
      | v ->
          error l
            (spf "length operates on strings, objects, and arrays, got %s"
               (sv v)))
  | "makeArray", [ Arg e; Arg e' ] -> (
      log_call env ("std." ^ method_str) l;
      match (eval_expr env e, eval_expr env e') with
      | Primitive (Double (n, tk)), Lambda fdef ->
          if Float.is_integer n then
            let n = Float.to_int n in
            let e i =
              Call
                ( Lambda fdef,
                  (fk, [ Arg (L (Number (string_of_int i, fk))) ], fk) )
            in
            Array
              ( fk,
                Array.init n (fun i -> { V.value = Unevaluated (e i); env }),
                fk )
          else error tk (spf "Got non-integer %f in std.makeArray" n)
      | v, _e' ->
          error tk (spf "Improper arguments to std.makeArray: %s" (sv v)))
  | "makeArray", _else_ ->
      error tk
        (spf "Improper number of arguments to std.makeArray: expected 2, got %d"
           (List.length args))
  | "filter", [ Arg e; Arg e' ] -> (
      match (eval_expr env e, eval_expr env e') with
      | Lambda f, Array (l, eis, r) ->
          (* note that we do things lazily even here, so we still
           * return an Array with the same lazy value elements in it,
           * but just filtered
           *)
          let elts' =
            (* TODO? use Array.to_seqi instead? *)
            eis |> Array.to_list |> Common.index_list
            |> List.filter_map (fun (ei, ji) ->
                   match eval_std_filter_element env tk f ei with
                   | Primitive (Bool (false, _)), _ -> None
                   | Primitive (Bool (true, _)), _ -> Some ji
                   | v ->
                       error tk
                         (spf "filter function must return boolean, got: %s"
                            (sv (fst v))))
            |> Array.of_list
            |> Array.map (fun idx -> eis.(idx))
          in
          Array (l, elts', r)
      | v1, v2 ->
          error tk
            (spf
               "Builtin function filter expected (function, array) but got \
                (%s, %s)"
               (sv v1) (sv v2)))
  | "filter", _else_ ->
      error tk
        (spf "Improper number of arguments to std.filter: expected 2, got %d"
           (List.length args))
  | "objectHasEx", [ Arg e; Arg e'; Arg e'' ] -> (
      match (eval_expr env e, eval_expr env e', eval_expr env e'') with
      | V.Object o, Primitive (Str (s, _)), Primitive (Bool (b, _)) ->
          let _, (_asserts, flds), _ = o in
          let eltopt =
            flds |> List.find_opt (fun { V.fld_name; _ } -> fst fld_name = s)
          in
          let b =
            match eltopt with
            | None -> false
            | Some { fld_hidden = visibility, _; _ } ->
                visibility <> A.Hidden || b
          in
          Primitive (Bool (b, tk))
      | v1, v2, v3 ->
          error tk
            (spf
               "Builtin function objectHasEx expected (object, string, \
                boolean), got (%s, %s, %s)"
               (sv v1) (sv v2) (sv v3)))
  | "objectHasEx", _else_ ->
      error tk
        (spf
           "Improper number of arguments to std.objectHasEx: expected 3, got %d"
           (List.length args))
  (* default to regular call, handled by std.jsonnet code hopefully *)
  | _else_ -> eval_call env e0 (l, args, r)

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
      (eval_expr { (*env with*) depth = env.depth + 1; locals } f_body, env)
  | _else_ -> error tk "filter function takes 1 parameter"

and eval_call env e0 (largs, args, _rargs) =
  match eval_expr env e0 with
  | Lambda { f_tok = _; f_params = lparams, params, rparams; f_body = eb } ->
      let fstr =
        match e0 with
        | Id (s, _) -> s
        | ArrayAccess
            ( Id (obj, _),
              (_, L (Str (None, DoubleQuote, (_, [ (meth, _) ], _))), _) ) ->
            spf "%s.%s" obj meth
        | _else_ -> "<unknown>"
      in
      log_call env fstr largs;
      (* the named_args are supposed to be the last one *)
      let basic_args, named_args =
        args
        |> Common.partition_either (function
             | Arg ei -> Left ei
             | NamedArg (id, _tk, ei) -> Right (fst id, ei))
      in
      (* opti? use a hashtbl? but for < 5 elts, probably worse? *)
      let hnamed_args = Common.hash_of_list named_args in
      let basic_args = Array.of_list basic_args in
      let m = Array.length basic_args in
      let binds =
        params
        |> List.mapi (fun i (P (id, teq, ei')) ->
               let ei'' =
                 match i with
                 | _ when i < m -> basic_args.(i) (* ei *)
                 | _ when Hashtbl.mem hnamed_args (fst id) ->
                     Hashtbl.find hnamed_args (fst id)
                 | _else_ -> ei'
               in
               B (id, teq, ei''))
      in
      eval_expr
        { env with depth = env.depth + 1 }
        (Local (lparams, binds, rparams, eb))
  | v -> error largs (spf "not a function: %s" (sv v))

and eval_plus_object _env _tk objl objr : V.object_ A.bracket =
  let l, (lassert, lflds), _r = objl in
  let _, (rassert, rflds), r = objr in
  let hobjr =
    rflds
    |> Common.map (fun { V.fld_name = s, _; _ } -> s)
    |> Common.hashset_of_list
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
    |> Common.map (fun ({ V.fld_value = { value; env }; _ } as fld) ->
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
           let locals =
             env.locals
             |> Map_.add V.LSuper { V.value = V.Val (V.Object objl); env }
           in
           { fld with fld_value = { value; env = { env with locals } } })
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
        let a0 = evaluate_lazy_value_ ax.(0) in

        let b0 = evaluate_lazy_value_ bx.(0) in

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
        |> Common.map_filter
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
                       fld_value = { value = Unevaluated fld_value; env };
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
(* Entry points *)
(*****************************************************************************)
and tostring (v : V.t) : string =
  let j = manifest_value v in
  JSON.string_of_json j

(*Same as eval_expr but with profiling *)
and eval_program_with_env (env : V.env) (e : Core_jsonnet.program) : V.t =
  eval_expr env e
[@@profiling]

and eval_program (e : Core_jsonnet.program) : V.t =
  eval_program_with_env V.empty_env e

(*****************************************************************************)
(* Manfestation *)
(*****************************************************************************)
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
        |> Common.map (fun (entry : V.lazy_value) ->
               manifest_value (evaluate_lazy_value_ entry)))
  | V.Object (_l, (_assertsTODO, fields), _r) as obj ->
      (* TODO: evaluate asserts *)
      let xs =
        fields
        |> Common.map_filter (fun { V.fld_name; fld_hidden; fld_value } ->
               match fst fld_hidden with
               | A.Hidden -> None
               | A.Visible
               | A.ForcedVisible ->
                   (* similar to what we do in eval_expr on field access *)
                   let locals =
                     fld_value.env.locals
                     |> Map_.add V.LSelf
                          { V.value = Val obj; env = fld_value.env }
                   in
                   let v =
                     match fld_value.value with
                     | Val v -> v
                     | Unevaluated e ->
                         eval_expr { fld_value.env with locals } e
                   in
                   let j = manifest_value v in
                   Some (fst fld_name, j))
      in
      J.Object xs
