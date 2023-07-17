(* Yoann Padioleau
 *
 * Copyright (C) 2022 r2c
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

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Core_jsonnet to Value_jsonnet Jsonnet evaluator.
 *
 * See https://jsonnet.org/ref/spec.html#semantics
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(*type env = {
    (* The spec uses a lambda-calculus inspired substitution model, but
     * it is probably simpler and more efficient to use a classic
     * environment where the locals are defined. Jsonnet uses lazy
     * evaluation so we model this by using Lazy below.
     *)
    locals : (local_id, (expr * env)) Map_.t;
    (* for call tracing *)
    depth : int;
  }

  and local_id = LSelf | LSuper | LId of string*)

exception Error of string * Tok.t

(* -1, 0, 1 *)
type cmp = Inf | Eq | Sup

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let error tk s =
  (* TODO? if Parse_info.is_fake tk ... *)
  raise (Error (s, tk))

let fk = Tok.unsafe_fake_tok ""

let sv e =
  let s = show_value_ e in
  if String.length s > 100 then Str.first_chars s 100 ^ "..." else s

let eval_bracket ofa env (v1, v2, v3) =
  let v2 = ofa env v2 in
  (v1, v2, v3)

let int_to_cmp = function
  | -1 -> Inf
  | 0 -> Eq
  | 1 -> Sup
  (* all the OCaml Xxx.compare should return only -1, 0, or 1 *)
  | _else_ -> assert false

let string_of_local_id = function
  | LSelf -> "self"
  | LSuper -> "super"
  | LId s -> s

let log_call env str tk =
  logger#trace "calling %s> %s at %s"
    (Common2.repeat "-" env.depth |> Common.join "")
    str (Tok.stringpos_of_tok tk)

(*****************************************************************************)
(* Builtins *)
(*****************************************************************************)
(* alt: could move to Value_jsonnet.ml *)
let std_type _env (v : value_) : string =
  match v with
  | Primitive (Null _) -> "null"
  | Primitive (Bool _) -> "boolean"
  | Primitive (Double _) -> "number"
  | Primitive (Str _) -> "string"
  | ObjectVal _ -> "object"
  | Array _ -> "array"
  | Function _ -> "function"

let std_primivite_equals _env (v : value_) (v' : value_) : bool =
  match (v, v') with
  | Primitive p, Primitive p' -> (
      match (p, p') with
      (* alt: use deriving and Primitive.eq *)
      | Null _, Null _ -> true
      | Bool (b, _), Bool (b', _) -> b =:= b'
      | Str (s, _), Str (s', _) -> s = s'
      | Double (f, _), Double (f', _) -> f =*= f'
      | Null _, _
      | Bool _, _
      | Str _, _
      | Double _, _ ->
          false)
  (* Should we raise an exn if one of the value is not a primitive?
   * No, the spec seems to not restrict what v and v' can be.
   *)
  | _else_ -> false

let rec lookup env tk local_id =
  let entry =
    try Map_.find local_id env.locals with
    | Not_found ->
        error tk
          (spf "could not find '%s' in the environment"
             (string_of_local_id local_id))
  in
  match entry with
  | Val v -> fst v
  | Expr e -> eval_expr (snd e) (fst e)
(*****************************************************************************)
(* eval_expr *)
(*****************************************************************************)

and eval_expr (env : env) (v : expr) : value_ =
  match v with
  | L v ->
      let prim =
        match v with
        | A.Null tk -> Null tk
        | A.Bool (b, tk) -> Bool (b, tk)
        | A.Str x -> Str (A.string_of_string_ x)
        | A.Number (s, tk) ->
            (* TODO: double check things *)
            let f = float_of_string s in
            Double (f, tk)
      in
      Primitive prim
  (* lazy evaluation of Array elements and Functions *)
  | Array (l, xs, r) ->
      let elts = xs |> Common.map (fun x -> (x, env)) |> Array.of_list in
      Array (l, elts, r)
  | Lambda v -> Function v
  | O v -> eval_obj_inside env v
  | Id (s, tk) -> lookup env tk (LId s)
  | IdSpecial (Self, tk) -> lookup env tk LSelf
  | IdSpecial (Super, tk) -> lookup env tk LSuper
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
               let binding = Expr (e_i, env) in
               Map_.add (LId (fst id)) binding acc)
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
                eval_expr (snd ei) (fst ei)
                (*Lazy.force ei.v*)
            | _else_ ->
                error tkf (spf "Out of bound for array index: %s" (sv index))
          else error tkf (spf "Not an integer: %s" (sv index))
      (* TODO: THIS NEEDS TO BE MORE COMPLEX  *)
      | ObjectVal (_l, (_assertsTODO, fields), _r), Primitive (Str (fld, tk))
        -> (
          match
            fields
            |> List.find_opt (fun (field : value_field) ->
                   fst field.vfld_name = fld)
          with
          | None -> error tk (spf "field '%s' not present in %s" fld (sv e))
          | Some fld -> eval_expr env (fst fld.vfld_value))
      (* TODO? support ArrayAccess for Strings? *)
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
      let s = std_type env v in
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
      let b = std_primivite_equals env v v' in
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
      | ObjectVal (_, (_asserts, flds), _) ->
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
      | Primitive (Double (n, tk)), Function fdef ->
          if Float.is_integer n then
            let n = Float.to_int n in
            let e i =
              Call
                ( Lambda fdef,
                  (fk, [ Arg (L (Number (string_of_int i, fk))) ], fk) )
            in
            Array (fk, Array.init n (fun i -> (e i, env)), fk)
          else error tk (spf "Got non-integer %f in std.makeArray" n)
      | v, _e' ->
          error tk (spf "Improper arguments to std.makeArray: %s" (sv v)))
  | "makeArray", _else_ ->
      error tk
        (spf "Improper number of arguments to std.makeArray: expected 2, got %d"
           (List.length args))
  | "filter", [ Arg e; Arg e' ] -> (
      match (eval_expr env e, eval_expr env e') with
      | Function f, Array (l, eis, r) ->
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
      | ObjectVal o, Primitive (Str (s, _)), Primitive (Bool (b, _)) ->
          let _, (_asserts, flds), _ = o in
          let eltopt =
            flds |> List.find_opt (fun { vfld_name; _ } -> fst vfld_name = s)
          in
          let b =
            match eltopt with
            | None -> false
            | Some { vfld_hidden = visibility, _; _ } ->
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
and eval_std_filter_element (env : env) (tk : tok) (f : function_definition)
    (ei : program * env) : value_ * env =
  (*ignore (env, f, ei);*)
  match f with
  | { f_params = _l, [ P (id, _eq, _default) ], _r; f_body; _ } ->
      (* similar to eval_expr for Local *)
      let locals = Map_.add (LId (fst id)) (Expr ei) env.locals in
      (* similar to eval_call *)
      (*TODO: Is the environment correct? *)
      (eval_expr { (*env with*) depth = env.depth + 1; locals } f_body, env)
  | _else_ -> error tk "filter function takes 1 parameter"

and eval_call env e0 (largs, args, _rargs) =
  match eval_expr env e0 with
  | Function { f_tok = _; f_params = lparams, params, rparams; f_body = eb } ->
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

(* This is a very naive implementation of plus for objects that
 * just merge the fields.
 * TODO: handle inheritance with complex self/super semantic in the presence
 * of plus
 *)
and eval_plus_object _env _tk objl objr : object_ A.bracket =
  let l, (lassert, lflds), _r = objl in
  let _, (rassert, rflds), r = objr in
  let hobjr =
    rflds
    |> Common.map (fun { vfld_name = s, _; _ } -> s)
    |> Common.hashset_of_list
  in
  (* TODO: this currently just merges the f *)
  let asserts = lassert @ rassert in
  let lflds' =
    lflds
    |> List.filter (fun { vfld_name = s, _; _ } -> not (Hashtbl.mem hobjr s))
  in
  let flds = lflds' @ rflds in
  (l, (asserts, flds), r)

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
      | ObjectVal objl, ObjectVal objr ->
          let obj = eval_plus_object env tk objl objr in
          ObjectVal obj
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
  let rec eval_std_cmp_value_ (v_el : value_) (v_er : value_) : cmp =
    match (v_el, v_er) with
    | Array (_, [||], _), Array (_, [||], _) -> Eq
    | Array (_, [||], _), Array (_, _, _) -> Inf
    | Array (_, _, _), Array (_, [||], _) -> Sup
    | Array (al, ax, ar), Array (bl, bx, br) -> (
        let a0 = eval_expr (snd ax.(0)) (fst ax.(0)) in
        let b0 = eval_expr (snd ax.(0)) (fst bx.(0)) in
        (*let a0 = Lazy.force ax.(0).v in*)
        (*let b0 = Lazy.force bx.(0).v in*)
        match eval_std_cmp_value_ a0 b0 with
        | (Inf | Sup) as r -> r
        | Eq ->
            let a_sub = Array (al, Array.sub ax 1 (Array.length ax - 1), ar) in
            let b_sub = Array (bl, Array.sub bx 1 (Array.length bx - 1), br) in
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

and eval_obj_inside env (l, x, r) : value_ =
  let hdupes = Hashtbl.create 16 in
  match x with
  | Object (assertsTODO, fields) ->
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
                       vfld_name = fld_name;
                       vfld_hidden = fld_hidden;
                       vfld_value = (fld_value, env);
                     }
               | v -> error tk (spf "field name was not a string: %s" (sv v)))
      in
      let asserts_with_env = List.map (fun x -> (x, env)) assertsTODO in
      ObjectVal (l, (asserts_with_env, fields), r)
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
and tostring (v : value_) : string =
  let j = manifest_value v in
  JSON.string_of_json j

(*Same as eval_expr but with profiling *)
and eval_program (e : Core_jsonnet.program) (env : env) : value_ =
  let v = eval_expr env e in
  v
  [@@profiling]

(*****************************************************************************)
(* Manfestation *)
(*****************************************************************************)
and manifest_value (v : value_) : JSON.t =
  match v with
  | Primitive x -> (
      match x with
      | Null _t -> J.Null
      | Bool (b, _tk) -> J.Bool b
      | Double (f, _tk) -> J.Float f
      | Str (s, _tk) -> J.String s)
  | Function { f_tok = tk; _ } -> error tk (spf "Function value: %s" (sv v))
  | Array (_, arr, _) ->
      J.Array
        (arr |> Array.to_list
        |> Common.map (fun entry ->
               (*let v = Lazy.force lzv.v*)
               let v = eval_program (fst entry) (snd entry) in
               manifest_value v))
  | ObjectVal (_l, (_assertsTODO, fields), _r) as _o ->
      (* TODO: evaluate asserts *)
      let xs =
        fields
        |> Common.map_filter (fun { vfld_name; vfld_hidden; vfld_value } ->
               match fst vfld_hidden with
               | A.Hidden -> None
               | A.Visible
               | A.ForcedVisible ->
                   (*let v = Lazy.force fld_value.v*)
                   let v = eval_program (fst vfld_value) (snd vfld_value) in
                   let j = manifest_value v in
                   Some (fst vfld_name, j))
      in
      J.Object xs

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
