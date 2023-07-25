open Core_jsonnet
open Common
module V = Value_jsonnet
module A = AST_jsonnet

exception Error of string * Tok.t

type cmp = Inf | Eq | Sup

let fk = Tok.unsafe_fake_tok ""

let std_type _env (v : V.value_) : string =
  match v with
  | V.Primitive (Null _) -> "null"
  | V.Primitive (Bool _) -> "boolean"
  | V.Primitive (Double _) -> "number"
  | V.Primitive (Str _) -> "string"
  | V.Object _ -> "object"
  | V.Array _ -> "array"
  | V.Lambda _ -> "function"

let std_primivite_equals (v : V.value_) (v' : V.value_) : bool =
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

let error tk s =
  (* TODO? if Parse_info.is_fake tk ... *)
  raise (Error (s, tk))

let logger = Logging.get_logger [ __MODULE__ ]

let log_call (env : V.env) str tk =
  logger#trace "calling %s> %s at %s"
    (Common2.repeat "-" env.depth |> Common.join "")
    str (Tok.stringpos_of_tok tk)

let int_to_cmp = function
  | -1 -> Inf
  | 0 -> Eq
  | 1 -> Sup
  (* all the OCaml Xxx.compare should return only -1, 0, or 1 *)
  | _else_ -> assert false

let sv e =
  let s = V.show_value_ e in
  if String.length s > 100 then Str.first_chars s 100 ^ "..." else s

let rec bind_list_contains binds id =
  match binds with
  | B ((name, _), _, _) :: t ->
      if id =*= name then true else bind_list_contains t id
  | [] -> false

let rec parameter_list_contains parameters id =
  match parameters with
  | P ((name, _), _, _) :: t ->
      if id =*= name then true else parameter_list_contains t id
  | [] -> false

let eval_bracket ofa (v1, v2, v3) =
  let v2 = ofa v2 in
  (v1, v2, v3)

(*TODOS: std library? Object Comp? *)
let rec substitute id sub expr =
  match expr with
  | L v -> L v
  | Array (l, xs, r) -> Array (l, Common.map (substitute id sub) xs, r)
  | Lambda { f_tok; f_params = lparams, params, rparams; f_body } ->
      if parameter_list_contains params id then
        Lambda { f_tok; f_params = (lparams, params, rparams); f_body }
      else
        let new_params =
          Common.map
            (fun (P (name, tk, expr)) -> P (name, tk, substitute id sub expr))
            params
        in
        Lambda { f_tok; f_params = (lparams, new_params, rparams); f_body }
  | O (l, v, r) -> (
      match v with
      | Object (asserts, fields) ->
          let new_asserts =
            Common.map (fun (tk, expr) -> (tk, substitute id sub expr)) asserts
          in
          let new_fields =
            Common.map
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
  | Id (s, tk) -> if s =*= id then sub else Id (s, tk)
  | IdSpecial id -> IdSpecial id
  | Local (_tlocal, binds, _tsemi, e) ->
      if bind_list_contains binds id then Local (_tlocal, binds, _tsemi, e)
      else
        let new_binds =
          Common.map
            (fun (B (name, tk, expr)) -> B (name, tk, substitute id sub expr))
            binds
        in
        Local (_tlocal, new_binds, _tsemi, substitute id sub e)
  | ArrayAccess (v1, (l, v2, r)) ->
      ArrayAccess (substitute id sub v1, (l, substitute id sub v2, r))
  | Call (e0, (l, args, r)) ->
      let new_func = substitute id sub e0 in
      let new_args =
        Common.map
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
  | ExprTodo ((s, tk), _ast_expr) -> ExprTodo ((s, tk), _ast_expr)
(*TODO*)

let rec eval_expr expr =
  match expr with
  | L v ->
      let prim =
        match v with
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
        |> Common.map (fun x -> { V.value = Unevaluated x; env = V.empty_env })
        |> Array.of_list
      in
      Array (l, elts, r)
  | Lambda v -> Lambda v
  | O v -> eval_obj_inside v
  | Id _ -> failwith "shouldn't happen"
  | IdSpecial _ -> failwith "shouldn't happen"
  | Call
      ( (ArrayAccess
           (Id ("std", _), (_, L (Str (None, DoubleQuote, (_, [ meth ], _))), _))
        as e0),
        (l, args, r) ) ->
      eval_std_method e0 meth (l, args, r)
  | Local (_tlocal, binds, _tsemi, e) ->
      (* NOT SURE IF CORRECT, NOT QUITE THE SAME AS SEMANTICS, BUT I THINK SEMANTICS ARE WRONG*)
      let new_e =
        List.fold_left
          (fun e_1 (B ((name, _), _, e')) ->
            substitute name (Local (_tlocal, binds, _tsemi, e')) e_1)
          e binds
      in
      eval_expr new_e
  | ArrayAccess (v1, v2) -> (
      let e = eval_expr v1 in
      let l, index, _r = (eval_bracket eval_expr) v2 in
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
      (* Field access! A tricky operation. *)
      | V.Object (_l, (_assertsTODO, fields), _r), Primitive (Str (fld, tk))
        -> (
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
                  let new_e = substitute "self" v1 e in
                  (* TODO implement self/super substitution*)
                  eval_expr new_e))
      (* TODO? support ArrayAccess for Strings? *)
      | _else_ -> error l (spf "Invalid ArrayAccess: %s[%s]" (sv e) (sv index)))
  | Call (e0, args) -> eval_call e0 args
  | UnaryOp ((op, tk), e) -> (
      match op with
      | UBang -> (
          match eval_expr e with
          | Primitive (Bool (b, tk)) -> Primitive (Bool (not b, tk))
          | v -> error tk (spf "Not a boolean for unary !: %s" (sv v)))
      | UPlus -> (
          match eval_expr e with
          | Primitive (Double (f, tk)) -> Primitive (Double (f, tk))
          | v -> error tk (spf "Not a number for unary +: %s" (sv v)))
      | UMinus -> (
          match eval_expr e with
          | Primitive (Double (f, tk)) -> Primitive (Double (-.f, tk))
          | v -> error tk (spf "Not a number for unary -: %s" (sv v)))
      | UTilde -> (
          match eval_expr e with
          | Primitive (Double (f, tk)) ->
              let f = f |> Int64.of_float |> Int64.lognot |> Int64.to_float in
              Primitive (Double (f, tk))
          | v -> error tk (spf "Not a number for unary -: %s" (sv v))))
  | BinaryOp (el, (op, tk), er) -> eval_binary_op el (op, tk) er
  | If (tif, e1, e2, e3) -> (
      match eval_expr e1 with
      | Primitive (Bool (b, _)) -> if b then eval_expr e2 else eval_expr e3
      | v -> error tif (spf "not a boolean for if: %s" (sv v)))
  | Error (tk, e) -> (
      match eval_expr e with
      | Primitive (Str (s, tk)) -> error tk (spf "ERROR: %s" s)
      | v -> error tk (spf "ERROR: %s" (tostring v)))
  | ExprTodo ((s, tk), _ast_expr) -> error tk (spf "ERROR: ExprTODO: %s" s)

and eval_binary_op el (op, tk) er =
  match op with
  | Plus -> (
      match (eval_expr el, eval_expr er) with
      | Array (l1, arr1, _r1), Array (_l2, arr2, r2) ->
          Array (l1, Array.append arr1 arr2, r2)
      | Primitive (Double (f1, tk)), Primitive (Double (f2, _)) ->
          Primitive (Double (f1 +. f2, tk))
      | Primitive (Str (s1, tk1)), Primitive (Str (s2, _tk2)) ->
          Primitive (Str (s1 ^ s2, tk1))
      | Primitive (Str (s, tk)), v -> Primitive (Str (s ^ tostring v, tk))
      | v, Primitive (Str (s, tk)) -> Primitive (Str (tostring v ^ s, tk))
      | V.Object objl, V.Object objr ->
          let obj = eval_plus_object tk objl objr in
          V.Object obj
      | v1, v2 ->
          error tk (spf "TODO: Plus (%s, %s) not yet handled" (sv v1) (sv v2)))
  | And -> (
      match eval_expr el with
      | Primitive (Bool (b, _)) as v -> if b then eval_expr er else v
      | v -> error tk (spf "Not a boolean for &&: %s" (sv v)))
  | Or -> (
      match eval_expr el with
      | Primitive (Bool (b, _)) as v -> if b then v else eval_expr er
      | v -> error tk (spf "Not a boolean for ||: %s" (sv v)))
  | Lt
  | LtE
  | Gt
  | GtE ->
      let cmp = eval_std_cmp tk el er in
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
      match (eval_expr el, eval_expr er) with
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
      let v1 = eval_expr el in
      let v2 = eval_expr er in
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

and eval_std_cmp tk (el : expr) (er : expr) : cmp =
  let rec eval_std_cmp_value_ (v_el : V.value_) (v_er : V.value_) : cmp =
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
  eval_std_cmp_value_ (eval_expr el) (eval_expr er)

and eval_call e0 (largs, args, _rargs) =
  match eval_expr e0 with
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
      log_call V.empty_env fstr largs;
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
      eval_expr (Local (lparams, binds, rparams, eb))
  | v -> error largs (spf "not a function: %s" (sv v))

and eval_std_method e0 (method_str, tk) (l, args, r) =
  match (method_str, args) with
  | "type", [ Arg e ] ->
      log_call V.empty_env ("std." ^ method_str) l;
      let v = eval_expr e in
      let s = std_type V.empty_env v in
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
      log_call V.empty_env ("std." ^ method_str) l;
      let v = eval_expr e in
      let v' = eval_expr e' in
      let b = std_primivite_equals v v' in
      Primitive (Bool (b, l))
  | "primitiveEquals", _else_ ->
      error tk
        (spf "Improper #arguments to std.primitiveEquals: expected 2, got %d"
           (List.length args))
  | "length", [ Arg e ] -> (
      log_call V.empty_env ("std." ^ method_str) l;
      match eval_expr e with
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
      log_call V.empty_env ("std." ^ method_str) l;
      match (eval_expr e, eval_expr e') with
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
                Array.init n (fun i ->
                    { V.value = Unevaluated (e i); env = V.empty_env }),
                fk )
          else error tk (spf "Got non-integer %f in std.makeArray" n)
      | v, _e' ->
          error tk (spf "Improper arguments to std.makeArray: %s" (sv v)))
  | "makeArray", _else_ ->
      error tk
        (spf "Improper number of arguments to std.makeArray: expected 2, got %d"
           (List.length args))
  | "filter", [ Arg e; Arg e' ] -> (
      match (eval_expr e, eval_expr e') with
      | Lambda f, Array (l, eis, r) ->
          (* note that we do things lazily even here, so we still
           * return an Array with the same lazy value elements in it,
           * but just filtered
           *)
          let elts' =
            (* TODO? use Array.to_seqi instead? *)
            eis |> Array.to_list |> Common.index_list
            |> List.filter_map (fun (ei, ji) ->
                   match eval_std_filter_element tk f ei with
                   | Primitive (Bool (false, _)) -> None
                   | Primitive (Bool (true, _)) -> Some ji
                   | v ->
                       error tk
                         (spf "filter function must return boolean, got: %s"
                            (sv v)))
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
      match (eval_expr e, eval_expr e', eval_expr e'') with
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
  | _else_ -> eval_call e0 (l, args, r)

and tostring (_ : V.value_) : string = "unimplemented"

and eval_std_filter_element (tk : tok) (f : function_definition)
    (ei : V.lazy_value) : V.value_ =
  match f with
  | { f_params = _l, [ P (_id, _eq, _default) ], _r; f_body = _; _ } -> (
      (* similar to eval_expr for Local *)
      (* similar to eval_call *)
      (*TODO: Is the environment correct? *)
      match ei.value with
      | Val _ -> failwith "shouldn't happen"
      | Unevaluated e -> eval_call (Lambda f) (_l, [ Arg e ], _r))
  | _else_ -> error tk "filter function takes 1 parameter"

and eval_obj_inside _o = failwith "unimplemented"
and eval_plus_object _tk _el _er = failwith "unimplented"

and evaluate_lazy_value_ (v : V.lazy_value) =
  match v.value with
  | Val v -> v
  | Unevaluated e -> eval_expr e
