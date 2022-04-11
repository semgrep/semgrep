(* Yoann Padioleau
 *
 * Copyright (C) 2019-2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common
module G = AST_generic
module MV = Metavariable
module J = JSON

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A simple interpreter for a simple subset of the generic AST.
 *
 * This can be used to safely execute a subset of pattern-where-python:
 * expressions, and to handle metavariable-comparison:.
 * It is safe because OCaml itself does not provide any 'eval()',
 * so we just execute code that we can explicitely handle.
 *
 * related work:
 * - https://github.com/google/cel-spec by Google
 * - https://github.com/facebook/Haxl by Facebook (was called FXL before)
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* This is the (partially parsed/evaluated) content of a metavariable *)
type value =
  | Bool of bool
  | Int of int
  | Float of float
  | String of string (* string without the enclosing '"' *)
  | List of value list
  (* default case where we don't really have good builtin operations.
   * This should be a AST_generic.any once parsed.
   * See JSON_report.json_metavar().
   *)
  | AST of string (* any AST, e.g., "x+1" *)
(* less: Id of string (* simpler to merge with AST *) *)
[@@deriving show]

type env = { mvars : (MV.mvar, value) Hashtbl.t; constant_propagation : bool }

(* we restrict ourselves to simple expressions for now *)
type code = AST_generic.expr

exception NotHandled of code
exception NotInEnv of Metavariable.mvar

(*****************************************************************************)
(* JSON Parsing *)
(*****************************************************************************)
let metavar_of_json s = function
  | J.Int i -> Int i
  | J.Bool b -> Bool b
  | J.String s -> String s
  | J.Float f -> Float f
  | _ -> failwith (spf "wrong format for metavar %s" s)

(* JSON format used internally in semgrep-python for metavariable-comparison.
 * metavariable-comparison is actually now handled directly in semgrep-core,
 * so this format is not used anymore in semgrep-python, but we still
 * use it for some of our regression tests in tests/OTHER/eval/.
 *)
let parse_json file =
  let json = JSON.load_json file in
  match json with
  | J.Object xs -> (
      match Common.sort_by_key_lowfirst xs with
      | [
       ("code", J.String code);
       ("language", J.String lang);
       ("metavars", J.Object xs);
      ] ->
          let lang =
            try Hashtbl.find Lang.lang_map lang with
            | Not_found -> failwith (spf "unsupported language %s" lang)
          in
          (* less: could also use Parse_pattern *)
          let code =
            match Parse_pattern.parse_pattern lang code with
            | G.E e -> e
            | _ -> failwith "only expressions are supported"
          in
          let metavars =
            xs |> Common.map (fun (s, json) -> (s, metavar_of_json s json))
          in
          let env =
            {
              mvars = Common.hash_of_list metavars;
              constant_propagation = true;
            }
          in
          (env, code)
      | _ -> failwith "wrong json format")
  | _ -> failwith "wrong json format"

(*****************************************************************************)
(* Reporting *)
(*****************************************************************************)

(* alt: could use exit code, or return JSON *)
let print_result xopt =
  match xopt with
  | None -> pr "NONE"
  | Some v -> (
      match v with
      | Bool b -> pr (string_of_bool b)
      (* allow to abuse int to encode boolean ... ugly C tradition *)
      | Int 0 -> pr (string_of_bool false)
      | Int _ -> pr (string_of_bool true)
      | _ -> pr "NONE")
  [@@action]

(*****************************************************************************)
(* Eval algorithm *)
(*****************************************************************************)

let value_of_lit ~code x =
  match x with
  | G.Bool (b, _t) -> Bool b
  | G.String (s, _t) -> String s
  (* big integers or floats can't be evaluated (Int (None, ...)) *)
  | G.Int (Some i, _t) -> Int i
  | G.Float (Some f, _t) -> Float f
  | _ -> raise (NotHandled code)

let rec eval env code =
  match code.G.e with
  | G.L x -> value_of_lit ~code x
  | G.N (G.Id ((_, _), { id_svalue = { contents = Some (G.Lit lit) }; _ }))
  (* coupling: Constant_propagation.eval *)
  | G.Call
      ( { e = G.N (G.Id (("!dockerfile_expand!", _), _)); _ },
        ( _,
          [
            G.Arg
              {
                e =
                  G.N
                    (G.Id
                      ( (_, _),
                        { id_svalue = { contents = Some (G.Lit lit) }; _ } ));
                _;
              };
          ],
          _ ) )
    when env.constant_propagation ->
      value_of_lit ~code lit
  | G.N (G.Id ((s, _t), _idinfo))
    when MV.is_metavar_name s || MV.is_metavar_ellipsis s -> (
      try Hashtbl.find env.mvars s with
      | Not_found ->
          logger#trace "could not find a value for %s in env" s;
          raise (NotInEnv s))
  (* Python int() operator *)
  | G.Call ({ e = G.N (G.Id (("int", _), _)); _ }, (_, [ Arg e ], _)) -> (
      let v = eval env e in
      match v with
      | Int _ -> v
      | String s -> (
          match int_of_string_opt s with
          | None -> raise (NotHandled code)
          | Some i -> Int i)
      | __else__ -> raise (NotHandled code))
  | G.Call ({ e = G.IdSpecial (G.Op op, _t); _ }, (_, args, _)) ->
      let values =
        args
        |> Common.map (function
             | G.Arg e -> eval env e
             | _ -> raise (NotHandled code))
      in
      eval_op op values code
  | G.Container (G.List, (_, xs, _)) ->
      let vs = Common.map (eval env) xs in
      List vs
  (* Emulate Python re.match just enough *)
  | G.Call
      ( {
          e =
            G.DotAccess
              ( { e = G.N (G.Id (("re", _), _)); _ },
                _,
                FN (Id (("match", _), _)) );
          _;
        },
        (_, [ G.Arg { e = G.L (G.String (re, _)); _ }; G.Arg e2 ], _) ) -> (
      (* alt: take the text range of the metavariable in the original file,
       * and enforce e1 can only be an Id metavariable.
       * alt: let s = value_to_string v in
       * to convert anything in a string before using regexps on it
       *)
      let v = eval env e2 in

      match v with
      | String s ->
          (* todo? factorize with Matching_generic.regexp_matcher_of_regexp_.. *)
          let regexp = SPcre.regexp ~flags:[ `ANCHORED ] re in
          let res = SPcre.pmatch_noerr ~rex:regexp s in
          let v = Bool res in
          logger#info "regexp %s on %s return %s" re s (show_value v);
          v
      | _ -> raise (NotHandled code))
  | _ -> raise (NotHandled code)

and eval_op op values code =
  match (op, values) with
  | G.And, [ Bool b1; Bool b2 ] -> Bool (b1 && b2)
  | G.Not, [ Bool b1 ] -> Bool (not b1)
  | G.Or, [ Bool b1; Bool b2 ] -> Bool (b1 || b2)
  | G.Gt, [ Int i1; Int i2 ] -> Bool (i1 > i2)
  | G.Gt, [ Float i1; Float i2 ] -> Bool (i1 > i2)
  | G.Gt, [ Int i1; Float i2 ] -> Bool (float_of_int i1 > i2)
  | G.Gt, [ Float i1; Int i2 ] -> Bool (i1 > float_of_int i2)
  | G.GtE, [ Int i1; Int i2 ] -> Bool (i1 >= i2)
  | G.GtE, [ Float i1; Float i2 ] -> Bool (i1 >= i2)
  | G.GtE, [ Int i1; Float i2 ] -> Bool (float_of_int i1 >= i2)
  | G.GtE, [ Float i1; Int i2 ] -> Bool (i1 >= float_of_int i2)
  | G.Lt, [ Int i1; Int i2 ] -> Bool (i1 < i2)
  | G.Lt, [ Float i1; Float i2 ] -> Bool (i1 < i2)
  | G.Lt, [ Int i1; Float i2 ] -> Bool (float_of_int i1 < i2)
  | G.Lt, [ Float i1; Int i2 ] -> Bool (i1 < float_of_int i2)
  | G.LtE, [ Int i1; Int i2 ] -> Bool (i1 <= i2)
  | G.LtE, [ Float i1; Float i2 ] -> Bool (i1 <= i2)
  | G.LtE, [ Int i1; Float i2 ] -> Bool (float_of_int i1 <= i2)
  | G.LtE, [ Float i1; Int i2 ] -> Bool (i1 <= float_of_int i2)
  | G.Div, [ Int i1; Int i2 ] -> Int (i1 / i2)
  | G.Div, [ Float i1; Float i2 ] -> Float (i1 /. i2)
  | G.Div, [ Int i1; Float i2 ] -> Float (float_of_int i1 /. i2)
  | G.Div, [ Float i1; Int i2 ] -> Float (i1 /. float_of_int i2)
  | G.Minus, [ Int i1 ] -> Int (-i1)
  | G.Minus, [ Float i1 ] -> Float (-.i1)
  | G.Minus, [ Int i1; Int i2 ] -> Int (i1 - i2)
  | G.Minus, [ Float i1; Float i2 ] -> Float (i1 -. i2)
  | G.Minus, [ Int i1; Float i2 ] -> Float (float_of_int i1 -. i2)
  | G.Minus, [ Float i1; Int i2 ] -> Float (i1 -. float_of_int i2)
  | G.Mod, [ Int i1; Int i2 ] -> Int (i1 mod i2)
  | G.Mod, [ Float i1; Float i2 ] -> Float (Float.rem i1 i2)
  | G.Mod, [ Int i1; Float i2 ] -> Float (Float.rem (float_of_int i1) i2)
  | G.Mod, [ Float i1; Int i2 ] -> Float (Float.rem i1 (float_of_int i2))
  | G.Mult, [ Int i1; Int i2 ] -> Int (i1 * i2)
  | G.Mult, [ Float i1; Float i2 ] -> Float (i1 *. i2)
  | G.Mult, [ Int i1; Float i2 ] -> Float (float_of_int i1 *. i2)
  | G.Mult, [ Float i1; Int i2 ] -> Float (i1 *. float_of_int i2)
  | G.Plus, [ Int i1 ] -> Int i1
  | G.Plus, [ Float i1 ] -> Float i1
  | G.Plus, [ Int i1; Int i2 ] -> Int (i1 + i2)
  | G.Plus, [ Float i1; Float i2 ] -> Float (i1 +. i2)
  | G.Plus, [ Int i1; Float i2 ] -> Float (float_of_int i1 +. i2)
  | G.Plus, [ Float i1; Int i2 ] -> Float (i1 +. float_of_int i2)
  | G.Eq, [ Int v1; Float v2 ] -> Bool (float_of_int v1 = v2)
  | G.Eq, [ Float v1; Int v2 ] -> Bool (v1 = float_of_int v2)
  | G.Eq, [ v1; v2 ] -> Bool (v1 = v2)
  | G.NotEq, [ Int v1; Float v2 ] -> Bool (float_of_int v1 <> v2)
  | G.NotEq, [ Float v1; Int v2 ] -> Bool (v1 <> float_of_int v2)
  | G.NotEq, [ v1; v2 ] -> Bool (v1 <> v2)
  | G.In, [ v1; v2 ] -> (
      match v2 with
      | List xs -> Bool (List.mem v1 xs)
      | _ -> Bool false)
  (* less: it would be better to show the actual values not handled,
   * rather than the code, because this may differ as the code does not
   * contained the resolved content of metavariables *)
  | _ -> raise (NotHandled code)

(*****************************************************************************)
(* Env builders *)
(*****************************************************************************)

(* when called from the new semgrep-full-rule-in-ocaml *)

let bindings_to_env (config : Config_semgrep.t) xs =
  let constant_propagation = config.constant_propagation in
  let mvars =
    xs
    |> Common.map_filter (fun (mvar, mval) ->
           let try_bind_to_exp e =
             try
               Some
                 ( mvar,
                   eval { mvars = Hashtbl.create 0; constant_propagation } e )
               (* this can happen when a metavar is binded to a complex expression,
                * e.g., os.getenv("foo") which can't be evaluated. It's ok to
                * filter those metavars then.
                *)
             with
             | NotHandled _
             | NotInEnv _ ->
                 logger#debug "filtering mvar %s, can't eval %s" mvar
                   (MV.show_mvalue mval);
                 (* todo: if not a value, could default to AST of range *)
                 None
           in
           match mval with
           (* this way we can leverage the constant propagation analysis
            * in metavariable-comparison: too! This simplifies some rules.
            *)
           | MV.Id (i, Some id_info) ->
               try_bind_to_exp (G.e (G.N (G.Id (i, id_info))))
           | MV.E e -> try_bind_to_exp e
           | x ->
               logger#debug "filtering mvar %s, not an expr %s" mvar
                 (MV.show_mvalue x);
               None)
    |> Common.hash_of_list
  in
  { mvars; constant_propagation }

let text_of_binding mvar mval =
  match mval with
  | MV.Text (text, _) ->
      (* Note that `text` may be produced by constant folding, in which
       * case we will not have range info. *)
      Some text
  | _ -> (
      let any = MV.mvalue_to_any mval in
      match Visitor_AST.range_of_any_opt any with
      | None ->
          (* TODO: Report a warning to the user? *)
          logger#error "We lack range info for metavariable %s: %s" mvar
            (G.show_any any);
          None
      | Some (min, max) ->
          let file = min.Parse_info.file in
          let range = Range.range_of_token_locations min max in
          Some (Range.content_at_range file range))

(* this is for metavariable-regexp *)
let bindings_to_env_with_just_strings (config : Config_semgrep.t) bindings =
  let ( let* ) = Option.bind in
  let mvars =
    bindings
    |> Common.map_filter (fun (mvar, mval) ->
           let* text = text_of_binding mvar mval in
           Some (mvar, String text))
    |> Common.hash_of_list
  in
  { mvars; constant_propagation = config.constant_propagation }

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* for testing purpose, via semgrep-core -test_eval *)
let test_eval file =
  try
    let env, code = parse_json file in
    let res = eval env code in
    print_result (Some res)
  with
  | NotHandled e ->
      pr2 (G.show_expr e);
      raise (NotHandled e)

(* We need to swallow most exns in eval_bool(). This is because the
 * metavariable-comparison code in [e] may be valid
 * (expressions we can handle), but the metavariables may bind to complex
 * expressions, or have the wrong types, which would generate a NotHandled.
 *
 * For example, in the Python insecure-file-permissions rule, we use
 * a complex set of patterns with the very general
 * 'os.$METHOD($FILE, $BITS)' and metavariable-comparison '$BITS <= 0o650',
 * which is ANDed with a metavariable-pattern to restrict later what
 * $METHOD can be. Unfortunately that means the first pattern can match
 * code like `os.getenv('a', 'defaulta')` where $BITS will bind
 * to a string literal, which can't be compared with '<= 0o650'.
 *)
let eval_bool env e =
  try
    let res = eval env e in
    match res with
    | Bool b -> b
    | _ ->
        logger#trace "not a boolean: %s" (show_value res);
        false
  with
  (* this can happen when a metavar is binded to a complex expression,
   * in which case it's filtered in bindings_to_env(), in which case
   * it generates a NotInEnv when we run eval with such an environment.
   *)
  | NotInEnv _ -> false
  | NotHandled e ->
      logger#trace "NotHandled: %s" (G.show_expr e);
      false
