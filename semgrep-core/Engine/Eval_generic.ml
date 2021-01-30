(* Yoann Padioleau
 *
 * Copyright (C) 2019-2020 r2c
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

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A simple interpreter for a simple subset of the generic AST.
 *
 * This can be used to safely execute a subset of pattern-where-python:
 * expressions.
 *
 * related work:
 * - https://github.com/google/cel-spec by Google
 * - https://github.com/facebook/Haxl by Facebook (was called FXL before)
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* This is the (partially parsed) content of a metavariable *)
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

type env = (MV.mvar, value) Hashtbl.t

(* we restrict ourselves to simple expressions for now *)
type code = AST_generic.expr

exception NotHandled of code

(*****************************************************************************)
(* JSON Parsing *)
(*****************************************************************************)
let metavar_of_json s = function
  | J.Int i -> Int i
  | J.Bool b -> Bool b
  | J.String s -> String s
  | J.Float f -> Float f
  | _ -> failwith (spf "wrong format for metavar %s" s)

let parse_json file =
  let json = JSON.load_json file in
  match json with
  | J.Object xs ->
      (match Common.sort_by_key_lowfirst xs with
       | ["code", J.String code;
          "language", J.String lang;
          "metavars", J.Object xs;
         ] ->
           let lang =
             try Hashtbl.find Lang.lang_of_string_map lang
             with Not_found -> failwith (spf "unsupported language %s" lang)
           in
           (* less: could also use Parse_pattern *)
           let code =
             match Parse_generic.parse_pattern lang code with
             | G.E e -> e
             | _ -> failwith "only expressions are supported"
           in
           let metavars =
             xs |> List.map (fun (s, json) -> s, metavar_of_json s json)
           in
           Common.hash_of_list metavars, code

       | _ -> failwith "wrong json format"
      )
  | _ -> failwith "wrong json format"

(*****************************************************************************)
(* Converting *)
(*****************************************************************************)
let _value_to_string = function
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> s
  | _ -> raise Todo

(*****************************************************************************)
(* Reporting *)
(*****************************************************************************)

(* alt: could use exit code, or return JSON *)
let print_result xopt =
  match xopt  with
  | None -> pr "NONE"
  | Some v ->
      (match v with
       | Bool b -> pr (string_of_bool b)
       (* allow to abuse int to encode boolean ... ugly C tradition *)
       | Int 0 -> pr (string_of_bool false)
       | Int _ -> pr (string_of_bool true)
       | _ -> pr "NONE"
      )
[@@action]

(*****************************************************************************)
(* Eval algorithm *)
(*****************************************************************************)

let rec eval env code =
  match code with
  | G.L x ->
      (match x with
       | G.Bool (b, _t) -> Bool (b)
       | G.String (s, _t) -> String s
       (* this assumes the string format of the language is handled by
        * OCaml xx_of_string builtins.
       *)
       | G.Int (s, _t) -> Int (int_of_string s)
       | G.Float (s, _t) -> Float (float_of_string s)
       | _ -> raise (NotHandled code)
      )
  | G.Id ((s, _t), _idinfo) ->
      (try Hashtbl.find env s
       with Not_found -> raise (NotHandled code)
      )
  | G.Call (G.IdSpecial (G.Op op, _t), (_, args, _)) ->
      let values = args |> List.map (function
        | G.Arg e -> eval env e
        | _ -> raise (NotHandled code)
      ) in
      eval_op op values code
  | G.Container (G.List, (_, xs, _)) ->
      let vs = List.map (eval env) xs in
      List vs
  (* TODO: Python In could be made an AST_generic.operator at some point *)
  | G.OtherExpr (G.OE_In, [G.E e1; G.E e2]) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v2 with
       | List xs -> Bool (List.mem v1 xs)
       | _ -> Bool (false)
      )
  (* see Convert_rule.ml and the new formula format *)
  | G.Call (G.Id (("semgrep_re_match", _), _),
            (_, [G.Arg e1; G.Arg (G.L (G.String (re, _)))], _)) ->
      (* alt: take the text range of the metavariable in the original file *)
      let v = eval env e1 in
      (* old: let s = value_to_string v in *)
      (match v with
       | String s ->
           (* todo? factorize with Matching_generic.regexp_matcher_of_regexp_.. *)
           (* use of `ANCHORED to simulate Python re.match() (vs re.search) *)
           let regexp = Re.Pcre.regexp ~flags:[`ANCHORED] re in
           let res = Re.Pcre.pmatch ~rex:regexp s in
           Bool res
       | _ -> raise (NotHandled code)
      )

  | _ -> raise (NotHandled code)

and eval_op op values code =
  match op, values with
  | G.Gt, [Int i1; Int i2] -> Bool (i1 > i2)
  | G.GtE, [Int i1; Int i2] -> Bool (i1 >= i2)
  | G.Lt, [Int i1; Int i2] -> Bool (i1 < i2)
  | G.LtE, [Int i1; Int i2] -> Bool (i1 <= i2)

  (* todo: we should perform cast and allow to mix Float and Int *)
  | G.Gt, [Float i1; Float i2] -> Bool (i1 > i2)
  | G.GtE, [Float i1; Float i2] -> Bool (i1 >= i2)
  | G.Lt, [Float i1; Float i2] -> Bool (i1 < i2)
  | G.LtE, [Float i1; Float i2] -> Bool (i1 <= i2)

  | G.And, [Bool b1; Bool b2] -> Bool (b1 && b2)
  | G.Or, [Bool b1; Bool b2] -> Bool (b1 || b2)
  | G.Not, [Bool b1] -> Bool (not b1)

  | G.Plus, [Int i1] -> Int i1
  | G.Plus, [Int i1; Int i2] -> Int (i1 + i2)
  | G.Minus, [Int i1] -> Int (-i1)
  | G.Minus, [Int i1; Int i2] -> Int (i1 - i2)
  | G.Mult, [Int i1; Int i2] -> Int (i1 * i2)
  | G.Div, [Int i1; Int i2] -> Int (i1 / i2)
  | G.Mod, [Int i1; Int i2] -> Int (i1 mod i2)

  (* todo: we should perform automatic cast and allow to mix Float and Int *)
  | G.Plus, [Float i1] -> Float i1
  | G.Plus, [Float i1; Float i2] -> Float (i1 +. i2)
  | G.Minus, [Float i1] -> Float (-. i1)
  | G.Minus, [Float i1; Float i2] -> Float (i1 -. i2)
  | G.Mult, [Float i1; Float i2] -> Float (i1 *. i2)
  | G.Div, [Float i1; Float i2] -> Float (i1 /. i2)

  (* abuse generic =. Not that this will prevent
   * Int 0 to be equal to Float 0.0.
   * Again need automatic cast.
  *)
  | G.Eq, [v1; v2] -> Bool (v1 = v2)
  | G.NotEq, [v1; v2] -> Bool (v1 <> v2)

  | _ -> raise (NotHandled code)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* This is when called from the semgrep Python wrapper for the
 * metavariable-comparison: condition.
*)
let eval_json_file file =
  try
    let (env, code) = parse_json file in
    let res = eval env code in
    print_result (Some res)
  with
  | NotHandled e ->
      logger#sdebug (G.show_any (G.E e));
      print_result None
  | exn ->
      logger#debug "exn: %s" (Common.exn_to_s exn);
      print_result None

(* for testing purpose *)
let test_eval file =
  try
    let (env, code) = parse_json file in
    let res = eval env code in
    print_result (Some res)
  with NotHandled e ->
    pr2 (G.show_expr e);
    raise (NotHandled e)

let _eval_bindings xs =
  xs |> Common.map_filter (fun (mvar, mval) ->
    match mval with
    | MV.E e -> Some (mvar, eval (Hashtbl.create 0) e)
    | x ->
        logger#debug "filtering mvar %s, not an expr %s" mvar (MV.show_mvalue x);
        None
  ) |> Common.hash_of_list

(* this is for metavariable-regexp *)
let bindings_to_env_with_just_strings xs =
  xs |> List.map (fun (mvar, mval) ->
    let any = MV.mvalue_to_any mval in
    let (min, max) = Lib_AST.range_of_any any in
    let file = min.Parse_info.file in
    let range = Range.range_of_token_locations min max in
    mvar, String (Range.content_at_range file range)
  ) |> Common.hash_of_list

(* when called from the new semgrep-full-rule-in-ocaml *)
let eval_expr_with_bindings bindings e =
  try
    let env = bindings_to_env_with_just_strings bindings in
    let res = eval env e in
    (match res with
     | Bool b -> b
     | _ -> failwith (spf "not a boolean: %s" (show_value res))
    )
  with NotHandled e ->
    pr2 (G.show_expr e);
    raise (NotHandled e)
