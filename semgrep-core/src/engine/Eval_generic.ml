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

let logger = Logging.get_logger [ __MODULE__ ]

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

(* This is the (partially parsed/evalulated) content of a metavariable *)
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

(* JSON format used internally in semgrep-python for metavariable-comparison *)
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
            try Hashtbl.find Lang.lang_of_string_map lang
            with Not_found -> failwith (spf "unsupported language %s" lang)
          in
          (* less: could also use Parse_pattern *)
          let code =
            match Parse_pattern.parse_pattern lang code with
            | G.E e -> e
            | _ -> failwith "only expressions are supported"
          in
          let metavars =
            xs |> List.map (fun (s, json) -> (s, metavar_of_json s json))
          in
          (Common.hash_of_list metavars, code)
      | _ -> failwith "wrong json format" )
  | _ -> failwith "wrong json format"

(*****************************************************************************)
(* Converting *)
(*****************************************************************************)
(* to allow regexp to match regular ints, identifiers, or any text code *)
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
  match xopt with
  | None -> pr "NONE"
  | Some v -> (
      match v with
      | Bool b -> pr (string_of_bool b)
      (* allow to abuse int to encode boolean ... ugly C tradition *)
      | Int 0 -> pr (string_of_bool false)
      | Int _ -> pr (string_of_bool true)
      | _ -> pr "NONE" )
  [@@action]

(*****************************************************************************)
(* Eval algorithm *)
(*****************************************************************************)

let rec eval env code =
  match code with
  | G.L x -> (
      match x with
      | G.Bool (b, _t) -> Bool b
      | G.String (s, _t) -> String s
      (* big integers or floats can't be evaluated (Int (None, ...)) *)
      | G.Int (Some i, _t) -> Int i
      | G.Float (Some f, _t) -> Float f
      | _ -> raise (NotHandled code) )
  (* less: sanity check that s is a metavar_name? *)
  | G.N (G.Id ((s, _t), _idinfo)) -> (
      try Hashtbl.find env s
      with Not_found ->
        logger#debug "could not find a value for %s in env" s;
        raise Not_found )
  | G.Call (G.N (G.Id (("int", _), _)), (_, [ Arg e ], _)) -> (
      let v = eval env e in
      match v with
      | Int _ -> v
      | String s -> (
          match int_of_string_opt s with
          | None -> raise (NotHandled code)
          | Some i -> Int i )
      | __else__ -> raise (NotHandled code) )
  | G.Call (G.IdSpecial (G.Op op, _t), (_, args, _)) ->
      let values =
        args
        |> List.map (function
             | G.Arg e -> eval env e
             | _ -> raise (NotHandled code))
      in
      eval_op op values code
  | G.Container (G.List, (_, xs, _)) ->
      let vs = List.map (eval env) xs in
      List vs
  (* Emulate Python re.match just enough *)
  | G.Call
      ( G.DotAccess (G.N (G.Id (("re", _), _)), _, EN (Id (("match", _), _))),
        (_, [ G.Arg e1; G.Arg (G.L (G.String (re, _))) ], _) ) -> (
      (* alt: take the text range of the metavariable in the original file,
       * and enforce e1 can only be an Id metavariable.
       * alt: let s = value_to_string v in
       * to convert anything in a string before using regexps on it
       *)
      let v = eval env e1 in

      match v with
      | String s ->
          (* todo? factorize with Matching_generic.regexp_matcher_of_regexp_.. *)
          (* use of `ANCHORED to simulate Python re.match() (vs re.search) *)
          let regexp = Pcre.regexp ~flags:[ `ANCHORED ] re in
          let res = Pcre.pmatch ~rex:regexp s in
          let v = Bool res in
          logger#info "regexp %s on %s return %s" re s (show_value v);
          v
      | _ -> raise (NotHandled code) )
  | _ -> raise (NotHandled code)

and eval_op op values code =
  match (op, values) with
  | G.Gt, [ Int i1; Int i2 ] -> Bool (i1 > i2)
  | G.GtE, [ Int i1; Int i2 ] -> Bool (i1 >= i2)
  | G.Lt, [ Int i1; Int i2 ] -> Bool (i1 < i2)
  | G.LtE, [ Int i1; Int i2 ] -> Bool (i1 <= i2)
  (* todo: we should perform cast and allow to mix Float and Int *)
  | G.Gt, [ Float i1; Float i2 ] -> Bool (i1 > i2)
  | G.GtE, [ Float i1; Float i2 ] -> Bool (i1 >= i2)
  | G.Lt, [ Float i1; Float i2 ] -> Bool (i1 < i2)
  | G.LtE, [ Float i1; Float i2 ] -> Bool (i1 <= i2)
  | G.And, [ Bool b1; Bool b2 ] -> Bool (b1 && b2)
  | G.Or, [ Bool b1; Bool b2 ] -> Bool (b1 || b2)
  | G.Not, [ Bool b1 ] -> Bool (not b1)
  | G.Plus, [ Int i1 ] -> Int i1
  | G.Plus, [ Int i1; Int i2 ] -> Int (i1 + i2)
  | G.Minus, [ Int i1 ] -> Int (-i1)
  | G.Minus, [ Int i1; Int i2 ] -> Int (i1 - i2)
  | G.Mult, [ Int i1; Int i2 ] -> Int (i1 * i2)
  | G.Div, [ Int i1; Int i2 ] -> Int (i1 / i2)
  | G.Mod, [ Int i1; Int i2 ] -> Int (i1 mod i2)
  (* todo: we should perform automatic cast and allow to mix Float and Int *)
  | G.Plus, [ Float i1 ] -> Float i1
  | G.Plus, [ Float i1; Float i2 ] -> Float (i1 +. i2)
  | G.Minus, [ Float i1 ] -> Float (-.i1)
  | G.Minus, [ Float i1; Float i2 ] -> Float (i1 -. i2)
  | G.Mult, [ Float i1; Float i2 ] -> Float (i1 *. i2)
  | G.Div, [ Float i1; Float i2 ] -> Float (i1 /. i2)
  (* abuse generic =. Not that this will prevent
   * Int 0 to be equal to Float 0.0.
   * Again need automatic cast.
   *)
  | G.Eq, [ v1; v2 ] -> Bool (v1 = v2)
  | G.NotEq, [ v1; v2 ] -> Bool (v1 <> v2)
  | G.In, [ v1; v2 ] -> (
      match v2 with List xs -> Bool (List.mem v1 xs) | _ -> Bool false )
  | _ -> raise (NotHandled code)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* This is when called from the semgrep Python wrapper for the
 * metavariable-comparison: condition.
 *)
let eval_json_file file =
  try
    let env, code = parse_json file in
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
    let env, code = parse_json file in
    let res = eval env code in
    print_result (Some res)
  with NotHandled e ->
    pr2 (G.show_expr e);
    raise (NotHandled e)

(* when called from the new semgrep-full-rule-in-ocaml *)

let bindings_to_env xs =
  xs
  |> Common.map_filter (fun (mvar, mval) ->
         match mval with
         | MV.E e -> (
             try Some (mvar, eval (Hashtbl.create 0) e)
             with NotHandled _e ->
               logger#debug "can't eval %s value %s" mvar (MV.show_mvalue mval);
               (* todo: if not a value, could default to AST of range *)
               None )
         | x ->
             logger#debug "filtering mvar %s, not an expr %s" mvar
               (MV.show_mvalue x);
             None)
  |> Common.hash_of_list

(* this is for metavariable-regexp *)
let bindings_to_env_with_just_strings xs =
  xs
  |> List.map (fun (mvar, mval) ->
         let any = MV.mvalue_to_any mval in
         let min, max = Visitor_AST.range_of_any any in
         let file = min.Parse_info.file in
         let range = Range.range_of_token_locations min max in
         (mvar, String (Range.content_at_range file range)))
  |> Common.hash_of_list

let eval_bool env e =
  try
    let res = eval env e in
    match res with
    | Bool b -> b
    | _ -> failwith (spf "not a boolean: %s" (show_value res))
  with
  | Not_found ->
      (* this can be because a metavar is binded to a complex expression,
       * e.g., os.getenv("foo") which can't be evaluated. It's ok to
       * return false then.
       * todo: should reraise?
       *)
      false
  | NotHandled e ->
      pr2 (G.show_expr e);
      raise (NotHandled e)
