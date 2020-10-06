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

module MV = Metavars_generic
module J = JSON

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A simple interpreter for a simple subset of the generic AST.
 *
 * This can be used to safely execute a subset of pattern-where-python:
 * expressions.
 *)

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* This is the (partially parsed) content of a metavariable *)
type value =
  | Int of int
  | Bool of bool
  (* less: Float? *)
  | String of string (* string without the enclosing '"' *)
  | List of value list
  (* default case where we don't really have good builtin operations.
   * This should be a AST_generic.any once parsed.
   * See JSON_report.json_metavar().
   *)
  | AST of string (* any AST, e.g., "x+1" *)
  (* lesS: Id of string (* simpler to merge with AST *) *)

type env = (MV.mvar, value) Hashtbl.t

(* we restrict ourselves to simple expression for now *)
type code = AST_generic.expr

exception NotHandled of code

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)
let parse_metavar s =
  match s with
  | "true" | "True" -> Bool (true)
  | "false" | "False" -> Bool (false)
  | _ when s =~ "^[0-9]+$" -> Int (int_of_string s)
  | _ when s =~ "^\"\\(.*\\)\"$" -> String (Common.matched1 s)
  | _ -> AST s

let parse_json file =
  let json = JSON.load_json file in
  match json with
  | J.Object [
      "metavars", J.Object xs;
      "language", J.String lang;
      "code", J.String code;
      ] ->
      let lang = Hashtbl.find Lang.lang_of_string_map lang in
      (* less: could also use Parse_pattern *)
      let code =
        match Parse_generic.parse_pattern lang code with
        | G.E e -> e
        | _ -> failwith "only expressions are supported"
      in
      let metavars = xs |> List.map (fun (s, json) ->
            match json with
            | J.String s2 -> s, parse_metavar s2
            | _ -> failwith "wrong json format for metavar"
       ) in
      Common.hash_of_list metavars, code

  | _ -> failwith "wrong json format"

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
(* Entry point *)
(*****************************************************************************)

let rec eval env code =
  match code with
  | G.L x ->
      (match x with
      | G.Bool (b, _t) -> Bool (b)
      | G.Int (s, _t) -> Int (int_of_string s)
      | G.String (s, _t) -> String s
      | _ -> raise (NotHandled code)
      )
  | G.Id ((s, _t), _idinfo) ->
      Hashtbl.find env s
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
  | _ -> raise (NotHandled code)

and eval_op op values code =
  match op, values with
  | G.Gt, [Int i1; Int i2] -> Bool (i1 > i2)
  | G.GtE, [Int i1; Int i2] -> Bool (i1 >= i2)
  | G.Lt, [Int i1; Int i2] -> Bool (i1 < i2)
  | G.LtE, [Int i1; Int i2] -> Bool (i1 <= i2)

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

  | G.Eq, [v1; v2] -> Bool (v1 = v2)
  | G.NotEq, [v1; v2] -> Bool (v1 <> v2)

  | _ -> raise (NotHandled code)

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
