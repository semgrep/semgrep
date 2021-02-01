(*s: semgrep/engine/Convert_rule.ml *)
(*s: pad/r2c copyright *)
(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
(*e: pad/r2c copyright *)
open Common

open Rule

let logger = Logging.get_logger [__MODULE__]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* The goal of this module is to convert rules in whatever format to
 * the latest format. Its main goal is to maintain backward compatibility.
 * For actual optimizations or complex transformations on rules, see
 * Transform_rule.ml.
 *
*)

let convert_extra x =
  let s =
    match x with
    | MetavarRegexp (mvar, re_str) ->
        (* less: we could use re.match(), to be close to python, but really
         * Eval_generic must do something special here with the metavariable
         * which may not always be a string. The regexp is really done on
         * the text representation of the metavar content.
        *)
        spf "semgrep_re_match(%s, \"%s\")" mvar re_str
    | _ -> failwith (spf "convert_extra: TODO: %s" (Rule.show_extra x))
  in
  logger#debug "convert_extra: %s" s;
  Parse_rule.parse_metavar_cond s

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let (convert_formula_old: formula_old -> formula) = fun e ->
  let rec aux e =
    match e with
    | Pat x | PatInside x -> P x
    | PatNot x | PatNotInside x -> Not (P x)
    | PatEither xs ->
        let xs = List.map aux xs in
        Or xs
    | Patterns xs ->
        let xs = List.map aux xs in
        And xs
    | PatExtra x ->
        let e = convert_extra x in
        MetavarCond e
  in
  aux e

(*e: semgrep/engine/Convert_rule.ml *)
