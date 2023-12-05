(* Yoann Padioleau
 *
 * Copyright (C) 2023 Semgrep Inc.
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
module V = Value_jsonnet

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Helpers common to Eval_jsonnet.ml and Eval_jsonnet_subst.ml *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

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

let sv v =
  let s = V.show v in
  if String.length s > 100 then Str.first_chars s 100 ^ "..." else s

let int_to_cmp = function
  | -1 -> Inf
  | 0 -> Eq
  | 1 -> Sup
  (* all the OCaml Xxx.compare should return only -1, 0, or 1 *)
  | _else_ -> assert false

let log_call (env : V.env) str tk =
  Logs.debug (fun m ->
      m "calling %s> %s at %s"
        (Common2.repeat "-" env.depth |> String.concat "")
        str (Tok.stringpos_of_tok tk))

(*****************************************************************************)
(* Builtins *)
(*****************************************************************************)

(* alt: could move to Value_jsonnet.ml *)
let std_type (v : V.t) : string =
  match v with
  | V.Primitive (Null _) -> "null"
  | V.Primitive (Bool _) -> "boolean"
  | V.Primitive (Double _) -> "number"
  | V.Primitive (Str _) -> "string"
  | V.Object _ -> "object"
  | V.Array _ -> "array"
  | V.Lambda _ -> "function"

let std_primivite_equals (v : V.t) (v' : V.t) : bool =
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
