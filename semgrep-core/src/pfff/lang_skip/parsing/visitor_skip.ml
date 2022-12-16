(* Yoann Padioleau
 *
 * Copyright (C) 2019 Yoann Padioleau
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
open OCaml
open Ast_skip

(* Disable warnings against unused variables *)
[@@@warning "-26"]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* hooks *)
type visitor_in = {
  kinfo: tok vin;
}
and 'a vin = ('a  -> unit) * visitor_out -> 'a  -> unit

and visitor_out = any -> unit


let default_visitor = {
  kinfo   = (fun (k,_) x -> k x);
}


let (mk_visitor: visitor_in -> visitor_out) = fun vin ->

  (* start of auto generation *)

  let rec v_info x =
    let k x = match x with { Parse_info.
                             token = _v_pinfox; transfo = _v_transfo
                           } ->
(*
    let _arg = Parse_info.v_pinfo v_pinfox in
    let _arg = OCaml.v_unit v_comments in
    let _arg = Parse_info.v_transformation v_transfo in
*)
        ()
    in
    vin.kinfo (k, all_functions) x

  and v_tok v = v_info v

  and v_wrap: 'a. ('a -> unit) -> 'a wrap -> unit = fun  _of_a (v1, v2) ->
    let v1 = _of_a v1 and v2 = v_info v2 in ()

  and v_program v = v_unit v

  and v_any = function
    | Program _ -> ()
    | Info v1 -> let v1 = v_info v1 in ()
  and all_functions x = v_any x
  in
  v_any
