(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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
open Ast_json
module PI = Parse_info
module M = Map_AST
module G = AST_generic

let parse_json_string json =
  try
    Some
      (Yojson.Safe.read_string
         (Yojson.init_lexer ~buf:(Bi_outbuf.create 128) ())
         (Lexing.from_string json))
  with
  | Yojson.Json_error _ -> None

(* This is not very efficient but yojson doesn't provide a function to parse
   the string contents without quotes. *)
let unescape_json_string_contents data = parse_json_string ("\"" ^ data ^ "\"")

let expr ?(unescape_strings = false) x =
  let e = Js_to_generic.expr x in

  let visitor =
    M.mk_visitor
      {
        M.default_visitor with
        M.kexpr =
          (fun (k, _) e ->
            (* apply on children *)
            let e = k e in
            match e.G.e with
            | Record (lp, xs, rp) ->
                let ys =
                  xs
                  |> Common.map (function
                       | G.F
                           {
                             s =
                               G.DefStmt
                                 ( { G.name = G.EN (G.Id (id, _)); _ },
                                   FieldDefColon { vinit = Some e; _ } );
                             _;
                           } ->
                           Left (id, e)
                       | G.F { s = ExprStmt ({ e = Ellipsis t; _ }, _); _ } ->
                           Right t
                       | x ->
                           failwith
                             (spf "not a JSON field: %s" (G.show_field x)))
                in
                let zs =
                  ys
                  |> Common.map (function
                       | Left (id, e) ->
                           let key =
                             (* we don't want $FLD: 1 to be transformed
                              * in "$FLD" : 1, which currently would not match
                              * anything in Semgrep (this may change though) *)
                             if AST_generic_.is_metavar_name (fst id) then
                               G.N (G.Id (id, G.empty_id_info ())) |> G.e
                             else G.L (G.String id) |> G.e
                           in
                           G.Container
                             (G.Tuple, PI.unsafe_fake_bracket [ key; e ])
                           |> G.e
                       | Right t -> G.Ellipsis t |> G.e)
                in
                G.Container (G.Dict, (lp, zs, rp)) |> G.e
            | G.L (G.String (escaped, t)) when unescape_strings ->
                let unescaped =
                  match unescape_json_string_contents escaped with
                  | Some s -> s
                  | None ->
                      (* This really should not happen, but this is also
                         decent error recovery.
                         Compare with: assert false *)
                      escaped
                in
                G.L (G.String (unescaped, t)) |> G.e
            | _ -> e);
      }
  in
  visitor.M.vexpr e

let program ?unescape_strings ast =
  ignore unescape_strings;
  let e = expr ?unescape_strings ast in
  [ G.exprstmt e ]

let any x =
  match x with
  | E e -> G.E (expr e)
  | PartialSingleField (v1, _v2, v3) ->
      (* This code copies how the expr function translates fields of a dictionary.
         This is important because metavariables need to be properly translated to
         allow unification. *)
      let key =
        if AST_generic_.is_metavar_name (fst v1) then
          G.N (G.Id (v1, G.empty_id_info ())) |> G.e
        else G.L (G.String v1) |> G.e
      in
      G.E (G.Container (G.Tuple, PI.unsafe_fake_bracket [ key; expr v3 ]) |> G.e)
