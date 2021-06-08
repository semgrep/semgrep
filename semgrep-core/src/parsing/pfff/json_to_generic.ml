(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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
open Ast_json
module M = Map_AST
module G = AST_generic

let expr x =
  let e = Js_to_generic.expr x in

  let visitor =
    M.mk_visitor
      {
        M.default_visitor with
        M.kexpr =
          (fun (k, _) e ->
            (* apply on children *)
            let e = k e in
            match e with
            | Record (lp, xs, rp) ->
                let ys =
                  xs
                  |> List.map (function
                       | G.FieldStmt
                           {
                             s =
                               G.DefStmt
                                 ( { G.name = G.EN (G.Id (id, _)); _ },
                                   FieldDefColon { vinit = Some e; _ } );
                             _;
                           } ->
                           Left (id, e)
                       | G.FieldStmt { s = ExprStmt (Ellipsis t, _); _ } ->
                           Right t
                       | x ->
                           failwith
                             (spf "not a JSON field: %s" (G.show_field x)))
                in
                let zs =
                  ys
                  |> List.map (function
                       | Left (id, e) ->
                           let key =
                             (* we don't want $FLD: 1 to be transformed
                              * in "$FLD" : 1, which currently would not match
                              * anything in Semgrep (this may change though) *)
                             if AST_generic_.is_metavar_name (fst id) then
                               G.N (G.Id (id, G.empty_id_info ()))
                             else G.L (G.String id)
                           in
                           G.Tuple (G.fake_bracket [ key; e ])
                       | Right t -> G.Ellipsis t)
                in
                G.Container (G.Dict, (lp, zs, rp))
            | x -> x);
      }
  in
  visitor.M.vexpr e

let program ast =
  let e = expr ast in
  [ G.exprstmt e ]

let any x =
  match x with
  | E e -> G.E (expr e)
  | PartialSingleField (v1, v2, v3) ->
      G.Partial (G.PartialSingleField (v1, v2, expr v3))
