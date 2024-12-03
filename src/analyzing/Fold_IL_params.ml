(* Iago Abal
 *
 * Copyright (C) 2024 Semgrep Inc.
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

let fold :
    ('acc ->
    AST_generic.ident ->
    AST_generic.id_info ->
    AST_generic.expr option ->
    'acc) ->
    'acc ->
    IL.param list ->
    'acc =
 fun f acc params ->
  (* For each argument, check if it's a source and, if so, add it to the input
     environment. *)
  List.fold_left
    IL.(
      fun acc par ->
        match par with
        | Param { pname = name; pdefault } ->
            f acc name.ident name.id_info pdefault
        (* JS: {arg} : type *)
        | PatternParam
            (G.OtherPat
              ( ("ExprToPattern", _),
                [
                  G.E
                    { e = G.Cast (_, _, { e = G.Record (_, fields, _); _ }); _ };
                ] ))
        (* JS: {arg} *)
        | PatternParam
            (G.OtherPat
              (("ExprToPattern", _), [ G.E { e = G.Record (_, fields, _); _ } ]))
          ->
            List.fold_left
              (fun acc field ->
                match field with
                | G.F
                    {
                      s =
                        G.DefStmt
                          ( _,
                            G.FieldDefColon
                              { vinit = Some { e = G.N (G.Id (id, ii)); _ }; _ }
                          );
                      _;
                    } ->
                    f acc id ii None
                | G.F _ -> acc)
              acc fields
        | PatternParam pat ->
            (* Here, we just get all the identifiers in the pattern, which may
               themselves be sources.
               This is so we can handle patterns such as:
               (x, (y, z, (a, b)))
               and taint all the inner identifiers
            *)
            let ids = Visit_pattern_ids.visit (G.P pat) in
            List.fold_left (fun acc (id, pinfo) -> f acc id pinfo None) acc ids
        | IL.FixmeParam -> acc)
    acc params
