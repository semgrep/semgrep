(* Brandon Wu
 *
 * Copyright (C) 2023 Semgrep Inc.
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

module G = AST_generic

class ['self] pat_id_visitor =
  object (_self : 'self)
    inherit ['self] AST_generic.iter_no_id_info as super

    method! visit_pattern store pat =
      match pat with
      | PatAs (_, (id, id_info))
      | PatId (id, id_info) ->
          super#visit_pattern store pat;
          Stack_.push (id, id_info) store
      | G.OtherPat (_, [ G.E { e = G.Record (_b1, fields, _b2); _ } ]) ->
          (* JS allows object destructuring in the parameter list. *)
          (* TODO Handle array destructuring *)
          (* TODO Recurse into multi-level destructuring *)
          (* TODO Make this analysis field-sensitive *)
          List.iter
            (function
              | G.F
                  {
                    s =
                      G.DefStmt
                        ( { name = G.EN (G.Id (_id, _idinfo)); _ },
                          G.FieldDefColon
                            {
                              vinit =
                                Some
                                  { e = G.N (G.Id (local_id, local_idinfo)); _ };
                              _;
                            } );
                    _;
                  } ->
                  Stack_.push (local_id, local_idinfo) store
              | _else_ -> ())
            fields
      | PatRecord _
      | PatLiteral _
      | PatConstructor _
      | PatTuple _
      | PatList _
      | PatKeyVal _
      | PatWildcard _
      | PatDisj _
      | PatTyped _
      | PatWhen _
      | PatType _
      | PatEllipsis _
      | DisjPat _
      | OtherPat _ ->
          super#visit_pattern store pat
  end

let visit : AST_generic.any -> (G.ident * G.id_info) list =
  let v = new pat_id_visitor in
  fun any ->
    let ids = ref [] in
    v#visit_any ids any;
    !ids
[@@profiling]
