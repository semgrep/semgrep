(* Brandon Wu
 *
 * Copyright (C) 2023 r2c
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
          Common.push (id, id_info) store
      | PatLiteral _
      | PatConstructor _
      | PatRecord _
      | PatTuple _
      | PatList _
      | PatKeyVal _
      | PatUnderscore _
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
  let ids = ref [] in
  fun any ->
    v#visit_any ids any;
    !ids
  [@@profiling]
