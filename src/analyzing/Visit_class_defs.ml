(*
 * Copyright (C) 2022 r2c
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

class ['self] visitor =
  object (_self : 'self)
    inherit [_] G.iter_no_id_info as super

    method! visit_definition f ((ent, def_kind) as def) =
      match def_kind with
      | G.ClassDef cdef ->
          f (Some ent) cdef;
          super#visit_class_definition f cdef
      | __else__ -> super#visit_definition f def

    method! visit_class_definition f def =
      f None def;
      super#visit_class_definition f def
  end

let visitor_instance = lazy (new visitor)

(* Visit all class definitions in an AST. *)
let visit (f : G.entity option -> G.class_definition -> unit) (ast : G.program)
    : unit =
  let (lazy v) = visitor_instance in
  (* Check each class definition. *)
  v#visit_program f ast
