(* Iago Abal
 *
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
module H = AST_generic_helpers

class ['self] visitor =
  object (self : 'self)
    inherit [_] G.iter_no_id_info as super

    method! visit_definition f ((ent, def_kind) as def) =
      match def_kind with
      | G.FuncDef fdef ->
          f (Some ent) fdef;
          (* Go into nested functions
             but do NOT revisit the function definition again! *)
          let body = H.funcbody_to_stmt fdef.G.fbody in
          self#visit_stmt f body
      | __else__ -> super#visit_definition f def

    method! visit_function_definition f fdef =
      f None fdef;
      (* go into nested functions *)
      super#visit_function_definition f fdef
  end

let visitor_instance = lazy (new visitor)

(* Visit all function definitions in an AST. *)
let visit (f : G.entity option -> G.function_definition -> unit)
    (ast : G.program) : unit =
  let (lazy v) = visitor_instance in
  (* Check each function definition. *)
  v#visit_program f ast
