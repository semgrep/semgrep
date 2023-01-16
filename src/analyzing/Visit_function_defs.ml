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
module V = Visitor_AST

(* Visit all function definitions in an AST. *)
let visit (f : G.entity option -> G.function_definition -> unit)
    (ast : G.program) : unit =
  let v =
    V.mk_visitor
      {
        V.default_visitor with
        V.kdef =
          (fun (k, v) ((ent, def_kind) as def) ->
            match def_kind with
            | G.FuncDef fdef ->
                f (Some ent) fdef;
                (* go into nested functions
                   but do NOT revisit the function definition again
                   with `kfunction_definition` below! *)
                let body = H.funcbody_to_stmt fdef.G.fbody in
                v (G.S body)
            | __else__ -> k def);
        V.kfunction_definition =
          (fun (k, _v) def ->
            f None def;
            (* go into nested functions *)
            k def);
      }
  in
  (* Check each function definition. *)
  v (G.Pr ast)
