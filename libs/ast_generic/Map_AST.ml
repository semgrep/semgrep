(* Yoann Padioleau
 *
 * Copyright (C) 2019 r2c
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
open AST_generic
module G = AST_generic

(*****************************************************************************)
(* Fix token locations *)
(*****************************************************************************)

let fix_token_locations_visitor =
  object (_self : 'self)
    inherit [_] AST_generic.map_legacy as super

    method! visit_id_info _fix ii =
      (* The id_info contains locations that should not be modified, and they
       * are likely outside the sub-AST of interest anyways. *)
      ii

    method! visit_expr fix e =
      super#visit_expr fix
        { e with e_range = Option.map (fun (x, y) -> (fix x, fix y)) e.e_range }

    method! visit_stmt fix s =
      super#visit_stmt fix
        { s with s_range = Option.map (fun (x, y) -> (fix x, fix y)) s.s_range }

    method! visit_tok fix t = Tok.fix_location fix t
  end
