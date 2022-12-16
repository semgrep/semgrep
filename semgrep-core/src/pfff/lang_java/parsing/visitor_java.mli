(* Copyright (C) 2012 Facebook
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

open Ast_java

type visitor_in = {
  kident:   (ident       -> unit) * visitor_out -> ident       -> unit;
  kexpr:    (expr        -> unit) * visitor_out -> expr        -> unit;
  kstmt:    (stmt        -> unit) * visitor_out -> stmt        -> unit;
  ktype:    (typ         -> unit) * visitor_out -> typ         -> unit;
  kvar:     (var_definition -> unit) * visitor_out -> var_definition -> unit;
  kinit:    (init        -> unit) * visitor_out -> init        -> unit;
  kmethod:  (method_decl -> unit) * visitor_out -> method_decl -> unit;
  kfield:   (field       -> unit) * visitor_out -> field       -> unit;
  kclass:   (class_decl  -> unit) * visitor_out -> class_decl  -> unit;
  kdecl:    (decl        -> unit) * visitor_out -> decl        -> unit;
  kprogram: (program     -> unit) * visitor_out -> program     -> unit;

  kinfo: (tok -> unit) * visitor_out -> tok -> unit;
}

and visitor_out = any -> unit

val default_visitor : visitor_in

val mk_visitor : visitor_in -> visitor_out
