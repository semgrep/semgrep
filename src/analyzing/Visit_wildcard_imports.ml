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

(* alt:
   class ['self] import_visitor =
     object (_self : 'self)
       inherit ['self] AST_generic.iter

       method! visit_directive_kind store dk =
         match dk with
         | ImportAll (_t, DottedName name, _t') -> Common.push name store
         | ImportAll (_, FileName _, _)
         | _ ->
             ()
     end

   let visit : AST_generic.any -> G.ident list list =
     let v = new import_visitor in
     fun any ->
       let ids = ref [] in
       v#visit_any ids any;
       !ids
     [@@profiling]

   This is more intensive, and will result in us tracking all wildcard imports,
   even those occurring not at the top level of the program.

   To make sure we don't affect perf, and so we don't have more false positives,
   let's not consider those.

   UPDATE: we now have LocalImportAll in AST_generic.ml and we perform
   scoped wildcard imports in Generic_vs_generic.ml
*)

let visit_toplevel prog : G.dotted_ident list =
  (* we only collect top-level wildcard imports, for the reasons
     discussed above
  *)
  List_.fold_right
    (fun x acc ->
      match x with
      | { G.s = DirectiveStmt { d = ImportAll (_t, modname, _t'); _ }; _ } -> (
          match modname with
          | DottedName name -> name :: acc
          | FileName _ -> acc)
      | _ -> acc)
    prog []
