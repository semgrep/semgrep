 (* PUT YOUR NAME HERE
  *
  * Copyright (c) PUT YOUR COPYRIGHT HERE
  *
  * This program is free software; you can redistribute it and/or
  * modify it under the terms of the GNU General Public License (GPL)
  * version 2 as published by the Free Software Foundation.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * file license.txt for more details.
  *)
 module CST = Tree_sitter_kotlin.CST
 module H = Parse_tree_sitter_helpers

 (*****************************************************************************)
 (* Prelude *)
 (*****************************************************************************)
 (* Csharp parser using ocaml-tree-sitter-lang/charp and converting
  * directly to pfff/h_program-lang/AST_generic.ml
  *
  *)

 (*****************************************************************************)
 (* Helpers *)
 (*****************************************************************************)
 type env = H.env
 let _fake = AST_generic.fake
 let token = H.token
 let str = H.str


 (*****************************************************************************)
 (* Entry point *)
 (*****************************************************************************)
 let parse file =
   H.wrap_parser
     (fun () ->
        Parallel.backtrace_when_exn := false;
        Parallel.invoke Tree_sitter_csharp.Parse.file file ()
     )
