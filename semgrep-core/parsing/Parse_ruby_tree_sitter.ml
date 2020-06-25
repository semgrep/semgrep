(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Ruby parser using ocaml-tree-sitter-lang/ruby and converting
 * to pfff/lang_ruby/parsing/ast_ruby.ml
 * This can then be converted to the generic AST by using
 * pfff/lang_ruby/analyze/ruby_to_generic.ml
 *)

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* This was started by copying ocaml-tree-sitter-lang/ruby/Boilerplate.ml *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  (* TODO: tree-sitter bindings are buggy so we cheat and fork to
   * avoid segfaults to popup. See Main.ml test_parse_ruby function.
   *)
   let _cst =
      if false
      then Tree_sitter_ruby.Parse.file file
      else begin
         Parallel.backtrace_when_exn := false;
         Parallel.invoke Tree_sitter_ruby.Parse.file file ()
      end
   in
   failwith "Todo"
