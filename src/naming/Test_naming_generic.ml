(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
module H = AST_generic_helpers

let test_naming_generic ~parse_program file =
  let ast = parse_program file in
  let lang = Lang.lang_of_filename_exn file in
  Naming_AST.resolve lang ast;
  let s = AST_generic.show_any (AST_generic.Pr ast) in
  UCommon.pr2 s

let actions ~parse_program =
  [
    ( "-naming_generic",
      " <file>",
      Arg_.mk_action_1_conv Fpath.v (test_naming_generic ~parse_program) );
  ]
