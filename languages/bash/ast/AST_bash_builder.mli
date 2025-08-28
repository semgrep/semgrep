(*
   Copyright (c) 2023-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val blist_as_expression : AST_bash.blist -> AST_bash.expression option

val add_redirects_to_last_command_of_pipeline :
  AST_bash.pipeline -> AST_bash.redirect list -> AST_bash.pipeline

val concat_blists : AST_bash.blist list -> AST_bash.blist
