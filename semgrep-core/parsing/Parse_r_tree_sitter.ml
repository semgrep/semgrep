(* Ross Nanopoulos (zythosec)
 *
 * Copyright (c) 2020
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
module H = Parse_tree_sitter_helpers


(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () ->
       Parallel.backtrace_when_exn := false;
       Parallel.invoke Tree_sitter_r.Parse.file file ()
    )
