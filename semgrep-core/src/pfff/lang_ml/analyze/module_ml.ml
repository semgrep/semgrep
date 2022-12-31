(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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
open Common

type name = string
type long_name = string list

(* meant to be in a analyze_ml/basic/ at some point *)

let module_name_of_filename file =
  let _d, b, _e = Common2.dbe_of_filename file in
  let module_name = String.capitalize_ascii b in
  module_name

let top_module_of_node (s, kind) =
  if s =~ "^\\([A-Z][A-Za-z0-9_]*\\)" then Common.matched1 s
  else
    failwith
      (spf "could not find top module of %s"
         (Graph_code.string_of_node (s, kind)))
