(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 Semgrep Inc.
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
open Common
open AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Various helper functions to normalize AST elements.
 *
 * TODO: merge with normalize_ast.ml from pfff at some point
 *
 *)

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* Normalize imports for matching purposes.
 * Examples (for Python):
 *   from foo import bar -> import foo.bar
 *   from foo.bar import baz -> import foo.bar.baz
 *)

let full_module_names is_pattern from_module_name imports_opt =
  match (from_module_name, imports_opt) with
  | DottedName idents, Some import_ident_names ->
      let new_module_names : module_name list =
        List_.map
          (fun import_ident_name -> DottedName (idents @ [ import_ident_name ]))
          import_ident_names
      in
      Some new_module_names
  | DottedName idents, None -> Some [ DottedName idents ]
  | FileName s, None -> Some [ FileName s ]
  | FileName s, _ when not is_pattern ->
      (* bugfix: for languages such as JS, 'import x from "path"' should not
       * be converted in just "path". We should return None here as it
       * does not make sense to allow this pattern to match
       * import y from "path". Use just 'import "path"' if you just want
       * to check you vaguely imported a package.
       *)
      Some [ FileName s ]
  | FileName _, Some _ -> None

let normalize_import_opt is_pattern i =
  match i with
  | ImportFrom (t, module_name, import_from_kinds) ->
      let imports =
        (* Drop the local aliases *)
        List_.map
          (function
            | Direct (id, _id_info) -> id
            | Aliased (id, _) -> id)
          import_from_kinds
      in
      let* x = full_module_names is_pattern module_name (Some imports) in
      Some (t, x)
  | ImportAs (t, module_name, _alias_opt) ->
      let* x = full_module_names is_pattern module_name None in
      Some (t, x)
  | ImportAll (t, module_name, _t2) ->
      let* x = full_module_names is_pattern module_name None in
      Some (t, x)
  | Package _
  | PackageEnd _
  | Pragma _
  | OtherDirective _ ->
      None
