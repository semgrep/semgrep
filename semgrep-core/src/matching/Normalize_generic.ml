(* Yoann Padioleau
 *
 * Copyright (C) 2019-2021 r2c
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
open Common (* >>= *)
open AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Various helper functions to normalize AST elements.
 *
 * TODO: merge with pfff/.../normalize_ast.ml at some point
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

let full_module_name is_pattern from_module_name import_opt =
  match (from_module_name, import_opt) with
  | DottedName idents, Some import_ident_name ->
      let new_module_name : dotted_ident = idents @ [ import_ident_name ] in
      Some (DottedName new_module_name)
  | DottedName idents, None -> Some (DottedName idents)
  | FileName s, None -> Some (FileName s)
  | FileName s, _ when not is_pattern ->
      (* bugfix: for languages such as JS, 'import x from "path"' should not
       * be converted in just "path". We should return None here as it
       * does not make sense to allow this pattern to match
       * import y from "path". Use just 'import "path"' if you just want
       * to check you vaguely imported a package.
       *)
      Some (FileName s)
  | FileName _, Some _ -> None

let normalize_import_opt is_pattern i =
  match i with
  | ImportFrom (t, module_name, m, _alias_opt) ->
      full_module_name is_pattern module_name (Some m) >>= fun x -> Some (t, x)
  | ImportAs (t, module_name, _alias_opt) ->
      full_module_name is_pattern module_name None >>= fun x -> Some (t, x)
  | ImportAll (t, module_name, _t2) ->
      full_module_name is_pattern module_name None >>= fun x -> Some (t, x)
  | Package _
  | PackageEnd _
  | Pragma _
  | OtherDirective _ ->
      None
