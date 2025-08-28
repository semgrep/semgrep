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
(* We expose these language-specific functions here because they are needed in
 * Semgrep Pro.
 *
 * This lets us avoid having to duplicate the logic of these functions. *)
val is_js_angular_decorator : string -> bool
val go_package_alias : string -> string
