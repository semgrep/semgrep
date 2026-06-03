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

(** Reduce a Dart import URI's dotted segments to the conventional
    library-prefix basename. Used by Naming_AST when resolving
    `import '...' as p;` so a rule pattern written against the
    conventional prefix matches code that aliases the same library
    under any local name. See the .ml for the convention (Dart spec
    §17.2) and the relative-path / `package:` / `dart:` handling. *)
val dart_canonical_segments :
  (string * 'tok) list -> (string * 'tok) list
