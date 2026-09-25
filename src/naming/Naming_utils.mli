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

val dart_canonical_segments : (string * 'tok) list -> (string * 'tok) list
(** Reduce a Dart import URI's dotted segments to the conventional
    library-prefix basename. Used by Naming_AST when resolving
    `import '...' as p;` so a rule pattern written against the
    conventional prefix matches code that aliases the same library
    under any local name. See the .ml for the convention (Dart spec
    §17.2) and the relative-path / `package:` / `dart:` handling. *)

val unsafe_canonicals :
  full_identity:('a -> string) ->
  candidate_canonical:('a -> string) ->
  'a list ->
  string list
(** Among [items], return the candidate canonical names (as computed by
    [candidate_canonical]) that are claimed by more than one distinct
    [full_identity] -- i.e., names a caller must not collapse to, because
    doing so would conflate two genuinely different items (e.g. two
    different Dart packages whose main files share a basename). See the
    .ml for the motivating example and why this is language-agnostic. *)
