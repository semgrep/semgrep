(* Copyright (C) 2020-2023 r2c
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

(* In Angular JS, we have some "Injectable" classes, which are marked with an
   @Injectable decorator.
   https://angular.io/guide/dependency-injection-in-action
   These classes may reference parameters to the constructor of the class, outside
   of the actual code of the constructor itself.
   So we must add them to the scope, should we find the decorator and a constructor's
   parameters.
   This also works for `@Component`.
*)
let is_js_angular_decorator s =
  match s with
  | "Injectable"
  | "Component" ->
      true
  | _else_ -> false

(* This extracts package aliases from Go import specifiers that users may find
 * convenient for rule-writing purposes. *)
let go_package_alias s =
  let pkgpath, pkgbase = Common2.dirs_and_base_of_file s in
  if pkgbase =~ "v[0-9]+" then
    (* e.g. google.golang.org/api/youtube/v3 *)
    match pkgpath with
    | [] -> pkgbase
    | _else_ -> Common2.list_last pkgpath
  else if pkgbase =~ "\\(.+\\)-go" then
    (* e.g. github.com/dgrijalva/jwt-go *)
    matched1 pkgbase
  else (* default convention *)
    pkgbase

(* Pick the canonical library-prefix name from a Dart import URI's dotted
 * segments. The Dart language spec (§17.2 "URI references in imports")
 * does not define a "package name", but real-world Dart code follows the
 * convention that the library prefix matches the imported file's basename
 * without its `.dart` extension — e.g. `import 'package:http/http.dart'
 * as http;`, `import 'dart:async' as async;`. We mirror that convention
 * so a pattern written as `http.get(...)` matches code that imports
 * `package:http/http.dart` under any local alias.
 *
 * Input: dotted segments produced by Parse_dart_tree_sitter.map_uri,
 * e.g. ["package"; "http"; "http.dart"] or ["dart"; "async"]. For
 * `package:` and `dart:` URIs, map_uri splits on `/` so the last segment
 * is already path-component-free (`"http.dart"`, `"async"`). For
 * relative-path imports like `'./util/helper.dart'`, map_uri's
 * else-branch leaves the whole URI as a single segment containing `/`
 * characters, so we run Filename.basename before chop_extension to
 * isolate `helper` from `./util/helper.dart`.
 *
 * Output: the conventional library prefix as a single segment,
 * e.g. ["http"] or ["async"]. Falls back to the original segments when
 * the input has no recognizable basename (defensive — keeps name
 * resolution at parity with the previous behavior).
 *)
let dart_canonical_segments (xs : (string * 'tok) list) :
    (string * 'tok) list =
  match List.rev xs with
  | [] -> xs
  | (last_str, t) :: _ ->
      let base = Filename.basename last_str in
      let stem =
        try Filename.chop_extension base with Invalid_argument _ -> base
      in
      if stem = "" then xs else [ (stem, t) ]
