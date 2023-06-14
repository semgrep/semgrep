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
