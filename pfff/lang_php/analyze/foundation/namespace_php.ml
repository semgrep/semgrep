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

open Cst_php 

(* see also scoping_php.ml.
 * redundant ?
 *)

type nameS = 
  (* e.g. foo *)
  | NameS of string
  (* e.g. A::foo *)
  | NameQualifiedS of string * string
  (* e.g. package1\package2\foo 
   * todo: when add PHP 5.3 namespace
   * | NameSpaceQualifiedS of string * nameS
   *)


type dnameS = 
  | DNameS of string (* without the dollar *)


let name_to_nameS_wrap name =
  let s = Cst_php.str_of_name name in
  let ii  = Cst_php.info_of_name name in 
  NameS s, ii

let dnameS_of_dname (DName (s,_ii)) = DNameS s

let nameS x = 
  match x with
  | NameS s -> s
  | NameQualifiedS (aclass, s) ->
      aclass ^ "::" ^ s

let dnameS (DNameS s) = s
