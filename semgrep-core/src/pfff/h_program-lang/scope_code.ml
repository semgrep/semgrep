(* Yoann Padioleau
 *
 * Copyright (C) 2009-2010 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * It would be more convenient to move this file elsewhere like in analyse_xxx/
 * but we want our AST to contain scope annotations so it's convenient to
 * have the type definition of scope there.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* todo? could use open polymorphic variant for that ? the scoping will
 * be differerent for each language but they will also have stuff
 * in common which may be a good spot for open polymorphic variant.
*)
type t =
  | Global
  | Local
  | Param
  | Static

  | Class

  | LocalExn
  | LocalIterator

  (* php specific? *)
  | ListBinded
  (* closure, could be same as Local, but can be good to visually
   * differentiate them in codemap
  *)
  | Closed

  | NoScope

(*****************************************************************************)
(* String-of *)
(*****************************************************************************)

let string_of_scope = function
  | Global ->  "Global"
  | Local ->  "Local"
  | Param ->  "Param"
  | Static ->  "Static"
  | Class ->  "Class"
  | LocalExn ->  "LocalExn"
  | LocalIterator ->  "LocalIterator"
  | ListBinded ->  "ListBinded"
  | Closed ->  "Closed"
  | NoScope ->  "NoScope"

(*****************************************************************************)
(* Meta *)
(*****************************************************************************)

let vof_scope x =
  match x with
  | Global -> OCaml.VSum ("Global", [])
  | Local -> OCaml.VSum ("Local", [])
  | Param -> OCaml.VSum ("Param", [])
  | Static -> OCaml.VSum ("Static", [])
  | Class -> OCaml.VSum ("Class", [])
  | LocalExn -> OCaml.VSum ("LocalExn", [])
  | LocalIterator -> OCaml.VSum ("LocalIterator", [])
  | ListBinded -> OCaml.VSum ("ListBinded", [])
  | Closed -> OCaml.VSum ("Closed", [])
  | NoScope -> OCaml.VSum ("NoScope", [])
