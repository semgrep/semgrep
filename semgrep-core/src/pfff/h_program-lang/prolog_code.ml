(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
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

module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * For more information look at h_program-lang/prolog_code.pl
 * and its many predicates.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* mimics prolog_code.pl top comment *)
type fact =
  | At of entity * Common.filename (* readable path *) * int (* line *)
  | Kind of entity * Entity_code.entity_kind

  | Type of entity * string (* could be more structured ... *)

  | Extends of string * string
  | Implements of string * string
  | Mixins of string * string

  | Privacy of entity * Entity_code.privacy

  (* direct use of entities, e.g. foo() *)
  | Call of entity * entity
  | UseData of entity * entity * bool option (* read/write *)
  (* indirect uses of entities, e.g. xxx.f = &foo; *)
  | Special of entity (* enclosing *) *
               entity (* ctx entity, e.g. function/field/global *) *
               entity (* the value *) *
               string (* field/function *)

  | Misc of string

(* todo? could use a record with
 *  namespace: string list;
 *  enclosing: string option;
 *  name: string
*)
and entity =
  string list (* package/module/namespace/class/struct/type qualifier*) *
  string (* name *)


(*****************************************************************************)
(* IO *)
(*****************************************************************************)
(* todo: hmm need to escape x no? In OCaml toplevel values can have a quote
 * in their name, like foo'', which will not work well with Prolog atoms.
*)

(* http://pleac.sourceforge.net/pleac_ocaml/strings.html *)
let escape charlist str =
  let rx = Str.regexp ("\\([" ^ charlist ^ "]\\)") in
  Str.global_replace rx "\\\\\\1" str

let escape_quote_and_double_quote s = escape "'\"" s

let string_of_entity (xs, x) =
  match xs with
  | [] -> spf "'%s'" (escape_quote_and_double_quote x)
  | xs -> spf "('%s', '%s')" (Common.join "." xs)
            (escape_quote_and_double_quote x)

(* Quite similar to database_code.string_of_id_kind, but with lowercase
 * because of prolog atom convention. See also prolog_code.pl comment
 * about kind/2.
*)
let string_of_entity_kind = function
  | E.Function -> "function"
  | E.Constant -> "constant"
  | E.Global -> "global"
  | E.Macro -> "macro"
  | E.Class -> "class"
  | E.Type -> "type"

  | E.Method -> "method"
  | E.ClassConstant -> "constant"
  | E.Field -> "field"
  | E.Constructor -> "constructor"

  | E.TopStmts  -> "stmtlist"
  | E.Other _ -> "idmisc"
  | E.Exception -> "exception"

  | E.Module -> "module"
  | E.Package -> "package"

  | E.Prototype -> "prototype"
  | E.GlobalExtern -> "global_extern"

  | (E.MultiDirs|E.Dir|E.File) ->
      raise Impossible

let string_of_fact fact =
  let s =
    match fact with
    | Kind (entity, kind) ->
        spf "kind(%s, %s)" (string_of_entity entity)
          (string_of_entity_kind kind)
    | At (entity, file, line) ->
        spf "at(%s, '%s', %d)" (string_of_entity entity) file line
    | Type (entity, str) ->
        spf "type(%s, '%s')" (string_of_entity entity)
          (escape_quote_and_double_quote str)

    | Extends (s1, s2) ->
        spf "extends('%s', '%s')" s1 s2
    | Mixins (s1, s2) ->
        spf "mixins('%s', '%s')" s1 s2
    | Implements (s1, s2) ->
        spf "implements('%s', '%s')" s1 s2

    | Privacy (entity, p) ->
        let predicate =
          match p with
          | E.Public -> "is_public"
          | E.Private -> "is_private"
          | E.Protected -> "is_protected"
        in
        spf "%s(%s)" predicate (string_of_entity entity)

    (* less: depending on kind of e1 we could have 'method' or 'constructor'*)
    | Call (e1, e2) ->
        spf "docall(%s, %s)"
          (string_of_entity e1) (string_of_entity e2)
    | UseData (e1, e2, b) ->
        spf "use(%s, %s, %s)"
          (string_of_entity e1) (string_of_entity e2)
          (match b with
           | None -> "na"
           | Some true -> "write"
           | Some false -> "read"
          )
    | Special (e1, e2, e3, str) ->
        spf "special(%s, %s, %s, '%s')"
          (string_of_entity e1)
          (string_of_entity e2)
          (string_of_entity e3)
          str

    | Misc s -> s
  in
  s ^ "."

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let entity_of_str s =
  let xs = Common.split "\\." s in
  match List.rev xs with
  | [] -> raise Impossible
  | [x] -> ([], x)
  | x::xs -> (List.rev xs, x)
