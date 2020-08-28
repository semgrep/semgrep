(* Yoann Padioleau
 *
 * Copyright (C) 2010-2011 Facebook
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

open Cst_php

module Ast = Cst_php
module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * todo: differentiate Interface/Trait in lookup?
 *)

(*****************************************************************************)
(* Types and globals *)
(*****************************************************************************)

(* PHP let people intercept a "UndefinedMethod" error, a la Perl ... *)
exception Use__Call
exception UndefinedClassWhileLookup of string

(* Actually sometimes it can also be the name of the class (especially in
 * third party code).
*)
let constructor_name = "__construct"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let equal ~case_insensitive a b =
  if case_insensitive
  then String.lowercase_ascii a =$= String.lowercase_ascii b
  else a =$= b

(* This is ugly. Some of the code requires to have a 'name' type
 * for every "entities" we are defining and checking. For a class
 * constant we should really have a pair of name, one for the class
 * and one for the constant itself. Instead we abuse 'name' and
 * pack into it also the classname.
 *)

(*
let rewrap_name_with_class_name classname name =
  match name with
  | Name (s, info) ->
      let new_str = spf "%s::%s" classname s in
      Name (new_str, Ast.rewrap_str new_str info)
  (* only classnames can be a XhpName. Constants (or functions)
   * are always simple names
   *)
  | XhpName _ ->
      failwith "Impossible: only classes can be XhpName"

let mk_class_name s info =
  Name (s, info)

let resolve_class_name qu =
  match fst qu with
  | ClassName (name) -> Some name
  | Self _ | Parent _ ->
      pr2_once "check_functions_php: call unsugar_self_parent";
      None
  | LateStatic _ ->
      pr2 "late static: can't resolve, add a pattern match for LateStatic";
      None
*)

(*****************************************************************************)
(* Ast Helpers *)
(*****************************************************************************)

let is_static_method def =
  def.f_modifiers |> List.map Ast.unwrap |> List.mem Ast.Static

let has_visiblity_modifier xs =
  xs |> List.map Ast.unwrap |> List.exists (function
  | Public  | Private | Protected -> true
  | _ -> false
  )

let is_interface def =
  match def.c_type with
  | Interface _ -> true
  | _ -> false

(* less: it could also be one which has the same name than the class.
 * print a warning to tell to use __construct instead ?
 *)
let get_constructor def =
  def.c_body |> Ast.unbrace |> Common.find_some (fun class_stmt ->
    match class_stmt with
    | Method def when Ast.str_of_ident def.f_name =$= constructor_name ->
        Some def
    | _ -> None
  )

(* This is used in check_variables_php.ml to allow inherited
 * visible variables to be used in scope
 *)
let get_public_or_protected_vars_of_class def =

  def.c_body |> Ast.unbrace |> Common.map_filter (function
  |  ClassVariables (modifiers, _opt_ty, class_vars, _tok) ->

       let modifiers = Ast.unmodifiers modifiers in

       if List.mem Public modifiers ||
          List.mem Protected modifiers
       then
         let dnames =
           class_vars |> Ast.uncomma |> List.map fst
         in
         Some dnames
       else None

  (* could maybe do something with XhpDecl ? *)
  | _ -> None
  ) |> List.flatten

(* This is useful when one needs to add class variables in scope.
 * Because they may be at the end and that simple algorithm are just
 * one pass on the ast, just simple to reorder the variables so that
 * they are first. See Check_variables_php.
 *)
let class_variables_reorder_first def =
  let (lb, body, rb) = def.c_body in
  let body' =
    let (vars, rest) =
      body |> List.partition (function
      | ClassVariables _ -> true
      | _ -> false
      )
    in
    vars @ rest
  in
  { def with
    c_body = (lb, body', rb);
  }

let class_kind_of_ctype ctype =
  match ctype with
  | ClassRegular _ | ClassFinal _ | ClassAbstract _ | ClassAbstractFinal _ -> E.Class_
  | Interface _ -> E.Interface
  | Trait _ -> E.Trait
  | Enum _ -> E.Enum

let string_of_class_type = function
  | ClassRegular _ | ClassFinal _ | ClassAbstract _ | ClassAbstractFinal _ -> "class"
  | Interface _ -> "interface"
  | Trait _ -> "trait"
  | Enum _ -> "enum"

let interfaces c =
  match c.c_implements with
  | None -> []
  | Some (_, xs) ->
      xs |> Ast.uncomma

let traits c =
  c.c_body |> Ast.unbrace |> Common.map_filter (function
  | UseTrait (_, names, rewrite_rule) ->
      (match rewrite_rule with
      | Left _nowrite -> ()
      | Right _ -> failwith "not handling rewrite rules in traits"
      );
      Some (Ast.uncomma names)
  | _ -> None
  ) |> List.flatten

(*****************************************************************************)
(* Lookup *)
(*****************************************************************************)

(* todo: for privacy aware lookup we will need more context
 * about where is coming from the lookup (from the class itself?).
 *
 * PHP is case insensitive, but we also want our PHP checkers to be
 * case sensitive (in strict mode for instance) hence the parameter below.
 *)
let lookup_gen aclass find_entity hook =

  let find_class_or_trait x =
    let xs = find_entity (E.Class, x) in
    (* todo?
     *  if null xs
     * then find_entity (E.Class E.Trait, x)
     * else xs
     *)
    xs
  in
  (* all those try are ugly, should maybe use the Option monad *)
  let rec aux aclass =
    match find_class_or_trait aclass with
    | [ClassE def] ->
        (try def.c_body |> Ast.unbrace |> Common.find_some hook
        with Not_found ->
          (* traits have priority over inheritance *)
          let xs = traits def in
          (try
            xs |> Common2.return_when (fun trait ->
              let str = Ast.str_of_class_name trait in
              (* recurse *)
              try Some (aux str)
              with Not_found -> None
            )
          with Not_found ->
            (* ok try inheritance now *)
            (match def.c_extends with
            | None -> raise Not_found
            | Some (_, name) ->
                let str = Ast.str_of_class_name name in
                (* recurse *)
                aux str
            )
          )
        )
    | [] -> raise (UndefinedClassWhileLookup aclass)
    | _x::_y::_xs -> raise Multi_found
    | [_] -> raise Impossible
  in
  aux aclass


let lookup_method ?(case_insensitive=false) (aclass, amethod) find_entity =
  let eq = equal ~case_insensitive in
  lookup_gen aclass find_entity
    (function
    | Method def when eq (Ast.str_of_ident def.f_name) amethod -> Some def
    | Method def when (Ast.str_of_ident def.f_name) =$= "__call" ->
        raise Use__Call
    | Method def when (Ast.str_of_ident def.f_name) =$= "__callStatic" ->
        raise Use__Call
    | _ -> None
    )

let lookup_member ?(case_insensitive=false) (aclass, afield) find_entity =
  let eq = equal ~case_insensitive in
  lookup_gen aclass find_entity
    (function
    | ClassVariables (modifier, _opt_ty, class_vars, _tok) ->
        (try
          Some (class_vars |> Ast.uncomma |> Common.find_some
            (fun (dname, affect_opt) ->
              if eq (Ast.str_of_dname dname) afield
              then Some ((dname, affect_opt), modifier)
              else None
            ))
        with Not_found -> None
        )
    | _ -> None
    )

let lookup_constant (aclass, aconstant) find_entity =
  lookup_gen aclass find_entity
    (function
    | ClassConstants (_, _, _, xs, _) ->
        (try
          Some (xs |> Ast.uncomma |> Common.find_some
            (fun (name, affect) ->
              if Ast.str_of_ident name =$= aconstant
              then Some (name, affect)
              else None
            ))
        with Not_found -> None
        )
    | _ -> None
    )


(*****************************************************************************)
(* Collect *)
(*****************************************************************************)

let collect_members aclass find_entity =

  let res = ref [] in
  (try
    let _ = lookup_gen aclass find_entity (function
    | ClassVariables (_, _, class_vars, _) ->
        class_vars |> Ast.uncomma |> List.iter (fun (dname, _affect) ->
          Common.push dname res;
        );
        None
    | _ -> None;
    )
    in
    ()
   with Not_found | UndefinedClassWhileLookup _ | Multi_found ->
    ()
  );
  !res
