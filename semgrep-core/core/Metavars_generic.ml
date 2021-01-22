(*s: semgrep/core/Metavars_generic.ml *)
(*s: pad/r2c copyright *)
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
(*e: pad/r2c copyright *)
open Printf
open Common
module G = AST_generic

(* Provide hash_* and hash_fold_* for the core ocaml types *)
open Ppx_hash_lib.Std.Hash.Builtin

let debug = false

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* less: could want to remember the position in the pattern of the metavar
 * for error reporting on pattern itself? so use a 'string AST_generic.wrap'?
*)
(*s: type [[Metavars_generic.mvar]] *)
type mvar = string
(*e: type [[Metavars_generic.mvar]] *)
[@@deriving show, eq, hash]

(* 'mvalue' below used to be just an alias to AST_generic.any, but it is more
 * precise to have a type just for the metavariable values; we do not
 * need all the AST_generic.any cases (however this forces us to
 * define a few boilerplate functions like mvalue_to_any below).
 *
 * AST_generic.any is already (ab)used for many things: for representing
 * a semgrep pattern, for being able to dump any AST constructs,
 * for poor's man overloading for visiting, mapping, so there's no
 * need to add an extra thing. It would probably be better to also
 * define our own Pattern.t with just the valid cases, but we don't
 * want code in pfff to depend on semgrep/core/Pattern.ml, hence the
 * use of AST_generic.any for patterns.
*)
type mvalue =
  | Id of AST_generic.ident * AST_generic.id_info option
  | E of AST_generic.expr
  | S of AST_generic.stmt
  | Ss of AST_generic.stmt list
  | T of AST_generic.type_
  | P of AST_generic.pattern
  | Args of AST_generic.argument list
[@@deriving show, eq, hash]

(* we sometimes need to convert to an any to be able to use
 * Lib_AST.ii_of_any, or Lib_AST.abstract_position_info_any
*)
let mvalue_to_any = function
  | E e -> G.E e
  | S s -> G.S s
  (* bugfix: do not return G.I id. We need the id_info because
   * it can be used to check if two metavars are equal and have the same
   * sid (single unique id).
  *)
  | Id (id, Some idinfo) -> G.E (G.Id (id, idinfo))
  | Id (id, None) -> G.E (G.Id (id, G.empty_id_info()))
  | Ss x -> G.Ss x
  | Args x -> G.Args x
  | T x -> G.T x
  | P x -> G.P x

(* update: you should use equal_mvalue (from deriving eq) now *)
let _abstract_position_info_mval x =
  x |> mvalue_to_any |> Lib_AST.abstract_for_comparison_any

let str_of_any any =
  if !Flag_semgrep.debug_with_full_position
  then Meta_parse_info._current_precision :=
      { Meta_parse_info.default_dumper_precision with Meta_parse_info.
                                                   full_info = true };
  let s = AST_generic.show_any any in
  s

let ii_of_mval x =
  x |> mvalue_to_any |> Lib_AST.ii_of_any
let str_of_mval x =
  x |> mvalue_to_any |> str_of_any


(*s: type [[Metavars_generic.metavars_binding]] *)
(* note that the mvalue acts as the value of the metavar and also
   as its concrete code "witness". You can get position information from it,
   it is not Parse_info.Ab(stractPos)

   TODO: ensure that ["$A", Foo; "$B", Bar] and ["$B", Bar; "$A", Foo]
   are equivalent for the equal and hash functions.
   The current implementation is incorrect in general but should work in the
   context of memoizing pattern matching.
*)
type metavars_binding = (mvar * mvalue) list (* = Common.assoc *)
[@@deriving show, eq, hash]
(*e: type [[Metavars_generic.metavars_binding]] *)

(*
   Environment that is carried along and modified while matching a
   pattern AST against a target AST. It holds the captured metavariables
   which are eventually returned if matching is successful.
*)
module Env = struct
  type t = {
    (* All metavariable captures *)
    full_env: metavars_binding;

    (* Only the captures that are used in the rest of the pattern.
       Used in the cache key. *)
    min_env: metavars_binding;

    (* This is the set of metavariables referenced in the rest of the pattern.
       It's used to determine the subset of bindings that should be kept in
       min_env. It comes from the last stmt node encountered in the pattern. *)
    last_stmt_backrefs: AST_generic.String_set.t;
  }

  let empty = {
    full_env = [];
    min_env = [];
    last_stmt_backrefs = Set_.empty;
  }

  (* Get the value bound to a metavariable or return None. *)
  let get_capture k env =
    List.assoc_opt k env.full_env

  (*
     A pattern node provides the set of metavariables that are already bound
     and checked against in the rest of the pattern. This is e.g. the
     's_backrefs' field for a statement node.
  *)
  let has_backref k backrefs =
    Set_.mem k backrefs

  (*
     To be called each time a new value is captured, i.e. bound to a
     metavariable.
  *)
  let add_capture k v env =
    if debug then
      printf "add_capture %s\n" k;
    let kv = (k, v) in
    let full_env = kv :: env.full_env in
    let min_env = kv :: env.min_env
    in
    { env with full_env; min_env }

  (*
     This is used for tracking the span of a matched sequence of statements.
  *)
  let replace_capture k v env =
    if debug then
      printf "replace_capture %s\n" k;
    let kv = (k, v) in
    let full_env = kv :: List.remove_assoc k env.full_env in
    let min_env = kv :: List.remove_assoc k env.min_env
    in
    { env with full_env; min_env }

  let remove_capture k env =
    if debug then
      printf "remove_capture %s\n" k;
    {
      env with
      full_env = List.remove_assoc k env.full_env;
      min_env = List.remove_assoc k env.min_env;
    }

  (*
     To be called as early as possible after passing a backreference
     which may no longer be needed when descending down the pattern.
     For now, we call this only when reaching a new stmt pattern node.

     For simplicity, we assume any member of 'min_env' may no longer be
     needed. It may be more efficient to accumulate the backreferences
     that were encountered since the last stmt and only consider removing
     their bindings, rather than considering all the bindings in min_env.

     If we don't call this, the cache keys will be overspecified, reducing
     or preventing reuse.
  *)
  let update_min_env env (stmt_pat : G.stmt) =
    if debug then
      printf "update_min_env\n";
    let backrefs =
      match stmt_pat.s_backrefs with
      | None -> assert false (* missing initialization *)
      | Some x -> x
    in
    let min_env =
      List.filter (fun (k, _v) ->
        let keep =
          Set_.mem k backrefs
        in
        if debug then
          printf "keep %s in min env: %B\n" k keep;
        keep
      ) env.min_env
    in
    {
      env with
      min_env;
      last_stmt_backrefs = backrefs;
    }
end

(*s: constant [[Metavars_generic.metavar_regexp_string]] *)
(* ex: $X, $FAIL, $VAR2, $_
 * Note that some languages such as PHP or Javascript allows '$' in identifier
 * names, so forcing metavariables to have uppercase letters at least allow
 * us to match specifically also identifiers in lower case (e.g., $foo will
 * only match the $foo identifiers in some concrete code; this is not a
 * metavariable).
 * We allow _ as a prefix to disable the unused-metavar check (we use
 * the same convention than OCaml).
 * However this conflicts with PHP superglobals, hence the special
 * cases below in is_metavar_name.
 * coupling: AST_generic.is_metavar_name
*)
let metavar_regexp_string =
  "^\\(\\$[A-Z_][A-Z_0-9]*\\)$"
(*e: constant [[Metavars_generic.metavar_regexp_string]] *)

(*s: function [[Metavars_generic.is_metavar_name]] *)
(*
 * Hacks abusing existing constructs to encode extra constructions.
 * One day we will have a pattern_ast.ml that mimics mostly
 * AST.ml and extends it with special sgrep constructs.
 *)
let is_metavar_name s =
  match s with
  (* ugly: we should probably pass the language to is_metavar_name, but
   * that would require to thread it through lots of functions, so for
   * now we have this special case for PHP superglobals.
   * ref: https://www.php.net/manual/en/language.variables.superglobals.php
  *)
  | "$_SERVER" | "$_GET" | "$_POST" | "$_FILES"
  | "$_COOKIES" | "$_REQUEST" | "$_ENV"

    (* todo: there's also "$GLOBALS" but this may interface with existing rules*)
    ->
      false
  | _ ->
      s =~ metavar_regexp_string
(*e: function [[Metavars_generic.is_metavar_name]] *)

let metavar_ellipsis_regexp_string =
  "^\\(\\$\\.\\.\\.[A-Z_][A-Z_0-9]*\\)$"
let is_metavar_ellipsis s =
  s =~ metavar_ellipsis_regexp_string

module Structural = struct
  let equal_mvalue = AST_generic.with_structural_equal equal_mvalue
  let equal_metavars_binding =
    AST_generic.with_structural_equal equal_metavars_binding
end

module Referential = struct
  let equal_mvalue = AST_generic.with_referential_equal equal_mvalue
  let equal_metavars_binding =
    AST_generic.with_referential_equal equal_metavars_binding

  let hash_metavars_binding = hash_metavars_binding
end

(*e: semgrep/core/Metavars_generic.ml *)
