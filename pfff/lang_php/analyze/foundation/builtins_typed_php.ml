(* Julien Verlaguet
 *
 * Copyright (C) 2011 Facebook
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

open Env_typing_php
module Env = Env_typing_php
module GEnv = Typing_helpers_php.GEnv

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let or_ l =
  let l = List.sort (fun x y -> Env.proj x - Env.proj y) l in
  Tsum l

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*-------------------------------------------------------------------*)
(* Core *)
(*-------------------------------------------------------------------*)

let id =
  let v = Tvar (fresh()) in
  fun_ [v] v

(*-------------------------------------------------------------------*)
(* Arrays *)
(*-------------------------------------------------------------------*)

let array_fill =
  let v = Tvar (fresh()) in
  fun_ [int; int; v] (array (int, v))

let array_merge =
  let v = array (Tvar (fresh()), Tvar (fresh())) in
  fun_ [v;v;v;v;v;v;v;v;v;v;v;v;v] v

(*-------------------------------------------------------------------*)
(* String, regexps *)
(*-------------------------------------------------------------------*)

let preg_match =
  fun_ [string; string; array (int, string); int; int] int

let strpos =
  fun_ [string; string; int] (or_ [pint; pstring])

(*-------------------------------------------------------------------*)
(* Misc *)
(*-------------------------------------------------------------------*)

let implode =
  fun_ [string; array (any, string)] string

let preg_replace =
  fun_ [string; string; string; int; int] string

let array_change_key_case =
  let v = Tvar (fresh()) in
  let ien = SSet.add "CASE_UPPER" (SSet.add "CASE_LOWER" SSet.empty) in
  let ien = Tsum [Tienum ien] in
  fun_ [array (string, v); ien] (array (string, v))

let array_chunk =
  let v = Tvar (fresh()) in
  fun_ [array (int, v); int; bool] (array (int, array(int, v)))

let array_combine =
  let k = Tvar (fresh()) in
  let v = Tvar (fresh()) in
  fun_ [k; v] (array (k, v))

let array_count_values =
  let v = Tvar (fresh()) in
  fun_ [array (int, v)] (array (v, int))

let array_fill_keys =
  let v = Tvar (fresh()) in
  let x = Tvar (fresh()) in
  fun_ [array (int, v); x] (array (v, x))

let array_filter =
  let k = Tvar (fresh()) in
  let v = Tvar (fresh()) in
  fun_ [array (k, v); any] (array (k, v))

let array_flip =
  let k = Tvar (fresh()) in
  let v = Tvar (fresh()) in
  fun_ [array (k, v)] (array (v, k))

let array_key_exists =
  let k = Tvar (fresh()) in
  let v = Tvar (fresh()) in
  fun_ [k; array (k, v)] bool

let array_keys =
  let k = Tvar (fresh()) in
  let v = Tvar (fresh()) in
  fun_ [array (k, v); v; bool] (array (int, k))

let array_map =
  let k = Tvar (fresh()) in
  fun_ [any; array (k, any)] (array (k, any))

let array_merge_recursive =
  let k = Tvar (fresh()) in
  let v = Tvar (fresh()) in
  let x = array (k, v) in
  fun_ [x;x;x;x;x;x;x;x;x;x;x;x] x

let array_multisort = id

let array_pad =
  let k = Tvar (fresh()) in
  let v = Tvar (fresh()) in
  fun_ [array(k, v); int; v] (array(k, v))

let array_pop =
  let v = Tvar (fresh()) in
  fun_ [array (int, v)] v

let array_product =
  fun_ [array (int, int)] int

let array_push =
  let v = Tvar (fresh()) in
  fun_ [array (int, v)] (array (int, v))

let array_rand =
  let v = Tvar (fresh()) in
  fun_ [array (int, v); int] v

let array_reduce =
  let v = Tvar (fresh()) in
  fun_ [array (int, v); any; v] v

let array_reverse =
  let v = Tvar (fresh()) in
  fun_ [array (int, v)] (array (int, v))

let array_search =
  let k = Tvar (fresh()) in
  let v = Tvar (fresh()) in
  fun_ [array (k, v); v; bool] k

let array_shift =
  let v = Tvar (fresh()) in
  fun_ [array (int, v)] (array (int, v))

let array_slice =
  let v = Tvar (fresh()) in
  fun_ [array (int, v); int; int] (array (int, v))

let array_splice =
  let v = Tvar (fresh()) in
  fun_ [array (int, v); int; int; v] (array (int, v))

let array_sum =
  let v = Tvar (fresh()) in
  fun_ [array (int, v)] v

let array_unique =
  let k = Tvar (fresh()) in
  let v = Tvar (fresh()) in
  let x = array (k, v) in
  fun_ [x] x

let array_unshift =
  let v = Tvar (fresh()) in
  fun_ [array (int, v);v;v;v;v;v;v;v;v;v;v;v] v

let array_values =
  let v = Tvar (fresh()) in
  fun_ [array (any, v)] (array (int, v))

let array_walk_recursive =
  let k = Tvar (fresh()) in
  fun_ [array (k, any); any; any] (array (k, any))

let array_walk = array_walk_recursive

let array_shuffle =
  let v = Tvar (fresh()) in
  let x = (array (int, v)) in
  fun_ [x] x

let current =
  let v = Tvar (fresh()) in
  fun_ [array (any, v)] v

let next =
  fun_ [array (any, any)] null

let pos = current
let prev = next
let reset = next
let end_ = next

let in_array =
  let v = Tvar (fresh()) in
  fun_ [array (any, v)] bool

let key =
  let k = Tvar (fresh()) in
  fun_ [array (k, any)] k

let range =
  let v = Tvar (fresh()) in
  fun_ [v; v; v] (array (int, v))

let array_diff =
  let k = Tvar (fresh()) in
  let v = Tvar (fresh()) in
  let x = array (k, v) in
  fun_ [x;x] x

let sort =
  let v = Tvar (fresh()) in
  fun_ [array (int, v); int; bool] (array (int, v))

let list =
  let v = Tvar (fresh()) in
  let a = array (int, v) in
  fun_ [a;a;a;a;a;a;a;a;a] any

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

let super_globals =
  let h = Hashtbl.create 23 in
  let add x = Hashtbl.add h x true in
  add "$GLOBALS";
  add "$_SERVER";
  add "$_GET";
  add "$_POST";
  add "$_FILES";
  add "$_COOKIE";
  add "$_SESSION";
  add "$_REQUEST";
  add "$_ENV";
  h

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let make env =
  let add x y = 
    env.builtins := SSet.add ("^Fun:"^x) !(env.builtins);
    GEnv.set_fun env x y 
  in

  (*-------------------------------------------------------------------*)
  (* Types *)
  (*-------------------------------------------------------------------*)

  add "int" int;
  add "bool" bool;
  add "float" float;
  add "string" string;
  add "u" (fun_ [string] string);

  add "null" null;

  (*-------------------------------------------------------------------*)
  (* Core *)
  (*-------------------------------------------------------------------*)

  add "isset" (fun_ [any] bool);
  add "count" (fun_ [any] int);
  add "sizeof" (fun_ [any] int);
  add "id" id;
  (*-------------------------------------------------------------------*)
  (* Arrays *)
  (*-------------------------------------------------------------------*)

  add "array_fill" array_fill;

  (*-------------------------------------------------------------------*)
  (* Strings *)
  (*-------------------------------------------------------------------*)

  add "sprintf" (fun_ [string] string);
  add "substr" (fun_ [string; int; int] string);
  add "intval" (fun_ [any] int);
  add "starts_with" (fun_ [string;string] bool);
  add "ends_with" (fun_ [string;string] bool);

  (*-------------------------------------------------------------------*)
  (* Misc *)
  (*-------------------------------------------------------------------*)

  add "array_merge" array_merge;
  add "preg_match" preg_match;
  add "preg_replace" preg_replace;
  add "strpos" strpos;
  add "time" (fun_ [] int);
  add "array_keys" array_keys;
  add "implode" implode;
  add "empty" (fun_ [any] bool);
  add "unset" (fun_ [any] null);
  add "trim" (fun_ [string; string] string);
  add "get_class" (fun_ [any] string);
  add "str_replace" (fun_ [string; string; string] string);
  add "strlen" (fun_ [string] int);
  add "is_array" (fun_ [array (any, any)] bool);
  add "is_string" (fun_ [string] bool);
  add "is_bool" (fun_ [bool] bool);
  add "is_int" (fun_ [int] int);
  add "is_float" (fun_ [float] bool);
  add "is_scalar" (fun_ [or_ [pint;pfloat;pbool]] bool);
  add "is_object" (fun_ [Tsum [Tobject (SMap.empty)]] bool);
  add "is_numeric" (fun_ [or_ [pint;pfloat]] bool);
  add "array_change_key_case" array_change_key_case;
  add "array_chunk" array_chunk;
  add "array_combine" array_combine;
  add "array_count_values" array_count_values;
  add "array_fill_keys" array_fill_keys;
  add "array_filter" array_filter;
  add "array_flip" array_flip;
  add "array_key_exists" array_key_exists;
  add "array_keys" array_keys;
  add "array_map" array_map;
  add "array_merge_recursive" array_merge_recursive;
  add "array_multisort" array_multisort;
  add "array_pad" array_pad;
  add "array_pop" array_pop;
  add "array_product" array_product;
  add "array_push" array_push;
  add "array_rand" array_rand;
  add "array_reduce" array_reduce;
  add "array_reverse" array_reverse;
  add "array_search" array_search;
  add "array_shift" array_shift;
  add "array_slice" array_slice;
  add "array_splice" array_splice;
  add "array_sum" array_sum;
  add "array_unique" array_unique;
  add "array_unshift" array_unshift;
  add "array_values" array_values;
  add "array_walk_recursive" array_walk_recursive;
  add "array_walk" array_walk;
  add "array_shuffle" array_shuffle;
  add "current" current;
  add "next" next;
  add "pos" pos;
  add "prev" prev;
  add "reset" reset;
  add "end" end_;
  add "in_array" in_array;
  add "key" key;
  add "range" range;
  add "array_diff" array_diff;
  add "explode" (fun_ [string; string; int] (array (int, string)));
  add "max" (fun_ [] any);
  add "chr" (fun_ [int] string);
  add "strtoupper" (fun_ [string] string);
  add "floor" (fun_ [float] int);
  add "strtotime" (fun_ [string; int] int);
  add "microtime" (fun_ [bool] (or_ [pint; pfloat]));
  add "echo" (fun_ [string] null);
  add "exit" (fun_ [int] null);
  add "print" (fun_ [string] null);
  add "json_encode" (fun_ [string] string);
  add "date" (fun_ [string; int] string);
  add "strftime" (fun_ [string; int] string);
  add "sort" sort;
  add "round" (fun_ [float] int);
  add "join" implode;
  add "htmlize" (fun_ [thtml] string);
  add "txt2html" (fun_ [thtml; bool] string);
  add "list" list;
