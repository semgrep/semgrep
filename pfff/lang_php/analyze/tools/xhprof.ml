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
open Common2
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* 
 * xhprof is a profiler for PHP. It records profiling information in the
 * form of a pair of *unique* caller and callee. One function can be mentionned
 * multiple times as a caller or callee. In principle this means that
 * in a codebase of 100 000 functions we could have 100 000 * 100 000 entries
 * in the profile but in practice function calls regularily the same 
 * functions. Moreover xhprof prune entries that do not participate
 * enough in the total time (it's a profiler so its goal is to find
 * performance bottlenecks).
 *
 *
 * src: xhprof_lib/utils/xhprof_lib.php
 * 
 * function xhprof_get_possible_metrics() {
 *  static $possible_metrics =
 *    array("wt" => array("Wall", "microsecs", "walltime"),
 *          "ut" => array("User", "microsecs", "user cpu time"),
 *          "st" => array("Sys", "microsecs", "system cpu time"),
 *          "cpu" => array("Cpu", "microsecs", "cpu time"),
 *          "mu" => array("MUse", "bytes", "memory usage"),
 *          "pmu" => array("PMUse", "bytes", "peak memory usage"),
 *          "samples" => array("Samples", "samples", "cpu time"));
 *  return $possible_metrics;
 * }
 *
*)

(*****************************************************************************)
(* Xhprof record *)
(*****************************************************************************)

(* 
 * Below are examples of entries in xhprof/phproflive. They mostly have the
 * 'caller==>callees' format with some special names and prefix/suffix 
 * sometimes such as main(), __pruned__child__(), the @digit and
 * the run_init::<file> special call when including a file.
 * 
 * "intl_activity_get==>cache_get_scb"
 * "call_user_func_array@1==>TSocket::setRecvTimeout"
 * "TBufferedTransport::open==>TSocketPool::open"
 * "SMHThriftWrapper::__call==>call_user_func_array"
 * "main()==>psp_execute_shutdown_functions"
 * "main()"
 * "call_user_func_array@1==>__thrift_autoload"
 * "__autoload==>call_user_func_array@1"
 * "FB_HotProfiler::disable==>__pruned_child__()"
 * "run_init::flib/insights/metrics/__init__.php==>__pruned_child__()"
 * "URI::__construct==>__pruned_child__()"
 * "run_init::lib/external_node/ExternalEvent.php==>run_init::lib/events/core.php"
 * "run_init::lib/platform_install.php==>run_init::lib/alerts/alerts.php"
 * "Preparer::_go@2==>memcache_dispatch"
 * "xhp_x__base::childToStringBuffer@3==>xhp_a::toStringBuffer"
 * main()==>run_init::html/home.php
 * "run_init::html/pagelet/home/navigation_coefficient2.php==>__pruned_child__()"
 * "xhp_x__composable_element::_getPrepared==>prep"
 * 
 * todo? could be moved in lang_php/analyze/tools/xhprof.ml
 *)

type xhprof_call = {
  src: xhprof_entity;
  dest: xhprof_entity;
}
 and xhprof_entity =
  | Function of string
  | Method of string * string
  | Main
  | PruneChild
  | RunInit of Common.filename
  | Misc of string


let string_of_xhprof_entity x =
  match x with
  | Function s -> s
  | Method (a, b) -> spf "%s::%s" a b
  | Main -> "main()"
  | PruneChild -> "__pruned__child__()"
  | RunInit file -> "run_init::" ^ file
  | Misc s -> s

let class_regexp = 
  Str.regexp "\\(.*\\)::\\(.*\\)"

let function_regexp = 
  Str.regexp "\\([a-zA-Z_]+\\)" (* \\(@[0-9]\\)? *)

let run_init_regexp = 
  Str.regexp "^run_init::\\(.*\\)"
  

let parse_xhprof_entity_string s =
  match s with
  | "main()" -> Main
  | "__pruned__child__()" -> PruneChild
  | "(trace buffer realloc)" -> Misc s

  | _ when s ==~ run_init_regexp ->
      let file = Common.matched1 s in
      RunInit file

  | _ when s ==~ class_regexp ->
      let (aclass, amethod) = Common.matched2 s in
      (* todo? unmangle here xhp classnames ? or let the caller do that ? *)
      Method (aclass, amethod)

  | _ when s ==~ function_regexp ->
      let (func) = Common.matched1 s in
      Function func

  | _ -> failwith ("xhprof entity wrong format: " ^ s)

 
let caller_callee_regexp = 
  Str.regexp  "\\(.*\\)==>\\(.*\\)"

let parse_caller_callee_string s =
  match s with
  | "main()" -> { src = Main; dest = Main; }
  | _ when s ==~ caller_callee_regexp ->
      let (src, dest) = Common.matched2 s in
      {
        src = parse_xhprof_entity_string src;
        dest = parse_xhprof_entity_string dest;
      }
  | _ ->
    failwith ("xhprof record wrong format: " ^ s)

(*
let _ = example 
  (parse_caller_callee_string "foo::bar==>foobar" =*=
   { src = Method ("foo", "bar"); dest = Function "foobar"; })
*)

let remove_at_suffix s =
  if s =~ "\\(.*\\)@[0-9]+$"
  then Common.matched1 s
  else s
