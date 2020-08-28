(* Julien Verlaguet, Yoann Padioleau
 *
 * Copyright (C) 2011, 2012 Facebook
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

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type node =
  | Function of string
  (* xhp classes do not have the : prefix, so x:frag::foo for calling
   * foo method on the x:frag XHP class
   *)
  | Method of string * string
  | File of Common.filename
  (* used to simplify code to provoke the call to toplevel functions *)
  | FakeRoot

type callgraph = (node, node Set_.t) Map_.t

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let (add_graph2: node -> node -> callgraph -> callgraph) =
 fun src target graph ->
  let vs = try Map_.find src graph with Not_found -> Set_.empty in
  let vs = Set_.add target vs in
  Map_.add src vs graph

let add_graph a b c =
  Common.profile_code "Callgraph.add" (fun () -> add_graph2 a b c)

(* it's more efficient if g2 is the small graph compared to g1 *)
let (union_graph2: callgraph -> callgraph -> callgraph) = fun g1 g2 ->
  Map_.fold (fun k set1 acc ->
    let set2 = try Map_.find k acc with Not_found -> Set_.empty in
    Map_.add k (Set_.union set1 set2) acc
  ) g2 g1

let union_graph a b =
  Common.profile_code "Callgraph.union" (fun () -> union_graph2 a b)

(*****************************************************************************)
(* string -> node, node -> string *)
(*****************************************************************************)

let string_of_node = function
  | File s -> "__TOP__" ^ s
  | Function s -> s
  | Method (s1, s2) -> s1 ^ "::" ^ s2
  | FakeRoot -> "__FAKE_ROOT__"

let node_of_string s =
  match s with
  | _ when Common.(=~) s "__TOP__\\(.*\\)" -> 
      File (Common.matched1 s)
  | _ when Common.(=~) s "\\(.*\\)::\\(.*\\)" -> 
      let (a, b) = Common.matched2 s in
      Method (a, b)
  | "__FAKE_ROOT__" -> FakeRoot
  | _ -> Function s
