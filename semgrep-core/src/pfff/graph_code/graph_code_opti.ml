(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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
module G = Graph_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Graph_code uses the 'node' type for keys into different hashtbl (see
 * commons/graph.ml). This is convenient when building such a graph
 * in the different graph_code_xxx.ml files. But it also leads to
 * many hashtbl operations when working on a Graph_code.
 * This module introduces a new 'graph' type optimized to use arrays
 * instead of hashtbl. Then certain operations like getting the list
 * of children (Has) or list of dependent (Use) is very fast.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type graph = {
  name_to_i: (Graph_code.node, int) Hashtbl.t;
  i_to_name: Graph_code.node array;

  has_children: (int list) array;
  use: (int list) array;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let hashtbl_find h n =
  try Hashtbl.find h n
  with Not_found ->
    pr2_gen ("PB:", n);
    raise Not_found

(*****************************************************************************)
(* API *)
(*****************************************************************************)
let nb_nodes g =
  Array.length g.i_to_name

(*****************************************************************************)
(* Converting *)
(*****************************************************************************)

let (convert2: Graph_code.t -> graph) = fun g ->
  let n = G.nb_nodes g in

  let h = {
    name_to_i = Hashtbl.create (n / 2);
    i_to_name = Array.make n ("",E.Dir);
    has_children = Array.make n [];
    use = Array.make n [];
  }
  in
  let i = ref 0 in
  g |> G.iter_nodes (fun node ->
    Hashtbl.add h.name_to_i node !i;
    h.i_to_name.(!i) <- node;
    incr i;
  );
  g |> G.iter_nodes (fun node ->
    let i = hashtbl_find h.name_to_i node in
    g |> G.succ node G.Has |> List.iter (fun node2 ->
      let j = hashtbl_find h.name_to_i node2 in
      h.has_children.(i) <- j :: h.has_children.(i);
    );
    g |> G.succ node G.Use |> List.iter (fun node2 ->
      (match node2 with
       (* ugly: less important dependency *)
       (*   | _, E.Constant  | _, E.ClassConstant -> () *)
       | _ ->
           let j = hashtbl_find h.name_to_i node2 in
           h.use.(i) <- j :: h.use.(i);
      )
    );
  );
  h

let convert a =
  Common.profile_code "Graph_code_opti.convert" (fun () -> convert2 a)

(*****************************************************************************)
(* Adapters *)
(*****************************************************************************)

let children n g =
  g.has_children.(hashtbl_find g.name_to_i n)
  |> List.map (fun i ->
    g.i_to_name.(i)
  )

(* todo? does it include n? *)
let all_children n g =

  let rec aux i =
    let xs = g.has_children.(i) in
    if null xs
    then [i]
    else i::(xs |> List.map (fun i -> aux i) |> List.flatten)
  in
  aux (hashtbl_find g.name_to_i n) |> List.map (fun i -> g.i_to_name.(i))


let has_node n g =
  Hashtbl.mem g.name_to_i n

(*****************************************************************************)
(* Adjust *)
(*****************************************************************************)


(* put polluting entries under an intermediate "parent/..." entry
 * less: use extensible array so faster?
*)
let adjust_graph_pack_some_children_under_dotdotdot parent to_pack g =
  let dotdotdot = fst parent ^ "/..." in
  let new_node = (dotdotdot, E.MultiDirs) in
  if (has_node new_node g)
  then failwith (spf "already a node with '%s' for a name" dotdotdot);

  let new_idx = Array.length g.i_to_name in
  let to_pack_idx = to_pack |> List.map (fun n -> hashtbl_find g.name_to_i n)in
  let new_g = {
    name_to_i = Hashtbl.copy g.name_to_i;
    i_to_name = Array.append g.i_to_name [|new_node|];
    has_children = Array.append g.has_children [|to_pack_idx|];
    use = Array.append g.use [| [] |];
  } in
  Hashtbl.add new_g.name_to_i new_node new_idx;
  let idx_parent = hashtbl_find new_g.name_to_i parent in
  let idx_packs = to_pack_idx |> Common.hashset_of_list in
  new_g.has_children.(idx_parent) <-
    (* bugfix: don't forget to add new_idx *)
    new_idx ::
    new_g.has_children.(idx_parent) |> Common.exclude (fun i ->
      Hashtbl.mem idx_packs i
    );
  new_g, new_node
