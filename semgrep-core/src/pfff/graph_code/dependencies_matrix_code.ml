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

module G = Graph_code
module G2 = Graph_code_opti

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * See http://en.wikipedia.org/wiki/Design_structure_matrix
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* Dependency Structure Matrix.
 *
 * If A depends on B, e.g. visual/ depends on commons/,
 * then we will increment 'row of visual' x 'column of commons',
 * that way one can easily see all the modules that visual/ depends
 * on by looking at the 'row of visual'.
 *
 * todo: I think Ndepend does the reverse. Also I think Ndepends uses
 * a symetric matrix model where one can get more information by looking
 * at both directions. In one direction one can know how many entities A is
 * using in B, and in the other by how many entities in A some stuff in B
 * are used. For instance one can see that A is using many different functions
 * in B, and then can see that actually all those functions are used by
 * only one thing in A, in which case it's a sign that maybe this function
 * should be moved in B. This is done I think only when there is a clean
 * layered archi. When there are cycles then NDepend uses another color
 * for the cell.
 *
 * todo: coupling/cohesion metrics! the dsm can be helpful to visualize
 * this? see patterns? use more colors?
*)
type dm = {
  matrix: int array array;
  i_to_name: Graph_code.node array;
  name_to_i: (Graph_code.node, int) Hashtbl.t;
  (* which nodes are currently expanded *)
  config: config;
}
(* It's actually more a 'tree set' than a 'tree list' below
 * when we pass the config to build(). Indeed it's build() which
 * will order this set according to some partitionning algorithm
 * that tries to "layer" the code.
 * less: could reuse Common.tree2.
*)
and tree =
  | Node of Graph_code.node * tree list
and config = tree


let basic_config g =
  Node (G.root, Graph_code.children G.root g
                |> List.map (fun n -> Node (n, [])))
let basic_config_opti gopti =
  Node (G.root, Graph_code_opti.children G.root gopti
                |> List.map (fun n -> Node (n, [])))

type config_path_elem =
  | Expand of Graph_code.node
  | Focus of Graph_code.node * deps_style

and deps_style =
  | DepsIn
  | DepsOut
  | DepsInOut

type config_path = config_path_elem list

(* We sometimes want to manually order certain entries in the matrix,
 * especially when the code is a mess with cycles everywhere in
 * which case the default partitionning algorithm does not help.
 * The hashtbl maps string nodes to the ordered list of children
 * we want. We use a hash and not a tree because at some point
 * we may want to specify the order only for certain deeply
 * nested directories in which case we will do a find -name "info.txt"
 * to build all the partial constraints.
*)
type partition_constraints =
  (string, string list) Hashtbl.t

(* let tasks = ref 16 *)

(* Phantom types for safer array access between the graph_opti, dm, and
 * the full matrix dm. Not really used, but could one day.
*)
type 'a idx = int

type idm
type igopti

type cell_coord = int * int

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
let verbose = ref false

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let rec final_nodes_of_tree tree =
  match tree with
  | Node (n, xs) ->
      if null xs
      then [n]
      else List.map final_nodes_of_tree xs |> List.flatten

let hashtbl_find_node h n =
  try Hashtbl.find h n
  with Not_found ->
    (* pr2 (spf "PB: %s" (G.string_of_node n));*)
    (* raise Not_found *)
    failwith (spf "Not_found: %s" (G.string_of_node n))

(*****************************************************************************)
(* Display *)
(*****************************************************************************)

(* poor's man DSM visualizer (use codegraph for a real visualization) *)
let display dm =
  pr2_gen dm;
  ()

(*****************************************************************************)
(* Explain the matrix *)
(*****************************************************************************)

(* history:
 * - iterate over all edges
 * - iterate only on the children of i
 * - use graph_opti instead of the memoized projection index
*)
let explain_cell_list_use_edges (i, j) dm gopti =
  let res = ref [] in

  let n_nodes = G2.nb_nodes gopti in
  let igopti_to_idm = Array.make n_nodes (-1) in
  dm.i_to_name |> Array.iteri (fun idm node ->
    igopti_to_idm.(hashtbl_find_node gopti.G2.name_to_i node) <- idm;
  );
  let (projected_parent_of_igopti: idm idx array) = Array.make n_nodes (-1) in
  let (iroot: igopti idx) = hashtbl_find_node gopti.G2.name_to_i G.root in
  let rec depth parent igopti =
    let children = gopti.G2.has_children.(igopti) in
    let idm = igopti_to_idm.(igopti) in
    let project =
      if idm = -1
      then parent
      else idm
    in
    projected_parent_of_igopti.(igopti) <- project;
    children |> List.iter (depth project);
  in
  depth (-1) iroot;

  gopti.G2.use |> Array.iteri (fun i2 xs ->
    let parent_i2 = projected_parent_of_igopti.(i2) in
    xs |> List.iter (fun j2 ->
      let parent_j2 = projected_parent_of_igopti.(j2) in
      if parent_i2 = i && parent_j2 = j
      then
        Common.push (
          gopti.G2.i_to_name.(i2),
          gopti.G2.i_to_name.(j2)
        ) res;
    )
  );
(*
  let (src: igopti idx) = hashtbl_find gopti.G2.name_to_i dm.i_to_name.(i) in
  let (dst: idm idx) = j in

  let rec aux n1 =
    let uses = gopti.G2.use.(n1) in
    uses +> List.iter (fun n2 ->
      let idm = igopti_to_idm.(n2) in
      if idm = dst
      then Common.push2 (gopti.G2.i_to_name.(n1), gopti.G2.i_to_name.(n2)) res;
    );
    let children = gopti.G2.has_children.(n1) in
    List.iter aux children
  in
  aux src;
*)
  !res


(*
let explain_cell_list_use_edges a b c =
  Common.profile_code "DM.explain_cell" (fun () ->
    explain_cell_list_use_edges2 a b c)
*)

(*****************************************************************************)
(* tree config manipulation *)
(*****************************************************************************)
let expand_node n tree g =
  let rec aux tree =
    match tree with
    | Node (n2, xs) ->
        if n =*= n2
        then
          (* less: assert null xs? *)
          let succ = G.succ n G.Has g in
          Node (n2, succ |> List.map (fun n -> Node (n, [])))
        else Node (n2, xs |> List.map aux)
  in
  aux tree

let expand_node_opti n tree g =
  let rec aux tree =
    match tree with
    | Node (n2, xs) ->
        if n =*= n2
        then
          (* less: assert null xs? *)
          let succ = Graph_code_opti.children n g in
          Node (n2, succ |> List.map (fun n -> Node (n, [])))
        else Node (n2, xs |> List.map aux)
  in
  aux tree



(* To focus on a node we need to know its dependencies to filter
 * the irrelevant one and so we need a dm passed as a parameter.
 * This function is mainly used in a Model.config_of_path
 * where we fold over an initial dm and given a path element
 * expand or focus to get a new dm and so on.
*)
let focus_on_node n deps_style tree dm =
  let i = hashtbl_find_node dm.name_to_i n in
  let (deps: int list ref) = ref [] in
  let nb_elts = Array.length dm.matrix in
  for j = 0 to nb_elts - 1 do
    let to_include =
      match deps_style with
      | DepsOut -> dm.matrix.(i).(j) > 0
      | DepsIn -> dm.matrix.(j).(i) > 0
      | DepsInOut -> dm.matrix.(i).(j) > 0 || dm.matrix.(j).(i) > 0
    in
    (* we do || i = j because we want the node under focus in too, in the
     * right order
    *)
    if to_include || i = j
    then Common.push j deps
  done;
  (* old: this was not keeping the hierarchy (which can be a feature)
   *  Node (G.root, !deps +> List.rev +> List.map (fun i ->
   *    Node (hashtbl_find_node dm.i_to_name i, []))
   *  )
  *)
  let rec aux tree =
    match tree with
    | Node (n2, []) ->
        let j = hashtbl_find_node dm.name_to_i n2 in
        if i = j || List.mem j !deps
        then Some (Node (n2, []))
        else None
    | Node (n2, xs) ->
        let xs = xs |> Common.map_filter aux in
        if null xs
        then None
        else Some (Node (n2, xs))
  in
  (* should be a Some cos at least we have 'n' in the tree *)
  Common2.some (aux tree)

(*****************************************************************************)
(* Config path *)
(*****************************************************************************)

let string_of_config_path_elem = function
  | Expand n ->
      spf "Expand(%s)" (G.string_of_node n)
  | Focus (n, style) ->
      spf "Focus%s(%s)"
        (match style with
         | DepsIn -> "<-"
         | DepsOut -> "->"
         | DepsInOut -> "<->"
        )
        (G.string_of_node n)

let string_of_config_path xs =
  xs |> List.map string_of_config_path_elem |> Common.join "/"

(*****************************************************************************)
(* Matrix analysis *)
(*****************************************************************************)
let is_dead_column j dm =
  let mat = dm.matrix in
  let has_user = ref false in
  for i = 0 to Array.length mat - 1 do
    if mat.(i).(j) > 0 && i <> j then has_user := true
  done;
  not !has_user

let is_dead_line i dm =
  let mat = dm.matrix in
  let use_stuff = ref false in
  for j = 0 to Array.length mat - 1 do
    if mat.(i).(j) > 0 && i <> j then use_stuff := true
  done;
  not !use_stuff


let parents_of_indexes dm =
  let arr = Array.make (Array.length dm.matrix) [] in
  let i = ref 0 in
  let rec aux acc tree =
    match tree with
    (* a leaf *)
    | Node (_, []) ->
        arr.(!i) <- List.rev acc;
        incr i
    (* a node *)
    | Node (n, xs) ->
        xs |> List.iter (aux (n::acc))
  in
  aux [] dm.config;
  arr

(* ex: dist  a/b/c to a/b/d/e should be ? *)
let distance_entity (i, j) arr =
  let xs = arr.(i) in
  let ys = arr.(j) in
  let rec aux xs ys =
    match xs, ys with
    | [], [] -> 0
    | _, [] -> 1
    (* if it's a subentity of a brother, then distance should still be 0 *)
    | [], _ -> 0

    | x::xs, y::ys ->
        if x =*= y
        then aux xs ys
        else 1
  in
  aux xs ys

(* less: more fine grained internal modules in package where can see what
 * is the scope of the module. So can see stuff really important in
 * a whole package because they are really used outside this package,
 * so depth of escape > X. ===> remember max depth of escape
 * 0 = same module, 1, brother, etc.
*)
let is_internal_helper j dm =
  let mat = dm.matrix in
  let arr = parents_of_indexes dm in

  let has_users_outside_parent = ref false in
  let parents = arr.(j) in
  for i = 0 to Array.length mat - 1 do
    if mat.(i).(j) > 0 && i <> j && distance_entity (j, i) arr > 0
    then has_users_outside_parent := true
  done;
  not !has_users_outside_parent &&
  (* the elements at the root can't have dependencies outside parents *)
  List.length parents > 1

let score_upper_triangle dm exclude_nodes =
  let score = ref 0 in
  let exclude_idx = exclude_nodes |> List.map (fun n ->
    hashtbl_find_node dm.name_to_i n) in

  for i = 0 to Array.length dm.matrix -1 do
    for j = i + 1 to Array.length dm.matrix -1 do
      if (List.mem i exclude_idx) || (List.mem j exclude_idx)
      then ()
      else score := !score + dm.matrix.(i).(j)
    done
  done;
  !score

let score_downer_triangle dm exclude_nodes =
  let score = ref 0 in
  let exclude_idx = exclude_nodes |> List.map (fun n ->
    hashtbl_find_node dm.name_to_i n) in

  for i = 0 to Array.length dm.matrix -1 do
    for j = 0 to i - 1 do
      if (List.mem i exclude_idx) || (List.mem j exclude_idx)
      then ()
      else score := !score + dm.matrix.(i).(j)
    done
  done;
  !score

let score_upper_triangle_nodes dm =
  let score = Array.make (Array.length dm.matrix) 0 in
  for i = 0 to Array.length dm.matrix -1 do
    for j = i + 1 to Array.length dm.matrix -1 do
      let v = dm.matrix.(i).(j) in
      score.(i) <- score.(i) + v;
      score.(j) <- score.(j) + v;
    done
  done;
  score |> Array.mapi (fun i v -> (dm.i_to_name.(i), v)) |> Array.to_list

let score_upper_triangle_cells dm =
  let res = ref [] in
  for i = 0 to Array.length dm.matrix -1 do
    for j = i + 1 to Array.length dm.matrix -1 do
      Common.push ((i, j), dm.matrix.(i).(j)) res
    done
  done;
  !res
