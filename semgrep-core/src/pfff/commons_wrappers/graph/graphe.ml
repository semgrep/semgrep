(* Yoann Padioleau
 *
 * Copyright (C) 2010, 2013 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * There are multiple libraries for graphs in OCaml, incorporating each
 * different graph algorithms:
 *
 *  - OCamlGraph, by Filliatre, Signoles, et al. It has transitive closure,
 *    Kruskal, Floyd, topological sort, CFC, etc. Probably the best. But it is
 *    heavily functorized. I thought it was too complicated because of all
 *    those functors but they also provide an easy interface without functor
 *    in pack.mli and sig_pack.mli which makes it almost usable
 *    (see paper from jfla05 on ocamlgraph).
 *
 *  - A small graph library in ocamldot by Trevor Jim to compute the
 *    transitive reduction of a graph, aka its kernel.
 *
 *  - A small library in ocamldoc by Guesdon, who ported into ocamldoc
 *    the functionality of ocamldot, and apparently uses the opportunity
 *    to rewrite too his own graph library. Has also the transitive
 *    reduction.
 *
 *  - Camllib by jeannet ?
 *
 *  - probably more on the caml hump.
 *
 * I have also developed a few graph libraries in commons/, but really just
 * to have a data type with successors/predecessors accessors:
 *
 *  - common.ml type 'a graph. No algorithm, just builder/accessors.
 *  - ograph.ml object version of Common.graph, just the interface.
 *
 *  - ograph2way.ml a generic version, inherit ograph.
 *  - ograph_extended.ml, implicit nodei = int for key.
 *  - ograph_simple.ml, key can be specified, for instance can be a string,
 *    so dont have to pass through the intermediate nodei for everything.
 *
 * I have also included in commons/ the small code from ocamldot/ocamldoc in:
 *  - ocamlextra/graph_ocamldot.ml
 *  - ocamlextra/graph_ocamldoc.ml
 *
 * ograph_simple and ograph_extended and ograph2way show that there is not
 * a single graph that can accomodate all needs while still being convenient.
 * ograph_extended is more generic, but you pay a little for that by
 * forcing the user to have this intermediate 'nodei'. The people
 * from ocamlgraph have well realized that and made it possible
 * to have different graph interface (imperative/pure, directed/undirected,
 * with/witout nodes, parametrized vertex or not, ...) and reuse
 * lots of code for the algorithm. Unfortunately, just like for the C++
 * STL, it comes at a price: lots of functors. The sig_pack.mli and pack.ml
 * tries to solve this pb, but they made some choices about what should
 * be the default that are not always good, and they do not allow
 * polymorphic nodes, which I think is quite useful (especially when
 * you want to display your graph with dot, you want to see the label
 * of the nodes, and not just integers.
 *
 *
 * So, this module is a small wrapper around ocamlgraph, to have
 * more polymorphic graphs with some defaults that makes sense most
 * of the time (Directed graph, Imperative, vertex ints with mapping
 * to node information), and which can use algorithms defined in
 * other libraries by making some small converters from one representation
 * to the other (e.g. from my ograph_simple to ocamlgraph, and vice versa).
 *
 * Note that even if ocamlgraph is really good and even if this file is useful,
 * for quick and dirty trivial graph stuff then ograph_simple
 * should be simpler (less dependencies). You can
 * use it directly from common.cma. Then with the converter to ocamlgraph,
 * you can start with ograph_simple, and if in a few places you need
 * to use the graph algorithm provided by ocamlgraph or ocamldot, then
 * use the adapters.
 *
 * Alternatives in other languages:
 *  - boost C++ BGL,
 *    http://www.boost.org/doc/libs/1_45_0/libs/graph/doc/index.html
 *  - quickGraph for .NET, http://quickgraph.codeplex.com/
 *    apparently inspired by the boost one
 *  - c++ GTL, graph template library
 *  - c++ ASTL, automata library
 *  - See the book by Skienna "Algorithm Design Manual" which gives many
 *    resources related to graph libraries.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* OG for ocamlgraph.
 *
 * todo: maybe time to use the non generic implementation and
 * use something more efficient, especially for G.pred,
 * see Imperative.ConcreteBidirectional for instance
*)
module OG = Graph.Pack.Digraph

(* Polymorphic graph *)
type 'key graph = {
  og: OG.t;

  (* Note that OG.V.t is not even an integer. It's an abstract data type
   * from which one can get its 'label' which is an int. It's a little
   * bit tedious because to create such a 't' you also have to use
   * yet another function: OG.V.create that takes an int ...
  *)

  key_of_vertex: (OG.V.t, 'key) Hashtbl.t;
  vertex_of_key: ('key, OG.V.t) Hashtbl.t;

  (* used to create vertexes (OG.V.create n) *)
  cnt: int ref;
}

(*
module OG :
  sig
    type t = Ocamlgraph.Pack.Digraph.t
    module V :
      sig
        type t = Ocamlgraph.Pack.Digraph.V.t
        val compare : t -> t -> int
        val hash : t -> int
        val equal : t -> t -> bool
        type label = int
        val create : label -> t
        val label : t -> label
      end
    type vertex = V.t
    module E :
      sig
        type t = Ocamlgraph.Pack.Digraph.E.t
        val compare : t -> t -> int
        val src : t -> V.t
        val dst : t -> V.t
        type label = int
        val create : V.t -> label -> V.t -> t
        val label : t -> label
        type vertex = V.t
      end
    type edge = E.t

NA    val is_directed : bool
DONE    val create : ?size:int -> unit -> t
    val copy : t -> t
DONE    val add_vertex : t -> V.t -> unit
DONE    val remove_vertex : t -> V.t -> unit
DONE    val add_edge : t -> V.t -> V.t -> unit
    val add_edge_e : t -> E.t -> unit
DONE    val remove_edge : t -> V.t -> V.t -> unit
    val remove_edge_e : t -> E.t -> unit
    module Mark :
      sig
        type graph = t
        type vertex = V.t
        val clear : t -> unit
        val get : V.t -> int
        val set : V.t -> int -> unit
      end

    val is_empty : t -> bool
DONE    val nb_vertex : t -> int
DONE    val nb_edges : t -> int
DONE    val out_degree : t -> V.t -> int
DONE    val in_degree : t -> V.t -> int
    val mem_vertex : t -> V.t -> bool
    val mem_edge : t -> V.t -> V.t -> bool
    val mem_edge_e : t -> E.t -> bool
    val find_edge : t -> V.t -> V.t -> E.t
DONE    val succ : t -> V.t -> V.t list
DONE    val pred : t -> V.t -> V.t list
    val succ_e : t -> V.t -> E.t list
    val pred_e : t -> V.t -> E.t list
DONE    val iter_vertex : (V.t -> unit) -> t -> unit
    val iter_edges : (V.t -> V.t -> unit) -> t -> unit
    val fold_vertex : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
    val fold_edges : (V.t -> V.t -> 'a -> 'a) -> t -> 'a -> 'a
    val map_vertex : (V.t -> V.t) -> t -> t
    val iter_edges_e : (E.t -> unit) -> t -> unit
    val fold_edges_e : (E.t -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_succ : (V.t -> unit) -> t -> V.t -> unit
    val iter_pred : (V.t -> unit) -> t -> V.t -> unit
    val fold_succ : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
    val fold_pred : (V.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
    val iter_succ_e : (E.t -> unit) -> t -> V.t -> unit
    val fold_succ_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
    val iter_pred_e : (E.t -> unit) -> t -> V.t -> unit
    val fold_pred_e : (E.t -> 'a -> 'a) -> t -> V.t -> 'a -> 'a
    val find_vertex : t -> int -> V.t


DONE    val transitive_closure : ?reflexive:bool -> t -> t
    val add_transitive_closure : ?reflexive:bool -> t -> t
DONE    val mirror : t -> t
    val complement : t -> t
    val intersect : t -> t -> t
    val union : t -> t -> t

    module Dfs :
      sig
        val iter : ?pre:(V.t -> unit) -> ?post:(V.t -> unit) -> t -> unit
        val prefix : (V.t -> unit) -> t -> unit
        val postfix : (V.t -> unit) -> t -> unit
        val iter_component :
          ?pre:(V.t -> unit) -> ?post:(V.t -> unit) -> t -> V.t -> unit
        val prefix_component : (V.t -> unit) -> t -> V.t -> unit
        val postfix_component : (V.t -> unit) -> t -> V.t -> unit
        val has_cycle : t -> bool
      end
    module Bfs :
      sig
        val iter : (V.t -> unit) -> t -> unit
        val iter_component : (V.t -> unit) -> t -> V.t -> unit
      end
    module Marking : sig val dfs : t -> unit val has_cycle : t -> bool end

    module Classic :
      sig
        val divisors : int -> t
        val de_bruijn : int -> t
        val vertex_only : int -> t
        val full : ?self:bool -> int -> t
      end
    module Rand :
      sig
        val graph : ?loops:bool -> v:int -> e:int -> unit -> t
        val labeled :
          (V.t -> V.t -> E.label) ->
          ?loops:bool -> v:int -> e:int -> unit -> t
      end
    module Components :
      sig
        val scc : t -> int * (V.t -> int)
        val scc_array : t -> V.t list array
        val scc_list : t -> V.t list list
      end

DONE    val shortest_path : t -> V.t -> V.t -> E.t list * int

    val ford_fulkerson : t -> V.t -> V.t -> (E.t -> int) * int

    val goldberg : t -> V.t -> V.t -> (E.t -> int) * int

    module PathCheck :
      sig
        type path_checker = Ocamlgraph.Pack.Digraph.PathCheck.path_checker
        val create : t -> path_checker
        val check_path : path_checker -> V.t -> V.t -> bool
      end
    module Topological :
      sig
        val fold : (V.t -> 'a -> 'a) -> t -> 'a -> 'a
        val iter : (V.t -> unit) -> t -> unit
      end
    val spanningtree : t -> E.t list

    val dot_output : t -> string -> unit
DONE    val display_with_gv : t -> unit
    val parse_gml_file : string -> t
    val parse_dot_file : string -> t
    val print_gml_file : t -> string -> unit
  end
*)


(*****************************************************************************)
(* Graph construction *)
(*****************************************************************************)

let create () = {
  og = OG.create ();
  key_of_vertex = Hashtbl.create 101;
  vertex_of_key = Hashtbl.create 101;
  cnt = ref 0;
}

let add_vertex_if_not_present key g =
  if Hashtbl.mem g.vertex_of_key key
  then ()
  else begin
    incr g.cnt;
    let v = OG.V.create !(g.cnt) in
    Hashtbl.replace g.key_of_vertex v key;
    Hashtbl.replace g.vertex_of_key key v;
    (* not necessary as add_edge automatically do that *)
    OG.add_vertex g.og v;
  end
let vertex_of_key key g =
  Hashtbl.find g.vertex_of_key key
let key_of_vertex v g =
  Hashtbl.find g.key_of_vertex v

let add_edge k1 k2 g =
  let vx = g |> vertex_of_key k1 in
  let vy = g |> vertex_of_key k2 in
  OG.add_edge g.og vx vy;
  ()

(*****************************************************************************)
(* Graph access *)
(*****************************************************************************)

let nodes g =
  Common2.hkeys g.vertex_of_key

let out_degree k g = OG.out_degree g.og (g |> vertex_of_key k)
let in_degree k g  = OG.in_degree  g.og (g |> vertex_of_key k)

let nb_nodes g = OG.nb_vertex g.og
let nb_edges g = OG.nb_edges g.og

let succ k g = OG.succ g.og (g |> vertex_of_key k)
               |> List.map (fun k -> key_of_vertex k g)
(* this seems slow on the version of ocamlgraph I currently have *)
let pred k g  = OG.pred  g.og (g |> vertex_of_key k)
                |> List.map (fun k -> key_of_vertex k g)

let ivertex k g =
  let v = vertex_of_key k g in
  OG.V.label v

let has_node k g =
  try
    let _ = ivertex k g in
    true
  with Not_found -> false

let entry_nodes2 g =
  (* old: slow: nodes g +> List.filter (fun n -> pred n g = [])
   * Once I use a better underlying graph implementation maybe I
   * will not need this kind of things.
  *)
  let res = ref [] in
  let hdone = Hashtbl.create 101 in
  let finished = ref false in
  g.og |> OG.Topological.iter (fun v ->
    if !finished || Hashtbl.mem hdone v
    then finished := true
    else begin
      let xs = OG.succ g.og v in
      xs |> List.iter (fun n -> Hashtbl.replace hdone n true);
      Common.push v res;
    end
  );
  !res |> List.map (fun i -> key_of_vertex i g) |> List.rev


let entry_nodes a =
  Common.profile_code "Graph.entry_nodes" (fun () -> entry_nodes2 a)

(*****************************************************************************)
(* Iteration *)
(*****************************************************************************)
let iter_edges f g =
  g.og |> OG.iter_edges (fun v1 v2 ->
    let k1 = key_of_vertex v1 g in
    let k2 = key_of_vertex v2 g in
    f k1 k2
  )

let iter_nodes f g =
  g.og |> OG.iter_vertex (fun v ->
    let k = key_of_vertex v g in
    f k
  )
(*****************************************************************************)
(* Graph deletion *)
(*****************************************************************************)

let remove_vertex k g =
  let vk = g |> vertex_of_key k in
  OG.remove_vertex g.og vk;
  Hashtbl.remove g.vertex_of_key k;
  Hashtbl.remove g.key_of_vertex vk;
  ()

let remove_edge k1 k2 g =
  let vx = g |> vertex_of_key k1 in
  let vy = g |> vertex_of_key k2 in
  (* todo? assert edge exists? *)
  OG.remove_edge g.og vx vy;
  ()

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

(* todo? make the graph more functional ? it's very imperative right now
 * which forces the caller to write in an imperative way and use functions
 * like this 'copy()'. Look at launchbary haskell paper?
*)
let copy oldg =
(*
 * bugfix: we can't just OG.copy the graph and Hashtbl.copy the vertex because
 * the vertex will actually be different in the copied graph, and so the
 * vertex_of_key will return a vertex in the original graph, not in
 * the new copied graph.
 *)
  (* {
     og = OG.copy g.og;
     key_of_vertex = Hashtbl.copy g.key_of_vertex;
     vertex_of_key = Hashtbl.copy g.vertex_of_key;
     cnt = ref !(g.cnt);
     }
  *)
  (* naive way, enough? optimize? all those iter are ugly ... *)
  let g = create () in
  let nodes = nodes oldg in
  nodes |> List.iter (fun n -> add_vertex_if_not_present n g);
  nodes |> List.iter (fun n ->
    (* bugfix: it's oldg, not 'g', wow, copying stuff is error prone *)
    let succ = succ n oldg in
    succ |> List.iter (fun n2 -> add_edge n n2 g)
  );
  g

(*****************************************************************************)
(* Graph algorithms *)
(*****************************************************************************)

let shortest_path k1 k2 g =
  let vx = g |> vertex_of_key k1 in
  let vy = g |> vertex_of_key k2 in

  let (edges, _len) = OG.shortest_path g.og vx vy in
  let vertexes =
    vx::(edges |> List.map (fun edge -> OG.E.dst edge))
  in
  vertexes |> List.map (fun v -> key_of_vertex v g)


(* todo? this works? I get some
 * Fatal error: exception Invalid_argument("[ocamlgraph] fold_succ")
 * when doing:
 *   let g = ...
 *   let node = whatever g in
 *   let g2 = transitive_closure g in
 *   let succ = succ node g2
 *  is it because node references something from g? Is is the same
 *  issue that for copy?
 *
*)
let transitive_closure g =

  let label_to_vertex = Hashtbl.create 101 in
  g.og |> OG.iter_vertex (fun v ->
    let lbl = OG.V.label v in
    Hashtbl.replace label_to_vertex lbl v
  );

  let og' = OG.transitive_closure ~reflexive:true g.og in
  let g' = create () in

  og' |> OG.iter_vertex (fun v ->
    let lbl = OG.V.label v in
    let vertex_in_g = Hashtbl.find label_to_vertex lbl in
    let key_in_g = Hashtbl.find g.key_of_vertex vertex_in_g in
    Hashtbl.replace g'.key_of_vertex v key_in_g;
    Hashtbl.replace g'.vertex_of_key key_in_g v;
  );
  { g' with og = og'  }


let mirror g =
  let og' = OG.mirror g.og in
  (* todo: have probably to do the same gymnastic than for transitive_closure*)
  { g with og = og';  }


(* http://en.wikipedia.org/wiki/Strongly_connected_component *)
let strongly_connected_components2 g =
  let scc_array_vt = OG.Components.scc_array g.og in
  let scc_array =
    scc_array_vt |> Array.map (fun xs -> xs |> List.map (fun vt ->
      key_of_vertex vt g
    ))
  in
  let h = Hashtbl.create 101 in
  scc_array |> Array.iteri (fun i xs ->
    xs |> List.iter (fun k ->
      if Hashtbl.mem h k
      then failwith "the strongly connected components should be disjoint";
      Hashtbl.add h k i
    ));
  scc_array, h

let strongly_connected_components a =
  Common.profile_code "Graph.scc" (fun () -> strongly_connected_components2 a)

(* http://en.wikipedia.org/wiki/Strongly_connected_component *)
let strongly_connected_components_condensation2 g (scc, hscc) =
  let g2 = create () in
  let n = Array.length scc in
  for i = 0 to n -1 do
    g2 |> add_vertex_if_not_present i
  done;
  g |> iter_edges (fun n1 n2 ->
    let k1 = Hashtbl.find hscc n1 in
    let k2 = Hashtbl.find hscc n2 in
    if k1 <> k2
    then g2 |> add_edge k1 k2;
  );
  g2
let strongly_connected_components_condensation a b =
  Common.profile_code "Graph.scc_condensation" (fun () ->
    strongly_connected_components_condensation2 a b)


let depth_nodes2 g =
  if OG.Dfs.has_cycle g.og
  then failwith "not a DAG";

  let hres = Hashtbl.create 101 in

  (* do in toplogical order *)
  g.og |> OG.Topological.iter (fun v ->
    let ncurrent =
      if not (Hashtbl.mem hres v)
      then 0
      else Hashtbl.find hres v
    in
    Hashtbl.replace hres v ncurrent;
    let xs = OG.succ g.og v in
    xs |> List.iter (fun v2 ->
      let nchild =
        if not (Hashtbl.mem hres v2)
        then ncurrent + 1
        (* todo: max or min? can lead to different metrics,
         * either to know the longest path from the top, or to know some
         * possible shortest path from the top.
        *)
        else max (Hashtbl.find hres v2) (ncurrent + 1)
      in
      Hashtbl.replace hres v2 nchild;
      ()
    );
  );

  let hfinalres = Hashtbl.create 101 in
  hres |> Hashtbl.iter (fun v n ->
    Hashtbl.add hfinalres (key_of_vertex v g) n
  );
  hfinalres

let depth_nodes a =
  Common.profile_code "Graph.depth_nodes" (fun () -> depth_nodes2 a)

(*****************************************************************************)
(* Graph visualization and debugging *)
(*****************************************************************************)

let display_with_gv g =
  OG.display_with_gv g.og

let print_graph_generic ?(launch_gv=true) ?(extra_string="") ~str_of_key
    filename g =
  Common.with_open_outfile filename (fun (pr,_) ->
    pr "digraph misc {\n" ;
    (* pr "size = \"10,10\";\n" ; *)
    pr extra_string;
    pr "\n";

    g.og |> OG.iter_vertex (fun v ->
      let k = key_of_vertex v g in
      (* todo? could also use the str_of_key to represent the node *)
      pr (spf "%d [label=\"%s\"];\n"
            (OG.V.label v)
            (str_of_key k));
    );

    g.og |> OG.iter_vertex (fun v ->
      let succ = OG.succ g.og v in
      succ |> List.iter (fun v2 ->
        pr (spf "%d -> %d;\n" (OG.V.label v) (OG.V.label v2));
      )
    );
    pr "}\n" ;
  );
  if launch_gv
  then failwith "TODO: Ograph_extended.launch_gv_cmd filename";
  (* Ograph_extended.launch_gv_cmd filename; *)
  ()

let tmpfile = "/tmp/graph_ml.dot"

let display_strongly_connected_components ~str_of_key hscc g =
  print_graph_generic ~str_of_key:(fun k ->
    let s = str_of_key k in
    spf "%s (scc=%d)" s (Hashtbl.find hscc k)
  ) tmpfile g


(*****************************************************************************)
(* stat *)
(*****************************************************************************)
let stat g =
  pr2_gen ("cnt = ", g.cnt);
  ()
