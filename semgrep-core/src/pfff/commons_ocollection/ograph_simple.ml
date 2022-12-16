open Common

open Oassocb
open Osetb

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An imperative directed polymorphic graph.
 *
 * What is the difference with ograph_extended? With ograph_extended we
 * dont force the user to have a key; we generate those keys as he
 * adds nodes. Here we assume the user already has an idea of what kind
 * of key he wants to use (a string, a filename, a, int, whatever).
 * This removes the need to remember some 'node -> nodeindex' mapping.
 * It's very easy to add edge between entities with ograph_simple.
*)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

class ['key, 'a,'b] ograph_mutable =
  let build_assoc () = new oassocb [] in
  let build_set ()   = new osetb Set_.empty in

  object(o)

    val mutable succ = build_assoc()
    val mutable pred = build_assoc()
    val mutable nods = (build_assoc() : ('key, 'a) Oassocb.oassocb)

    method add_node i (e: 'a) =
      nods <- nods#add (i, e);
      pred <- pred#add (i, build_set() );
      succ <- succ#add (i, build_set() );

    method del_node (i) =
      (* check: e is effectively the index associated with e,
         and check that already in *)

      (* todo: assert that have no pred and succ, otherwise
       * will have some dangling pointers
      *)
      nods <- nods#delkey i;
      pred <- pred#delkey i;
      succ <- succ#delkey i;

    method del_leaf_node_and_its_edges (i) =
      let succ = o#successors i in
      if not (succ#null)
      then failwith "del_leaf_node_and_its_edges: have some successors"
      else begin
        let pred = o#predecessors i in
        pred#tolist |> List.iter (fun (k, edge) ->
          o#del_arc (k,i) edge;
        );
        o#del_node i
      end

    method leaf_nodes () =
      let (set : 'key Oset.oset)  = build_set () in
      o#nodes#tolist |> List.fold_left (fun acc (k,_v) ->
        if (o#successors k)#null
        then acc#add k
        else acc
      ) set


    method replace_node i (e: 'a) =
      assert (nods#haskey i);
      nods <- nods#replkey (i, e);

    method add_node_if_not_present i (e: 'a) =
      if nods#haskey i
      then ()
      else o#add_node i e

    method add_arc (a,b) (v: 'b) =
      succ <- succ#replkey (a, (succ#find a)#add (b, v));
      pred <- pred#replkey (b, (pred#find b)#add (a, v));
    method del_arc (a,b) v =
      succ <- succ#replkey (a, (succ#find a)#del (b,v));
      pred <- pred#replkey (b, (pred#find b)#del (a,v));

    method successors   e = succ#find e
    method predecessors e = pred#find e

    method nodes = nods
    method allsuccessors = succ

    (* detect if no loop ? *)
    method ancestors k =
      let empty_set = build_set() in


      let rec aux acc x =
        if acc#mem x
        then
          (* bugfix: have_loop := true; ? not, not necessarally.
           * if you got a diamon, seeing a second time the same
           * x does not mean we are in a loop
          *)
          acc
        else
          let acc = acc#add x in
          let prefs = o#predecessors x in
          let prefs = prefs#tolist |> List.map fst in
          prefs |> List.fold_left (fun acc x -> aux acc x) acc
      in
      let set = aux empty_set k in
      let set = set#del k in
      set

  end

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

let print_ograph_generic ~str_of_key ~str_of_node filename g =
  Common.with_open_outfile filename (fun (pr,_) ->
    pr "digraph misc {\n" ;
    pr "size = \"10,10\";\n" ;

    let nodes = g#nodes in
    nodes#iter (fun (k,node) ->
      pr (spf "%s [label=\"%s\"];\n" (str_of_key k) (str_of_node k node))
    );
    nodes#iter (fun (k,_node) ->
      let succ = g#successors k in
      succ#iter (fun (j,_edge) ->
        pr (spf "%s -> %s;\n" (str_of_key k) (str_of_key j));
      );
    );
    pr "}\n" ;
  );
  Ograph_extended.launch_gv_cmd filename;
  ()
