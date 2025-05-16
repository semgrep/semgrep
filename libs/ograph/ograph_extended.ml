(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * An imperative directed polymorphic graph.
 *
 * todo?: prendre en parametre le type de finitemap et set?
 * todo?: add_arc doit ramer, car del la key, puis add. Better to
 *  have a ref to a set?
 *
 * opti: graph with pointers and a tag visited => need keep global value
 *  visited_counter.  check(that node is in, ...), display.
 * opti: when the graph structure is stable, have a method compact,  that
 *  transforms that in a matrix (assert that all number between 0 and
 *  free_index are used,  or do some defrag-like-move/renaming).
 *)

open Maps

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type nodei = int

(*****************************************************************************)
(* Mutable version *)
(*****************************************************************************)

class ['a, 'b] ograph_mutable =
  object
    val mutable free_index = 0
    val mutable succ = Int_map.empty
    val mutable pred = Int_map.empty
    val mutable nods = Int_map.empty

    method add_node (e : 'a) =
      let i = free_index in
      nods <- Int_map.add i e nods;
      pred <- Int_map.add i Set_.empty pred;
      succ <- Int_map.add i Set_.empty succ;
      free_index <- i + 1;
      i

    method add_nodei i (e : 'a) =
      nods <- Int_map.add i e nods;
      pred <- Int_map.add i Set_.empty pred;
      succ <- Int_map.add i Set_.empty succ;
      free_index <- max free_index i + 1

    method del_node i =
      (* check: e is effectively the index associated with e,
         and check that already in *)

      (* todo: assert that have no pred and succ, otherwise
       * will have some dangling pointers
       *)
      nods <- Int_map.remove i nods;
      pred <- Int_map.remove i pred;
      succ <- Int_map.remove i succ

    method replace_node (i, (e : 'a)) =
      assert (Int_map.mem i nods);
      nods <- Int_map.add i e nods

    method add_arc ((a, b), (v : 'b)) =
      succ <- Int_map.add a (Set_.add (b, v) (Int_map.find a succ)) succ;
      pred <- Int_map.add b (Set_.add (a, v) (Int_map.find b pred)) pred

    method del_arc ((a, b), v) =
      succ <- Int_map.add a (Set_.remove (b, v) (Int_map.find a succ)) succ;
      pred <- Int_map.add b (Set_.remove (a, v) (Int_map.find b pred)) pred

    method successors e = Int_map.find e succ
    method predecessors e = Int_map.find e pred
    method nodes = nods
    method nb_nodes = Int_map.cardinal nods
  end
