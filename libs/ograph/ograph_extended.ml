open Oassocb

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

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type nodei = int

(*****************************************************************************)
(* Mutable version *)
(*****************************************************************************)

class ['a, 'b] ograph_mutable =
  let build_assoc () = new oassocb [] in

  object
    val mutable free_index = 0
    val mutable succ = build_assoc ()
    val mutable pred = build_assoc ()
    val mutable nods = build_assoc ()

    method add_node (e : 'a) =
      let i = free_index in
      nods <- nods#add (i, e);
      pred <- pred#add (i, Set_.empty);
      succ <- succ#add (i, Set_.empty);
      free_index <- i + 1;
      i

    method add_nodei i (e : 'a) =
      nods <- nods#add (i, e);
      pred <- pred#add (i, Set_.empty);
      succ <- succ#add (i, Set_.empty);
      free_index <- max free_index i + 1

    method del_node i =
      (* check: e is effectively the index associated with e,
         and check that already in *)

      (* todo: assert that have no pred and succ, otherwise
       * will have some dangling pointers
       *)
      nods <- nods#delkey i;
      pred <- pred#delkey i;
      succ <- succ#delkey i

    method replace_node (i, (e : 'a)) =
      assert (nods#haskey i);
      nods <- nods#replkey (i, e)

    method add_arc ((a, b), (v : 'b)) =
      succ <- succ#replkey (a, Set_.add (b, v) (succ#find a));
      pred <- pred#replkey (b, Set_.add (a, v) (pred#find b))

    method del_arc ((a, b), v) =
      succ <- succ#replkey (a, Set_.remove (b, v) (succ#find a));
      pred <- pred#replkey (b, Set_.remove (a, v) (pred#find b))

    method successors e = succ#find e
    method predecessors e = pred#find e
    method nodes = nods
    method nb_nodes = nods#length
  end
