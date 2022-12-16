open Common

open Ocollection
open Oset
open Ograph

open Osetb

(* graph2way  prend en parametre le type de finitemap et set a prendre
 * todo? add_arc doit ramer, car del la key, puis add =>
 * better to have a ref to a set
 * todo: efficient graph: with pointers and a tag: visited
 * => need keep global value visited_counter
 * check(that node is in, ...), display
 *
 * pourrait remettre val nods, a la place de les calculer. mais bon
 * s'en sert pas vraiment car y'a pas de notion d'identifiant de noeud
 * et de label.
 *
 * invariant: key in pred is also in succ (completness) and value in
 * either table is a key also
*)
class ['a] ograph2way   asucc apred (*f1*) f2 =
  object(o)
    inherit ['a] ograph

    val succ = asucc  (* f1() ## new oassocb [] *)
    val pred = apred  (* f1() ## new oassocb [] *)
    (* val nods = anodes ##f2() ## new osetb [] *)

    method empty = raise Todo (*{< succ = f1() ;(* new oassocb []; *)
                                     pred = f1(); (* new oassocb []; *)
                                     (* nods = f2(); ##new osetb []; *)
                                  >}*)

    method add_node e = {< (* nods = nods#add e; *)
      pred = pred#add (e, f2() );(* new osetb []); *)
      succ = succ#add (e, f2() );(* new osetb []); *)
    >}
    method del_node e = {< (* nods = nods#del e; *)
      pred = pred#delkey e;
      succ = succ#delkey e;
    >}
    method add_arc (a,b) = {<
      succ = succ#replkey (a, (succ#find a)#add b);
      pred = pred#replkey (b, (pred#find b)#add a);
    >}
    method del_arc (a,b) = {<
      succ = succ#replkey (a, (succ#find a)#del b);
      pred = pred#replkey (b, (pred#find b)#del a);
    >}
    method successors   e = succ#find e
    method predecessors e = pred#find e
    method nodes = (* nods *)
      (*  could take pred,  same *)
      (* caml typing sux, arrive pas a faire:  pred#fold (fun a (k,v) -> a#add k) (new osetb Set_.empty)  *)
      let a = ref (new osetb Set_.empty) in
      succ#iter (fun (k,_v) -> a := !a#add k);
      !a



    method ancestors xs =
      let rec aux xs acc =
        match xs#view with (* could be done with an iter *)
        | Empty -> acc
        | Cons(x, xs) -> (acc#add x)
                         |> (fun newacc -> aux (o#predecessors x) newacc)
                         |> (fun newacc -> aux xs newacc)
      in aux xs (f2()) (* (new osetb []) *)

    method children  xs =
      let rec aux xs acc =
        match xs#view with (* could be done with an iter *)
        | Empty -> acc
        | Cons(x, xs) -> (acc#add x)
                         |> (fun newacc -> aux (o#successors x) newacc)
                         |> (fun newacc -> aux xs newacc)
      in aux xs (f2()) (* (new osetb []) *)


    method brothers  x =
      let parents = o#predecessors x in
      (parents#fold (fun acc e -> acc $++$ o#successors e) (f2()))#del x

  end
