
class ['a] ograph2way :
  (< add : 'a * 'a Oset.oset -> 'c; delkey : 'a -> 'c;
     find : 'a -> 'a Oset.oset; iter : ('a * 'd -> unit) -> 'e;
     replkey : 'a * 'a Oset.oset -> 'c; .. >
   as 'c) ->
  (< add : 'a * 'a Oset.oset -> 'f; delkey : 'a -> 'f;
     find : 'a -> 'a Oset.oset; replkey : 'a * 'a Oset.oset -> 'f; .. >
   as 'f) ->
  (unit -> 'a Oset.oset) ->
  object ('o)
    inherit ['a] Ograph.ograph
    val pred : 'f
    val succ : 'c

    (* ograph concrete instantiation of virtual methods *)
    method empty : 'o

    method add_node : 'a -> 'o
    method del_node : 'a -> 'o

    method add_arc : 'a * 'a -> 'o
    method del_arc : 'a * 'a -> 'o

    method nodes : 'a Oset.oset
    method predecessors : 'a -> 'a Oset.oset
    method successors : 'a -> 'a Oset.oset

    method ancestors : 'a Oset.oset -> 'a Oset.oset
    method brothers : 'a -> 'a Oset.oset
    method children : 'a Oset.oset -> 'a Oset.oset

  end
