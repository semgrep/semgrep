(*s: ograph.mli *)
class virtual ['a] ograph :
  object ('o)
    method virtual empty : 'o

    method virtual add_node : 'a -> 'o
    method virtual del_node : 'a -> 'o

    method virtual add_arc : 'a * 'a -> 'o
    method virtual del_arc : 'a * 'a -> 'o


    method virtual nodes : 'a Oset.oset
    method virtual predecessors : 'a -> 'a Oset.oset
    method virtual successors : 'a -> 'a Oset.oset

    method virtual ancestors : 'a Oset.oset -> 'a Oset.oset
    method virtual brothers : 'a -> 'a Oset.oset
    method virtual children : 'a Oset.oset -> 'a Oset.oset

    method mydebug : ('a * 'a list) list
  end

(*e: ograph.mli *)
