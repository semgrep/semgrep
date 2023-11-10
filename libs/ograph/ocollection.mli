(*s: ocollection.mli *)
type ('a, 'b) view = Empty | Cons of 'a * 'b

class virtual ['a] ocollection : object ('o)
  inherit Objet.objet
  method virtual empty : 'o
  method virtual add : 'a -> 'o
  method virtual iter : ('a -> unit) -> unit
  method virtual view : ('a, 'o) view

  (* no need virtual, but better to force redefine for efficiency *)
  method virtual del : 'a -> 'o
  method virtual mem : 'a -> bool
  method virtual null : bool

  (* effect version *)
  method add2 : 'a -> unit
  method del2 : 'a -> unit
  method clear : unit
  method fold : ('c -> 'a -> 'c) -> 'c -> 'c
  method fromlist : 'a list -> 'o
  method tolist : 'a list
  method exists : ('a -> bool) -> bool
  method filter : ('a -> bool) -> 'o
  method length : int
  method getone : 'a
  method others : 'o
end

(*e: ocollection.mli *)
