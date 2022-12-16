(*s: oarray.mli *)
(* !!take care!!, this is not a pure data structure *)

class ['a] oarray : int -> 'a ->
  object ('o)
    inherit ['a] Osequence.osequence

    (* ocollection concrete instantiation of virtual methods *)
    method empty : 'o
    method add : (int * 'a) -> 'o

    method iter : (int * 'a -> unit) -> unit
    method view : (int * 'a, 'o) Ocollection.view

    method del : (int * 'a) -> 'o
    method mem : int * 'a -> bool
    method null : bool


    (* oassoc concrete instantiation of virtual methods *)
    method assoc : int -> 'a
    method delkey : int -> 'o

    method keys: int list

    (* osequence concrete instantiation of virtual methods *)
    method first : 'a
    method last : 'a
    method nth : int -> 'a

  end

(*e: oarray.mli *)
