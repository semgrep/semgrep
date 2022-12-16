open Common

open Osequence

(* growing array ? initialise with None,
 * and generate exception when not defined or have an arraydefault
 * update: can use dynArray ?
*)

(* !!take care!!, this is not a pure data structure *)
class ['a] oarray n el =
  object(o: 'o)
    inherit ['a] osequence

    val data = Array.make n el

    method empty = raise Todo
    method add (i,v)  =
      Array.set data i v;
      o

    method iter f =
      Array.iteri (Common2.curry f) data
    method view = raise Todo

    method assoc i =
      Array.get data i

    method null = raise Todo
    method nth = raise Todo
    method mem = raise Todo
    method last = raise Todo
    method first = raise Todo
    method delkey = raise Todo

    method keys = raise Todo

    method del = raise Todo
    method fromlist = raise Todo
    method length =
      Array.length data

    (* method create: int -> 'a -> 'o =
       raise Todo
    *)
    (* method put: make more explicit the fact that array do side effect *)
  end
