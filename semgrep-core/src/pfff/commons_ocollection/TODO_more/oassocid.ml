open Common
open Oassoc

(* just a class that behave as fun x -> x *)
class ['a] oassoc_id xs =
  object(o)
    inherit ['a,'a] oassoc

    method empty = {< >}
    method add (k,v) = {<  >}
    method iter f = raise Todo
    method view = raise Todo

    method del (k,v) = {<  >}
    method mem e = raise Todo
    method null = raise Todo

    method assoc k = k
    method delkey k = {<  >}

    method keys =
      List.map fst (o#tolist)

  end
