open Common
open Oassoc

class ['a, 'b] oassocb _xs =
  object (o)
    inherit ['a, 'b] oassoc
    val data = Map_.empty
    method empty = {<data = Map_.empty>}
    method add (k, v) = {<data = Map_.add k v data>}
    method! replkey (k, v) = {<data = Map_.add k v (Map_.remove k data)>}
    method iter f = Map_.iter (Common.curry f) data
    method view = raise Todo
    method del (k, _v) = {<data = Map_.remove k data>}
    method mem _e = raise Todo
    method null = Map_.is_empty data
    method assoc k = Map_.find k data
    method delkey k = {<data = Map_.remove k data>}
    method keys = List_.map fst o#tolist
  end
