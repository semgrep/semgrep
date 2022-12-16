open Ocollection
open Oset

class ['a] oseti xs   =
  object(o)
    inherit [int] oset

    val data = xs (*  Seti.empty *)
    method toseti = data
    method toset = Obj.magic data

    method empty = {< data = Seti.empty >}
    method add e = {< data = Seti.add e data >}
    method iter f = Seti.iter f   data
    method view =
      if Seti.is_empty data
      then Empty
      else let el = Seti.choose data in Cons (el, o#del el)

    method del e = {< data = Seti.remove e data >}
    method mem e  = Seti.mem e    data
    method null   = Seti.is_empty data

    method tolist = Seti.elements data
    method length = Seti.cardinal data

    method union s = {< data = Seti.union data s#toseti >}
    method inter s = {< data = Seti.inter data s#toseti >}
    method minus s = {< data = Seti.diff  data s#toseti >}

    method invariant () = Seti.invariant data
    method to_string () = Seti.string_of_seti data

    method misc_op_hook () = {< data = Seti.patch3 data >}
  end
