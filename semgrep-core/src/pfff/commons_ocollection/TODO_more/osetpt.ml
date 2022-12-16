open Ocollection
open Oset


class ['a] osetpt xs   =
  object(o)
    inherit [int] oset

    val data = SetPt.empty
    method tosetpt = data
    (* if put [] then no segfault, if [11] then segfault *)
    method toset = Obj.magic data

    method empty = {< data = SetPt.empty >}
    method add e = {< data = SetPt.add e data >}
    method iter f = SetPt.iter f   data
    method view =
      if SetPt.is_empty data
      then Empty
      else let el = SetPt.choose data in Cons (el, o#del el)

    method del e = {< data = SetPt.remove e data >}
    method mem e  = SetPt.mem e    data
    method null   = SetPt.is_empty data

    method tolist = SetPt.elements data
    method length = SetPt.cardinal data

    method union s = {< data = SetPt.union data s#tosetpt >}
    method inter s = {< data = SetPt.inter data s#tosetpt >}
    method minus s = {< data = SetPt.diff  data s#tosetpt >}

  end
