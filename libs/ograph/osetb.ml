open Ocollection
open Oset

let empty = Set_.empty

class ['a] osetb xs =
  object (o)
    inherit ['a] oset
    val data = xs (*  Set_.empty *)
    method! tosetb = data

    (* if put [] then no segfault, if [11] then segfault *)
    (* nosemgrep: forbid-obj-magic *)
    method toset = Obj.magic data
    method empty = {<data = Set_.empty>}
    method add e = {<data = Set_.add e data>}
    method iter f = Set_.iter f data

    method view =
      if Set_.is_empty data then Empty
      else
        let el = Set_.choose data in
        Cons (el, o#del el)

    method del e = {<data = Set_.remove e data>}
    method mem e = Set_.mem e data
    method null = Set_.is_empty data
    method! tolist = Set_.elements data
    method! length = Set_.cardinal data
    method union s = {<data = Set_.union data s#tosetb>}
    method inter s = {<data = Set_.inter data s#tosetb>}
    method minus s = {<data = Set_.diff data s#tosetb>}
    (* todo: include, ... *)
  end
