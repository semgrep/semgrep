open Oassoc

class virtual ['a] osequence =
  object(_o: 'o)
    (* inherit ['a] ocollection *)
    inherit [int, 'a] oassoc

    method virtual nth: int -> 'a
    method virtual first: 'a
    method virtual last: 'a
    (* head tail push pop top cons snoc *)
  end
