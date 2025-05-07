(*s: oset.mli *)
class virtual ['a] oset : object ('o)
  inherit ['a] Ocollection.ocollection
  method cardinal : int
  method virtual inter : 'o -> 'o
  method virtual minus : 'o -> 'o
  method virtual union : 'o -> 'o
  method is_singleton : bool
  method is_subset_of : 'o -> bool
  method is_equal : 'o -> bool
  method virtual toset : 'd
  method tosetb : 'a Set_.t
  (*
    method toseti : Seti.seti
    method tosetpt : SetPt.t
     *)
end

(*e: oset.mli *)
