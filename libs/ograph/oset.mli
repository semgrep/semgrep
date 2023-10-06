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

val ( $??$ ) : 'a -> < mem : 'a -> bool ; .. > -> bool
val ( $++$ ) : < union : 'a -> 'o ; .. > -> 'a -> 'o
val ( $**$ ) : < inter : 'a -> 'o ; .. > -> 'a -> 'o
val ( $--$ ) : < minus : 'a -> 'o ; .. > -> 'a -> 'o
val ( $<<=$ ) : < is_subset_of : 'a -> bool ; .. > -> 'a -> bool
val ( $==$ ) : < is_equal : 'a -> bool ; .. > -> 'a -> bool
val mapo : ('a -> 'o) -> 'o oset -> 'a oset -> 'o oset

(*e: oset.mli *)
