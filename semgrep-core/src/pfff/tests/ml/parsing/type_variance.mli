module IntMap :
sig
  type key = int
  type +'a t
  val empty : 'a t
end
