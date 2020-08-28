module NameID : sig
  type t
  val get : t -> int
  val set : int -> t
end = struct
  type t = int
  let get x = x
  let set y = y
end
