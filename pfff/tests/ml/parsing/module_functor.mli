
module Make(Ord: Map.OrderedType): (S with type key = Ord.t)
