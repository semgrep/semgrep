module type S = sig type t end
module type T = (X : S) -> S with type t = X.t
module type U = () -> S
module type V = functor (X : S) -> S
