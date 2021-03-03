
module Str_engine : sig
  type t = Str.regexp
  val matching_string: string -> t
  val run: t -> string -> bool
end
