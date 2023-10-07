(*s: oassoc.mli *)

class virtual ['a, 'b] oassoc : object ('o)
  inherit ['a * 'b] Ocollection.ocollection
  method virtual assoc : 'a -> 'b
  method virtual delkey : 'a -> 'o

  (* may raise NotFound *)
  method find : 'a -> 'b
  method find_opt : 'a -> 'b option
  method haskey : 'a -> bool
  method replkey : 'a * 'b -> 'o

  (* better to implement it yourself *)
  method virtual keys : 'a list
  method apply : 'a -> ('b -> 'b) -> 'o
  method apply_with_default : 'a -> ('b -> 'b) -> (unit -> 'b) -> 'o

  (* effect version *)
  method apply_with_default2 : 'a -> ('b -> 'b) -> (unit -> 'b) -> unit
end

(*e: oassoc.mli *)
