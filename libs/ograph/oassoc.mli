(*s: oassoc.mli *)

class virtual ['a, 'b] oassoc : object ('o)
  inherit ['a * 'b] Ocollection.ocollection
  method virtual assoc : 'a -> 'b
  method virtual delkey : 'a -> 'o

  (* may raise NotFound *)
  method find : 'a -> 'b
  method haskey : 'a -> bool
  method replkey : 'a * 'b -> 'o

  (* better to implement it yourself *)
  method virtual keys : 'a list
end

(*e: oassoc.mli *)
