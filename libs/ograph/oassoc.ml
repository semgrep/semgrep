open Ocollection

(* assoc, also called map or dictionnary *)
class virtual ['a, 'b] oassoc =
  object (o : 'o)
    inherit ['a * 'b] ocollection
    method virtual assoc : 'a -> 'b
    method virtual delkey : 'a -> 'o

    (* pre: must be in *)
    method replkey : 'a * 'b -> 'o = fun (k, v) -> o#add (k, v)
    method virtual keys : 'a list (* or 'a oset ? *)
    method find : 'a -> 'b = fun k -> o#assoc k

    method haskey : 'a -> bool =
      fun k ->
        try
          ignore (o#assoc k);
          true
        with
        | Not_found -> false
  end
