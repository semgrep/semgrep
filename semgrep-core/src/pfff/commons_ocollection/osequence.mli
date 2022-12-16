(*s: osequence.mli *)
class virtual ['a] osequence :
  object ('o)
    inherit [int, 'a] Oassoc.oassoc

    method virtual nth : int -> 'a
    method virtual first : 'a
    method virtual last : 'a
  end

(*e: osequence.mli *)
