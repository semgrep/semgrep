class ['a] oassoc_id xs =
  object(o)
    inherit ['a,'a] oassoc

    method empty = {< >}
  end
