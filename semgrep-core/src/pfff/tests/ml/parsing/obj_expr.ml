let hash_with_default fv =
  object
    val h = Hashtbl.create 101
    method to_list = hash_to_list h
    method to_h = h

    method add k v =
      Hashtbl.replace h k v
    method assoc k =
      Hashtbl.find h k
    method update k f =
      hupdate_default k ~update:f ~default:fv h
  end
