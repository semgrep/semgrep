
let require capa l =
  List.fold_right (&&)
    (List.map (require_capa capa) l)
    true
