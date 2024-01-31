module D = Dependency

let version_parser =
  let open Angstrom in
  let int =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| int_of_string
  in
  let dot = char '.' in
  let any_string = take_while1 (function _ -> true) in
  let version =
    let* major = int <* dot in
    let* minor = int <* dot in
    let* incrementals = sep_by1 dot int in
    return @@ D.Version { major; minor; incrementals }
  in
  version <|> (any_string >>| fun s -> D.Other s)

let constraint_parser =
  let open Angstrom in
  let constr =
    choice
      [
        string "==" *> return D.Eq;
        string ">=" *> return D.Gte;
        string "<=" *> return D.Lte;
        string ">" *> return D.Gt;
        string "<" *> return D.Lt;
      ]
  in
  let* c = constr in
  many (char ' ')
  *>
  let* v = version_parser in
  return D.{ version = v; constraint_ = c }

let parse_version str =
  Angstrom.parse_string ~consume:All version_parser str |> Result.get_ok

let parse_constraint str =
  Angstrom.parse_string ~consume:All constraint_parser str |> Result.get_ok
