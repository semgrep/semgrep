(*
   String manipulation utilities.
*)

let starts_with ~prefix s =
  if String.length s >= String.length prefix then
    try
      for i = 0 to String.length prefix - 1 do
        if s.[i] <> prefix.[i] then
          raise Exit
      done;
      true
    with Exit ->
      false
  else
    false
