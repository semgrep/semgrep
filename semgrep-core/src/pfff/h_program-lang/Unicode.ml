(*
   Utilities for dealing with Unicode issues.
*)

let input_all ic =
  (* works only on regular files *)
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

let input_and_replace_non_ascii ~replacement_byte ic =
  input_all ic
  |> String.map (fun c ->
    if Char.code c >= 128 then replacement_byte
    else c
  )
