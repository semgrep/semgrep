(* TODO: Will not work in Windows! *)

let arch () =
  let chan = Unix.open_process_in "uname -m" in
  In_channel.input_all chan |> String.trim

let kernel () =
  let chan = Unix.open_process_in "uname" in
  In_channel.input_all chan |> String.trim
