
let string_of_any ast =
  Ast_js.show_any ast

let load db gen =
  try
    let in_channel = open_in_bin db in
    let data = Marshal.from_channel in_channel in
    close_in in_channel;
    data
  with _ ->
    let data = gen() in
    let out_channel = open_out_bin db in
    Marshal.to_channel out_channel data [];
    close_out out_channel;
    data
