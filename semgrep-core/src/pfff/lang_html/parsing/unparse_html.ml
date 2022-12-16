
(*
let (unparse_tree: html_tree2 -> html_raw) = fun html ->
  let buf = Buffer.create 1000 in
  let ch = new  Netchannels.output_buffer buf in
  Nethtml.write ch html;
  HtmlRaw (Buffer.contents buf)
*)
