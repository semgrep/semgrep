open Js_of_ocaml

let make_js_module parse parse_pattern =
  (* if running as WebWorker, handle parse() and parsePattern() messages *)
  if Js.Unsafe.global##.onmessage != Js.undefined then
    let handle_message msg = match msg with
      | ("parse", filename) -> Worker.post_message (parse filename)
      | ("parsePattern", str) -> Worker.post_message (parse_pattern str)
    | _ -> () in
      Worker.set_onmessage handle_message
  else
    (); (* TODO: am I understanding ocaml if-statements correctly? didn't seem to work without the else *)
  Js.export_all (object%js
      method parse = parse
      method parsePattern = parse_pattern
    end)
