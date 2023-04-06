open Js_of_ocaml

let make_js_module parse parse_pattern =
  Js.export_all
    (object%js
       method parseFile = parse
       method parsePattern = parse_pattern

       method writeFile filename contents =
         let oc = open_out_bin filename in
         Printf.fprintf oc "%s" contents;
         close_out oc

       method readFile filename =
         let ic = open_in_bin filename in
         let s = really_input_string ic (in_channel_length ic) in
         close_in ic;
         s
    end)
