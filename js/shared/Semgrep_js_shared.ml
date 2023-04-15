open Js_of_ocaml

(* js_of_ocaml gives each executable its own pseudo-filesystem, which is problematic for browser environments
   because it effectively segregates the engine from the parsers, and means we have to write the rules and target
   everywhere. to work around this, we expose getters and setters so that we can have our parsers
   inherit their engine's mount points. This is effectively a no-op in node.
   (see companion setter in ../engine/Main.ml) *)
external set_jsoo_mount_point : 'any list -> unit = "set_jsoo_mount_point"
external set_parser_wasm_module : 'any -> unit = "set_parser_wasm_module"

let make_js_module (lang : Lang.t) parse_target parse_pattern =
  Js.export_all
    (object%js
       method setMountPoints = set_jsoo_mount_point
       method setParserWasmModule = set_parser_wasm_module
       method getLang = lang
       method parseTarget file = parse_target (Js.to_string file)

       method parsePattern print_errors str =
         parse_pattern (Js.to_bool print_errors) (Js.to_string str)
    end)
