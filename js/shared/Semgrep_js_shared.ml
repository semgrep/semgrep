open Js_of_ocaml

(* js_of_ocaml gives each executable its own pseudo-filesystem, which is problematic for browser environments
   because it effectively segregates the engine from the parsers, and means we have to write the rules and target
   everywhere. to work around this, we expose getters and setters so that we can have our parsers
   inherit their engine's mount points. This is effectively a no-op in node.
   (see companion setter in ../engine/Main.ml) *)
external set_jsoo_mount_point : 'any Js.js_array -> unit
  = "set_jsoo_mount_point"

external set_parser_wasm_module : 'any -> unit = "set_parser_wasm_module"

let make_js_module (langs : Lang.t list) parse_target parse_pattern =
  let lang_names =
    Array.of_list
      (Common.map (fun x -> Js.string (Lang.to_lowercase_alnum x)) langs)
  in
  Js.export "createParser" (fun wasm_module ->
      set_parser_wasm_module wasm_module;
      object%js
        method getLangs = Js.array lang_names
        method setMountpoints = set_jsoo_mount_point

        method parseTarget lang file =
          parse_target (Lang.of_string (Js.to_string lang)) (Js.to_string file)

        method parsePattern print_errors lang str =
          parse_pattern (Js.to_bool print_errors)
            (Lang.of_string (Js.to_string lang))
            (Js.to_string str)
      end)
