(* Exposing this function allows us to share the libyaml WASM module with the engine without depending on global state *)
external set_libyaml_wasm_module : 'any -> unit = "set_libyaml_wasm_module"
