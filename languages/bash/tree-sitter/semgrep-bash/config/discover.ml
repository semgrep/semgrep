(* This was adapted from the dune manual at
   https://dune.readthedocs.io/en/stable/quick-start.html
*)
module C = Configurator.V1

let () =
  C.main ~name:"configure-c-flags" (fun c ->
    let c_library_flags =
      (* The -rpath option tells the linker to hardcode this search location in
         the binary.
         This works as long as libtree-sitter stays where it is, which is fine
         for test executables. Production executables should instead link
         statically against libtree-sitter to avoid problems in locating the
         library at runtime. *)
      match C.ocaml_config_var c "os_type" with
      | Some "Win32" -> ["()"] (* Compilation on Windows does not support rpath *)
      | _ -> ["(-Wl,-rpath,%{env:TREESITTER_LIBDIR=/usr/local/lib})"]
    in
    C.Flags.write_lines "c_library_flags.sexp" c_library_flags)
