let () =
  let module C = Configurator.V1 in
  C.main ~name:"unwind" (fun c ->
      let pc = C.Pkg_config.get c in
      if Option.is_none pc then failwith "pkg-config not found"
      else
        let pc = Option.get pc in
        let get_lib lib = C.Pkg_config.query pc ~package:lib in
        let conf_symbolization : C.Pkg_config.package_conf =
          match (get_lib "libdw", get_lib "libdwarf") with
          | None, None -> { C.Pkg_config.libs = []; cflags = [] }
          | Some libdw, _ ->
              {
                libdw with
                C.Pkg_config.libs =
                  libdw.libs @ [ "-lbz2"; "-lz"; "-llzma"; "-lzstd" ];
              }
          | None, Some libdwarf ->
              { libdwarf with C.Pkg_config.libs = libdwarf.libs @ [] }
        in
        let default_unwind : C.Pkg_config.package_conf =
          { libs = []; cflags = [] }
        in
        let conf_unwind : C.Pkg_config.package_conf =
          match get_lib "libunwind" with
          | _ when Sys.win32 || Sys.cygwin ->
              (*https://github.com/bombela/backward-cpp/issues/176#issuecomment-1382442652  *)
              { libs = [ "-ldbghelp" ]; cflags = [] }
          | None -> default_unwind
          | Some unwind -> { unwind with libs = unwind.libs }
        in
        let conf_c_flags : C.Pkg_config.package_conf =
          { libs = [ "-lstdc++" ]; cflags = [ "-fPIC" ] }
        in

        let conf_cpp_flags = [ "-std=c++11"; "-fPIC" ] in
        C.Flags.write_sexp "cpp_flags.sexp"
          (* nosemgrep: no-list-concat *)
          (List.flatten
             [ conf_symbolization.cflags; conf_unwind.cflags; conf_cpp_flags ]);
        C.Flags.write_sexp "c_flags.sexp"
          (* nosemgrep: no-list-concat *)
          (List.flatten
             [
               conf_symbolization.cflags;
               conf_unwind.cflags;
               conf_c_flags.cflags;
             ]);
        C.Flags.write_sexp "c_library_flags.sexp"
          (* nosemgrep: no-list-concat *)
          (List.flatten
             [ conf_symbolization.libs; conf_unwind.libs; conf_c_flags.libs ]))
