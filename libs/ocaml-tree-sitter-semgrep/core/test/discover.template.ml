(* Configure tree-sitter C flags and library flags using pkg-config. *)
open Base
module C = Configurator.V1

let make_absolute path =
  if Stdlib.Filename.is_relative path then
    Stdlib.Filename.concat (Stdlib.Sys.getcwd ()) path
  else
    path

let set_pkg_config_path () =
  match Stdlib.Sys.getenv_opt "PKG_CONFIG_PATH" with
  | None -> ()
  | Some pkg_config_path ->
      let separator = if Sys.win32 then ';' else ':' in
      let paths = String.split pkg_config_path ~on:separator in
      let absolute_paths = List.map paths ~f:make_absolute in
      let new_pkg_config_path = String.concat ~sep:(String.of_char separator) absolute_paths in
      Unix.putenv "PKG_CONFIG_PATH" new_pkg_config_path

let () =
  C.main ~name:"tree-sitter-config" (fun c ->
    (* Set PKG_CONFIG_PATH with absolute paths before using pkg-config. This is
       because pkg-config via configurator may be run in arbitrary directories
       by dune, rather than just relative to the prior working directory, so
       relative paths will no longer be relevant. *)
    set_pkg_config_path ();
    let conf =
      match C.Pkg_config.get c with
      | None -> failwith "discover.ml failed to invoke pkg-config\n";
      | Some pc ->
          (match C.Pkg_config.query pc ~package:"tree-sitter" with
           | None ->
               (match Stdlib.Sys.getenv_opt "PKG_CONFIG_PATH" with
                | Some path -> Stdlib.Printf.eprintf "[INFO] PKG_CONFIG_PATH is %s\n" path
                | None -> ());
               failwith "discover.ml failed to find tree-sitter with pkg-config\n";
           | Some deps -> deps)
    in
    C.Flags.write_sexp "c_flags.sexp" conf.cflags;
    C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
