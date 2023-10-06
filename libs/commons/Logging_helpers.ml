(*
   Set up logging globally and for each module based on what
   we found on the command line or config files.

   TODO? could move setup in commons/Logging.ml
*)

open File.Operators

let logger = Logging.get_logger [ __MODULE__ ]

let setup ~debug ~log_config_file ~log_to_file =
  (*
     Logging: set global level to Info and then make exceptions for debugging
     specific modules.
  *)
  let log_config_file =
    if Sys.file_exists !!log_config_file then Some log_config_file else None
  in
  let want_logging = debug || log_config_file <> None || log_to_file <> None in

  (* Set log destination: none, stderr, or file *)
  (if want_logging then
     let handler =
       match log_to_file with
       | None -> Easy_logging.(Handlers.make (CliErr Debug))
       | Some file -> Easy_logging.(Handlers.make (File (!!file, Debug)))
     in
     Logging.apply_to_all_loggers (fun logger -> logger#add_handler handler));

  (* Set default level to Info rather than logging nothing (NoLevel). *)
  (if want_logging then Logging.(set_global_level Warning));

  (*
     Fine-tune log levels for each module, as instructed in the file
     'log_config.json' or the file specified with '-log_config_file'.
  *)
  match log_config_file with
  | None -> ()
  | Some file ->
      Logging.load_config_file !!file;
      logger#info "loaded %s" !!file
