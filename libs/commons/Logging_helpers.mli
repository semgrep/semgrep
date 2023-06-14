(*
   Set up logging globally and for each module based on what
   we found on the command line or config files.
*)
val setup :
  debug:bool -> log_config_file:Fpath.t -> log_to_file:Fpath.t option -> unit
