let setup_logging level =
  (* TODO: Fmt_tty.setup_std_outputs ?style_renderer (); *)
  Logs.set_level level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()
