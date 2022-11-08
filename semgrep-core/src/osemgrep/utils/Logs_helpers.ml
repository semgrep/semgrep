let setup_logging level =
  (* TODO: Fmt_tty.setup_std_outputs ?style_renderer (); *)
  Logs.set_level ~all:true level;
  Logs.set_reporter (Logs_fmt.reporter ());
  (* from https://github.com/mirage/ocaml-cohttp#debugging *)
  (* Disable all third-party libs logs *)
  Logs.Src.list ()
  |> List.iter (fun src ->
         match Logs.Src.name src with
         | "cohttp.lwt.io"
         | "cohttp.lwt.server"
         | "cohttp.lwt.client"
         | "conduit_lwt_server"
         | "ca-certs"
         | "bos"
         | "mirage-crypto-rng.lwt"
         | "mirage-crypto-rng.unix"
         | "handshake"
         | "tls.config"
         | "tls.tracing"
         | "x509" ->
             Logs.Src.set_level src None
         | "application" -> ()
         | s -> failwith ("Logs library not handled: " ^ s))
