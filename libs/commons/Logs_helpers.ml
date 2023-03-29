(* Enable basic logging (level = Logs.Warning) so that you can use Logging
 * calls even before a precise call to setup_logging.
 *)
let enable_logging () =
  Logs.set_level ~all:true (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

(* TOPORT: with Logs a warning is displayed as:
 *    osemgrep: [WARNING] Paths that match both --include ...
 *  with the WARNING in yellow. In python it's displayed as:
 *    Paths that match both --include ...
 *  without any header but with the whole line in yellow.
 *)
let setup_logging ~force_color ~level =
  let style_renderer = if force_color then Some `Ansi_tty else None in
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level ~all:true level;
  Logs.set_reporter (Logs_fmt.reporter ());
  (* from https://github.com/mirage/ocaml-cohttp#debugging *)
  (* Disable all third-party libs logs *)
  Logs.Src.list ()
  |> List.iter (fun src ->
         match Logs.Src.name src with
         | "dns"
         | "dns_cache"
         | "dns_client"
         | "dns_client_lwt"
         | "ca-certs"
         | "bos"
         | "happy-eyeballs"
         | "happy-eyeballs.lwt"
         | "mirage-crypto-rng.lwt"
         | "mirage-crypto-rng-lwt"
         | "mirage-crypto-rng.unix"
         | "handshake"
         | "tls.config"
         | "tls.tracing"
         | "x509" ->
             Logs.Src.set_level src None
         | "application" -> ()
         | s -> failwith ("Logs library not handled: " ^ s))
