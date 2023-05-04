(* ANSI escape sequences for colored output, depending on log level *)
let color level =
  match level with
  | Logs.Warning -> Some "33" (*yellow*)
  | Logs.Error -> Some "31" (*red*)
  | _else -> None

(* print an ANSI escape sequence - not worth to use an extra library
   (such as ANSIterminal) for this *)
let pp_sgr ppf style =
  Format.pp_print_as ppf 0 "\027[";
  Format.pp_print_as ppf 0 style;
  Format.pp_print_as ppf 0 "m"

(* log reporter *)
let reporter ?(dst = Format.err_formatter) () =
  let report _src level ~over k msgf =
    let pp_style, style, style_off =
      match color level with
      | None -> ((fun _ppf _style -> ()), "", "")
      | Some x -> (pp_sgr, x, "0")
    in
    let k _ =
      over ();
      k ()
    in
    let r =
      msgf @@ fun ?header:_ ?tags:_ fmt ->
      Format.kfprintf k dst ("@[%a" ^^ fmt ^^ "@]@.") pp_style style
    in
    Format.fprintf dst "%a" pp_style style_off;
    r
  in
  { Logs.report }

(* Enable basic logging (level = Logs.Warning) so that you can use Logging
 * calls even before a precise call to setup_logging.
 *)
let enable_logging () =
  Logs.set_level ~all:true (Some Logs.Warning);
  Logs.set_reporter (reporter ());
  ()

let setup_logging ~force_color ~level =
  let style_renderer = if force_color then Some `Ansi_tty else None in
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level ~all:true level;
  Logs.set_reporter (reporter ());
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
