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

(** This global is used by our reporter to do the diff between the last
    log we outputed and the current one. Actually, the implementation is a
    bit dumb and probably show weird metrics when we use [lwt]. For such
    case, it's better to add a _counter_ and use the tag mechanism to really
    show right metrics.

    This variable is set when we configure loggers. *)

let time_program_start = ref 0.

let now () =
  Mtime.to_uint64_ns (Mtime_clock.now ())
  |> Duration.to_sec_64 |> Int64.to_float

(* log reporter *)
let reporter ?(with_timestamp = false) ?(dst = Fmt.stderr) () =
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
      msgf @@ fun ?header ?tags:_ fmt ->
      match with_timestamp with
      | true ->
          let current = now () in
          Format.kfprintf k dst
            ("@[[+%010.2fms]%a: " ^^ fmt ^^ "@]@.")
            (current -. !time_program_start)
            Logs_fmt.pp_header (level, header)
      | _ -> Format.kfprintf k dst ("@[%a" ^^ fmt ^^ "@]@.") pp_style style
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
  let with_timestamp = level = Some Logs.Debug in
  time_program_start := now ();
  Logs.set_reporter (reporter ~with_timestamp ());
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
