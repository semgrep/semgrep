let with_setenv envvar str f =
  let old = USys.getenv_opt envvar in
  UUnix.putenv envvar str;
  Common.finalize f (fun () ->
      match old with
      | Some str -> UUnix.putenv envvar str
      (* ugly: Unix does not provide unsetenv,
       * see https://discuss.ocaml.org/t/unset-environment-variable/9025
       *)
      | None -> UUnix.putenv envvar "")

let with_mocked_logs ~f ~final =
  let buffer = Buffer.create 1000 in
  let (ppf : Format.formatter) =
    (* old: I was using Format.str_formatter but
     * some libraries like Cmdliner are also using it
     * and so parsing arguments with cmdliner has the side
     * effect of cleaning Format.stdbuf used by str_formatter,
     * so better to use a separate buffer
     *)
    Format.formatter_of_buffer buffer
  in
  let reporter_to_format_strbuf =
    {
      Logs.report =
        (fun (_src : Logs.src) (_level : Logs.level) ~over k msgf ->
          let k _ =
            over ();
            k ()
          in
          msgf (fun ?header:_ ?tags:_ fmt -> UFormat.kfprintf k ppf fmt));
    }
  in
  let old_reporter = Logs.reporter () in
  Common.finalize
    (fun () ->
      Logs.set_reporter reporter_to_format_strbuf;
      Common.save_excursion Logs_.in_mock_context true (fun () ->
          (* f() might call Logs_helpers.setup_logging() internally, but this will not
           * call Logs.set_reporter and override the reporter we set above
           * thx to disable_set_reporter
           *)
          let res =
            try Ok (f ()) with
            | exn -> Error exn
          in
          Format.pp_print_flush ppf ();
          let content = Buffer.contents buffer in
          final content res))
    (fun () -> Logs.set_reporter old_reporter)
