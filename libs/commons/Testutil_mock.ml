let with_setenv envvar str f =
  let old = Sys.getenv_opt envvar in
  Unix.putenv envvar str;
  Common.finalize f (fun () ->
      match old with
      | Some str -> Unix.putenv envvar str
      (* ugly: Unix does not provide unsetenv,
       * see https://discuss.ocaml.org/t/unset-environment-variable/9025
       *)
      | None -> Unix.putenv envvar "")
