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
