open Fpath_.Operators

(* This was originally in `cli_login`, but causes JS test failures due to
   `libev_init` because that pulls in LWT instead of `jsoo`.
   To fix this, we just separate out the common CLI utils, such as this function,
   rather than exposing it in `Test_login_subcommmand`.
*)
(* we return a fun () to match Testo.test second element *)
let with_login_test_env ?(chdir = true) f () =
  Testutil_files.with_tempdir ~chdir (fun tmp_path ->
      Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE"
        !!(tmp_path / "settings.yaml")
        f)
