open Fpath_.Operators

(* we return a fun () to match Testo.test second element *)
let with_login_test_env ?(chdir = true) f () =
  Testutil_files.with_tempdir ~chdir (fun tmp_path ->
      Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE"
        !!(tmp_path / "settings.yaml")
        f)
