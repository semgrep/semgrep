let src = Logs.Src.create "semgrep"

module Log = (val Logs.src_log src : Logs.LOG)

let help_msg =
  {|
If you want to show the logs of a particular library
(e.g., the semgrep targeting library), you'll need to adjust the
SEMGREP_LOG_SRCS environment variable as in

  SEMGREP_LOG_SRCS="semgrep.targeting" semgrep ... --debug

or for osemgrep you can use it with any log level as in

  SEMGREP_LOG_SRCS="semgrep.targeting" ./bin/osemgrep ... --verbose

You can see the list of possible libraries above in this log as in
...
[00.04][DEBUG](default): Skipping logs for semgrep.targeting
...
[00.04][DEBUG](default): Skipping logs for commons.pcre
...
For more information, See
https://www.notion.so/semgrep/Logging-in-semgrep-semgrep-core-osemgrep-67c9046fa53744728d9d725a5a244f64
|}

(* Small wrapper around Logs_.setup() (itself a wrapper around Logs.set_xxx)
 * TODO: add some --semgrep-log-xxx flags in osemgrep CLI for the envvars so
 * they can be set also with CLI flags and will be part of the man page.
 *)
let setup ?log_to_file ?require_one_of_these_tags ~force_color ~level () =
  UConsole.setup ~highlight_setting:(if force_color then On else Auto) ();
  (* We override the default use of LOG_XXX env var in Logs_.setup() with
   * SEMGREP_LOG_XXX env vars because Gitlab was reporting perf problems due
   * to all the logging produced by Semgrep. Indeed, Gitlab CI itself is running
   * jobs with LOG_LEVEL=debug, so better to use a different name for now.
   * LATER: once we migrate most of our logs to use Logs.src, we should have
   * far less logging by default, even in debug level, so we can maybe restore
   * the use of the standard LOG_LEVEL envvar.
   *
   * The PYTEST_XXX env vars allows modifying the logging behavior of pytest
   * tests since pytest clears the environment except for variables starting
   * with "PYTEST_".
   * LATER: once we remove pysemgrep and switch from pytest to Testo, we
   * can get rid of those PYTEST_xxx env vars.
   *)
  Logs_.setup ?log_to_file ?require_one_of_these_tags
    ~read_level_from_env_vars:
      [ "PYTEST_SEMGREP_LOG_LEVEL"; "SEMGREP_LOG_LEVEL" ]
    ~read_srcs_from_env_vars:[ "PYTEST_SEMGREP_LOG_SRCS"; "SEMGREP_LOG_SRCS" ]
    ~read_tags_from_env_vars:[ "PYTEST_SEMGREP_LOG_TAGS"; "SEMGREP_LOG_TAGS" ]
    ~level ();
  Logs.debug (fun m -> m "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
  Logs.debug (fun m -> m "%s" help_msg);
  Logs.debug (fun m -> m "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
  ()
