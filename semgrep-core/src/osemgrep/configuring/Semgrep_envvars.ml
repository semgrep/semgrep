(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Typed access to Semgrep environment variables (e.g., SEMGREP_IN_DOCKER).

   Translated from env.py.

   There are other Semgrep environment variables which are not mentioned
   in this file because their value is accessed by Cmdliner
   in Scan_CLI.conf (SEMGREP_BASELINE_COMMIT, SEMGREP_SEND_METRICS,
   SEMGREP_TIMEOUT, and SEMGREP_RULES).
*)

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

let settings_filename = "settings.yml"

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* LATER: use Fpath.of_string or something at some point *)
let path_of_string s = s
let ( / ) a b = Filename.concat a b

let%test_unit "Semgrep_envvars.(/)" =
  [%test_eq: Base.string] ("a/b/" / "c/d" / "foo.c") "a/b/c/d/foo.c"

let env_opt var = Sys.getenv_opt var

let env_or conv var default =
  match Sys.getenv_opt var with
  | None -> conv default
  | Some x -> conv x

let in_env var = env_opt var <> None

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

(* TODO: could we use deriving to automate the generation of
 * env below? [@default = ...] or use ATD?
 *)
type t = {
  semgrep_url : Uri.t;
  fail_open_url : Uri.t;
  app_token : string option;
  version_check_url : Uri.t;
  version_check_timeout : int;
  (* LATER: use FPath.t *)
  version_check_cache_path : Common.filename;
  git_command_timeout : int;
  src_directory : Common.filename;
  user_data_folder : Common.filename;
  user_log_file : Common.filename;
  user_settings_file : Common.filename;
  in_docker : bool;
  in_gh_action : bool;
  (* deprecated *)
  in_agent : bool;
  min_fetch_depth : int;
  shouldafound_base_url : Uri.t;
  shouldafound_no_email : bool;
}

(* less: make it Lazy? *)
let env : t =
  let user_data_folder =
    let parent_dir =
      match Sys.getenv_opt "XDG_CONFIG_HOME" with
      | Some x when Sys.is_directory x -> x
      | _else_ -> Sys.getenv "HOME"
    in
    parent_dir / ".semgrep"
  in
  {
    (* TOPORT: also SEMGREP_APP_URL *)
    semgrep_url = env_or Uri.of_string "SEMGREP_URL" "https://semgrep.dev";
    fail_open_url =
      env_or Uri.of_string "SEMGREP_FAIL_OPEN_URL"
        "https://fail-open.prod.semgrep.dev/failure";
    shouldafound_base_url =
      env_or Uri.of_string "SEMGREP_SHOULDAFOUND_BASE_URL"
        "https://shouldafound.semgrep.dev";
    app_token = env_opt "SEMGREP_APP_TOKEN";
    version_check_url =
      env_or Uri.of_string "SEMGREP_VERSION_CHECK_URL"
        "https://semgrep.dev/api/check-version";
    version_check_timeout =
      env_or int_of_string "SEMGREP_VERSION_CHECK_TIMEOUT" "2";
    version_check_cache_path =
      env_or path_of_string "SEMGREP_VERSION_CACHE_PATH"
        (Sys.getcwd () / ".cache" / "semgrep_version");
    git_command_timeout =
      env_or int_of_string "SEMGREP_GIT_COMMAND_TIMEOUT" "300";
    src_directory = env_or path_of_string "SEMGREP_SRC_DIRECTORY" "/src";
    user_data_folder;
    user_log_file =
      env_or path_of_string "SEMGREP_LOG_FILE" (user_data_folder / "semgrep.log");
    user_settings_file =
      env_or path_of_string "SEMGREP_SETTINGS_FILE"
        (user_data_folder / settings_filename);
    in_docker = in_env "SEMGREP_IN_DOCKER";
    in_gh_action = in_env "GITHUB_WORKSPACE";
    in_agent = in_env "SEMGREP_AGENT";
    shouldafound_no_email = in_env "SEMGREP_SHOULDAFOUND_NO_EMAIL";
    min_fetch_depth = env_or int_of_string "SEMGREP_GHA_MIN_FETCH_DEPTH" "0";
  }
