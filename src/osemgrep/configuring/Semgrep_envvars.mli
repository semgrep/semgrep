(*
   Typed access to Semgrep environment variables (e.g., SEMGREP_IN_DOCKER).
 *)

(* Most (typed) fields below correspond directly to an uppercase environment
 * metavariable with the SEMGREP_ prefix.
 * For example, [git_command_timeout] will contain the content of
 * SEMGREP_GIT_COMMAND_TIMEOUT in the environment (or a default value if
 * it's not in the environment).
 *)
type t = {
  (* $SEMGREP_URL *)
  semgrep_url : Uri.t;
  (* $SEMGREP_xxx *)
  fail_open_url : Uri.t;
  app_token : string option;
  version_check_url : Uri.t;
  version_check_timeout : int;
  (* .cache/semgrep_version *)
  version_check_cache_path : Fpath.t;
  git_command_timeout : int;
  (* "/src" *)
  src_directory : Fpath.t;
  (* $XDG_CONFIG_HOME/.semgrep or ~/.semgrep *)
  user_dot_semgrep_dir : Fpath.t;
  (* $SEMGREP_LOG_FILE or ~/.semgrep/semgrep.log  *)
  user_log_file : Fpath.t;
  (* $SEMGREP_SETTINGS_FILE ~/.semgrep/settings.yml *)
  user_settings_file : Fpath.t;
  in_docker : bool;
  (* $GITHUB_WORKSPACE *)
  in_gh_action : bool;
  (* $SEMGREP_AGENT (deprecated) *)
  in_agent : bool;
  (* $SEMGREP_xxx *)
  min_fetch_depth : int;
}

val v : t
