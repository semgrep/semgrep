type t = {
  semgrep_url : Uri.t;
  fail_open_url : Uri.t;
  app_token : string option;
  version_check_url : Uri.t;
  version_check_timeout : int;
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

val env : t
