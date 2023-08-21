(** Collect information about the project contributions from git log. *)

let get_contributions () : Semgrep_output_v1_j.contributions =
  let logs = Git_wrapper.get_git_logs () in
  let contributions =
    Common.map (fun f -> Semgrep_output_v1_j.contribution_of_string f) logs
  in
  contributions
