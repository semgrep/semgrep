(** Collect information about the project contributions from git log. *)

let get_contributions () : Semgrep_output_v1_j.contribution list =
  (* ugly: this works because get_git_logs() uses a special format string
   * to output the git log in a JSON format that matches
   * the definition in semgrep_output_v1.atd contribution type
   *)
  Git_wrapper.get_git_logs ()
  |> Common.map Semgrep_output_v1_j.contribution_of_string
