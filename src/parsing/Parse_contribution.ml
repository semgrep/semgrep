module Out = Semgrep_output_v1_j

(** Collect information about the project contributions from git log. *)

let contrib_to_contrib (contrib : Git_wrapper.contribution) : Out.contribution =
  let Git_wrapper.
        {
          commit_hash;
          commit_timestamp;
          commit_author_name;
          commit_author_email;
        } =
    contrib
  in
  Out.
    {
      commit_hash;
      commit_timestamp = ATD_string_wrap.Datetime.wrap commit_timestamp;
      contributor = { commit_author_name; commit_author_email };
    }

let get_contributions (caps : < Cap.exec >) :
    Semgrep_output_v1_j.contribution list =
  (* We use ~since:"last 30 days" because of our usage policy.
   * See https://semgrep.dev/docs/usage-limits
   *)
  let thirty_days_ago =
    Datetime_.time_n_days_ago ~days:30 ~time:(Datetime_.now ())
  in
  Git_wrapper.logs caps ~since:thirty_days_ago |> List_.map contrib_to_contrib
