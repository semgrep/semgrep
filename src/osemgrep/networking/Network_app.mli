(* internally rely on api_token in ~/.settings and SEMGREP_REPO_NAME *)
val url_for_policy : unit -> Uri.t

(* auth *)

(* ?? *)
val get_deployment_from_token : string -> string option
