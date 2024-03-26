(* GitHub REST API *)

val find_branchoff_point_async :
  < Cap.network > ->
  gh_token:Auth.token ->
  api_url:Uri.t ->
  repo_name:string ->
  base_branch_hash:Digestif.SHA1.t ->
  Digestif.SHA1.t ->
  Digestif.SHA1.t option Lwt.t
