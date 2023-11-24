module Http_helpers = Http_helpers.Make (Lwt_platform)

(* GitHub REST API *)

let find_branchoff_point_async ~gh_token ~api_url ~repo_name ~base_branch_hash
    head_branch_hash =
  let headers = [ ("Authorization", Fmt.str "Bearer %s" gh_token) ] in
  let%lwt response =
    Http_helpers.get_async ~headers
      (Uri.of_string
         (Fmt.str "%a/repos/%s/compare/%a...%a" Uri.pp api_url repo_name
            Digestif.SHA1.pp base_branch_hash Digestif.SHA1.pp head_branch_hash))
  in
  match response with
  | Ok body ->
      let body = body |> Yojson.Basic.from_string in
      let commit =
        Option.bind
          Glom.(
            get_and_coerce_opt string body [ k "merge_base_commit"; k "sha" ])
          Digestif.SHA1.of_hex_opt
      in
      Lwt.return commit
  | __else__ -> Lwt.return_none
