(*
   Copyright (c) 2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
let test_upload_subproject_symbol_analysis caps =
  let with_mock_response =
    Http_mock_client.with_mocked_http (fun req _ ->
        match Uri.path (Cohttp.Request.uri req) with
        | "/api/agent/scans/123/subproject_symbols_upload_url" ->
            let body =
              Cohttp_lwt.Body.of_string "{\"upload_url\":\"test-s3-url\"}"
            in
            Lwt.return Http_mock_client.(basic_response ~status:200 body)
        | "/test-s3-url" ->
            let body = Cohttp_lwt.Body.of_string "" in
            Lwt.return Http_mock_client.(basic_response ~status:200 body)
        | other -> failwith (Printf.sprintf "no mock responses for %s" other))
  in
  let test () =
    let token = Auth.unsafe_token_of_string "test_token" in
    let manifest = Some (Fpath.v "/path/to/manifest") in
    let lockfile = Some (Fpath.v "/path/to/lockfile") in
    let result =
      Semgrep_App.upload_subproject_symbol_analysis caps ~token ~scan_id:123
        ~manifest ~lockfile []
    in
    match result with
    | Ok _ -> ()
    | Error msg ->
        failwith (Printf.sprintf "Unexpected error in mock s3 upload: %s" msg)
  in
  with_mock_response test

let tests caps =
  [
    Testo.create "upload_subproject_symbol_analysis"
      (test_upload_subproject_symbol_analysis caps);
  ]
