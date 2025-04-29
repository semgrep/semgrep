let t = Testo.create

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Unit tests for our CapExec module *)

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

let tests caps =
  Testo.categorize "CapExec"
    [
      t "string_of_run and putenv" (fun () ->
          Testutil_mock.with_setenv "FOO" "bar" (fun () ->
              let cmd = (Cmd.Name "sh", [ "-c"; "echo $FOO" ]) in
              let res = CapExec.string_of_run caps#exec ~trim:true cmd in
              match res with
              | Ok (str, _status) ->
                  Alcotest.(check string)
                    "it should exec with parent env and find FOO value" "bar"
                    str
              | Error _err -> failwith "error"));
      t "string_of_run and passed env" (fun () ->
          Testutil_mock.with_setenv "FOO" "bar" (fun () ->
              let cmd = (Cmd.Name "sh", [ "-c"; "echo $FOO" ]) in
              let env = Cmd.env_of_list [ ("FOO", "foo") ] in
              let res = CapExec.string_of_run caps#exec ~trim:true ~env cmd in
              match res with
              | Ok (str, _status) ->
                  Alcotest.(check string)
                    "it should exec with new env and find FOO overriden value"
                    "foo" str
              | Error _err -> failwith "error"));
      t "string_of_run and passed env and inherited env " (fun () ->
          Testutil_mock.with_setenv "FOO" "bar" (fun () ->
              let cmd = (Cmd.Name "sh", [ "-c"; "echo $FOO" ]) in
              let env = Cmd.env_of_list [ ("BAR", "foo") ] in
              let res = CapExec.string_of_run caps#exec ~trim:true ~env cmd in
              match res with
              | Ok (str, _status) ->
                  Alcotest.(check string)
                    "it should find FOO value from inherited env" "bar" str
              | Error _err -> failwith "error"));
      t "string_of_run preserves parent env when passing new env" (fun () ->
          Testutil_mock.with_setenv "FOO" "bar" (fun () ->
              let cmd = (Cmd.Name "sh", [ "-c"; "echo $FOO:$BAR" ]) in
              let env = Cmd.env_of_list [ ("BAR", "foo") ] in
              let res = CapExec.string_of_run caps#exec ~trim:true ~env cmd in
              match res with
              | Ok (str, _status) ->
                  Alcotest.(check string)
                    "it should have both parent FOO and passed BAR" "bar:foo"
                    str
              | Error _err -> failwith "error"));
    ]
