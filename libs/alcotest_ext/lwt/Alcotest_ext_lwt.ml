(*
   Run tests that return Lwt promises
*)

type test = unit Lwt.t Alcotest_ext.t

let mona : unit Lwt.t Alcotest_ext.Mona.t =
  let catch func handler =
    Lwt.catch func (fun exn ->
        (* TODO: need to capture the stack trace earlier? How? *)
        let trace = Printexc.get_raw_backtrace () in
        handler exn trace)
  in
  { return = Lwt.return; bind = Lwt.bind; catch }

let create ?category ?expected_outcome ?mask_output ?output_kind ?skipped ?tags
    ?tolerate_chdir name func =
  Alcotest_ext.create_gen ?category ?expected_outcome ?mask_output ?output_kind
    ?skipped ?tags ?tolerate_chdir mona name func

let interpret_argv = Alcotest_ext.interpret_argv_gen ~mona
