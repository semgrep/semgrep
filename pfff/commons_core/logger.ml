open Common

(* module J = Json_type *)

(* see opa/pfff_logger.opa *)
let log config cmd extra_args =
  config |> do_option (fun server ->
    let unixname = Common2.unixname() in
    let extra_args = 
      match extra_args with
      | Some s -> s
      | None -> ""
    in
(*
    let json = J.Object [
      ("unixname", J.String unixname);
      ("extra_args", J.String extra_args);
    ]
    in
    let str = Json_out.string_of_json json in
*)
    let str = spf "unixname:%s\nextra_args:%s" unixname extra_args in
    let tmpfile = Common.new_temp_file "logger" "json" in
    Common.write_file tmpfile str;
    let cmd = 
      spf "curl http://%s/_rest_/%s/ --data @%s 2>/dev/null 1>/dev/null" 
        server cmd tmpfile in
    profile_code "pfff_logger" (fun () ->
      try
       timeout_function 1 (fun () ->
        command2 cmd
      ) with Timeout -> ()
    )
  )
