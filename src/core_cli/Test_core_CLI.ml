(* Mostly a copy-paste of Test_pro_core_CLI.ml *)
type exn_res = ExnExit of int

let run_main (caps : Cap.all_caps) (cmd : string) : (unit, exn_res) result =
  let args = String_.split ~sep:"[ \t]+" cmd in
  (* we run main_exn() below in a child process because it modifies many globals
   * and we don't want to write code to reset those globals between two
   * tests; simpler to just fork.
   *)
  CapProcess.apply_in_child_process
    (caps :> < Cap.fork >)
    (fun () ->
      try
        Ok (Core_CLI.main_exn caps (Array.of_list ("semgrep-core" :: args)))
      with
      | Common.UnixExit n -> Error (ExnExit n))
    () ()

let semgrep_core_tests (caps : Cap.all_caps) : Testo.t list =
  Testo.categorize "semgrep-core CLI (e2e)"
    [
      Testo.create "--help" (fun () ->
          match run_main caps "--help" with
          (* old: exception (Common.UnixExit 0) -> *)
          | Error (ExnExit 0) -> print_string "OK"
          | _ -> failwith "Not OK");
      (* alt: could also use snapshot assert, but without -debug below *)
      Testo.create "semgrep-core -rules <rule> -l <lang> <single_file>"
        (fun () ->
          let cmd =
            "-rules tests/rules_v2/new_syntax.yaml -l python \
             tests/rules_v2/new_syntax.py -debug"
          in
          match run_main caps cmd with
          | Ok () -> print_string "OK"
          | _ -> failwith "Not OK");
    ]

let tests (caps : Cap.all_caps) : Testo.t list = semgrep_core_tests caps
