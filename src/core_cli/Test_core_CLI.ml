open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* End-to-end (e2e) testing of the semgrep-core CLI program
 *
 * See also tests for the semgrep-core -generate_ast_json in
 * in semgrep-interfaces/tests/test-ast run from make core-test-e2 run
 * itself from .github/workflow/test.yml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let t = Testo.create

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
        print_string (spf "executing: semgrep-core %s\n" cmd);
        Ok (Core_CLI.main_exn caps (Array.of_list ("semgrep-core" :: args)))
      with
      | Common.UnixExit n -> Error (ExnExit n))
    ()

let assert_Ok res =
  match res with
  | Ok () -> print_string "OK"
  | _ -> failwith "Not OK"

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

let semgrep_core_tests (caps : Cap.all_caps) : Testo.t list =
  Testo.categorize "semgrep-core CLI (e2e)"
    [
      t "--help" (fun () ->
          match run_main caps "handle --help" with
          (* old: exception (Common.UnixExit 0) -> *)
          | Error (ExnExit 0) -> print_string "OK"
          | _ -> failwith "Not OK");
      t "handle -rules <rule> -l <lang> <single_file>" (fun () ->
          let cmd =
            "-rules tests/rules_v2/new_syntax.yaml -l python \
             tests/rules_v2/new_syntax.py -debug"
          in
          run_main caps cmd |> assert_Ok);
      t ~checked_output:(Testo.stdout ())
        "output of -rules <rule> -l <lang> <single_file>" (fun () ->
          let cmd =
            "-rules tests/semgrep-core-e2e/rules/basic.yaml -l python \
             tests/semgrep-core-e2e/targets/basic.py -debug"
          in
          run_main caps cmd |> assert_Ok);
      (* we could also assert that the output is actually equal to the
       * previous one
       *)
      t ~checked_output:(Testo.stdout ()) "handle -targets" (fun () ->
          let cmd =
            "-rules tests/semgrep-core-e2e/rules/basic.yaml  -targets \
             tests/semgrep-core-e2e/targets.json -debug"
          in
          run_main caps cmd |> assert_Ok);
    ]

let tests (caps : Cap.all_caps) : Testo.t list = semgrep_core_tests caps
