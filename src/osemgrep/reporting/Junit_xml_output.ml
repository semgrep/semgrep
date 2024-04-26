module OutT = Semgrep_output_v1_t

let string_of_severity = function
  | `Info -> "INFO"
  | `Warning -> "WARNING"
  | `Error -> "ERROR"
  | `Experiment -> "EXPERIMENT"
  | `Inventory -> "INVENTORY"
  | `Critical -> "CRITICAL"
  | `High -> "HIGH"
  | `Medium -> "MEDIUM"
  | `Low -> "LOW"

let junit_test_cases out (results : OutT.cli_match list) =
  results
  |> List.iter (fun (result : OutT.cli_match) ->
         let open Xmlm in
         output out
           (`El_start
             ( ("", "testcase"),
               [
                 (("", "name"), Rule_ID.to_string result.check_id);
                 (("", "classname"), Fpath.to_string result.path);
                 (("", "file"), Fpath.to_string result.path);
                 (("", "line"), string_of_int result.start.line);
               ] ));
         output out
           (`El_start
             ( ("", "failure"),
               [
                 (("", "type"), string_of_severity result.extra.severity);
                 (("", "message"), result.extra.message);
               ] ));
         output out (`Data result.extra.lines);
         output out `El_end;
         output out `El_end)

let junit_xml_output (cli_output : OutT.cli_output) =
  let b = Buffer.create 1024 in
  let open Xmlm in
  let out = Xmlm.make_output (`Buffer b) in
  let num_results = List.length cli_output.results in
  output out (`Dtd None);
  output out
    (`El_start
      ( ("", "testsuites"),
        [
          (("", "disabled"), "0");
          (("", "errors"), "0");
          (("", "failures"), string_of_int num_results);
          (("", "tests"), string_of_int num_results);
          (* XXX(reynir): due to python quirk this is a flaot *)
          (("", "time"), "0.0");
        ] ));
  output out
    (`El_start
      ( ("", "testsuite"),
        [
          (("", "disabled"), "0");
          (("", "errors"), "0");
          (("", "failures"), string_of_int num_results);
          (("", "name"), "semgrep results");
          (("", "skipped"), "0");
          (("", "tests"), string_of_int num_results);
          (* XXX(reynir): due to python quirk this is an integer *)
          (("", "time"), "0");
        ] ));
  junit_test_cases out cli_output.results;
  output out `El_end;
  output out `El_end;
  Buffer.contents b
