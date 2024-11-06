module OutT = Semgrep_output_v1_t

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Output findings compatible with Junix XML format using the Xmlm OPAM
 * library.
 *
 * Information about the format can be found here:
 *  - https://github.com/testmoapp/junitxml
 *  - https://github.com/windyroad/JUnit-Schema/blob/master/JUnit.xsd
 *  - https://github.com/kyrus/python-junit-xml
 *
 * Here is an example Based on the understanding of what Jenkins can parse for
 * JUnit XML files:
 *
 * <?xml version="1.0" encoding="utf-8"?>
 * <testsuites errors="1" failures="1" tests="4" time="45">
 *   <testsuite errors="1" failures="1" hostname="localhost" id="0" name="test1"
 *              package="testdb" tests="4" timestamp="2012-11-15T01:02:29">
 *     <properties>
 *       <property name="assert-passed" value="1"/></properties>
 *       <testcase classname="testdb.directory" name="1-passed-test" time="10"/>
 *       <testcase classname="testdb.directory" name="2-failed-test" time="20">
 *          <failure message="Assertion FAILED: failed assert" type="failure">
 *                 the output of the testcase
 *          </failure>
 *       </testcase>
 *       <testcase classname="package.directory" name="3-errord-test" time="15">
 *         <error message="Assertion ERROR: error assert" type="error">
 *                the output of the testcase
 *         </error>
 *       </testcase>
 *       <testcase classname="package.directory" name="3-skipped-test" time="0">
 *          <skipped message="SKIPPED Test" type="skipped">
 *                  the output of the testcase
 *          </skipped>
 *        </testcase>
 *        <testcase classname="testdb.directory" name="3-passed-test" time="10">
 *             <system-out>
 *                 I am system output
 *             </system-out>
 *             <system-err>
 *                 I am the error output
 *             </system-err>
 *         </testcase>
 *     </testsuite>
 * </testsuites>
 *
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* This is put in the "type" field of the "failure" XML element,
 * which does not have much constraint; it must just be a string
 *)
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

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let junit_xml_output (cli_output : OutT.cli_output) : string =
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
