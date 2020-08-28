
(* relevant test files exercising source, with term-frequency of 
 * file in the test *)
type tests_coverage = (Common.filename (* source *), tests_score) Common.assoc
 and tests_score = (Common.filename (* a test *) * float) list

type lines_coverage = (Common.filename, file_lines_coverage) Common.assoc
 and file_lines_coverage = {
   covered_sites: int list;
   all_sites: int list;
 }

(* input/output *)
val json_of_tests_coverage: tests_coverage -> Json_type.json_type
val json_of_lines_coverage: lines_coverage -> Json_type.json_type

val tests_coverage_of_json: Json_type.json_type -> tests_coverage
val lines_coverage_of_json: Json_type.json_type -> lines_coverage

(* shortcuts *)
val save_tests_coverage: tests_coverage -> Common.filename -> unit
val load_tests_coverage: Common.filename -> tests_coverage

val save_lines_coverage: lines_coverage -> Common.filename -> unit
val load_lines_coverage: Common.filename -> lines_coverage
