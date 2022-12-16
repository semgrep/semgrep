val version : string

(* just append tests/ to the path assuming you run the Test.exe program from
 * the pfff/tests/ directory (as done by dune runtest) *)
val tests_path: string -> string

val regression_data_dir: string

(* deprecated, use relative path from tests/ for testing code *)
val path_pfff_home : string

val std_xxx : string ref

val logger : string option
