open Common;
module CST = Ast_arithmetic;

/* ran from _build/default/tests/arithmetic hence the '..'s below */
let tests_path = "../../../../tests/arithmetic"

let test_end_to_end = (cst_json_file: string) => {
  print_string(spf("Parse CST in %s\n", cst_json_file));
  let program = Arithmetic_cst_json_reader.parse(cst_json_file);
  print_string(CST.show_program(program));
}

let main = () => {
  let dir = Filename.concat(tests_path, "examples");
  let cst_files = Common2.glob(spf("%s/*.json", dir));
  let _ = List.map(test_end_to_end, cst_files);
}

let _ = main();
