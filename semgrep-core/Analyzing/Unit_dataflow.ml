open Common
open OUnit

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let timeout_secs = 1

let unittest =
  "dataflow_python" >::: [

    (* Just checking that it terminates without crashing. *)
    "regression files" >:: (fun () ->
      let dir = Config_pfff.tests_path "python/dataflow" in
      let files = Common2.glob (spf "%s/*.py" dir)in
      files |> List.iter (fun file ->
        let ast = Parse_generic.parse_program file in
        let lang = List.hd (Lang.langs_of_filename file) in
        Naming_AST.resolve lang ast;
        try
          timeout_function timeout_secs (fun () ->
            Constant_propagation.propagate_basic lang ast;
            Constant_propagation.propagate_dataflow ast)
        with Timeout -> assert_failure (
          spf "constant propagation should finish in less than %ds: %s"
            timeout_secs file)
      )
    );
  ]
