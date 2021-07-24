open Common
open OUnit

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let timeout_secs = 1.0

let unittest parse_program =
  "dataflow_python"
  >::: [
         (* Just checking that it terminates without crashing. *)
         ( "regression files" >:: fun () ->
           let dir = Config_pfff.tests_path "python/dataflow" in
           let files = Common2.glob (spf "%s/*.py" dir) in
           files
           |> List.iter (fun file ->
                  let ast = parse_program file in
                  let lang = List.hd (Lang.langs_of_filename file) in
                  Naming_AST.resolve lang ast;
                  match
                    set_timeout ~name:"cst_prop" timeout_secs (fun () ->
                        Constant_propagation.propagate_basic lang ast;
                        Constant_propagation.propagate_dataflow ast)
                  with
                  | Some res -> res
                  | None ->
                      assert_failure
                        (spf
                           "constant propagation should finish in less than \
                            %gs: %s"
                           timeout_secs file)) );
       ]
