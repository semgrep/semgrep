open Common
open Fpath_.Operators

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let timeout_secs = 1.0

(* ran from the root of the semgrep repository *)
let tests_path = "tests"

let tests (caps : < Cap.time_limit >)
    (parse_program : Fpath.t -> AST_generic.program) : Testo.t list =
  Testo.categorize "dataflow_python"
    [
      (* Just checking that it terminates without crashing. *)
      t "regression files" (fun () ->
          let dir = Filename.concat tests_path "dataflow/python" in
          let files = Common2.glob (spf "%s/*.py" dir) in
          files |> Fpath_.of_strings
          |> List.iter (fun file ->
                 let ast = parse_program file in
                 let lang = Lang.lang_of_filename_exn file in
                 Naming_AST.resolve lang ast;
                 match
                   Time_limit.set_timeout caps ~name:"cst_prop" timeout_secs
                     (fun () ->
                       Constant_propagation.propagate_basic lang ast;
                       Constant_propagation.propagate_dataflow lang ast)
                 with
                 | Some res -> res
                 | None ->
                     failwith
                       (spf
                          "constant propagation should finish in less than \
                           %gs: %s"
                          timeout_secs !!file)));
    ]
