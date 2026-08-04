(*
   Copyright (c) 2021-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
open Common
open Fpath_.Operators

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let timeout_secs = 1.0

(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"

let tests (parse_program : Fpath.t -> AST_generic.program) : Testo.t list =
  Testo.categorize "dataflow_python"
    [
      (* Just checking that it terminates without crashing. *)
      t "regression files" (fun () ->
          let files =
            Common2.glob Fpath.(tests_path / "patterns" / "python" / "*.py")
            @ Common2.glob Fpath.(tests_path / "rules" / "*.py")
          in
          files
          |> List.iter (fun file ->
              let ast = parse_program file in
              let lang = Lang.lang_of_filename_exn file in
              Naming_AST.resolve lang ast;
              match
                Time_limit.set_timeout ~name:"cst_prop" ~sigalrm:true
                  timeout_secs (fun () ->
                    Constant_propagation.propagate_basic lang ast;
                    Constant_propagation.propagate_dataflow lang ast)
              with
              | Some res -> res
              | None ->
                  failwith
                    (spf
                       "constant propagation should finish in less than %gs: %s"
                       timeout_secs !!file)));
    ]
