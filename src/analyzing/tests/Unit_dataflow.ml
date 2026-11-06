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
  @ Testo.categorize "implicit_return_ocaml"
      [
        t "function and lambda results" (fun () ->
            let ast =
              parse_program
                Fpath.(tests_path / "misc" / "dataflow" / "implicit_return.ml")
            in
            Naming_AST.resolve Lang.Ocaml ast;
            Implicit_return.mark_implicit_return Lang.Ocaml ast;
            let visitor =
              object
                inherit [_] AST_generic.iter_no_id_info as super

                method! visit_stmt () st =
                  (match st.AST_generic.s with
                  | OtherStmt (OS_ExprStmt2, [ E ({ e = L (Unit _); _ } as e) ])
                    ->
                      assert e.is_implicit_return
                  | _ -> ());
                  super#visit_stmt () st

                method! visit_expr () e =
                  (match e.AST_generic.e with
                  | Call ({ e = N (Id (("returned", _), _)); _ }, _) ->
                      assert e.is_implicit_return
                  | Call ({ e = N (Id (("discarded", _), _)); _ }, _) ->
                      assert (not e.is_implicit_return)
                  | N (Id (("returned_value", _), _))
                  | L (Int _) ->
                      assert e.is_implicit_return
                  | _ -> ());
                  super#visit_expr () e
              end
            in
            visitor#visit_program () ast;
            let rec count_returns stmts =
              stmts
              |> List.fold_left
                   (fun count stmt ->
                     count
                     +
                     match stmt.IL.s with
                     | Return _ -> 1
                     | If (_, _, yes, no) ->
                         count_returns yes + count_returns no
                     | _ -> 0)
                   0
            in
            (* Each [returned] call in tail position must lower to its own
               IL return, so a function has one return per [returned] call
               in its own body. Calls inside nested lambdas belong to those
               lambdas, which are checked separately. Functions without
               [returned] calls (unit, literal, or variable results) have a
               single return. *)
            let count_returned_calls fdef =
              let count = ref 0 in
              let counter =
                object
                  inherit [_] AST_generic.iter_no_id_info as super

                  method! visit_expr () e =
                    match e.AST_generic.e with
                    | Lambda _ -> ()
                    | Call ({ e = N (Id (("returned", _), _)); _ }, _) ->
                        incr count;
                        super#visit_expr () e
                    | _ -> super#visit_expr () e
                end
              in
              counter#visit_function_body () fdef.AST_generic.fbody;
              !count
            in
            Visit_function_defs.visit
              (fun _ent fdef ->
                let expected = max 1 (count_returned_calls fdef) in
                let il = AST_to_IL.function_definition Lang.Ocaml fdef in
                Alcotest.(check int)
                  "one IL return per returned call" expected
                  (count_returns il.fbody))
              ast);
      ]
