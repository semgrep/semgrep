open Fpath_.Operators

let t = Testo.create

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"

let tests =
  Testo.categorize "parsing_scala"
    [
      t "regression files" (fun () ->
          let dir = tests_path / "scala" / "parsing" in
          let files = Common2.glob (dir  / "*.scala") in
          files
          |> List.iter (fun file ->
                 try
                   let _ = Parse_scala.parse file in
                   ()
                 with
                 | exn ->
                     Alcotest.failf "it should correctly parse %a (exn = %s)"
                       Fpath.pp file (Common.exn_to_s exn)));
      t "rejecting bad code" (fun () ->
          let dir = tests_path / "scala" / "parsing_errors" in
          let files = Common2.glob (dir / "*.scala") in
          files
          |> List.iter (fun file ->
                 try
                   let _ast = Parse_scala.parse file in
                   Alcotest.failf "it should have thrown a Parse_error %a" Fpath.pp file
                 with
                 | Parsing_error.Syntax_error _ -> ()
                 | exn ->
                     Alcotest.failf "throwing wrong exn %s on %a"
                       (Common.exn_to_s exn) Fpath.pp file));
    ]
