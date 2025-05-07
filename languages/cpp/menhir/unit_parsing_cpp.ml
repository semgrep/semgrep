open Fpath_.Operators
module Flag = Flag_parsing

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let t = Testo.create

let parse file =
  Hook.with_hook_set Flag.error_recovery false (fun () ->
      Hook.with_hook_set Flag.show_parsing_error false (fun () ->
          Hook.with_hook_set Flag.verbose_parsing false (fun () ->
              Parse_cpp.parse file)))

(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"

let tests =
  Testo.categorize "parsing_cpp"
    [
      (*-----------------------------------------------------------------------*)
      (* Lexing *)
      (*-----------------------------------------------------------------------*)
      (* todo:
       * - make sure parse int correctly, and float, and that actually does
       *   not return multiple tokens for 42.42
       * - ...
       *)

      (*-----------------------------------------------------------------------*)
      (* Parsing *)
      (*-----------------------------------------------------------------------*)
      t "regression files" (fun () ->
          let dir = tests_path / "cpp" / "parsing" in
          let files =
            Common2.glob (dir / "*.cpp") @ Common2.glob (dir / "*.h")
          in
          files
          |> List.iter (fun file ->
                 try
                   let _ast = parse file in
                   ()
                 with
                 | Parsing_error.Syntax_error _ ->
                     Alcotest.failf "it should correctly parse %s" !!file));
      t "rejecting bad code" (fun () ->
          let dir = tests_path / "cpp" / "parsing_errors" in
          let files = Common2.glob (dir / "*.cpp") in
          files
          |> List.iter (fun file ->
                 try
                   let _ast = parse file in
                   Alcotest.failf "it should have thrown a Parse_error %s"
                     !!file
                 with
                 | Parsing_error.Syntax_error _ -> ()
                 | exn ->
                     Alcotest.failf "throwing wrong exn %s on %s"
                       (Common.exn_to_s exn) !!file));
      (* parsing C files (and not C++ files) possibly containing C++ keywords *)
      t "C regression files" (fun () ->
          let dir = tests_path / "c" / "parsing" in
          let files =
            Common2.glob (dir / "*.c")
            (* @ Common2.glob (spf "%s/*.h" dir) *)
          in
          files
          |> List.iter (fun file ->
                 try
                   let _ast = parse file in
                   ()
                 with
                 | Parsing_error.Syntax_error _ ->
                     Alcotest.failf "it should correctly parse %s" !!file))
      (*-----------------------------------------------------------------------*)
      (* Misc *)
      (*-----------------------------------------------------------------------*);
    ]
