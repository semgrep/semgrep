open Common

module Flag = Flag_parsing

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let parse file =
  Common.save_excursion Flag.error_recovery false (fun () ->
    Common.save_excursion Flag.show_parsing_error false (fun () ->
      Common.save_excursion Flag.verbose_parsing false (fun () ->
        Parse_cpp.parse file
      )))
(*****************************************************************************)
(* Unit tests *)
(*****************************************************************************)

let tests =
  Testutil.pack_tests "parsing_cpp" [

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
    "regression files", (fun () ->
      let dir = Config_pfff.tests_path "cpp/parsing" in
      let files =
        Common2.glob (spf "%s/*.cpp" dir) @ Common2.glob (spf "%s/*.h" dir) in
      files |> List.iter (fun file ->
        try
          let _ast = parse file in
          ()
        with Parse_info.Parsing_error _ ->
          Alcotest.failf "it should correctly parse %s" file
      )
    );

    "rejecting bad code", (fun () ->
      let dir = Config_pfff.tests_path "cpp/parsing_errors" in
      let files = Common2.glob (spf "%s/*.cpp" dir) in
      files |> List.iter (fun file ->
        try
          let _ast = parse file in
          Alcotest.failf "it should have thrown a Parse_error %s" file
        with
        | Parse_info.Parsing_error _ -> ()
        | exn -> Alcotest.failf "throwing wrong exn %s on %s"
                   (Common.exn_to_s exn) file
      )
    );

    (* parsing C files (and not C++ files) possibly containing C++ keywords *)
    "C regression files", (fun () ->
      let dir = Config_pfff.tests_path "c/parsing" in
      let files =
        Common2.glob (spf "%s/*.c" dir)
        (* @ Common2.glob (spf "%s/*.h" dir) *) in
      files |> List.iter (fun file ->
        try
          let _ast = parse file in
          ()
        with Parse_info.Parsing_error _ ->
          Alcotest.failf "it should correctly parse %s" file
      )
    );

    (*-----------------------------------------------------------------------*)
    (* Misc *)
    (*-----------------------------------------------------------------------*)
  ]
