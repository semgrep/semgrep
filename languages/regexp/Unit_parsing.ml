(*
   Test regexp parsing on individual files in /tests
*)

open Common

let t = Testo.create

(* ran from the root of the semgrep repository *)
let tests_path = "tests"

let test_valid_files dialect rel_path () =
  let dir = Filename.concat tests_path rel_path in
  let files = Common2.glob (Filename.concat dir "*.regexp") in
  files
  |> List.iter (fun file ->
         try
           let _ = Parse.parse ~conf:(Dialect.conf dialect) (Fpath.v file) in
           ()
         with
         | exn ->
             Alcotest.failf "it should correctly parse %s (exn = %s)" file
               (Common.exn_to_s exn))

let test_invalid_files dialect rel_path () =
  let dir = Filename.concat tests_path rel_path in
  let files = Common2.glob (Filename.concat dir "*.regexp") in
  files
  |> List.iter (fun file ->
         try
           let _ast = Parse.file ~conf:(Dialect.conf dialect) (Fpath.v file) in
           Alcotest.failf "it should have thrown a Parse_error %s" file
         with
         | Parsing_error.Syntax_error _ -> ()
         | exn ->
             Alcotest.failf "throwing wrong exn %s on %s" (Common.exn_to_s exn)
               file)

let tests =
  Testo.categorize_suites "regexp parsing"
    [
      Testo.categorize "pcre"
        [
          t "valid files" (test_valid_files Dialect.PCRE "regexp/pcre/parsing");
          t "invalid files"
            (test_invalid_files Dialect.PCRE "regexp/pcre/parsing_errors");
        ];
      Testo.categorize "pcre_extended"
        [
          t "valid files"
            (test_valid_files Dialect.PCRE_extended
               "regexp/pcre_extended/parsing");
          t "invalid files"
            (test_invalid_files Dialect.PCRE_extended
               "regexp/pcre_extended/parsing_errors");
        ];
      Testo.categorize "perl_xx"
        [
          t "valid files"
            (test_valid_files Dialect.Perl_xx "regexp/perl_xx/parsing");
          t "invalid files"
            (test_invalid_files Dialect.Perl_xx "regexp/perl_xx/parsing_errors");
        ];
    ]
