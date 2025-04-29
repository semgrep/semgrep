(*
   Test regexp parsing on individual files in /tests
*)

open Fpath_.Operators

let t = Testo.create

(* ran from the root of the semgrep repository *)
let tests_path = Fpath.v "tests"

(* Check that no syntax error is raised (without checking the parse tree) *)
let test_syntax pat () =
  let _ = Parse.string pat in
  ()

let test_valid_files dialect rel_path () =
  let dir = tests_path // rel_path in
  let files = Common2.glob (dir / "*.regexp") in
  files
  |> List.iter (fun file ->
         try
           let _ = Parse.parse ~conf:(Dialect.conf dialect) file in
           ()
         with
         | exn ->
             Alcotest.failf "it should correctly parse %a (exn = %s)" Fpath.pp
               file (Common.exn_to_s exn))

let test_invalid_files dialect rel_path () =
  let dir = tests_path // rel_path in
  let files = Common2.glob (dir / "*.regexp") in
  files
  |> List.iter (fun file ->
         try
           let _ast = Parse.file ~conf:(Dialect.conf dialect) file in
           Alcotest.failf "it should have thrown a Parse_error %a" Fpath.pp file
         with
         | Parsing_error.Syntax_error _ -> ()
         | exn ->
             Alcotest.failf "throwing wrong exn %s on %a" (Common.exn_to_s exn)
               Fpath.pp file)

let tests =
  Testo.categorize_suites "regexp parsing"
    [
      Testo.categorize "pcre"
        [
          (* The user mostly likely intended '[[:alpha:]]'.
             PCRE reports an error when encountering a POSIX
             character class outside of square brackets but we don't. *)
          t "not a posix character class" (test_syntax {|[:alpha:]|});
          t "not a broken posix character class" (test_syntax {|[:]|});
          (* Tolerate some malformed input.
             We don't necessarily need to tolerate malformed input but
             we want at least to avoid uninformative errors such as
             'Failure "lexing: empty token"' *)
          t "tolerate unfinished character class" (test_syntax {|[a|});
          t "tolerate unfinished posix character class"
            (test_syntax {|[[:alpha|});
          t "tolerate unfinished non-capturing group" (test_syntax {|(?|});
          t "tolerate unfinished raw sequence" (test_syntax {|\Qabc|});
          (* Check regexps kept in files *)
          t "valid files"
            (test_valid_files Dialect.PCRE (Fpath.v "regexp/pcre/parsing"));
          t "invalid files"
            (test_invalid_files Dialect.PCRE
               (Fpath.v "regexp/pcre/parsing_errors"));
        ];
      Testo.categorize "pcre_extended"
        [
          t "valid files"
            (test_valid_files Dialect.PCRE_extended
               (Fpath.v "regexp/pcre_extended/parsing"));
          t "invalid files"
            (test_invalid_files Dialect.PCRE_extended
               (Fpath.v "regexp/pcre_extended/parsing_errors"));
        ];
      Testo.categorize "perl_xx"
        [
          t "valid files"
            (test_valid_files Dialect.Perl_xx
               (Fpath.v "regexp/perl_xx/parsing"));
          t "invalid files"
            (test_invalid_files Dialect.Perl_xx
               (Fpath.v "regexp/perl_xx/parsing_errors"));
        ];
    ]
