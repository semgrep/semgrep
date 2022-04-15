(*
   Unit tests for Guess_lang
*)

type exec = Exec | Nonexec
type success = OK | XFAIL

(*
   For these tests, the file doesn't need to exist.
*)
let name_tests : (string * Lang.t * string * success) list =
  [
    (* name, language, file name, expected result *)
    ("js", Js, "foo.js", OK);
    ("js relative path", Js, "./a/b.c/foo.js", OK);
    ("js absolute path", Js, "/a/b.c/foo.js", OK);
    ("js double extension", Js, "foo.bar.js", OK);
    ("min js", Js, "foo.min.js", XFAIL);
    ("not js", Js, "foo.bar", XFAIL);
    ("jsx", Js, "foo.jsx", OK);
    ("typescript", Ts, "foo.ts", OK);
    ("typescript .d.ts", Ts, "foo.d.ts", XFAIL);
    ("spaces", Ruby, " a b  c.rb", OK);
  ]

let contents_tests : (string * Lang.t * string * string * exec * success) list =
  [
    (* name, language, file name, file contents, executable?, expected result *)
    ("correct extension nonexec", Js, "foo1.js", "", Exec, OK);
    ("correct extension exec", Js, "foo2.js", "", Exec, OK);
    ("wrong extension exec", Js, "foo3.bar", "", Exec, XFAIL);
    ("bash non-executable", Bash, "hello1.bash", "", Nonexec, OK);
    ("bash exec", Bash, "hello2", "#!/anything/bash\n", Exec, OK);
    ("sh exec", Bash, "hello3", "#!/bin/sh\n# hello!", Exec, OK);
    ("bash exec env", Bash, "hello4", "#! /usr/bin/env bash\n", Exec, OK);
    ("other exec env", Bash, "hello5", "#!/usr/bin/env bashxxxx\n", Exec, XFAIL);
    ("env -S", Bash, "hello6", "#!/usr/bin/env -Sbash -eu\n", Exec, OK);
    ("hack with .hack extension", Hack, "foo.hack", "", Nonexec, OK);
    ( "hack without extension",
      Hack,
      "foo",
      "#!/usr/bin/env hhvm\nxxxx",
      Exec,
      OK );
    ( "hack with .php extension and shebang",
      Hack,
      "foo.php",
      "#!/usr/bin/env hhvm\nxxxx",
      Nonexec,
      OK );
    ( "hack with .php extension no shebang",
      Hack,
      "foo.php",
      "<?hh\n",
      Nonexec,
      OK );
    ("php", Php, "foo.php", "", Nonexec, OK);
  ]

let ( // ) = Filename.concat
let mkdir path = if not (Sys.file_exists path) then Unix.mkdir path 0o777

(*
   Create a temporary file with the specified name, in a local tmp folder.
   We don't delete the files when we're done because it's easier when
   troubleshooting tests.
*)
let with_file name contents exec f =
  let dir = "tmp" in
  mkdir dir;
  let path = dir // name in
  let oc = open_out_bin path in
  (match exec with
  | Exec -> Unix.chmod path 0o755
  | Nonexec -> ());
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      output_string oc contents;
      close_out oc;
      f path)

let test_name_only lang path expectation =
  match (expectation, Guess_lang.inspect_file lang path) with
  | OK, Ok _
  | XFAIL, Error _ ->
      ()
  | _ -> assert false

let test_with_contents lang name contents exec expectation =
  with_file name contents exec (fun path ->
      match (expectation, Guess_lang.inspect_file lang path) with
      | OK, Ok _
      | XFAIL, Error _ ->
          ()
      | _ -> assert false)

(* This is necessary when running the tests on Windows. *)
let fix_path s =
  match Sys.os_type with
  | "Win32" ->
      String.map
        (function
          | '/' -> '\\'
          | c -> c)
        s
  | _ -> s

let test_inspect_file =
  Common.map
    (fun (test_name, lang, path, expectation) ->
      (test_name, fun () -> test_name_only lang (fix_path path) expectation))
    name_tests
  @ Common.map
      (fun (test_name, lang, file_name, contents, exec, expectation) ->
        ( test_name,
          fun () -> test_with_contents lang file_name contents exec expectation
        ))
      contents_tests

let tests =
  Testutil.pack_suites "Guess_lang"
    [ Testutil.pack_tests "inspect_file" test_inspect_file ]
