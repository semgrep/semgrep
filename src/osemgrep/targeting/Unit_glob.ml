(*
   Unit tests for glob pattern parsing and matching.
*)

open Printf

let test pattern path matches () =
  let pat = Glob_lexer.parse_string pattern in
  let compiled_pat =
    Glob_matcher.compile ~source:(Glob_matcher.string_loc pattern) pat
  in
  let res = Glob_matcher.run compiled_pat path in
  printf
    "\n\
     pattern: %s\n\
     path: %s\n\
     expected match: %B\n\
     actual match: %B\n\
     pattern info:\n\
     %s\n\
     %s\n"
    pattern path matches res
    (Glob_matcher.show_pattern pat)
    (Glob_matcher.show compiled_pat);
  Alcotest.(check bool) "equal" matches res

let tests =
  Testutil.pack_tests "Glob"
    [
      ("simple", test "abc" "abc" true);
      ("extension", test "*.c" "hello.c" true);
      ("wrong extension", test "*.c" "hello.h" false);
    ]
