(*
   Unit tests for glob pattern parsing and matching.
*)

open Printf

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let test pattern path matches () =
  let pat = Parse.parse_string pattern in
  let compiled_pat =
    Match.compile ~source:(Match.string_loc ~source_kind:None pattern) pat
  in
  let res = Match.run compiled_pat path in
  printf
    "pattern: %s\n\
     path: %s\n\
     expected match: %B\n\
     actual match: %B\n\
     pattern info:\n\
     %s\n\
     %s\n"
    pattern path matches res (Pattern.show pat) (Match.show compiled_pat);
  Alcotest.(check bool) "equal" matches res

(*****************************************************************************)
(* Test data *)
(*****************************************************************************)

let tests =
  Testutil.pack_tests "Glob"
    [
      ("simple", test "abc" "abc" true);
      ("anchor left", test "abc" "xabc" false);
      ("anchor right", test "abc" "abcd" false);
      ("absolute path", test "/a/b" "/a/b" true);
      ("star 1", test "*.c" "hello.c" true);
      ("star 2", test "*.c" "hello.h" false);
      (* '*' can not match a '/' *)
      ("star 3", test "*.c" "a/hello.c" false);
      ("question 1", test "a.?" "a.c" true);
      ("question 2", test "a.?" "b.c" false);
      ("relative path 1", test "ab/*.c" "ab/hello.c" true);
      ("relative path 2", test "ab/*.c" "hello.c" false);
      ("relative path 3", test "ab/*.c" "ab/" false);
      (* globs are left anchored, implicit '^' *)
      ("relative path 4", test "ab/*.c" "x/ab/hello.c" false);
      ("absolute path 1", test "/ab/*.c" "/ab/hello.c" true);
      ("absolute path 2", test "/ab/*.c" "ab/hello.c" false);
      (* anchored too, so no match *)
      ("absolute path 3", test "ab/*.c" "/ab/hello.c" false);
      ("char range 1", test "[a-bz]" "b" true);
      ("char range 2", test "[a-bz]" "c" false);
      ("char range 3", test "[^a-bz]" "b" false);
      ("char range 4", test "[^a-bz]" "c" true);
      (* alternative negative range syntax *)
      ("char range 5", test "[!a-bz]" "b" false);
      ("char range 6", test "[!a-bz]" "c" true);
      (* remember that '?' means any character; it is not a modifier
       * to the previous construct (like in regexps), hence
       * the match below because '?' matches 'c'
       *)
      ("char range 7", test "[a][^0-9]?" "abc" true);
      ("trailing slash 1", test "a/" "a/" true);
      ("trailing slash 2", test "a/" "a" false);
      ("trailing slash 3", test "a" "a/" false);
      (* gitignore extensions not found in glob(3).
       * As opposed to '*', '**' can match the '/' character.
       *)
      ("ellipsis 1", test "**" "a" true);
      ("ellipsis 2", test "**" "a/b/c" true);
      ("ellipsis 3", test "/**" "/a/b" true);
      ("ellipsis 4", test "/**" "a/b" false);
      ("ellipsis 5", test "**/" "a/b/c/" true);
      ("ellipsis 6", test "**/" "a/b/c" false);
      (* can match any intermediate segments *)
      ("ellipsis 7", test "a/**/z" "a/b/c/d/z" true);
      ("ellipsis 8", test "a**" "a" true);
      ("ellipsis 9", test "a**" "abc" true);
      (* Not matching because '**' is special only if it's alone
       * as a path segment (see the gitignore spec). a** is
       * equivalent to just a*.
       *)
      ("ellipsis 10", test "a**" "a/b" false);
      ("ellipsis 11", test "****" "a" true);
      (* not matching either *)
      ("ellipsis 12", test "****" "a/b" false);
      ("double slash", test "//a//b//" "//a//b//" true);
    ]
