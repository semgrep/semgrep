(*
   Unit tests for glob pattern parsing and matching.
*)

open Printf

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let t = Testo.create

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
    pattern path matches res (Pattern.show pat)
    (Match.show_compiled_pattern compiled_pat);
  Alcotest.(check bool) __LOC__ matches res

(*****************************************************************************)
(* Test data *)
(*****************************************************************************)

(*
   These tests conform with the gitignore specification, falling back to
   the official gitignore implementation in case some important behavior
   is unspecified (e.g. '**' matches dot files but '*' doesn't).
*)
let tests =
  Testo.categorize "Glob"
    [
      t "simple" (test "abc" "abc" true);
      t "anchor left" (test "abc" "xabc" false);
      t "anchor right" (test "abc" "abcd" false);
      t "absolute path" (test "/a/b" "/a/b" true);
      t "star 1" (test "*.c" "hello.c" true);
      t "star 2" (test "*.c" "hello.h" false);
      (* '*' can not match a '/' *)
      t "star 3" (test "*.c" "a/hello.c" false);
      t "question 1" (test "a.?" "a.c" true);
      t "question 2" (test "a.?" "b.c" false);
      t "relative path 1" (test "ab/*.c" "ab/hello.c" true);
      t "relative path 2" (test "ab/*.c" "hello.c" false);
      t "relative path 3" (test "ab/*.c" "ab/" false);
      (* globs are left anchored, implicit '^' *)
      t "relative path 4" (test "ab/*.c" "x/ab/hello.c" false);
      t "absolute path 1" (test "/ab/*.c" "/ab/hello.c" true);
      t "absolute path 2" (test "/ab/*.c" "ab/hello.c" false);
      (* anchored too, so no match *)
      t "absolute path 3" (test "ab/*.c" "/ab/hello.c" false);
      t "char range 1" (test "[a-bz]" "b" true);
      t "char range 2" (test "[a-bz]" "c" false);
      t "char range 3" (test "[^a-bz]" "b" false);
      t "char range 4" (test "[^a-bz]" "c" true);
      (* alternative negative range syntax *)
      t "char range 5" (test "[!a-bz]" "b" false);
      t "char range 6" (test "[!a-bz]" "c" true);
      (* remember that '?' means any character; it is not a modifier
       * to the previous construct (like in regexps), hence
       * the match below because '?' matches 'c'
       *)
      t "char range 7" (test "[a][^0-9]?" "abc" true);
      t "trailing slash 1" (test "a/" "a/" true);
      t "trailing slash 2" (test "a/" "a" false);
      t "trailing slash 3" (test "a" "a/" false);
      (* gitignore extensions not found in glob(3).
       * As opposed to '*', '**' can match the '/' character.
       *)
      t "ellipsis 1" (test "**" "a" true);
      t "ellipsis 2" (test "**" "a/b/c" true);
      t "ellipsis 3" (test "/**" "/a/b" true);
      t "ellipsis 4" (test "/**" "a/b" false);
      t "ellipsis 5" (test "**/" "a/b/c/" true);
      t "ellipsis 6" (test "**/" "a/b/c" false);
      (* can match any intermediate segments *)
      t "ellipsis 7" (test "a/**/z" "a/b/c/d/z" true);
      t "ellipsis 8" (test "a**" "a" true);
      t "ellipsis 9" (test "a**" "abc" true);
      (* Not matching because '**' is special only if it's alone
       * as a path segment (see the gitignore spec). a** is
       * equivalent to just a*.
       *)
      t "ellipsis 10" (test "a**" "a/b" false);
      t "ellipsis 11" (test "****" "a" true);
      t "ellipsis 12" (test "****" "a/b" false);
      t "ellipsis 13" (test "**/*b" "/a/b" true);
      t "ellipsis 14" (test "**/*b" "a/b" true);
      t "ellipsis 15" (test "**/*b" "//a/b" true);
      t "ellipsis 16" (test "a/**" "a/b/c" true);
      t "ellipsis 17" (test "a/**" "a/" true);
      t "ellipsis 18" (test "a/**" "a" false);
      t "double slash" (test "//a//b//" "//a//b//" true);
      t "double slash in pattern" (test "//a//b//" "/a/b/" true);
      t "double slash in path" (test "/a/b/" "//a//b//" true);
      t "empty trailing segment" (test "a/*" "a/" false);
      t "empty leading segment" (test "*/a" "/a" false);
      t "not a dot file" (test "*a" "b.a" true);
      t "don't match dot file with wildcard" (test "*" ".a" false);
      t "don't match dot file with wildcard 2" (test "*a" ".a" false);
      t "don't match dot file with wildcard 3" (test "*" ".a" false);
      t "match dot file with literal dot" (test ".?b" ".ab" true);
      t "don't match dot file with wildcard sequence" (test "*?a" ".a" false);
      t "don't match dot file with wildcard sequence 2" (test "?*a" ".a" false);
      t "match dot file with '**'" (test "**" ".a" true);
      t "match dot file with '**' 2" (test "a/**/c" "a/.b/c" true);
    ]
