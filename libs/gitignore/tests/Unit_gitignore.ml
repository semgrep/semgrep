(*
   Copyright (c) 2023-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Unit tests for our gitignore implementation
*)

open Printf
module F = Testutil_files

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let concat_lines lines = String.concat "\n" lines ^ "\n"
let gitignore lines : Testutil_files.t = File (".gitignore", concat_lines lines)

(* Test that Testutil_files works as it should *)
let test_list (files : F.t list) () =
  F.with_tempfiles ~verbose:true files (fun root ->
      let files2 = F.read_dir root |> F.sort in
      printf "Output files:\n";
      F.print_files files2;
      assert (files2 = files))

(*
   In these tests, the file hierarchy must contain the
   .gitignore files but the target files are not needed.
*)
let test_filter (files : F.t list) () =
  F.with_tempdir ~chdir:true (fun root ->
      let files = F.sort files in
      printf "--- All files ---\n";
      F.print_files files;
      F.write_dir root files;
      let files2 = F.read_dir root |> F.sort in
      assert (files2 = files);
      printf "--- Filtered files ---\n";
      let filter = Gitignore_filter.create ~project_root:root () in
      files |> F.flatten
      |> List.iter (fun path ->
          assert (Fpath.is_rel path);
          let path = Ppath.of_relative_fpath_exn path in
          let status, selection_events =
            (* Glob.Match.run is supposed to print detailed logs on which
                  path is matched against which pattern. Requires Debug
                  log level. *)
            Gitignore_filter.select filter path
          in
          printf "Selection events for path %s:\n"
            (Ppath.to_string_for_tests path);
          print_string (Gitignore.show_selection_events selection_events);
          match status with
          | Not_ignored ->
              printf "SEL ppath %s\n" (Ppath.to_string_for_tests path)
          | Ignored -> printf "IGN ppath %s\n" (Ppath.to_string_for_tests path)))

(*****************************************************************************)
(* The tests *)
(*****************************************************************************)

let t =
  Testo.create ~checked_output:(Testo.stdout ())
    ~normalize:[ Testutil.mask_temp_paths () ]
    ?skipped:Testutil.skip_on_windows

(*****************************************************************************)
(* Strategy classification tests *)
(*****************************************************************************)

let classify_pattern str =
  let source =
    Glob.Match.string_loc ~source_name:"test" ~source_kind:None str
  in
  let { Parse_gitignore.absolute_pattern; _ } =
    Parse_gitignore.parse_pattern ~source ~left_anchor:Glob.Pattern.root_pattern
      str
  in
  Gitignore_level_index.classify absolute_pattern

let show_strategy (s : Gitignore_level_index.strategy) =
  match s with
  | Basename_literal { basename; dir_only } ->
      sprintf "Basename_literal(%s, dir_only=%b)" basename dir_only
  | Literal { path; dir_only } ->
      sprintf "Literal(%s, dir_only=%b)" path dir_only
  | Extension { ext; dir_only } ->
      sprintf "Extension(%s, dir_only=%b)" ext dir_only
  | Required_extension { ext } -> sprintf "Required_extension(%s)" ext
  | Regex -> "Regex"

let test_classify () =
  let open Gitignore_level_index in
  let check str expected =
    let actual = classify_pattern str in
    if actual <> expected then
      failwith
        (sprintf "classify %S: expected %s, got %s" str (show_strategy expected)
           (show_strategy actual))
  in
  (* Basename literals *)
  check ".git" (Basename_literal { basename = ".git"; dir_only = false });
  check "build/" (Basename_literal { basename = "build"; dir_only = true });
  check "node_modules/"
    (Basename_literal { basename = "node_modules"; dir_only = true });
  check "CVS" (Basename_literal { basename = "CVS"; dir_only = false });
  (* Dotfile basenames should still classify as Basename_literal *)
  check ".env" (Basename_literal { basename = ".env"; dir_only = false });
  check ".gitignore"
    (Basename_literal { basename = ".gitignore"; dir_only = false });
  (* Extensions *)
  check "*.js" (Extension { ext = ".js"; dir_only = false });
  check "*.py" (Extension { ext = ".py"; dir_only = false });
  (* Required extensions *)
  check "*_test.go" (Required_extension { ext = ".go" });
  check "*.min.js" (Required_extension { ext = ".js" });
  (* Literals *)
  check "/src/vendor" (Literal { path = "/src/vendor"; dir_only = false });
  check "/src/vendor/" (Literal { path = "/src/vendor"; dir_only = true });
  (* Regex fallback *)
  check "[ab]*" Regex;
  check "dir/*" Regex;
  (* Patterns with required subpath after a basename must NOT reduce to a
     Basename_literal. Regression: a buggy guard once classified these as
     Basename_literal { basename = "<first-seg>"; dir_only = true }, which
     matched any directory named <first-seg> regardless of the required
     subpath. *)
  check "**/.claude/plans/" Regex;
  check "**/foo/bar/" Regex;
  check "**/foo/*.log/" Regex;
  (* Patterns with an empty basename segment (e.g. [**/]) must not reduce
     to a [Basename_literal { basename = "" }] — that would be a dead entry
     because no real path has an empty last/dir segment. *)
  check "**/" Regex;
  check "/**/" Regex;
  (* --- More Basename_literal shapes: mixed char classes in the basename. *)
  check "foo-bar" (Basename_literal { basename = "foo-bar"; dir_only = false });
  check "foo_123" (Basename_literal { basename = "foo_123"; dir_only = false });
  check "foo.bar.baz"
    (Basename_literal { basename = "foo.bar.baz"; dir_only = false });
  check "CVS/" (Basename_literal { basename = "CVS"; dir_only = true });
  (* [**/foo] is an explicit unanchored form and must classify identically
     to plain [foo]. *)
  check "**/foo" (Basename_literal { basename = "foo"; dir_only = false });
  check "**/foo/" (Basename_literal { basename = "foo"; dir_only = true });
  (* --- More Extension shapes: different suffix lengths and dir_only. *)
  check "*.c" (Extension { ext = ".c"; dir_only = false });
  check "*.properties" (Extension { ext = ".properties"; dir_only = false });
  check "*.js/" (Extension { ext = ".js"; dir_only = true });
  (* --- More Required_extension shapes. *)
  check "foo*.log" (Required_extension { ext = ".log" });
  check "*foo.log" (Required_extension { ext = ".log" });
  check "*.tar.gz" (Required_extension { ext = ".gz" });
  check "*?.log" (Required_extension { ext = ".log" });
  (* --- More Literal shapes: deep paths and dotfile anchored. *)
  check "/a" (Literal { path = "/a"; dir_only = false });
  check "/a/b/c/d" (Literal { path = "/a/b/c/d"; dir_only = false });
  check "/a/b/c/d/" (Literal { path = "/a/b/c/d"; dir_only = true });
  check "/.env" (Literal { path = "/.env"; dir_only = false });
  (* [foo/bar] is middle-slash-anchored per gitignore spec, i.e. equivalent
     to [/foo/bar]. *)
  check "foo/bar" (Literal { path = "/foo/bar"; dir_only = false });
  (* --- Regex fallback: patterns that resist fast-path classification. *)
  check "*" Regex;
  check "**" Regex;
  check "?foo" Regex;
  check "foo*" Regex;
  check "*foo*" Regex;
  check "foo?" Regex;
  check "*.*" Regex;
  check "foo.*" Regex;
  check "*." Regex;
  check "[abc]" Regex;
  check "foo[abc]" Regex;
  (* Anchored patterns with globs inside cannot become Literal. *)
  check "/foo/*" Regex;
  check "/foo/*.js" Regex;
  check "/foo/bar/*" Regex;
  (* [Any_subpath] not at the leading position forces Regex. *)
  check "foo/**/bar" Regex;
  check "/foo/**/bar" Regex;
  check "foo/**" Regex;
  (* Inspired by ripgrep's globset extract tests. *)
  (* Multi-dot extension: ripgrep rejects as Extension (gets the full
     [*.rs.bak] form), but our [required_extension] machinery captures
     the trailing [.bak] and uses it as a gate for the PCRE check. *)
  check "**/*.rs.bak" (Required_extension { ext = ".bak" });
  check "*.rs.bak" (Required_extension { ext = ".bak" });
  (* Extension position occupied by a character class must fall to Regex. *)
  check "*.[ch]" Regex;
  (* Numeric-range character class in the basename. *)
  check "a[0-9]b" Regex;
  (* Negated character class. *)
  check "a[!0-9]b" Regex;
  (* Recursive wildcard in the middle of an anchored path. *)
  check "some/**/needle.txt" Regex;
  (* Single-char wildcard alone. *)
  check "?" Regex

(* Build a single-level filter from a gitignore source (one pattern per line). *)
let single_level_filter gitignore_source =
  let anchor = Glob.Pattern.root_pattern in
  let patterns =
    Parse_gitignore.from_string ~anchor ~name:"test" ~source_kind:"test"
      gitignore_source
  in
  let level =
    Gitignore_level_index.of_parsed_patterns ~level_kind:"test"
      ~source_name:"test" patterns
  in
  Gitignore_filter.create ~higher_priority_levels:[ level ]
    ~project_root:(Fpath.v (Filename.get_temp_dir_name ()))
    ()

(* Verify that indexed matching agrees with the naive PCRE-only matcher
   at the [Gitignore_level_index] layer. We compare the set of selection
   events emitted for each (level, path) pair directly, since the filter's
   parent-propagation composes [select_level] calls identically regardless
   of which implementation is plugged in — so agreement at the level layer
   is sufficient for classifier correctness. *)
let test_strategy_equivalence () =
  let anchor = Glob.Pattern.root_pattern in
  let key_of_ev (ev : Gitignore.selection_event) =
    match ev with
    | Gitignore.Selected loc -> (loc.source_name, loc.line_number, `Sel)
    | Gitignore.Deselected loc -> (loc.source_name, loc.line_number, `Desel)
  in
  let sorted xs = List.sort compare (List.map key_of_ev xs) in
  let test_pattern gitignore_line paths =
    let patterns =
      Parse_gitignore.from_string ~anchor ~name:"test" ~source_kind:"test"
        gitignore_line
    in
    let level =
      Gitignore_level_index.of_parsed_patterns ~level_kind:"test"
        ~source_name:"test" patterns
    in
    List.iter
      (fun path_str ->
        let ppath = Ppath.of_string_for_tests path_str in
        let indexed = Gitignore_level_index.select_level level ppath in
        let naive = Gitignore_level_index.select_level_naive level ppath in
        if sorted indexed <> sorted naive then
          failwith
            (sprintf
               "equivalence failed for pattern %S, path %S:\n\
               \  indexed: %s  naive:   %s"
               gitignore_line path_str
               (Gitignore.show_selection_events indexed)
               (Gitignore.show_selection_events naive)))
      paths
  in
  (* Basename_literal *)
  test_pattern "build/"
    [ "/build/"; "/a/build/"; "/build"; "/a/build"; "/building/" ];
  test_pattern ".git" [ "/.git"; "/a/.git"; "/.gitsomething"; "/a/b/.git" ];
  test_pattern "node_modules/"
    [ "/node_modules/"; "/a/node_modules/"; "/node_modules"; "/a/node_modules" ];
  (* Extension *)
  test_pattern "*.js" [ "/a.js"; "/a/b.js"; "/.js"; "/a.jsx"; "/a/b/c.js" ];
  test_pattern "*.py" [ "/a.py"; "/.py"; "/a.pyc"; "/py"; "/a/b.py" ];
  (* Required_extension: extension gate + PCRE2 *)
  test_pattern "*_test.go"
    [
      "/foo_test.go";
      "/pkg/foo_test.go";
      "/foo.go";
      "/foo_test.js";
      "/_test.go";
      "/a/b/c_test.go";
    ];
  test_pattern "*.min.js"
    [ "/a.min.js"; "/x/y.min.js"; "/a.js"; "/a.min"; "/a.min.jsx" ];
  (* Literal (anchored) *)
  test_pattern "/src/vendor"
    [ "/src/vendor"; "/src/vendor2"; "/src"; "/vendor"; "/src/vendor/" ];
  test_pattern "/src/vendor/"
    [ "/src/vendor/"; "/src/vendor"; "/other/src/vendor/" ];
  (* Dotfiles as Extension targets: '*.env' matches '.env' because
     gitignore's glob_period=true lets '*' match a leading dot. *)
  test_pattern "*.env"
    [ "/.env"; "/a/.env"; "/.environment"; "/foo.env"; "/.env.local" ];
  (* Dotfiles as Basename_literal *)
  test_pattern ".gitignore"
    [ "/.gitignore"; "/sub/.gitignore"; "/.gitignore.bak"; "/gitignore" ];
  (* Regex fallback should still agree with indexed matching. Both sides
     go through the filter, so parent-propagation (e.g. [/dir/sub/a] being
     ignored because its parent [/dir/sub/] matched) is applied to both
     and won't cause a divergence. *)
  test_pattern "dir/*" [ "/dir/a"; "/dir/"; "/dir"; "/other/dir/a" ];
  test_pattern "[ab]*" [ "/a"; "/b"; "/c"; "/abc" ];
  (* Patterns with required subpath after a basename — must NOT reduce
     to a Basename_literal matching any directory of that name. *)
  test_pattern "**/.claude/plans/"
    [
      "/.claude/";
      "/a/.claude/";
      "/.claude/plans/";
      "/a/.claude/plans/";
      "/.claude/plans/foo";
    ];
  (* Patterns that produce an empty-basename segment — must NOT reduce
     to a Basename_literal { basename = "" }. *)
  test_pattern "**/" [ "/"; "/a"; "/a/"; "/a/b" ];
  test_pattern "/**/" [ "/"; "/a"; "/a/"; "/a/b" ];
  (* [**/foo] must behave the same as [foo] (both unanchored basenames). *)
  test_pattern "**/foo" [ "/foo"; "/foo/"; "/a/foo"; "/foo/a"; "/foobar" ];
  (* Dir-only basename with a surrounding dotfile check. *)
  test_pattern ".cache/"
    [ "/.cache/"; "/.cache"; "/a/.cache/"; "/.cached/"; "/not.cache/" ];
  (* Star-prefixed basename with hyphens/underscores. *)
  test_pattern "*_bak" [ "/a_bak"; "/_bak"; "/bak"; "/foo_bak/"; "/a/b_bak" ];
  (* Anchored deep literal. *)
  test_pattern "/a/b/c" [ "/a/b/c"; "/a/b/c/"; "/a/b"; "/a/b/c/d"; "/x/a/b/c" ];
  (* Anchored pattern with glob MUST fall through to Regex — the filter
     still needs to agree with PCRE on typical paths. *)
  test_pattern "/foo/*.log"
    [ "/foo/a.log"; "/foo/a"; "/foo/.log"; "/bar/a.log"; "/foo/a.log.bak" ];
  (* Basename with a dot in the middle; different from the [*.x] Extension
     class. *)
  test_pattern "foo.bar"
    [ "/foo.bar"; "/foo/bar"; "/foo.bar/"; "/a/foo.bar"; "/foo.barn" ];
  (* Char-class patterns (fall through to Regex). *)
  test_pattern "[Dd]ebug"
    [ "/Debug"; "/debug"; "/DEBUG"; "/a/Debug"; "/Debugger" ];
  (* Trailing-question-mark pattern (Regex). *)
  test_pattern "file.?" [ "/file.a"; "/file.ab"; "/file."; "/a/file.b" ];
  (* Numeric-range character class (Regex). *)
  test_pattern "a[0-9]b"
    [ "/a0b"; "/a9b"; "/a_b"; "/ab"; "/a1b"; "/a/a5b"; "/aXb" ];
  (* Negated character class (Regex). *)
  test_pattern "a[!0-9]b" [ "/a0b"; "/a_b"; "/axb"; "/ab"; "/a1b" ];
  (* Char-class in extension position (Regex). *)
  test_pattern "*.[ch]"
    [ "/a.c"; "/a.h"; "/a.cpp"; "/a.py"; "/.c"; "/a/b.h"; "/foo.c.bak" ];
  (* Dotfile recursive pattern: matches files whose basename starts with
     a dot, anywhere in the tree. *)
  test_pattern "**/.*"
    [
      "/.abc";
      "/ab.c";
      "/foo/.abc";
      "/.git";
      "/a/b/.env";
      "/foo/bar.baz";
      "/plain";
    ];
  (* Recursive wildcard between two fixed segments — classic Regex case. *)
  test_pattern "some/**/needle.txt"
    [
      "/some/needle.txt";
      "/some/one/needle.txt";
      "/some/a/b/c/needle.txt";
      "/some/other/notthis.txt";
      "/other/some/needle.txt";
    ];
  (* Multi-dot extension pattern (Required_extension with PCRE gate). *)
  test_pattern "**/*.rs.bak"
    [ "/foo.rs.bak"; "/a/b/foo.rs.bak"; "/foo.bak"; "/foo.rs"; "/x.rs.bak.tmp" ];
  (* Pattern with leading [*] and trailing literal — must still agree with
     PCRE on extra-suffix paths (ripgrep regression case). *)
  test_pattern "*hello.txt"
    [ "/hello.txt"; "/ahello.txt"; "/hello.txt-and-then-some"; "/a/hello.txt" ]

(* Verify that negated patterns stored in the strategy index produce
   Deselected events (i.e. un-ignore a previously ignored path). *)
let test_negation () =
  let check ~source ~path ~expected =
    let filter = single_level_filter source in
    let status, _ =
      Gitignore_filter.select filter (Ppath.of_string_for_tests path)
    in
    let actual = status = Gitignore.Ignored in
    if expected <> actual then
      failwith
        (sprintf "negation failed for source %S, path %S: expected=%b, got=%b"
           source path expected actual)
  in
  (* Basename_literal + basename negation *)
  check ~source:".env\n!.env" ~path:"/.env" ~expected:false;
  (* Extension + basename negation *)
  check ~source:"*.c\n!main.c" ~path:"/main.c" ~expected:false;
  check ~source:"*.c\n!main.c" ~path:"/foo.c" ~expected:true;
  (* Basename_literal ignore, Literal negation *)
  check ~source:"build/\n!/src/build/" ~path:"/src/build/" ~expected:false;
  check ~source:"build/\n!/src/build/" ~path:"/other/build/" ~expected:true;
  (* Required_extension negation *)
  check ~source:"*.go\n!*_test.go" ~path:"/foo_test.go" ~expected:false;
  check ~source:"*.go\n!*_test.go" ~path:"/foo.go" ~expected:true;
  (* Three-level override: ignore, re-ignore, un-ignore. Last pattern wins. *)
  check ~source:"*.log\n!debug.log\ndebug.log" ~path:"/debug.log" ~expected:true;
  (* Negation that crosses strategy types (Extension un-ignored via
     Basename_literal). *)
  check ~source:"*.log\n!keep.log" ~path:"/keep.log" ~expected:false;
  check ~source:"*.log\n!keep.log" ~path:"/other.log" ~expected:true;
  (* Negation landing in Regex bucket. *)
  check ~source:"*.log\n![ab].log" ~path:"/a.log" ~expected:false;
  check ~source:"*.log\n![ab].log" ~path:"/c.log" ~expected:true

(* Verify that multiple patterns sharing the same hash-table key all fire. *)
let test_multiple_patterns_same_key () =
  (* Two Extension patterns with different extensions, plus two Basename
     literals. The hash buckets each get a single entry; verify both patterns
     fire against the right paths. *)
  let filter = single_level_filter "*.js\n*.ts\nbuild\ndist" in
  let check path expected =
    let status, _ =
      Gitignore_filter.select filter (Ppath.of_string_for_tests path)
    in
    let actual = status = Gitignore.Ignored in
    if expected <> actual then
      failwith
        (sprintf "multi-pattern: %S expected=%b got=%b" path expected actual)
  in
  check "/a.js" true;
  check "/a.ts" true;
  check "/a.py" false;
  check "/build" true;
  check "/dist" true;
  check "/x/build" true;
  check "/other" false;

  (* Two Basename_literal patterns sharing a basename: verify that both
     entries end up in the same bucket and both fire in pattern order. *)
  let filter2 = single_level_filter ".env\n!.env" in
  let status, events =
    Gitignore_filter.select filter2 (Ppath.of_string_for_tests "/.env")
  in
  (* The later !.env should override the earlier .env *)
  if status <> Gitignore.Not_ignored then
    failwith
      (sprintf "shared-key bucket ordering wrong: status=%s"
         (match status with
         | Gitignore.Ignored -> "Ignored"
         | Gitignore.Not_ignored -> "Not_ignored"));
  (* Two events should have fired (Selected then Deselected) *)
  if List.length events < 2 then
    failwith
      (sprintf "shared-key bucket: expected >=2 events, got %d"
         (List.length events))

(* An empty gitignore line must not accidentally classify as a strategy
   that matches every path. Both the PCRE2 path and the strategy path
   should agree that it matches nothing real. *)
let test_empty_pattern () =
  let source = "" in
  let s = classify_pattern source in
  (* Not a requirement that the classifier pick a specific strategy for "",
     only that the filter behavior matches PCRE2 on real paths. *)
  ignore s;
  let filter = single_level_filter "\n\n" in
  List.iter
    (fun path ->
      let status, _ =
        Gitignore_filter.select filter (Ppath.of_string_for_tests path)
      in
      if status = Gitignore.Ignored then
        failwith (sprintf "empty lines should not ignore path %S" path))
    [ "/a"; "/foo.c"; "/a/b"; "/a/"; "/x/y/z" ]

let tests =
  let open F in
  Testo.categorize "Gitignore"
    [
      Testo.create "strategy classification" test_classify;
      Testo.create "strategy equivalence" test_strategy_equivalence;
      Testo.create "strategy negation" test_negation;
      Testo.create "strategy multi-pattern" test_multiple_patterns_same_key;
      Testo.create "strategy empty pattern" test_empty_pattern;
      t "list one file" (test_list [ file "a" ]);
      t "list hierarchy"
        (test_list
           [
             file "a";
             file "b";
             symlink "c" "a";
             dir "dir" [ file "d"; symlink "e" "f"; dir "g" [] ];
           ]);
      t "simple gitignore"
        (test_filter [ gitignore [ "*.c" ]; file "hello.c"; file "hello.ml" ]);
      t "relative paths"
        (test_filter [ gitignore [ "*.c" ]; file "hello.c"; file "hello.ml" ]);
      t "unanchored"
        (test_filter [ gitignore [ "a" ]; dir "dir" [ file "a" ]; file "a" ]);
      t "deep gitignore"
        (test_filter [ dir "dir" [ gitignore [ "a" ]; file "a" ]; file "a" ]);
      t "ignore directories only"
        (test_filter
           [ gitignore [ "a/" ]; dir "dir" [ file "a" ]; dir "a" [ file "b" ] ]);
      t "absolute patterns"
        (test_filter
           [
             (* [!] 'b/c' is treated as anchored just like '/b/c' because it
                contains a slash in the middle, as per the gitignore spec. *)
             gitignore [ "/a"; "b/c" ];
             dir "a" [ file "b" ];
             dir "b" [ file "a"; file "c"; file "d"; dir "b" [ file "c" ] ];
           ]);
      (* unanchored patterns should not match if the parent dir hasn't been
         matched, and the path is excluded *)
      t "excluded patterns"
        (test_filter
           [
             (* 'a/' excludes any folder or subfolder named 'a' *)
             gitignore [ "a/"; "!dir/a"; "!a/b" ];
             (* '/dir/a' is not ignored because '!dir/a' overrides 'a/'
                (therefore '/dir/a/b' is not ignored either) *)
             dir "dir" [ dir "a" [ file "b" ] ];
             (* /a/b is ignored because its parent is ignored;
                '!a/b' can do nothing about it. *)
             dir "a" [ file "b" ];
           ]);
      t "anchored with trailing wildcard"
        (test_filter
           [
             gitignore [ "dir/*" ];
             dir "dir" [ file "ignore-me" ];
             (* The slash in 'dir/*' anchors the pattern *)
             dir "sub" [ dir "dir" [ file "ignore-me-not" ] ];
           ]);
      t "ignore all but one"
        (test_filter
           [
             gitignore [ "dir/*"; "!dir/ignore-me-not" ];
             dir "dir" [ file "ignore-me"; file "ignore-me-not" ];
           ]);
    ]
