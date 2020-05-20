(*s: semgrep/finding/Unit_files.ml *)
open OUnit

(*s: constant [[Unit_files.unittest]] *)
let unittest =
  "file filtering" >::: [
    "basic exclude/include" >:: (fun () ->
        let files = [
            "a/b/foo.c";
            "a/b/foo.js";
            "a/b/bar.c";
            "a/b/bar.js";
            "a/b/foo.go";
            "a/c/foo.c";
            "a/c/foo.js";
        ] in
        let filters = Files_filter.mk_filters
          ~excludes:["*.{c,h}"; "*.go"]
          ~includes:["foo.*"]
          ~exclude_dirs:["c"] 
          ~include_dirs:["a"; "b"] in
        assert_equal ~msg:"it should filter files"
          ["a/b/foo.js"]
          (Files_filter.filter filters files)
     )
  ]
(*e: constant [[Unit_files.unittest]] *)
(*e: semgrep/finding/Unit_files.ml *)
