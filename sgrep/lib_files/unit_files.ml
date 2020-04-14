open OUnit

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
          ~exclude_dirs:["a/c"] in
        assert_equal ~msg:"it should filter files"
          ["a/b/foo.js"]
          (Files_filter.filter filters files)
     )
  ]
