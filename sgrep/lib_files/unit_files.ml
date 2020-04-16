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
            "a/b/c/foo.js";
            "a/c/foo.c";
            "a/c/foo.js";
            "a/c/d/foo.js"
        ] in
        let filters = Files_filter.mk_filters
          ~excludes:["*.{c,h}"; "*.go"]
          ~includes:["foo.*"]
          ~exclude_dirs:["a/c"] in  (* I would like this to be 'a/c' so a/b/c still exists but if I change to 'a/c' then 'a/c/d/foo.js doesn't get filtered out *)
        assert_equal ~msg:"it should filter files"
          ["a/b/foo.js"; "a/b/c/foo.js";]
          (Files_filter.filter filters files)
     )
  ]
