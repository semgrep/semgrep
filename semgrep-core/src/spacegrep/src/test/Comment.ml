(*
   Unit tests for spacegrep comment support
*)

module C = Spacegrep.Comment

(* name, comment style, input, expected output *)
let tests =
  [
    ("shell-style comments", C.shell_style, "a#xxx\nb\nc # yyy\n", "a\nb\nc\n");
    ("end of file comment", C.shell_style, "a #end", "a     ");
    ( "c-style comments",
      C.c_style,
      "hello */ /* ignore /* this */ world",
      "hello */                      world" );
    ( "cpp-style comments 1",
      C.cpp_style,
      "hello */ /* ignore /* this */ world",
      "hello */                      world" );
    ( "cpp-style comments 2",
      C.cpp_style,
      "hello // ignore this\nworld // ignore this too",
      "hello\nworld                   " );
    ( "end-of-line comments first",
      C.cpp_style,
      "hello /* //\n// */ world",
      "hello /*\n           " );
    ( "identical start and end",
      C.style ~delimiters:("|", "|") "custom",
      "a | ignore | b | c",
      "a            b | c" );
    ( "newline in delimiter",
      C.style ~delimiters:("comment", "end\n") "custom",
      "hello\ncomment\n  ign\n  ign end\nhey\ncomment end not\nend\nbye\n",
      "hello\n\n\n\nhey\n\n\nbye\n" );
  ]

let test =
  ( "Comment",
    tests
    |> List.map (fun (name, style, input, expected_output) ->
           let run () =
             let output = C.remove_comments_from_string style input in
             Alcotest.(check string "equal" expected_output output)
           in
           (name, `Quick, run)) )
