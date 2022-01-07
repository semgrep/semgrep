(*
   Unit tests for spacegrep comment support

   Note that ocamlformat removes end-of-line spaces from string literals.
   Make sure it's disabled on this file.
*)

module C = Spacegrep.Comment

(* name, comment style, input, expected output *)
let tests = [
  "shell-style comments",
  C.shell_style,
  "\
a#xxx
b
c # yyy
",
  "\
a
b
c
";

  "end of file comment",
  C.shell_style,
  "a #end",
  "a     ";

  "c-style comments",
  C.c_style,
  "hello */ /* ignore /* this */ world",
  "hello */                      world";

  "cpp-style comments 1",
  C.cpp_style,
  "hello */ /* ignore /* this */ world",
  "hello */                      world";

  "cpp-style comments 2",
  C.cpp_style,
  "\
hello // ignore this
world // ignore this too",
  "\
hello
world                   ";

  "end-of-line comments first",
  C.cpp_style,
  "\
hello /* //
// */ world",
  "\
hello /*
           ";

  "identical start and end",
  C.style ~delimiters:("|", "|") "custom",
  "a | ignore | b | c",
  "a            b | c";

  "newline in delimiter",
  C.style ~delimiters:("comment", "end\n") "custom",
  "\
hello
comment
  ign
  ign end
hey
comment end not
end
bye
",
  "\
hello



hey


bye
"
]

let test =
  "Comment",
  tests |> List.map (fun (name, style, input, expected_output) ->
    let run () =
      let output = C.remove_comments_from_string style input in
      Alcotest.(check string "equal" expected_output output)
    in
    (name, `Quick, run)
  )
