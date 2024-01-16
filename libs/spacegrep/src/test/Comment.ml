(*
   Unit tests for spacegrep comment support

   Note that pre-commit hooks don't like trailing whitespace:
   - a tool removes trailing whitespace, that's why we have to resort
     to using '\n\' instead of just a newline in some places.
   - ocamlformat may also rearrange string literals in a less readable way,
     so it's disabled via a .ocamlformat-ignore file.
   Additionally, OCaml ignores leading blanks in string literals
   if the previous line ends with a '\' (line continuation).
   If this gets too hard, just avoid line breaks in string literals.
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
a    \n\
b\n\
c      \n\
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
hello               \n\
world                   ";

  "end-of-line comments first",
  C.cpp_style,
  "\
hello /* //
// */ world",
  "hello /*   \n           ";

  "identical start and end",
  [C.Multiline ("|", "|")],
  "a | ignore | b | c",
  "a            b | c";

  "newline in delimiter",
  [C.Multiline ("comment", "end\n")],
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
  "hello\n       \n     \n         \nhey\n               \n   \nbye\n"
]

let test =
  tests |> List_.map (fun (name, style, input, expected_output) ->
    let run () =
      let output = C.remove_comments_from_string style input in
      Alcotest.(check string "equal" expected_output output)
    in
    Testo.create name run
  )
  |> Testo.categorize "Comment"
