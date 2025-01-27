(*
   Test the detection of non-text files to be ignored by spacegrep.
*)

open Spacegrep

let corpus =
  [
    ("hello", File_type.Text, "hello, world.\n");
    ("no newline", File_type.Text, "hello, world.");
    ("just newlines", File_type.Text, "\n\n\n\n\n\n\n\n\n\n\n");
    ( "some control characters",
      File_type.Binary,
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\x05zzzzzzzzzzzzzzzzzzzzzz" );
    ("some non-ascii", File_type.Text, "a\x80");
    ("just non-ascii", File_type.Text, "\x80\x90\xff");
    ( "one long line",
      File_type.Short,
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    );
    ( "one excessively long line",
      File_type.Minified,
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    );
  ]

let run_one expected_class data =
  let src = Src_file.of_string data in
  let class_ = File_type.classify src in
  Alcotest.(check string)
    "equal"
    (File_type.to_string expected_class)
    (File_type.to_string class_)

let test =
  let suite =
    List_.map
      (fun (name, expected_class, data) ->
        Testo.create name (fun () -> run_one expected_class data))
      corpus
  in
  Testo.categorize "File_type" suite
