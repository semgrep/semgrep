type make_fixed_lines_case = {
  test_name : string;
  contents : string;
  (* start, end, replacement *)
  fixes : (int * int * string) list;
  expected : string list option list;
}

let make_fixed_lines_cases =
  [
    {
      test_name = "Ordinary case with one overlapping fix";
      contents = "foo\nbar\n";
      fixes = [ (1, 5, "1\n2"); (1, 2, "1") ];
      expected = [ Some [ "f1"; "2ar" ]; None ];
    };
    {
      test_name = "Ordinary case with no overlapping fixes";
      contents = "foo\nbar\n";
      fixes = [ (1, 2, "1"); (5, 6, "2") ];
      expected = [ Some [ "f1o" ]; Some [ "b2r" ] ];
    };
    {
      test_name = "Two lines entirely deleted";
      contents = "foo\nbar\nbaz\n";
      fixes = [ (0, 7, "") ];
      expected = [ None ];
    };
    {
      test_name = "One line entirely deleted";
      contents = "foo\nbar\nbaz\n";
      fixes = [ (4, 7, "") ];
      expected = [ None ];
    };
    {
      test_name = "One line partially deleted";
      contents = "foo\nbar\nbaz\n";
      fixes = [ (5, 7, "") ];
      expected = [ Some [ "b" ] ];
    };
    {
      test_name = "Two lines partially deleted";
      contents = "foo\nbar\nbaz\n";
      fixes = [ (1, 7, "") ];
      expected = [ Some [ "f" ] ];
    };
    {
      test_name = "Second fix contained entirely in first";
      contents = "0123456789";
      fixes = [ (1, 4, "a"); (2, 3, "b") ];
      expected = [ Some [ "0a456789" ]; None ];
    };
    {
      test_name = "First fix contained entirely in second";
      contents = "0123456789";
      fixes = [ (2, 3, "a"); (1, 4, "b") ];
      expected = [ Some [ "01a3456789" ]; None ];
    };
    {
      test_name = "End of first fix overlaps with start of second";
      contents = "0123456789";
      fixes = [ (1, 4, "a"); (3, 6, "b") ];
      expected = [ Some [ "0a456789" ]; None ];
    };
    {
      test_name = "End of second fix overlaps with start of first";
      contents = "0123456789";
      fixes = [ (3, 6, "b"); (1, 4, "a") ];
      expected = [ Some [ "012b6789" ]; None ];
    };
    {
      test_name = "Start of second fix is adjacent to end of first";
      contents = "0123456789";
      fixes = [ (1, 4, "a"); (4, 6, "b") ];
      expected = [ Some [ "0a456789" ]; Some [ "0123b6789" ] ];
    };
    {
      test_name = "Start of first fix is adjacent to end of second";
      contents = "0123456789";
      fixes = [ (4, 6, "b"); (1, 4, "a") ];
      expected = [ Some [ "0123b6789" ]; Some [ "0a456789" ] ];
    };
  ]

let fakepath = Fpath.v (Filename.get_temp_dir_name () ^ "/fake.txt")

let create_make_fixed_lines_test { test_name; contents; fixes; expected } =
  Testo.create test_name (fun () ->
      let edits =
        List_.map
          (fun (start, end_, replacement_text) ->
            Textedit.{ start; end_; replacement_text; path = fakepath })
          fixes
      in
      let env = Fixed_lines.mk_env () in
      let all_fixed_lines : string list option list =
        List_.map (Fixed_lines.make_fixed_lines_of_string env contents) edits
      in
      Alcotest.(check (string |> list |> option |> list))
        "fixed_lines" expected all_fixed_lines)

let tests =
  Testo.categorize "autofix"
    (Testo.categorize "make_fixed_lines"
       (List_.map create_make_fixed_lines_test make_fixed_lines_cases))
