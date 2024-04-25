(*
   Tests for our String_ module
*)

let t = Testo.create

(* Try hard to raise an exception *)
let test_safe_sub () =
  let check str pos len expected =
    Printf.printf "safe_sub %S pos=%i len=%i\n%!" str pos len;
    Alcotest.(check string) __LOC__ expected (String_.safe_sub str pos len)
  in
  check "" 0 0 "";
  check "" 0 1 "";
  check "" 0 (-1) "";
  check "" 1 0 "";
  check "" 1 1 "";
  check "" 1 (-1) "";
  check "" (-1) 0 "";
  check "" (-1) 1 "";
  check "" (-1) (-1) "";
  check "abc" 0 3 "abc";
  check "abc" 0 2 "ab";
  check "abc" 1 2 "bc";
  check "abc" 2 2 "c";
  check "abc" (-1) 2 "a";
  check "abc" (-10) 5 "";
  check "abc" (-10) 20 "abc";
  check "abc" 10 5 "";
  check "abc" max_int 1 "";
  check "abc" 0 max_int "abc";
  check "abc" 1 max_int "bc";
  check "abc" min_int 1 ""

(* The output should look reasonably truncated *)
let test_show () =
  print_endline (String_.show "abc");
  print_endline (String_.show ~max_len:5 "123456789");
  print_endline (String_.show ~max_len:20 "123456789")

let test_trim_cr () =
  let check expected input =
    Alcotest.(check string) __LOC__ expected (String_.trim_cr input)
  in
  check "" "";
  check "a" "a";
  check "asdf asdf" "asdf asdf";
  check "foo\r\nbar" "foo\r\nbar";
  check "foo\r\n" "foo\r\n";
  check "foo" "foo\r";
  check "" "\r"

let test_lines_of_range () =
  let check expected input range =
    Alcotest.(check (list string))
      __LOC__ expected
      (String_.lines_of_range range input)
  in
  (* - Out of bounds start and end - *)
  check [] "foo\nbar\n" (-1, 5);
  check [] "foo\nbar\n" (4, 9);
  (* - Normal cases - *)
  check [ "bar" ] "foo\nbar\n" (4, 5);
  check [ "foo"; "bar" ] "foo\nbar\n" (0, 5);
  (* - No trailing newline - *)
  check [ "foo"; "bar" ] "foo\nbar" (0, 5);
  (* - No newline at all - *)
  check [ "foo" ] "foo" (1, 2);
  (* - Edge cases - *)
  check [ "x" ] "x\n" (0, 1);
  check [ ""; "x" ] "\nx\n" (0, 2);
  check [ ""; "x" ] "\n\nx\n" (1, 3);
  check [ "" ] "\n" (0, 0);
  check [ ""; "" ] "\n" (0, 1);
  check [ ""; "" ] "\n\n" (0, 1);
  check [ ""; ""; "" ] "\n\n\n" (0, 2);
  (* - Start on or adjacent to a newline character - *)
  check [ "foo" ] "foo\nbar\n" (3, 3);
  check [ "foo" ] "foo\nbar\n" (2, 3);
  check [ "foo"; "bar" ] "foo\nbar\n" (3, 4);
  check [ "foo"; "bar" ] "foo\nbar\n" (3, 5);
  check [ "bar" ] "foo\nbar\n" (4, 5);
  check [ "bar" ] "foo\nbar\n" (7, 7);
  (* This, like some of the edge cases above, is a bit weird. Position 8 is
   * after newline character, so we consider it part of a nonexistent third line
   * and end up including an empty string. I (nmote) am not sure that this is
   * really desirable, but it's also unlikely to come up often in practice since
   * typically newline characters are not typically used as the start or end of
   * a range. *)
  check [ "bar"; "" ] "foo\nbar\n" (7, 8);
  (* - End on or adjacent to a newline character - *)
  check [ "foo" ] "foo\nbar\n" (1, 2);
  check [ "foo" ] "foo\nbar\n" (1, 3);
  check [ "foo"; "bar" ] "foo\nbar\n" (1, 4);
  (* - Carriage return ('\r') - *)
  check [ "bar" ] "foo\r\nbar\r\n" (5, 6);
  check [ "foo"; "bar" ] "foo\r\nbar\r\n" (1, 6);
  (* - Empty string - *)
  check [] "" (0, 0);
  (* - Zero length range - *)
  check [ "bar" ] "foo\nbar\n" (4, 4)

let tests =
  Testo.categorize "String_"
    [
      t "safe_sub" test_safe_sub;
      t ~checked_output:(Testo.stdout ()) "show" test_show;
      t "trim_cr" test_trim_cr;
      t "lines_of_range" test_lines_of_range;
    ]
