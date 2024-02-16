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

let tests =
  Testo.categorize "String_"
    [ t "safe_sub" test_safe_sub; t ~checked_output:Stdout "show" test_show ]
