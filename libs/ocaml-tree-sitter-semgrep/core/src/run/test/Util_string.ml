(*
   Test the Util_string module.
*)

open Tree_sitter_run

let safe_sub_cases = [
  "", 0, 0, "";
  "abcd", 0, 4, "abcd";
  "abcd", 0, 2, "ab";
  "abcd", 2, 2, "cd";
  "", (-1), 1, "";
  "", 1, 1, "";
  "abcd", (-1), 2, "a";
  "abcd", 3, 2, "d";
  "abcd", (-1), 6, "abcd";
]

let test_safe_sub () =
  let check (src, pos, len, res) =
    Alcotest.(check string)
      "equal"
      (Util_string.safe_sub src pos len)
      res
  in
  List.iter check safe_sub_cases

let test = "Util_string", [
  "safe_sub", `Quick, test_safe_sub;
]
