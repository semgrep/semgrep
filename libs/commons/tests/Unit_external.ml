(*
   Track external bugs
*)

(*
   Getting consistent formatting is important for comparing snapshots
   of ASTs and such.

   When this test passes on Windows, we can update the 'numbers.yml'
   YAML parsing test.
*)
let test_consistent_float_formatting =
  let expected_outcome : Testo.expected_outcome =
    if Sys.win32 then Should_fail "gives 1.23e-034 and 1.23e+034 on Windows"
    else Should_succeed
  in
  Testo.create "float formatting" ~expected_outcome (fun () ->
      Alcotest.(check string) "equal" "1.23e-34" (string_of_float 1.23e-34);
      Alcotest.(check string) "equal" "1.23e+34" (string_of_float 1.23e34))

let tests =
  Testo.categorize "External bugs" [ test_consistent_float_formatting ]
