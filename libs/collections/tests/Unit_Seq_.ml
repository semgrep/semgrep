(*
   Unit tests for our Seq_ module.
*)

let t = Testo.create

let test_pp () =
  let string_of_list lst =
    (* Print in an hbox so as to avoid line splitting at breaks *)
    Format.asprintf "@[<h>%a@]"
      (Seq_.pp "(" ")" Format.pp_print_int)
      (List.to_seq lst)
  in

  let check_case (expected_str, lst) =
    let actual_str = string_of_list lst in
    Alcotest.(check string __LOC__ expected_str actual_str)
  in

  let cases = [ ("()", []); ("(5)", [ 5 ]); ("(1, 2, 3)", [ 1; 2; 3 ]) ] in

  List.iter check_case cases

let tests = Testo.categorize "Seq_" [ t "pp" test_pp ]
