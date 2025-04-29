(*
   Tests for our Random_ module
*)

let t = Testo.create

let test_shuffle () =
  let check input =
    let shuffled = Random_.shuffle input in
    (* Check that shuffled list has same length *)
    Alcotest.(check int) __LOC__ (List.length input) (List.length shuffled);
    (* Check that shuffled list has same elements (ignoring order) *)
    let sorted_input = List.sort compare input in
    let sorted_shuffled = List.sort compare shuffled in
    Alcotest.(check (list int)) __LOC__ sorted_input sorted_shuffled
  in
  (* Test empty list *)
  check [];
  (* Test single element *)
  check [ 1 ];
  (* Test multiple elements *)
  check [ 1; 2; 3 ];
  (* Test with duplicates *)
  check [ 1; 1; 2; 2; 3 ]

let tests = Testo.categorize "Random_" [ t "shuffle" test_shuffle ]
