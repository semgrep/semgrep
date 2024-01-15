(*
   Unit tests for Hashtbl_
*)

let test_stack () =
  let tbl = Hashtbl.create 100 in
  Hashtbl_.push tbl 42 "a";
  Hashtbl_.push tbl 17 "b";
  Hashtbl_.push tbl 42 "c";
  assert (Hashtbl_.get_stack tbl 42 = [ "c"; "a" ]);
  assert (Hashtbl_.get_stack tbl 17 = [ "b" ]);
  assert (Hashtbl_.get_stack tbl 110 = [])

let tests =
  Testo.categorize "Hashtbl_" [ Testo.create "push/get_stack" test_stack ]
