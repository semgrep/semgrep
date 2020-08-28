
open TestOpen
module TestAlias = TestAliased

let test_pattern  = function
  | TestTublePattern(Y1, Y2, Y3) -> raise Todo
  | TestListPattern( [Y1, Y2, Y3]) -> raise Todo

let test_parameters x y =
  raise Todo

let test_record_pattern x =
  let { fld1 = y; fld2} = x in
  raise Todo
