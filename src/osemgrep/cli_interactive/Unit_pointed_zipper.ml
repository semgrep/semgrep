module PZ = Pointed_zipper

let emit zipper = Common.pr2 (PZ.show Int.to_string zipper)

let%expect_test "empty" =
  let zipper = PZ.empty_with_max_len 5 in
  emit zipper;
  emit (PZ.move_right zipper);
  emit (PZ.move_left zipper);
  [%expect
    {|
    [5 | (<NONE>), <NONE>, <NONE>, <NONE>, <NONE>]
    [5 | (<NONE>), <NONE>, <NONE>, <NONE>, <NONE>]
    [5 | (<NONE>), <NONE>, <NONE>, <NONE>, <NONE>] |}]

let%expect_test "of_list" =
  let zipper1 = PZ.of_list 5 [ 1; 2; 3 ] in
  let zipper2 = PZ.of_list 0 [ 1; 2; 3 ] in
  let zipper3 = PZ.of_list 3 [ 1; 2; 3; 4; 5 ] in
  let zipper4 = PZ.of_list 5 [ 1; 2; 3; 4; 5 ] in
  emit zipper1;
  emit zipper2;
  emit zipper3;
  emit zipper4;
  [%expect
    {|
    [5 | (1), 2, 3, <NONE>, <NONE>]
    [0 | ]
    [3 | (1), 2, 3]
    [5 | (1), 2, 3, 4, 5] |}]

type move = Left | Right

let emit_with_move zipper move =
  let zipper =
    match move with
    | Left -> PZ.move_left zipper
    | Right -> PZ.move_right zipper
  in
  emit zipper;
  zipper

let emit_with_moves zipper moves =
  emit zipper;
  List.fold_left (fun zipper move -> emit_with_move zipper move) zipper moves
  |> ignore

let%expect_test "move_left_boundary" =
  emit_with_moves (PZ.of_list 3 [ 1; 2; 3 ]) [ Left ];
  [%expect {|
    [3 | (1), 2, 3]
    [3 | (1), 2, 3] |}]

let%expect_test "move_right_boundary" =
  emit_with_moves (PZ.of_list 3 [ 1; 2; 3 ]) [ Right; Right; Right ];
  [%expect
    {|
    [3 | (1), 2, 3]
    [3 | 1, (2), 3]
    [3 | 1, 2, (3)]
    [3 | 1, 2, (3)] |}]

let%expect_test "move_left_and_right_fitted" =
  let zipper = PZ.of_list 3 [ 1; 2; 3 ] in
  emit zipper;
  [%expect {| [3 | (1), 2, 3] |}];
  let zipper = emit_with_move zipper Right in
  [%expect {| [3 | 1, (2), 3] |}];
  let zipper = emit_with_move zipper Right in
  [%expect {| [3 | 1, 2, (3)] |}];
  let zipper = emit_with_move zipper Left in
  [%expect {| [3 | 1, (2), 3] |}];
  let zipper = emit_with_move zipper Left in
  [%expect {| [3 | (1), 2, 3] |}];
  let zipper = emit_with_move zipper Left in
  [%expect {| [3 | (1), 2, 3] |}];
  let zipper = emit_with_move zipper Right in
  [%expect {| [3 | 1, (2), 3] |}];
  let zipper = emit_with_move zipper Right in
  [%expect {| [3 | 1, 2, (3)] |}];
  let zipper = emit_with_move zipper Right in
  [%expect {| [3 | 1, 2, (3)] |}];
  let zipper = emit_with_move zipper Left in
  [%expect {| [3 | 1, (2), 3] |}];
  let zipper = emit_with_move zipper Left in
  [%expect {| [3 | (1), 2, 3] |}];
  let _ = emit_with_move zipper Right in
  [%expect {| [3 | 1, (2), 3] |}]

let%expect_test "move_left_and_right_framed" =
  let zipper = PZ.of_list 3 [ 1; 2; 3; 4; 5 ] in
  emit zipper;
  [%expect {| [3 | (1), 2, 3] |}];
  let zipper = emit_with_move zipper Right in
  [%expect {| [3 | 1, (2), 3] |}];
  let zipper = emit_with_move zipper Right in
  [%expect {| [3 | 1, 2, (3)] |}];
  let zipper = emit_with_move zipper Right in
  [%expect {| [3 | 2, 3, (4)] |}];
  let zipper = emit_with_move zipper Left in
  [%expect {| [3 | 2, (3), 4] |}];
  let zipper = emit_with_move zipper Left in
  [%expect {| [3 | (2), 3, 4] |}];
  let zipper = emit_with_move zipper Left in
  [%expect {| [3 | (1), 2, 3] |}];
  let zipper = emit_with_move zipper Right in
  [%expect {| [3 | 1, (2), 3] |}];
  let zipper = emit_with_move zipper Right in
  [%expect {| [3 | 1, 2, (3)] |}];
  let zipper = emit_with_move zipper Right in
  [%expect {| [3 | 2, 3, (4)] |}];
  let zipper = emit_with_move zipper Right in
  [%expect {| [3 | 3, 4, (5)] |}];
  let _ = emit_with_move zipper Right in
  [%expect {| [3 | 3, 4, (5)] |}]

let%expect_test "map_current" =
  let zipper = PZ.of_list 3 [ 1; 2; 3 ] in
  emit zipper;
  emit (PZ.map_current (fun x -> x + 1) zipper);
  [%expect {|
    [3 | (1), 2, 3]
    [3 | (2), 2, 3] |}]

let%expect_test "change_max_len" =
  let zipper = PZ.of_list 3 [ 1; 2; 3 ] in
  emit zipper;
  emit (PZ.change_max_len zipper 5);
  emit (PZ.change_max_len zipper 1);
  [%expect
    {|
    [3 | (1), 2, 3]
    [5 | (1), 2, 3, <NONE>, <NONE>]
    [1 | (1)] |}]

let%expect_test "position" =
  let zipper = PZ.of_list 3 [ 1; 2; 3; 4; 5 ] in
  let zipper = PZ.move_right zipper in
  let zipper = PZ.move_right zipper in
  let zipper = PZ.move_right zipper in
  let zipper = PZ.move_right zipper in
  emit zipper;
  Common.(
    pr2
      (spf "absolute: %d, relative: %d"
         (PZ.absolute_position zipper)
         (PZ.relative_position zipper)));
  [%expect {|
    [3 | 3, 4, (5)]
    absolute: 4, relative: 2 |}]
