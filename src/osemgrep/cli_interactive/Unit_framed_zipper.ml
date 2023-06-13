module FZ = Framed_zipper

let emit zipper = Common.pr2 (FZ.show Int.to_string zipper)

let%expect_test "empty" =
  let zipper = FZ.empty_with_max_len 5 in
  emit zipper;
  emit (FZ.move_down zipper);
  emit (FZ.move_up zipper);
  [%expect
    {|
    [5 | (<NONE>), <NONE>, <NONE>, <NONE>, <NONE>]
    [5 | (<NONE>), <NONE>, <NONE>, <NONE>, <NONE>]
    [5 | (<NONE>), <NONE>, <NONE>, <NONE>, <NONE>] |}]

let%expect_test "of_list" =
  let zipper1 = FZ.of_list 5 [ 1; 2; 3 ] in
  let zipper2 = FZ.of_list 0 [ 1; 2; 3 ] in
  let zipper3 = FZ.of_list 3 [ 1; 2; 3; 4; 5 ] in
  let zipper4 = FZ.of_list 5 [ 1; 2; 3; 4; 5 ] in
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
    | Left -> FZ.move_up zipper
    | Right -> FZ.move_down zipper
  in
  emit zipper;
  zipper

let emit_with_moves zipper moves =
  emit zipper;
  List.fold_left (fun zipper move -> emit_with_move zipper move) zipper moves
  |> ignore

let%expect_test "move_up_boundary" =
  emit_with_moves (FZ.of_list 3 [ 1; 2; 3 ]) [ Left ];
  [%expect {|
    [3 | (1), 2, 3]
    [3 | (1), 2, 3] |}]

let%expect_test "move_down_boundary" =
  emit_with_moves (FZ.of_list 3 [ 1; 2; 3 ]) [ Right; Right; Right ];
  [%expect
    {|
    [3 | (1), 2, 3]
    [3 | 1, (2), 3]
    [3 | 1, 2, (3)]
    [3 | 1, 2, (3)] |}]

let%expect_test "move_up_and_right_fitted" =
  let zipper = FZ.of_list 3 [ 1; 2; 3 ] in
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

let%expect_test "move_up_and_right_framed" =
  let zipper = FZ.of_list 3 [ 1; 2; 3; 4; 5 ] in
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
  let zipper = FZ.of_list 3 [ 1; 2; 3 ] in
  emit zipper;
  emit (FZ.map_current (fun x -> x + 1) zipper);
  [%expect {|
    [3 | (1), 2, 3]
    [3 | (2), 2, 3] |}]

let%expect_test "change_max_len" =
  let zipper = FZ.of_list 3 [ 1; 2; 3 ] in
  emit zipper;
  emit (FZ.change_max_len zipper 5);
  emit (FZ.change_max_len zipper 1);
  [%expect
    {|
    [3 | (1), 2, 3]
    [5 | (1), 2, 3, <NONE>, <NONE>]
    [1 | (1)] |}]

let%expect_test "position" =
  let zipper = FZ.of_list 3 [ 1; 2; 3; 4; 5 ] in
  let zipper = FZ.move_down zipper in
  let zipper = FZ.move_down zipper in
  let zipper = FZ.move_down zipper in
  let zipper = FZ.move_down zipper in
  emit zipper;
  Common.(
    pr2
      (spf "absolute: %d, relative: %d"
         (FZ.absolute_position zipper)
         (FZ.relative_position zipper)));
  [%expect {|
    [3 | 3, 4, (5)]
    absolute: 4, relative: 2 |}]
