let%expect_test "foo" =
  Common.pr2 "foo";
  [%expect {| wahdihaw |}]
