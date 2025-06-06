(* Amarin Phaosawasdi and Yosef Alsuhaibani
 *
 * Copyright (C) Semgrep, Inc. All rights reserved.
 *)

(* This uses the fisher/yates algorithm
 * source: https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
 *)
let shuffle xs =
  let arr = Array.of_list xs in
  let n = Array.length arr in
  for i = n - 1 downto 1 do
    let j = URandom.int (i + 1) in
    let temp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- temp
  done;
  Array.to_list arr
