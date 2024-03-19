(* Cooper Pierce
 *
 * Copyright (c) 2022 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

let rec fact = function
  | 0 -> 1
  | n -> fact (n - 1) * n

let fib x =
  let rec go = function
    | 0 -> (0, 0)
    | n ->
        let x, y = go (n - 1) in
        (y, x + y)
  in
  go x |> snd

module Fn = struct
  let uncurry f (x, y) = f x y
  let curry f x y = f (x, y)
  let flip = Fun.flip
end
