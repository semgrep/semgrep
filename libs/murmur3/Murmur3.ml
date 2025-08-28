(*
   Copyright (c) 2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
external murmur3_128 : string -> int64 * int64 = "caml_murmur3_128"

let hash128 data =
  let hi, lo = murmur3_128 data in
  let data = Bytes.make 16 '\000' in
  Bytes.set_int64_be data 0 lo;
  Bytes.set_int64_be data 8 hi;
  Bytes.unsafe_to_string data
