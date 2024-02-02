external murmur3_128 : string -> int64 * int64 = "caml_murmur3_128"

let hash128 data =
  let hi, lo = murmur3_128 data in
  let data = Bytes.make 16 '\000' in
  Bytes.set_int64_be data 0 lo;
  Bytes.set_int64_be data 8 hi;
  Bytes.unsafe_to_string data
