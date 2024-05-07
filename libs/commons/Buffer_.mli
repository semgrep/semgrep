(* Small extension to the official Buffer.ml module.
 * See also Immutable_buffer.ml
 *)

(* [with_buffer_to_string f] will call [f] with a new buffer and once
 * its computation is done it will return the content of this buffer.
 * You can use this function with Printf.bprintf as in:
 *
 *   Buffer_.with_buffer_to_string (fun buf ->
 *     let prf fmt = Printf.bprintf buf fmt in
 *     prf "%d" i;
 *     prf "%s" str;
 *     ...
 *   )
 *)
val with_buffer_to_string : (Buffer.t -> unit) -> string
