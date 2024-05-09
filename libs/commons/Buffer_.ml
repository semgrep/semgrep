(*
 * You can use this function with Printf.bprintf as in:
 *
 *   Buffer_.with_buffer_to_string (fun buf ->
 *     let prf fmt = Printf.bprintf buf fmt in
 *     prf "%d" i;
 *     prf "%s" str;
 *     ...
 *   )
 *
 * Note that you would think we can do even shorter and define
 * a Printf_.with_buffer_to_string as
 *
 *  let with_buffer_to_string f =
 *    let buf = Buffer.create 100 in
 *    f (Printf.bprintf buf);
 *    Buffer.contents buf
 *
 * But this will not work because of the value restriction. When
 * you will call this function as in
 *
 *   Printf_.with_buffer_to_string (prf ->
 *     prf "%d" i;
 *     prf "%s" str;
 *   )
 *
 * ocamlc will complain because the second prf has a different type
 * than the first prf. It can't generalize with a 'a. quantification.
 *
 * In fact in the Buffer_.with_buffer_to_string example above, you can
 * neither write
 *
 *   Buffer_.with_buffer_to_string (fun buf ->
 *     let prf = Printf.bprintf buf in
 *     prf "%d" i;
 *     prf "%s" str;
 *     ...
 *   )
 *
 * you need to eta expand to let prf fmt = ...
 *
 * See https://stackoverflow.com/questions/4047308/keeping-partially-applied-function-generic for more info (It's an F# example but it applies to OCaml too)
 *)

let with_buffer_to_string f =
  let buf = Buffer.create 100 in
  f buf;
  Buffer.contents buf
