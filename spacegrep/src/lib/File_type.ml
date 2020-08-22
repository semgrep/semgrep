(*
   A crude heuristic to determine if a file is human-readable.
*)

type t =
  | Text
  | Gibberish

let guess src =
  let contents = Src_file.contents src in
  let length = String.length contents in
  let newlines =
    let n = ref 0 in
    String.iter (function
      | '\n' -> incr n
      | _ -> ()
    ) contents;
    !n
  in
  (* If the average line is longer than 150, it's considered not
     human-readable.
     For random bytes, the average line length is 256.
     For minified source code, there may be not a single newline in the file.
  *)
  if length = 0 || float newlines /. float length >= 1./.150. then
    Text
  else
    Gibberish
