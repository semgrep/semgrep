(*
   Utilities for dealing with Unicode issues.
*)

exception Found

(* could probably be faster, but seems "good enough" *)
let file_contains_non_ascii file =
  (* copy-paste of Common.read_file without Bytes.to_string *)
  let ic = open_in file  in
  let size = in_channel_length ic in
  let buf = Bytes.create size in
  really_input ic buf 0 size;
  close_in ic;
  try
    buf |> Bytes.iter (fun char ->
        if Char.code char > 128
        then raise Found
    );
    false
  with Found -> true

module UTF8 = struct
  (* Guess the length of the original UTF-8 sequence by re-encoding the
     character. This is quite inefficient. *)
  let get_length uchar =
    let buf = Buffer.create 8 in
    let enc = Uutf.encoder `UTF_8 (`Buffer buf) in
    ignore (Uutf.encode enc uchar);
    ignore (Uutf.encode enc `End);
    Buffer.length buf

  let asciify ic =
    let src = `Channel ic in
    let buf = Buffer.create 4096 in
    let dec =
      Uutf.decoder
        ~nln:(`ASCII (Uchar.of_char '\n'))
        ~encoding:`UTF_8 src
    in
    let rec translate () =
      match Uutf.decode dec with
      | `Uchar x as uchar ->
          let code = Uchar.to_int x in
          if code < 128 then
            (* Waste as little time as possible here since most characters
               are ascii and need no translation. *)
            Buffer.add_char buf (Char.chr code)
          else (
            let len = get_length uchar in
            let replacement_string =
              if Uucp.White.is_white_space x then
                String.make len ' '
              else
                String.make len 'Z'
            in
            Buffer.add_string buf replacement_string
          );
          translate ()
      | `End ->
          ()
      | `Malformed s ->
          (* Keep it and hope for the best. *)
          Buffer.add_string buf s;
          translate ()
      | `Await ->
          assert false
    in
    translate ();
    Buffer.contents buf
end
