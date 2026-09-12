(*
   Generic utilities not provided by OCaml.
*)

(*
   [copied from pfff/commons/Common.ml]

   This implementation works even with Linux files like /dev/fd/63
   created by bash's process substitution e.g.

     my-ocaml-program <(echo contents)

   See https://www.gnu.org/software/bash/manual/html_node/Process-Substitution.html

   In bash, '<(echo contents)' is replaced by something like
   '/dev/fd/63' which is a special file of apparent size 0 (as
   reported by `Unix.stat`) but contains data (here,
   "contents\n"). So we can't use 'Unix.stat' or 'in_channel_length'
   to obtain the length of the file contents. Instead, we read the file
   chunk by chunk until there's nothing left to read.

   Why such a function is not provided by the ocaml standard library is
   unclear.
*)

(* copied from `OSS/libs/commons/UFile.ml` *)
(* Temporary files created using Python's [tempfile.NamedTemporaryFiles] on
    Windows enables the [FILE_SHARE_DELETE] sharing mode. Files that have open
    handles with the [FILE_SHARE_DELETE] sharing mode can only be re-opened in
    that mode. To make sure we won't run into problems opening the file, we
    add the [O_SHARE_DELETE] flag when opening all files. *)
let win_safe_open_in_bin file : Unix.file_descr =
  Unix.openfile file [ O_CREAT; O_RDONLY; O_SHARE_DELETE ] 0o666

let read_file path =
  let buf_len = 4096 in
  let extbuf = Buffer.create 4096 in
  let buf = Bytes.create buf_len in
  let rec loop fd =
    match Unix.read fd buf 0 buf_len with
    | 0 -> Buffer.contents extbuf
    | num_bytes ->
        assert (num_bytes > 0);
        assert (num_bytes <= buf_len);
        Buffer.add_subbytes extbuf buf 0 num_bytes;
        loop fd
  in
  let fd = win_safe_open_in_bin path in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () -> loop fd)
