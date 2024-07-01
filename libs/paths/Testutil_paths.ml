open Printf
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* TODO? seems very similar to Testutil_files.ml in libs/commons, we
 * should factorize and get rid of one.
 *)
type file_tree = Dir of string * file_tree list | File of string * file_kind
and file_kind = Regular of string | Symlink of string

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let write_file path data =
  let oc = open_out_bin !!path in
  output_string oc data;
  close_out oc

let rec create_files parent (x : file_tree) =
  match x with
  | Dir (name, files) ->
      let dir_path = parent / name in
      Unix.mkdir !!dir_path 0o777;
      List.iter (create_files dir_path) files
  | File (name, Regular data) ->
      let path = parent / name in
      write_file path data
  | File (name, Symlink target_path) ->
      let link_path = parent / name in
      Unix.symlink target_path !!link_path

let rec delete_files parent (x : file_tree) =
  match x with
  | Dir (name, files) ->
      let dir_path = parent / name in
      List.iter (delete_files dir_path) files;
      Sys.rmdir !!dir_path
  | File (name, _) -> Sys.remove !!(parent / name)

(*****************************************************************************)
(* API *)
(*****************************************************************************)

(*
   Create a temporary file tree as specified. The user-specified function
   takes the root folder as argument and can assume that all the files
   were created according to the specification.
   Files are deleted automatically.
*)
let with_file_trees trees func =
  let workspace =
    UTmp.get_temp_dir_name ()
    (* This is meant only to be used in test code. *)
    (* nosemgrep: forbid-random *)
    / sprintf "test-list_files-%i" (Random.bits ())
  in
  Unix.mkdir !!workspace 0o777;
  Common.protect
    ~finally:(fun () ->
      try Sys.rmdir !!workspace with
      | _ -> ())
    (fun () ->
      List.iter (create_files workspace) trees;
      Common.protect
        ~finally:(fun () -> List.iter (delete_files workspace) trees)
        (fun () -> func workspace))

(* Convenience function for cases where only one file tree is desired in the
 * root directory. *)
let with_file_tree tree func = with_file_trees [ tree ] func
