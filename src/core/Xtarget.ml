(* eXtended target.
 *
 * This type is mostly used in the engine, to pass around extra information
 * associated to each target.
 *
 * related: Input_to_core.target, which is what is passed
 * to semgrep-core via -target.
 *)

type block_info = {
  orig_file : Common.filename;
  orig_loc : Parse_info.token_location;
  block_lazy_content : string lazy_t;
}

type path = [ `Path of Common.filename ]
type block = [ `Block of block_info ]
type file = [ path | block ]

type 'file xtarget = {
  file : 'file;
  xlang : Xlang.t;
  lazy_content : string lazy_t;
  (* This is valid only for xlang = Xlang.L ..., not for LRegex|LGeneric *)
  lazy_ast_and_errors :
    (AST_generic.program * Parse_info.token_location list) lazy_t;
}

type input = Common.filename xtarget
type t = file xtarget

let cast (xtarget : input) : t =
  let file = xtarget.file in
  { xtarget with file = `Path file }

let path_of_file file =
  match file with
  | `Path path -> path
  | `Block block -> block.orig_file

let path (xtarget : t) = path_of_file xtarget.file
