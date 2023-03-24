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
  lazy_content : string Lazy.t;
}

type path = [ `Path of Common.filename ]
type block = [ `Block of block_info ]
type file = [ path | block ]

type 'file t = {
  file : 'file;
  xlang : Xlang.t;
  lazy_content : string lazy_t;
  (* This is valid only for xlang = Xlang.L ..., not for LRegex|LGeneric *)
  lazy_ast_and_errors :
    (AST_generic.program * Parse_info.token_location list) lazy_t;
}
