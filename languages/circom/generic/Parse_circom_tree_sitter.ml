open Fpath_.Operators
module CST = Tree_sitter_circom.CST
module H = Parse_tree_sitter_helpers
module G = AST_generic
module H2 = AST_generic_helpers

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
type _env = unit H.env

let _token = H.token
let _str = H.str
let _fb = Tok.unsafe_fake_bracket

(*****************************************************************************)
(* Boilerplate converter *)
(*****************************************************************************)
(* TODO start by copying from semgrep-circom/lib/Boilerplate.ml *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let parse file =
  H.wrap_parser
    (fun () -> Tree_sitter_circom.Parse.file !!file)
    (fun _cst ->
      (* TODO *)
      [])

(* todo: special mode to convert Ellipsis in the right construct! *)
let parse_pattern str =
  H.wrap_parser
    (fun () -> Tree_sitter_circom.Parse.string str)
    (fun _cst ->
      (* TODO *)
      G.Ss [])
