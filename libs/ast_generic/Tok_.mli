(*
   The token type Parse_info.t + derived interfaces suitable for the generic AST.

   The equal and hash functions are actually not automatically derived; Tok_.ml
   provides customized behavior where we assume all tokens are equal. This isn't
   always what we want, so this is why they're not provided by Tok.t

   Used by more than one module (AST_generic and Raw_tree).
*)
type t = Parse_info.t [@@deriving show, eq, hash]
