(* Yoann Padioleau
 *
 * Copyright (c) 2023 r2c
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* AST for Terraform/HCL.
 *
 * In theory, HCL is a general format (a bit like YAML or JSON) that could
 * be used not only for Terraform. In practice, only Terraform uses HCL
 * and all the semgrep rules people are writing are not about HCL
 * but about Terraform, hence the focus on Terraform here.
 *
 * I tried to keep the names used in the original documentation for
 * the AST constructs (e.g., block, labels, argument), even though
 * we use different names for similar concepts in the generic AST.

 * See https://developer.hashicorp.com/terraform/language for more information
 * on Terraform.
 * See https://github.com/hashicorp/hcl/blob/main/hclsyntax/spec.md for more
 * information on HCL itself.
 *)

(*****************************************************************************)
(* Names *)
(*****************************************************************************)

(* ------------------------------------------------------------------------- *)
(* Token/info *)
(* ------------------------------------------------------------------------- *)
type tok = Tok.t [@@deriving show]

(* a shortcut to annotate some information with token/position information *)
type 'a wrap = 'a * tok [@@deriving show]

(* round(), square[], curly{}, angle<> brackets *)
type 'a bracket = tok * 'a * tok [@@deriving show]

(* ------------------------------------------------------------------------- *)
(* Names  *)
(* ------------------------------------------------------------------------- *)
type ident = string wrap

(*****************************************************************************)
(* Expressions *)
(*****************************************************************************)
type expr = AST_generic.expr

(*****************************************************************************)
(* Arguments *)
(*****************************************************************************)

(* They use the name 'argument' for something that looks like a field.
 * Actually in the HCL spec they call this an 'attribute', which is still
 * a bad name (we use attributes for annotations in the generic AST), but
 * still better than argument.
 *)
type argument = ident * tok (* '=' *) * expr

(*****************************************************************************)
(* Meta arguments *)
(*****************************************************************************)
(* TODO: count, for_each, provider, lifecycle, depends_on *)

(*****************************************************************************)
(* Blocks *)
(*****************************************************************************)

type block = {
  btype : block_type wrap;
  (* The number of block_labels depend on the block_type:
   * Resource -> 2,  Module -> 1, etc.
   * The "arguments" in the body depends also on the block type:
   * Module -> source, version,  ... Variable -> type, default, etc.
   * TODO we should change the type to add the label arity in block_type below
   * and also handle encode their meaning
   * (e.g., Resource of {
   *   resource_kind: block_label;
   *   resource_name: block_label;
   *   ...})
   *)
  blabels : block_label list;
  bbody : block_body;
}

(* the same order than in the documentation table of contents *)
and block_type =
  | Resource
  | Data
  | Provider
  | Variable (* input variable *)
  | Output
  | Locals
  | Module
  | Terraform
  | OtherBlockType of string

and block_label =
  | LblStr of string wrap
  (* TODO bracket? *)
  | LblId of ident

and block_body = block_body_element list bracket

and block_body_element =
  | Argument of argument
  | Block of block
  (* semgrep-ext: *)
  | BlockEllipsis of tok

(*****************************************************************************)
(* Toplevel elements *)
(*****************************************************************************)
(* like block_body_element but Argument should not appear at the toplevel *)
type toplevel_block = block_body_element
type config = toplevel_block list

(* Note that a "module" is a set of terraform files in the same directory *)

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
type any = E of expr | Pr of config

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let block_type_of_string s =
  match s with
  | "resource" -> Resource
  | "data" -> Data
  | "provider" -> Provider
  | "variable" -> Variable
  | "output" -> Output
  | "locals" -> Locals
  | "module" -> Module
  | "terraform" -> Terraform
  | _else_ -> OtherBlockType s
