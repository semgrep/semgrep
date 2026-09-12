(*
   Error message formatting.
*)

(*
   The goal of this error_class object is to classify errors.
   It is applicable to ERROR nodes found in the tree-sitter output.

   We could return all the left siblings if we're in a tuple (seq())
   but it's not useful if we're in a list (repeat()), and there's no
   way to distinguish them at this stage (this info is recovered
   later, after the ERROR nodes are removed).

   We hope that this whole object is good enough to identify a class
   of errors, which will be used to determine which errors are more
   common or more impactful than others.
*)
type error_class = {
  parent: Tree_sitter_bindings.Tree_sitter_output_t.node_kind;
  left_sibling: Tree_sitter_bindings.Tree_sitter_output_t.node_kind option;
  first_child: Tree_sitter_bindings.Tree_sitter_output_t.node_kind option;
}

(*
   A printable string representation of the error class, without newlines.
*)
val string_of_error_class : error_class -> string

type t = {
  kind: Tree_sitter_error_t.error_kind;
  msg: string;

  (*
     Provides the path to the source file if applicable.
  *)
  file: Src_file.info;

  (*
     Beginning and end of the error node reported by tree-sitter.
     The end position is inclusive, i.e. it's the position of the last
     character of the selected region.
  *)
  start_pos: Tree_sitter_bindings.Tree_sitter_output_t.position;
  end_pos: Tree_sitter_bindings.Tree_sitter_output_t.position;

  (*
     The region of the code (tree-sitter error node) reported as an error.
     This could almost be extracted from the snippet field, but here we
     guarantee no ellipsis.
  *)
  substring: string;

  (*
     A structured snippet of code extracted from the source input,
     including lines of context, regions to be highlighted, and ellipses
     if a string is too long. See Snippet.format for rendering
     to a terminal or as plain text.
  *)
  snippet: Snippet.t;

  (*
     Classifies the error. Internal details are subject to change.
  *)
  error_class: error_class option;
}

(* Fatal parsing error. *)
exception Error of t

(*
   Create an error object from a node considered to be the cause of
   a parsing error.
*)
val create :
  Tree_sitter_error_t.error_kind ->
  Src_file.t ->
  ?parent:Tree_sitter_bindings.Tree_sitter_output_t.node ->
  Tree_sitter_bindings.Tree_sitter_output_t.node ->
  string -> t

(*
   Fail, raising an Internal_error exception.
   The string argument is an arbitrary message to be printed as part of
   an error message.
*)
val internal_error :
  Src_file.t ->
  Tree_sitter_bindings.Tree_sitter_output_t.node ->
  string -> 'a

(*
   Format an error message. Highlight the error according to the value
   of 'style' whose default is 'Auto' (see the Snippet module for details).
*)
val to_string : ?style:Snippet.style -> t -> string

(*
   Append errors to error log in json format, one object per line.

   The specific json format is unspecified and might change. This is for
   internal use within ocaml-tree-sitter.
*)
val log_json_errors : string -> t list -> unit
