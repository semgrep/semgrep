(*
   Copyright (c) 2021-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
val hook_preprocess_source : (path:Fpath.t -> string -> string) option Hook.t
(** Optional source-text rewrite applied to the raw file contents before
    tree-sitter parses. [path] is the file the contents were read from
    (for diagnostics or resolving file-relative references); the rewrite
    itself receives and returns the full source text. Default is [None]
    (identity). Preserves character offsets so [Tok.t] positions stay
    accurate — callers that overwrite ranges should substitute in place
    rather than deleting text. *)

val parse : Fpath.t -> (Ast_cpp.program, unit) Tree_sitter_run.Parsing_result.t

val parse_pattern :
  string -> (Ast_cpp.any, unit) Tree_sitter_run.Parsing_result.t
