(* Yoann Padioleau
 *
 * Copyright (C) 2025 Semgrep Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* An Abstract Syntax Tree (AST) for Yaml.
 *
 * There are already YAML parsers for OCaml (see below) with their own ASTs, but
 * those ASTs do not contain location information which is useful for error
 * reporting and also for Semgrep matching (Semgrep relies on "ranges" computed
 * from token locations during matching).
 *
 * alternatives:
 *  - opam yaml package: https://github.com/avsm/ocaml-yaml
 *    which provides some location information but with a separate
 *    "stream" API that is a bit complex to use (we internally rely on it
 *    to generate the AST in this file though).
 *    The original "ASTs" of the yaml library are defined as:
 *       type value =
 *         [ `Null
 *         | `Bool of bool
 *         | `Float of float
 *         | `String of string
 *         | `A of value list
 *         | `O of (string * value) list ]
 *     which is also compatible with the JSON type of the ezjsonm library
 *     (see libs/commons/JSON.ml)
 *     or the one representing also anchors:
 *        type yaml =
 *          [ `Scalar of scalar | `Alias of string | `A of sequence | `O of mapping ]
 *        and scalar = {
 *          anchor : string option;
 *          tag : string option;
 *          value : string;
 *          plain_implicit : bool;
 *          quoted_implicit : bool;
 *          style : scalar_style;
 *        }
 *        and sequence = {
 *          s_anchor : string option;
 *          s_tag : string option;
 *          s_implicit : bool;
 *          s_members : yaml list;
 *        }
 *        and mapping = {
 *          m_anchor : string option;
 *          m_tag : string option;
 *          m_implicit : bool;
 *          m_members : (yaml * yaml) list;
 *        }
 *        and scalar_style =
 *         [`Any |`Plain |`Single_quoted |`Double_quoted |`Literal |`Folded ]
 *
 *  - camlyaml https://github.com/Kakadu/camlyaml
 *)

(*****************************************************************************)
(* Leaf (tokens) *)
(*****************************************************************************)

(* a shortcut to annotate some information with position information *)
type 'a wrap = 'a * Tok.t [@@deriving show]

(* Use for square[], double[[]], curly{}, and strings''"" brackets *)
type 'a bracket = Tok.t * 'a * Tok.t [@@deriving show]

(*****************************************************************************)
(* Value *)
(*****************************************************************************)
type value =
  (* -------------- *)
  (* Literals *)
  (* -------------- *)
  | Null of Tok.t
  | Bool of bool wrap
  (* TODO? bracket? *)
  | S of string wrap
  (* no integers, just float, like in JSON *)
  | Float of float option wrap
  (* -------------- *)
  (* Composite *)
  (* -------------- *)
  (* A Sequence is an Array (YAML terminology) *)
  | Sequence of value list bracket
  (* A Mapping is a Dict (YAML terminology)
   * The key is usually a String but with Semgrep it can be an Ellipsis or
   * Metavar (and with anchors it might be also more?)
   * TODO: transform in (value * value) list bracket and remove KV (KeyValue)
   *)
  | Mapping of value list bracket
  | KV of (value * value) bracket
  (* ------------------------------------ *)
  (* Advanced YAML constructs not in JSON *)
  (* ------------------------------------ *)
  | Alias of string wrap * value (* alias value defined before *)
  (* A tag is a sort of type annotation e.g. '!number 42' tags the value
   * '42' with the tag 'number'.
   *)
  | Tag of string (* without the ! *) wrap * value
  (* ?? *)
  | OtherMapping of Tok.t * Tok.t
  (* -------------- *)
  (* Semgrep constructs *)
  (* -------------- *)
  | Metavar of string wrap
  | Ellipsis of Tok.t
[@@deriving show { with_path = false }]

(*****************************************************************************)
(* Document *)
(*****************************************************************************)

type document = value list [@@deriving show]

(*****************************************************************************)
(* Any *)
(*****************************************************************************)
type any = Doc of document | V of value
[@@deriving show { with_path = false }]
