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

(* Use for square[], curly{} brackets *)
type 'a bracket = Tok.t * 'a * Tok.t [@@deriving show]

(*****************************************************************************)
(* Value *)
(*****************************************************************************)
type value =
  (* -------------- *)
  (* Literals *)
  (* -------------- *)
  (* for "null", "NULL", "Null", or also "~" *)
  | Null of Tok.t
  (* for "y" (or "n"), "Y", "yes", "true", "on" (or "off"), ... *)
  | Bool of bool wrap
  (* no integers, just float, like in JSON *)
  | Float of float option wrap
  (* alt: 'string wrap bracket'? But in YAML most strings do not require
   * an enclosing "" like in JSON (part of the appeal of YAML over JSON).
   * See the scalar_style ocaml-yaml type in the comment further above.
   *)
  | S of string wrap
  (* -------------- *)
  (* Composite *)
  (* -------------- *)
  (* A Sequence is an Array (YAML terminology).
   * In YAML sequences can be defined inline as in '[1, 2]' or using '-'
   * and indentation as in:
   *   - python
   *   - ruby
   *)
  | Sequence of value list bracket
  (* A Mapping is a Dict (YAML terminology).
   * alt: transform in '(value * value) list bracket' or even better in
   * '(string wrap * value) list bracket' and remove KV below? But with Semgrep
   *  we might want to have a Metavar or Ellipsis for the key, so we need
   *  at least '(value * value) list bracket'. Then we might also want a Metavar
   *  or Ellipsis for the whole entry (and with anchors it might be also more?) so
   *  we need 'value list' and the intermediate KV.
   * In YAML mappings can be defined inline as in '{foo: 1, bar: 2}' or using ':'
   * and indentation as in:
   *    foo: 1
   *    bar: 2
   *)
  | Mapping of value list bracket
  (* Key x Value. The key is usually a string (but it can be a Metavar too and
   * maybe also an anchor?).
   * This can only appear inside a Mapping.
   *)
  | KV of value * value
  (* ?? *)
  | OtherMapping of Tok.t * Tok.t
  (* ------------------------------------ *)
  (* Advanced YAML constructs not in JSON *)
  (* ------------------------------------ *)
  | Alias of string wrap * value (* alias value defined before *)
  (* A tag is a sort of type annotation e.g. '!number 42' tags the value
   * '42' with the tag 'number'.
   *)
  | Tag of string (* without the ! *) wrap * value
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
