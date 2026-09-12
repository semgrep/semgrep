(*
   Tree-sitter grammar specification types.
   See <https://raw.githubusercontent.com/tree-sitter/tree-sitter/refs/tags/v0.26.3/docs/src/assets/schemas/grammar.schema.json>
*)

type ident = string

type prec_value =
  | Num of int
  | Named of string
[@@deriving yojson]

type named_prec_level =
  | Prec_symbol of ident
  | Prec_string of string
[@@deriving yojson]

type prec =
  | Default
  | Left
  | Right
  | Dynamic
[@@deriving ord]

module rec Alias : sig
  type t = {
    value: string;
    named: bool;
    content: Rule_body.t;
    must_be_preserved: bool;
  }
  [@@deriving ord, yojson]
end

and Reserved : sig
  type t = { context_name: string; content: Rule_body.t; }
  [@@deriving ord, yojson]
end

and Rule_body : sig
  type t =
    | Symbol of ident
    | Literal of string
    | Pattern of { value: string; flags: string option }
    | Blank
    | Repeat of t
    | Repeat1 of t
    | Choice of t list
    | Seq of t list
    | Prec of prec * prec_value * t
    | Alias of Alias.t
    | Field of ident * t
    | Immediate_token of t
    | Token of t
    | Reserved of Reserved.t
  [@@deriving ord, yojson]
end

type grammar = {
  name: ident;
  inherits: ident option;
  word: ident option;
  extras: Rule_body.t list;
  inline: ident list;
  conflicts: ident list list;
  precedences: named_prec_level list list;
  externals: Rule_body.t list;
  supertypes: ident list;
  reserved: (ident * Rule_body.t list) list;
  rules: (ident * Rule_body.t) list;
}
[@@deriving yojson]
