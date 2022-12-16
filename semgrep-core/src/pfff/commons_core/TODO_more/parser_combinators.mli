(*s: parser_combinators.mli *)
(*****************************************************************************)
(* src: Jon Harrop.
 *
 * "Certain applications are extremely well suited to functional
 * programming and parsing is one of them. Specifically, the ability to
 * write functional combinators that allow parsers for everything from
 * integers up to symbolic expressions to be composed is more general
 * and provides more opportunity for code reuse than the use of
 * conventional parser generators such as ocamllex and ocamlyacc. This
 * article explains how parser combinators may be designed and
 * implemented in OCaml, using the standard example of a calculator."
 *
 * pad: a few bugfixes. I also put more restrictive and descriptive types.
 *
*)

(*****************************************************************************)

(* A generic parser takes a list of stuff (either char for lexical
 * parser or tokens for grammar parser) and return something and the
 * remaing list of stuff. *)
type ('a, 'b) genp = 'a list -> 'b * 'a list
val val_of_parser : 'b * 'a list -> 'b

(* lexer = parser of char list *)
(* type 'a lexer = (char, 'a) genp *)

(* grammer = parser ot tokens *)
(* type 'a pparser = (token, 'a) genp *)


val ( ||| ) : ('a, 'b) genp -> ('a, 'b) genp -> ('a, 'b) genp
(* ('a -> 'b) -> ('a -> 'b) -> 'a -> 'b *)
val ( +++ ) : ('a, 'b) genp -> ('a, 'c) genp -> ('a,   'b * 'c) genp
(* ('a -> 'b * 'c) -> ('c -> 'd * 'e) -> 'a -> ('b * 'd) * 'e *)

val many : ('a, 'b) genp -> ('a, 'b list) genp
(* ('a -> 'b * 'a) -> 'a -> 'b list * 'a *)

val ( >| ) : ('a, 'b) genp -> ('b -> 'c) -> ('a, 'c) genp
(* ('a -> 'b * 'c) -> ('b -> 'd) -> 'a -> 'd * 'c *)

(* was called 'some', but confusing *)
val pred : ('a -> bool) -> ('a, 'a) genp
(* ('a -> bool) -> 'a list -> 'a * 'a list *)

val a : 'a -> ('a, 'a) genp
(* 'a -> 'a list -> 'a * 'a list *)

val several : ('a -> bool) -> ('a, 'a list) genp
(* ('a -> bool) -> 'a list -> 'a list * 'a list *)


module Abstr : sig
  type t
  val x : t
end

val fin : ('a, Abstr.t) genp
(* 'a list -> Abstr.t * 'b list *)


val digit    : char -> bool
val alpha    : char -> bool
val symbol   : char -> bool
val alphanum : char -> bool
val space    : char -> bool

val alphanum_underscore : char -> bool
val alphanum_minus : char -> bool
val alphanum_under_minus : char -> bool

val collect : char * char list -> string
val list_of_string : string -> char list


(*****************************************************************************)
type token =
  | IDENT of string
  | KWD of string
  | INT of string
  | SYM of string
  | STR of string

val string_of_token : token -> string

type lexer = (char, token) genp

val rawident : lexer
(* char list -> token * char list *)
val rawnumber : lexer
(* char list -> token * char list *)

val rawsymbol : lexer

(* not space, not digit *)
val rawkeyword : lexer
(* char list -> token * char list *)

val rawstring : lexer

val lex_gen : lexer -> string -> token list

(*****************************************************************************)
val token : lexer
(* char list -> token * char list *)
val tokens : (char, token list) genp
(* char list -> token list * char list *)

val alltokens : (char, token list) genp
(* char list -> token list * 'a list *)

val lex : string -> token list


(*****************************************************************************)
(* cant use parser as it's a reseverd word *)
type 'a pparser = (token, 'a) genp

val ident : string pparser
(* token list -> string * token list *)
val int :  string pparser
(* token list -> string * token list *)
val string : string pparser

type expr =
  | Int of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr

val atom : expr pparser
(* token list -> expr * token list *)
val factor : expr pparser
(* token list -> expr * token list *)
val term : expr pparser
(* token list -> expr * token list *)
val expr : expr pparser
(* token list -> expr * 'a list *)

val parse : 'a pparser -> string -> 'a
(* (token list -> 'a * 'b) -> string -> 'a *)


(*****************************************************************************)

module Infix : sig
  val ( ||| ) : ('a, 'b) genp -> ('a, 'b) genp -> ('a, 'b) genp
  (* ('a -> 'b) -> ('a -> 'b) -> 'a -> 'b *)
  val ( +++ ) : ('a, 'b) genp -> ('a, 'c) genp -> ('a,   'b * 'c) genp
  (* ('a -> 'b * 'c) -> ('c -> 'd * 'e) -> 'a -> ('b * 'd) * 'e *)
  val ( >| ) : ('a, 'b) genp -> ('b -> 'c) -> ('a, 'c) genp
  (* ('a -> 'b * 'c) -> ('b -> 'd) -> 'a -> 'd * 'c *)
end

(*e: parser_combinators.mli *)
