
(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Parser Combinators *)
(*****************************************************************************)

(* src: Jon Harrop. pad: a few bugfix *)

type ('a, 'b) genp = 'a list -> 'b * 'a list
let val_of_parser = fst

(* lexer = parser of char list *)
(* type 'a lexer = (char, 'a) genp *)

(* grammer = parser ot tokens *)
(* type 'a p = (token, 'a) genp *)


(* pad: could also do it by returning a Maybe and use monad *)
let ( ||| ) p1 p2 s =
  try
    p1 s
  with Not_found ->
    p2 s

let ( +++ ) p1 p2 s =
  let e1, s = p1 s in
  let e2, s = p2 s in
  (e1, e2), s

let rec many p s =
  try
    let e, s = p s in
    let es, s = many p s in
    e::es, s
  with Not_found ->
    [], s


let ( >| ) p k i =
  let e, s = p i in
  k e, s

(* was called 'some', but confusing *)
let pred p = function
  | h::t when p h -> h, t
  | _ -> raise Not_found

let a x = pred (( = ) x)

let several p = many (pred p)


module Abstr : sig
  type t
  val x : t
end = struct
  type t = int
  let x = 0
end

let fin = function
  | [] as t -> Abstr.x, t
  | _ -> raise Not_found


(*****************************************************************************)
(* Lexing *)
(*****************************************************************************)
(* a generic lexer *)

let digit = function
  | '0'..'9' -> true
  | _ -> false

let alpha = function
  | 'a'..'z' | 'A'..'Z' -> true
  | _ -> false


let symbol = function
  | '(' | ')'
  | '{' | '}'
  | '[' | ']'
  | '<' | '>'
  | '+' | '-' | '*' | '/'
  | '&' | '|' | '!'

  | '=' | '~' | '@'
    -> true
  | _ -> false

let space = function
  | ' ' | '\t' | '\n' -> true
  | _ -> false

let stringquote = function
  | '"' -> true
  | _ -> false

let quote = function
  | '\'' -> true
  | _ -> false


let alphanum c = digit c || alpha c


let alphanum_underscore c = digit c || alpha c || (c = '_')
let alphanum_minus c = digit c || alpha c || (c = '-')
let alphanum_under_minus c = digit c || alpha c || (c = '-') || (c = '_')



let (|>) o f = f o
let string_of_chars cs =
  cs |> List.map (String.make 1) |> String.concat ""


let collect(h, t) =
  String.concat "" (List.map (String.make 1) (h::t))

let collectbis(xs) =
  String.concat "" (List.map (String.make 1) (xs))

let list_of_string string =
  let list = ref [] in
  String.iter (fun c -> list := c :: !list) string;
  List.rev !list


(*****************************************************************************)
(* still generic *)
(*****************************************************************************)

type token =
  | IDENT of string
  | KWD of string
  | INT of string
  | SYM of string
  | STR of string

let string_of_token = function
  | IDENT string -> "IDENT:" ^ string
  | KWD string -> "KWD:" ^ string
  | INT string -> "INT:" ^ string
  | SYM string -> "SYM:" ^ string
  | STR string -> "STR:" ^ string


type lexer = (char, token) genp


let rawnumber =
  pred digit +++ several digit >| fun x -> INT(collect x)
let rawident =
  pred alpha +++ several alphanum >| fun x -> IDENT(collect x)
let rawsymbol =
  pred symbol +++ several symbol >| fun x -> SYM(collect x)

let rawkeyword =
  let p c = not(space c) && not(digit c) in
  pred p +++ several p >| fun x -> KWD(collect x)


(* todo: handle antislash *)
let rawstring =
  pred stringquote +++
  several (fun c -> not (stringquote c)) +++
  pred stringquote
  >| (fun ((c1, cs), c3) ->
    let s = string_of_chars cs in
    STR s (* exclude the marker *)
  )


let lex_gen tokenf str =
  let alltoks = (many tokenf) +++ fin >| fst in
  val_of_parser (alltoks (list_of_string str))

let parse_gen tokenf grammarf p string =
  val_of_parser (grammarf (lex_gen tokenf string))

(*****************************************************************************)
(* not generic anymore *)
(*****************************************************************************)
(* the order is important if some "rules" overlap, as in ocamllex *)
let token =
  (rawident ||| rawnumber ||| rawkeyword) +++ several space >| fst

(* pad: bugfix: was not defined in jon harrop article *)
let tokens = many token

let alltokens =
  tokens +++ fin >| fst

let lex (string : string) =
  val_of_parser (alltokens (list_of_string string))


let test1 () =
  assert
    (lex "a x^2 + b x + c"
     =
     [IDENT "a"; IDENT "x"; KWD "^"; INT "2"; KWD "+"; IDENT "b"; IDENT "x";
      KWD "+"; IDENT "c"]
    )

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

type expr =
  | Int of int
  | Var of string
  | Add of expr * expr
  | Mul of expr * expr

type 'a pparser = (token, 'a) genp

(*
open Format;;
# let rec print_expr ff = function
    | Int n -> fprintf ff "%d" n
    | Var x -> fprintf ff "%s" x
    | Add(f, g) ->
        fprintf ff "%a + %a" print_expr f print_expr g
    | Mul(f, g) ->
        fprintf ff "%a %a" print_mul f print_mul g
  and print_mul ff = function
    | Add _ as e -> fprintf ff "(%a)" print_expr e
    | e -> fprintf ff "%a" print_expr e
#install_printer print_expr
*)

let ident = function
  | IDENT x :: t -> x, t
  | _ -> raise Not_found

let int = function
  | INT n :: t -> n, t
  | _ -> raise Not_found

let string = function
  | STR x :: t -> x, t
  | _ -> raise Not_found

(* src: Jon Harrop
 * "This style of parsing, known as recursive descent parsing , has one
 * important caveat. If a rule tries to match itself immediately, even if
 * that is succeeded by other parsers, then the resulting program will go
 * into an infinite loops with the parser for that rule calling itself
 * indefinitely until a stack overflow occurs. Consequently, our
 * implementation of the factor parser is careful to parse an atom first,
 * and term calls factor first, to avoid this problem."
 *
 * pad: bugfix, added the KWD "*".
*)

(* pad: I think I remembered you cant eta-factorize the parameter
 * when you use mutually recursive
*)
let rec atom s =
  (
    (int             >| fun n -> Int(int_of_string n))
    |||
    (ident           >| fun x -> Var x)
    |||
    (a (KWD "(") +++ term +++ a (KWD ")")     >| fun ((_, e), _) -> e)
  ) s
and factor s =
  (
    (atom +++ a (KWD "*") +++ factor      >| fun ((f, _), g) -> Mul (f,g))
    |||
    atom
  ) s
and term s =
  (
    (factor +++ a (KWD "+") +++ term     >| fun ((f, _), g) -> Add (f,g))
    |||
    factor
  ) s


let expr =
  term +++ fin >| fst

let parse p string =
  val_of_parser(p(lex string))

(*
parse expr "a x x + b x + c"
*)


(*****************************************************************************)

module Infix = struct
  let (|||) = (|||)
  let (+++) = (+++)
  let (>|) = (>|)
end
