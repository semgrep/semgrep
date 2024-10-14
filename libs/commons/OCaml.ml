(*
 * Yoann Padioleau
 *
 * Copyright (C) 2009-2012 Facebook
 *
 * Most of the code in this file was inspired by code by Gazagnaire.
 * Here is the original copyright:
 *
 * Copyright (c) 2009 Thomas Gazagnaire <thomas@gazagnaire.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(*
 * OCaml hacks to support reflection.
 *
 * OCaml does not support reflection, and it's a good thing: we love
 * strong type-checking that forbids too clever hacks like 'eval', or
 * run-time reflection; it's too much power for you, you will misuse
 * it. At the same time it's sometimes useful. So at least we could make
 * it possible to still reflect on the type definitions or values in
 * OCaml source code. We can do it by processing ML source code and
 * emitting ML source code containing under the form of regular ML
 * value or functions meta-information about information in other
 * source code files. It's a little bit a poor's man reflection mechanism,
 * because it's more manual, but it's for the best. Metaprogramming had
 * to be painful, because it is dangerous!
 *
 * Example:
 *
 *      TODO
 *
 * In some sense we reimplement what is in the OCaml compiler, which
 * contains the full AST of OCaml source code. But the OCaml compiler
 * and its AST are too big, too scary for many tasks that would be satisfied
 * by a restricted but simpler AST.
 *
 * Camlp4 is obviously also a solution to this problem, but it has a
 * learning curve, and it's a slightly different world than the pure
 * regular OCaml world. So this module, and ocamltarzan together can
 * reduce the problem by taking the best of camlp4, while still
 * avoiding it.
 *
 * The support is partial. We support only the OCaml constructions
 * we found the most useful for programming stuff like
 * stub generators.
 *
 * less? not all OCaml so call it miniml.ml  ? or reflection.ml ?
 *
 *
 * Notes: 2 worlds
 *   - the type level world,
 *   - the data level world
 *
 * Then there is whether the code is generated on the fly, or output somewhere
 * to be compiled and linked again (so 2 steps process, more manual, but
 * arguably less complicated magic)
 *
 * different level of (meta)programming:
 *
 *  - programming in OCaml on OCaml values (classic)
 *  - programming in OCaml on Sexp.t value of value
 *  - programming in OCaml on Sexp.t value of type description
 *  - programming in OCaml on OCaml.v value of value
 *  - programming in OCaml on OCaml.t value of type description
 *
 * Depending on what you have to do, some levels are more suited than other.
 * For instance to do a show, to pretty print value, then sexp is good,
 * because really you just want to write code that handle 2 cases,
 * atoms and list. That's really what pretty printing is all about. You
 * could write a pretty printer for OCaml.v, but it will need to handle
 * 10 cases. Now if you want to write a code generator for python, or an ORM,
 * then OCaml.v is better than sexp, because in sexp you lost some valuable
 * information (that you may have to reverse engineer, like whether
 * a Sexp.List corresponds to a field, or a sum, or wether something is
 * null or an empty list, or wether it's an int or float, etc).
 *
 * Another way to do (meta)programming is:
 *  - programming in Camlp4 on OCaml ast
 *  - writing camlmix code to generate code.
 *
 * notes:
 *  - sexp value or sexp of type description, not as precise, but easier to
 *    write really generic code that do not need to have more information
 *    about the sexp nodes (such as wether it's a field, a constuctor, etc)
 *  - miniml value or type, not as precise that the regular type,
 *    but more precise than sexp, and allow write some generic code.
 *  - ocaml value (not type as you cant program at type level),
 *    precise type checking, but can be tedious to write generic
 *    code like generic visitors or pickler/unpicklers
 *
 * This file is working with ocamltarzan/pa/pa_type.ml (and so indirectly
 * it is working with camlp4).
 *
 * Note that can even generate sexp_of_x for miniML :) really
 * reflexive tower here
 *
 * Note that even if this module helps a programmer to avoid
 * using directly camlp4 to auto generate some code, it can
 * not solve all the tasks.
 *
 * history:
 *  - Thought about it when wanting to do the ast_php.ml to be
 *    transformed into a .adsl declaration to be able to generate
 *    corresponding python classes using astgen.py.
 *  - Thought about a miniMLType and miniMLValue, and then realize
 *    that that was maybe what code in the ocaml-orm-sqlite
 *    was doing (type-of et value-of), except I wanted the
 *    ocamltarzan style of meta-programming instead of the camlp4 one.
 *
 *
 * Alternatives:
 *  - camlp4
 *    obviously camlp4 has access to the full AST of OCaml, but
 *    that is one pb, that's too much. We often want only to do
 *    analysis on the type
 *  - type-conv
 *    good, but force to use camlp4. Can use the generic sexplib
 *    and then work on the generated sexp, but as explained below,
 *    is will be on the value.
 *  - use lib-sexp (just the sexp library part, not the camlp4 support part)
 *    but not enough info. Even if usually
 *    can reverse engineer the sexp to rediscover the type,
 *    you will reverse engineer a value; what you want
 *    is the sexp representation of the type! not a value of this type.
 *    Also lib-sexp autogenerated code can be hard to understand, especially
 *    if the type definition is complex. A good side effect of ocaml.ml
 *    is that it provides an intermediate step :) So even if you
 *    could pretty print value from your def to sexp directly, you could
 *    also use transform your value into a OCaml.v, then use
 *    the somehow more readable function that translate a v into a sexp,
 *    and same when wanting to read a value from a sexp, by using
 *    again OCaml.v as an intermediate. It's nevertheless obviously
 *    less efficient.
 *
 *  - zephyr, or thrift ?
 *  - F# ?
 *  - Lisp/Scheme ?
 *  - .Net interoperability
 *
 * UPDATE: only the 'v' type remains. For the 't' type see metagen/
 * or prefer to use otarzan and the generic ASC in semgrep.
 *
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* OCaml values (a restricted form of expressions) *)
type v =
  | VUnit
  | VBool of bool
  | VFloat of float
  | VInt of int64
  | VChar of char
  | VString of string
  | VTuple of v list
  | VDict of (string * v) list
  | VSum of string * v list
  | VVar of (string * int64)
  | VArrow of string
  (* special cases *)
  | VNone
  | VSome of v
  | VList of v list
  | VRef of v
  (*
  | VEnum of v list (* ??? *)
  | VRec of (string * int64) * v
  | VExt of (string * int64) * v
*)
  | VTODO of string
(* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* for generated code that want to transform and in and out of a v or t *)
let vof_unit () = VUnit
let vof_int x = VInt (Int64.of_int x)
let vof_int64 x = VInt x
let vof_float x = VFloat x
let vof_string x = VString x
let vof_bool b = VBool b
let vof_list ofa x = VList (List_.map ofa x)

let vof_option ofa x =
  match x with
  | None -> VNone
  | Some x -> VSome (ofa x)

let vof_ref ofa x =
  match x with
  | { contents = x } -> VRef (ofa x)

let vof_either _of_a _of_b = function
  | Either.Left v1 ->
      let v1 = _of_a v1 in
      VSum ("Left", [ v1 ])
  | Either.Right v1 ->
      let v1 = _of_b v1 in
      VSum ("Right", [ v1 ])

let vof_either3 _of_a _of_b _of_c = function
  | Either_.Left3 v1 ->
      let v1 = _of_a v1 in
      VSum ("Left3", [ v1 ])
  | Either_.Middle3 v1 ->
      let v1 = _of_b v1 in
      VSum ("Middle3", [ v1 ])
  | Either_.Right3 v1 ->
      let v1 = _of_c v1 in
      VSum ("Right3", [ v1 ])

let vof_all3 of_a of_b of_c (a, b, c) = VTuple [ of_a a; of_b b; of_c c ]

let int_ofv = function
  | VInt x -> x
  | _ -> failwith "ofv: was expecting a VInt"

let float_ofv = function
  | VFloat x -> x
  | _ -> failwith "ofv: was expecting a VFloat"

let string_ofv = function
  | VString x -> x
  | _ -> failwith "ofv: was expecting a VString"

let unit_ofv = function
  | VUnit -> ()
  | _ -> failwith "ofv: was expecting a VUnit"

let list_ofv a__of_sexp sexp =
  match sexp with
  | VList lst ->
      let rev_lst = List.rev_map a__of_sexp lst in
      List.rev rev_lst
  | _ -> failwith "list_ofv: VLlist needed"

let option_ofv a__of_sexp sexp =
  match sexp with
  | VNone -> None
  | VSome x -> Some (a__of_sexp x)
  | _ -> failwith "option_ofv: VNone or VSome needed"

(*****************************************************************************)
(* Format pretty printers *)
(*****************************************************************************)
let add_sep xs =
  xs |> List_.map (fun x -> Either.Right x) |> List_.join_gen (Either.Left ())

(*
 * OCaml value pretty printer. A similar functionnality is provided by
 * the OCaml toplevel interpreter ('/usr/bin/ocaml') but
 * sometimes it is useful to print values from a regular command
 * line program. You don't always want to run the ocaml interpreter (or
 * customized interpreter built by ocamlmktop), and type an expression
 * in to get the printed value.
 *
 * The v_of_xxx generated code by ocamltarzan is
 * the first part to make this possible. The function below
 * is the second part.
 *
 * The '@[', '@,', etc are Format printf tags. See the doc of the Format
 * module in the OCaml manual to understand their meaning. Mainly,
 * @[ and @] open and close a pretty print box, and '@ ' and '@,'
 * are to give breaking hints to the pretty printer.
 *
 * The output can be copy pasted in ML code directly, which can be
 * useful when you want to pattern match over complex ocaml value.
 *)

let string_of_v ?(max_depth = max_int) v =
  Fmt_.with_buffer_to_string (fun ppf ->
      let rec aux max_depth v =
        if max_depth <= 0 then Format.fprintf ppf "..."
        else
          match v with
          | VUnit -> Format.fprintf ppf "()"
          | VBool v1 ->
              if v1 then Format.fprintf ppf "true"
              else Format.fprintf ppf "false"
          | VFloat v1 -> Format.fprintf ppf "%f" v1
          | VChar v1 -> Format.fprintf ppf "'%c'" v1
          | VString v1 -> Format.fprintf ppf "\"%s\"" v1
          | VInt i -> Format.fprintf ppf "%Ld" i
          | VTuple xs ->
              Format.fprintf ppf "(@[";
              xs |> add_sep
              |> List.iter (function
                   | Either.Left _ -> Format.fprintf ppf ",@ "
                   | Either.Right v -> aux (max_depth - 1) v);
              Format.fprintf ppf "@])"
          | VDict xs ->
              Format.fprintf ppf "{@[";
              xs
              |> List.iter (fun (s, v) ->
                     (* less: could open a box there too? *)
                     Format.fprintf ppf "@,%s=" s;
                     aux (max_depth - 1) v;
                     Format.fprintf ppf ";@ ");
              Format.fprintf ppf "@]}"
          | VSum (s, xs) -> (
              match xs with
              | [] -> Format.fprintf ppf "%s" s
              | _y :: _ys ->
                  Format.fprintf ppf "@[<hov 2>%s(@," s;
                  xs |> add_sep
                  |> List.iter (function
                       | Either.Left _ -> Format.fprintf ppf ",@ "
                       | Either.Right v -> aux (max_depth - 1) v);
                  Format.fprintf ppf "@])")
          | VVar (s, i64) -> Format.fprintf ppf "%s_%Ld" s i64
          | VArrow _v1 -> failwith "Arrow TODO"
          | VNone -> Format.fprintf ppf "None"
          | VSome v ->
              Format.fprintf ppf "Some(@[";
              aux (max_depth - 1) v;
              Format.fprintf ppf "@])"
          | VRef v ->
              Format.fprintf ppf "Ref(@[";
              aux (max_depth - 1) v;
              Format.fprintf ppf "@])"
          | VList xs ->
              Format.fprintf ppf "[@[<hov>";
              xs |> add_sep
              |> List.iter (function
                   | Either.Left _ -> Format.fprintf ppf ";@ "
                   | Either.Right v -> aux (max_depth - 1) v);
              Format.fprintf ppf "@]]"
          | VTODO _v1 -> Format.fprintf ppf "VTODO"
      in
      aux max_depth v)

(*****************************************************************************)
(* Mapper Visitor *)
(*****************************************************************************)

let map_of_unit () = ()
let map_of_bool x = x
let map_of_float x = x
let map_of_char x = x
let map_of_string (s : string) = s
let map_of_ref f aref = ref (f !aref)
let map_of_ref_do_nothing_share_ref _f x = x (* dont go into ref *)

let map_of_option v_of_a v =
  match v with
  | None -> None
  | Some x -> Some (v_of_a x)

let map_of_list of_a xs = List_.map of_a xs
let map_of_int x = x
let map_of_int64 x = x

let map_of_either _of_a _of_b = function
  | Either.Left v1 ->
      let v1 = _of_a v1 in
      Either.Left v1
  | Either.Right v1 ->
      let v1 = _of_b v1 in
      Either.Right v1

let map_of_either3 _of_a _of_b _of_c = function
  | Either_.Left3 v1 ->
      let v1 = _of_a v1 in
      Either_.Left3 v1
  | Either_.Middle3 v1 ->
      let v1 = _of_b v1 in
      Either_.Middle3 v1
  | Either_.Right3 v1 ->
      let v1 = _of_c v1 in
      Either_.Right3 v1

let map_of_all3 of_a of_b of_c (a, b, c) = (of_a a, of_b b, of_c c)

(* this is subtle ... *)
let (map_v : f:(k:(v -> v) -> v -> v) -> v -> v) =
 fun ~f x ->
  let rec map_v v =
    (* generated by ocamltarzan with: camlp4o -o /tmp/yyy.ml -I pa/ pa_type_conv.cmo pa_map.cmo  pr_o.cmo /tmp/xxx.ml  *)
    let k x =
      match x with
      | VUnit -> VUnit
      | VBool v1 ->
          let v1 = map_of_bool v1 in
          VBool v1
      | VFloat v1 ->
          let v1 = map_of_float v1 in
          VFloat v1
      | VChar v1 ->
          let v1 = map_of_char v1 in
          VChar v1
      | VString v1 ->
          let v1 = map_of_string v1 in
          VString v1
      | VInt v1 ->
          let v1 = map_of_int64 v1 in
          VInt v1
      | VTuple v1 ->
          let v1 = map_of_list map_v v1 in
          VTuple v1
      | VDict v1 ->
          let v1 =
            map_of_list
              (fun (v1, v2) ->
                let v1 = map_of_string v1 and v2 = map_v v2 in
                (v1, v2))
              v1
          in
          VDict v1
      | VSum (v1, v2) ->
          let v1 = map_of_string v1 and v2 = map_of_list map_v v2 in
          VSum (v1, v2)
      | VVar v1 ->
          let v1 =
            match v1 with
            | v1, v2 ->
                let v1 = map_of_string v1 and v2 = map_of_int64 v2 in
                (v1, v2)
          in
          VVar v1
      | VArrow v1 ->
          let v1 = map_of_string v1 in
          VArrow v1
      | VNone -> VNone
      | VSome v1 ->
          let v1 = map_v v1 in
          VSome v1
      | VRef v1 ->
          let v1 = map_v v1 in
          VRef v1
      | VList v1 ->
          let v1 = map_of_list map_v v1 in
          VList v1
      | VTODO v1 ->
          let v1 = map_of_string v1 in
          VTODO v1
    in
    f ~k v
  in
  map_v x

(*****************************************************************************)
(* Iterator Visitor *)
(*****************************************************************************)

let v_unit _x = ()
let v_bool _x = ()
let v_int _x = ()
let v_float _x = ()
let v_string (_s : string) = ()
let v_ref_do_visit op aref = op !aref
let v_ref_do_not_visit _aref _x = () (* dont go into ref *)

let v_option v_of_a v =
  match v with
  | None -> ()
  | Some x -> v_of_a x

let v_list of_a xs = List.iter of_a xs

let v_either of_a of_b x =
  match x with
  | Either.Left a -> of_a a
  | Either.Right b -> of_b b

let v_either3 of_a of_b of_c x =
  match x with
  | Either_.Left3 a -> of_a a
  | Either_.Middle3 b -> of_b b
  | Either_.Right3 c -> of_c c
