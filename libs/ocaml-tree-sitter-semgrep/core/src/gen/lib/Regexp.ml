(*
   Compile a regular expression into a regular expression parser that captures
   all matched elements.
*)

open Printf

(* Details needed to generate OCaml code. This could become the argument
   to generic functor.
*)
module Param = struct
  open Indent.Types

  type code = Indent.t

  let code_of_string s =
    [Line s]

  let parse_fail =
    [Line "fun _nodes -> None"]

  let apply_variant name arg =
    sprintf "`%s %s" name arg
end

type rep_kind =
  | Optional
  | Repeat
  | Repeat1

(*
   Representation of the regular expression to match. It gets converted
   into a function that returns a pair (head, tail) upon success. More
   specifically, the type of this expression is:

     nodes -> ((head * tail) * nodes) option

   in which the variable '_parse_tail' provided in the environment is used
   to parse the tail.
*)
type seq = seq_elt list

and seq_elt =
  | Opaque of Param.code
  (* expression returning one element, of type:
       nodes -> (res * nodes) option

     which may call check_tail.
  *)
  | Seq of seq_elt list
  | Repeat of rep_kind * seq
  | Choice of (string * seq) list

(* For debugging *)
let print_seq_elt seq_elt =
  let open Indent.Types in

  let string_of_rep_kind = function
    | Optional -> "Optional"
    | Repeat -> "Repeat"
    | Repeat1 -> "Repeat1"
  in

  let rec fmt_seq seq =
    List.map (fun x -> Inline (fmt_seq_elt x)) seq
  and fmt_seq_elt seq_elt =
    match seq_elt with
    | Opaque code ->
        [
          Line "Opaque (";
          Block code;
          Line ")";
        ]
    | Seq seq ->
        [
          Line "Seq (";
          Block (fmt_seq seq);
          Line ")";
        ]
    | Repeat (rep_kind, seq) ->
        [
          Line "Repeat (";
          Block [
            Line (string_of_rep_kind rep_kind);
            Inline (fmt_seq seq);
          ];
          Line ")";
        ]
    | Choice cases ->
        [
          Line "Choice [";
          Block (List.map (fun (name, seq) ->
            Inline [
              Line name;
              Block (fmt_seq seq);
            ]
          ) cases);
          Line "]";
        ]
  in
  print_endline (Indent.to_string (fmt_seq_elt seq_elt))

(*
   Mini DSL, with external values injected as atoms.
*)
type exp =
  | Atom of Param.code             (* e *)
  | Var of string                  (* var *)
  | App of exp * exp               (* f e *)
  | Fun of string * exp            (* fun var -> e *)
  | Def of string * exp * exp      (* let var = e1 in e2 *)
  | Op of rep_kind * exp           (* op f *)
  | Alt of exp * exp               (* match e1 with
                                      | Some _ as res -> res
                                      | None -> e2 *)

  | Flatten of exp * int * (string -> string) option
  (* match e with
     | Some ((e1, (e2, e3))), nodes) ->
        Some (wrap (e1, e2, e3), nodes)
     | None -> None
  *)

  | Check_tail of exp
  (* check_seq f check_tail *)

  | Seq of exp
  (* parse_seq f parse_tail *)

let rec compile_seq seq =
  match seq with
  | [] -> assert false
  | [head] ->
      1, compile_seq_elt head None
  | head :: tail ->
      let n, parse_tail = compile_seq tail in
      (n + 1,
       Def ("_parse_tail",
            parse_tail,
            Seq (compile_seq_elt head (Some (Var "_parse_tail")))))

and compile_seq_flat seq opt_wrap =
  let n, f = compile_seq seq in
  Fun ("nodes", Flatten (App (f, Var "nodes"), n, opt_wrap))

and compile_seq_elt seq_elt opt_parse_tail =
  match seq_elt with
  | Opaque code ->
      Atom code
  | Seq seq ->
      compile_seq_flat seq None
  | Repeat (rep_kind, seq) ->
      let repeat =
        Op (rep_kind, compile_seq_flat seq None)
      in
      (match opt_parse_tail with
       | None -> repeat
       | Some parse_tail ->
           Def ("check_tail", Check_tail parse_tail, repeat)
      )
  | Choice cases ->
      compile_cases cases

and compile_cases cases =
  let rec compile_choice cases =
    match cases with
    | [] ->
        Atom Param.parse_fail
    | (name, seq) :: cases ->
        let wrap arg = Param.apply_variant name arg in
        let first = App (compile_seq_flat seq (Some wrap), Var "nodes") in
        match cases with
        | [] -> first
        | cases -> Alt (first, compile_choice cases)
  in
  let body = compile_choice cases in
  Fun ("nodes", body)
