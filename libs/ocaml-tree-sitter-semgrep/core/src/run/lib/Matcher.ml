(*
   Types shared by the matcher implementations.
*)

open Printf
open Tree_sitter_gen.Indent.Types

module Exp = struct
  (*
     A regular expression.
  *)
  type 'token_kind t =
    | Token of 'token_kind
    | Repeat of 'token_kind t
    | Repeat1 of 'token_kind t
    | Opt of 'token_kind t
    | Alt of 'token_kind t array
    | Seq of 'token_kind t list
    | Nothing

  let show show_token_kind x =
    let rec show (x : _ t) =
      match x with
      | Token tok -> [Line (sprintf "Token %s" (show_token_kind tok))]
      | Repeat x ->
          [
            Line "Repeat (";
            Block (show x);
            Line ")"
          ]
      | Repeat1 x ->
          [
            Line "Repeat1 (";
            Block (show x);
            Line ")"
          ]
      | Opt x ->
          [
            Line "Opt (";
            Block (show x);
            Line ")";
          ]
      | Alt cases ->
          let cases =
            Array.mapi (fun i x ->
              Inline [
                Line (sprintf "%i:" i);
                Block (show x)
              ]
            ) cases
            |> Array.to_list
          in
          [
            Line (sprintf "Alt (");
            Block cases;
            Line ")";
          ]
      | Seq l ->
          [
            Line "Seq (";
            Block (List.map show l |> List.flatten);
            Line ")";
          ]
      | Nothing ->
          [ Line "Nothing" ]
    in
    Tree_sitter_gen.Indent.to_string (show x)

end

module Capture = struct
  (*
     A list of tokens successfully matched against a regular expression.
  *)
  type 'token t =
    | Token of 'token
    | Repeat of 'token t list
    | Repeat1 of 'token t list
    | Opt of 'token t option
    | Alt of int * 'token t
    | Seq of 'token t list
    | Nothing

  let show show_token capture =
    let rec show (capture : _ t) =
      match capture with
      | Token tok -> [Line (sprintf "Token %s" (show_token tok))]
      | Repeat l ->
          [
            Line "Repeat [";
            Block (List.map show l |> List.flatten);
            Line "]"
          ]
      | Repeat1 l ->
          [
            Line "Repeat1 [";
            Block (List.map show l |> List.flatten);
            Line "]"
          ]
      | Opt None ->
          [ Line "None" ]
      | Opt (Some x) ->
          [
            Line "Some (";
            Block (show x);
            Line ")";
          ]
      | Alt (i, x) ->
          [
            Line (sprintf "Alt %i (" i);
            Block (show x);
            Line ")";
          ]
      | Seq l ->
          [
            Line "Seq (";
            Block (List.map show l |> List.flatten);
            Line ")";
          ]
      | Nothing ->
          [ Line "Nothing" ]
    in
    Tree_sitter_gen.Indent.to_string (show capture)
end

type 'token_kind exp = 'token_kind Exp.t
type 'token capture = 'token Capture.t

(* An element of the input sequence. *)
module type Token = sig
  (* The token kind, known in advance and comparable. *)
  type kind

  (* A captured token. *)
  type t

  (* Get the token kind. *)
  val kind : t -> kind

  (* Produce a human-readable view of the token kind. *)
  val show_kind : kind -> string

  (* Produce a human-readable view of the token. *)
  val show : t -> string
end

module type Matcher = sig
  type token_kind
  type token

  val show_exp : token_kind exp -> string
  val match_tree : token_kind exp -> token list -> token capture option
  val show_capture : token capture -> string
  val show_match : token capture option -> string
end

let show_match show_token opt_capture =
  match opt_capture with
  | None -> "[no match]"
  | Some capture -> Capture.show show_token capture
