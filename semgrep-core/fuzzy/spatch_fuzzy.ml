(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

module PI = Parse_info
open Parse_info
module V = Lib_ast_fuzzy

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * See https://github.com/facebook/pfff/wiki/Spatch
 *
 * Here is an example of a spatch file:
 *
 *    foo(2,
 * -      bar(2)
 * +      foobar(4)
 *       )
 *
 * This will replace all calls to bar(2) by foobar(4) when
 * the function call is the second argument of a call to
 * foo where its first argument is 2.
 *
 *
 * note: can we produce syntactically incorrect code? Yes ...
 *
 * less: mostly copy paste of spatch_php.ml
 *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

type pattern = Ast_fuzzy.trees

type line_kind =
  | Context
  | Plus of string
  | Minus

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(*****************************************************************************)
(* Parsing *)
(*****************************************************************************)

(*
 * Algorithm to parse a spatch file:
 *  - take lines of the file, index the lines
 *  - replace the + lines by an empty line and remember in a line_env
 *    the line and its index
 *  - remove the - in the first column and remember in a line_env
 *    that is was a minus line
 *  - unlines the filtered lines into a new string
 *  - call the parser on this new string
 *  - go through all tokens and adjust its transfo field using the
 *    information in line_env
 *)
let parse
  ~pattern_of_string
  ~ii_of_pattern
  file =
  let xs = Common.cat file |> Common.index_list_1 in

  let hline_env = Hashtbl.create 11 in

  let ys = xs |> List.map (fun (s, lineno) ->
    match s with
    (* ugly: for now I strip the space after the + because.
     * at some point we need to parse this stuff and
     * add the correct amount of indentation when it's processing
     * a token.
     *)
    | _ when s =~ "^\\+[ \t]*\\(.*\\)" ->
        let rest_line = Common.matched1 s in
        Hashtbl.add hline_env lineno (Plus rest_line);
        ""
    | _ when s =~ "^\\-\\(.*\\)" ->
        let rest_line = Common.matched1 s in
        Hashtbl.add hline_env lineno Minus;
        rest_line
    | _ ->
        Hashtbl.add hline_env lineno Context;
        s
  )
  in
  let spatch_without_patch_annot = Common2.unlines ys in
  (* pr2 spatch_without_patch_annot; *)

  let pattern = pattern_of_string spatch_without_patch_annot in

  (* need adjust the tokens in it now *)
  let toks = ii_of_pattern pattern in

  (* adjust with Minus info *)
  toks |> List.iter (fun tok ->
    let line = PI.line_of_info tok in

    (match Hashtbl.find_opt hline_env line with
    | Some Context -> ()
    | Some Minus -> tok.PI.transfo <- PI.Remove;
    | Some (Plus _) ->
        (* normally impossible since we removed the elements in the
         * plus line, except the newline. should assert it's only newline
         *)
        ()
    | None -> failwith ("could not find a line in the env")
    );
  );
  (* adjust with the Plus info. We need to annotate the last token
   * on the preceding line or next line.
   * e.g. on
   *     foo(2,
   *   +        42,
   *         3)
   * we could either put the + on the ',' of the first line (as an AddAfter)
   * or on the + on the '3' of the thirdt line (as an AddBefore).
   * The preceding and next line could also be a minus line itself.
   * Also it could be possible to have multiple + line in which
   * case we want to concatenate them together.
   *
   * TODO: for now I just associate it with the previous line ...
   * what if the spatch is:
   *   + foo();
   *     bar();
   * then there is no previous line ...
   *)

  let grouped_by_lines =
    toks |> Common.group_by_mapped_key (fun tok -> PI.line_of_info tok) in
  let rec aux xs =
    match xs with
    | (line, toks_at_line)::rest ->

        (* if the next line was a +, then associate with the last token
         * on this line
         *)
        (match Common2.hfind_option (line+1) hline_env with
        | None ->
            (* probably because was last line *)
            ()
        | Some (Plus toadd) ->
            (* todo? what if there is no token on this line ? *)
            let last_tok = Common2.list_last toks_at_line in

            (* ugly hack *)
            let toadd =
              match PI.str_of_info last_tok with
              | ";" -> "\n" ^ toadd
              | _ -> toadd
            in

            (match last_tok.PI.transfo with
            | Remove -> last_tok.PI.transfo <- Replace (AddStr toadd)
            | NoTransfo -> last_tok.PI.transfo <- AddAfter (AddStr toadd)
            | _ -> raise Impossible
            )
        | Some _ -> ()
        );
        aux rest

    | [] -> ()
  in
  aux grouped_by_lines;

  (* both the ast (here pattern) and the tokens share the same
   * reference so by modifying the tokens we actually also modifed
   * the AST.
   *)
  pattern


(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let spatch pattern ast =

  let was_modifed = ref false in

  let len = List.length pattern in

  (* visit AST and try to match pattern on it *)
  let hook =
    { V.default_visitor with
      V.ktrees = (fun (k, _) xs ->
        if List.length xs >= len then begin
          let shorter, rest = Common2.splitAt len xs in

          (* pr (Ocaml.string_of_v (Ast_fuzzy.vof_trees shorter));*)

          let matches_with_env =
            Matching_fuzzy.match_trees_trees pattern shorter
          in
          if matches_with_env = []
          then
            (* recurse on sublists *)
            k xs
          else begin
            was_modifed := true;
            Transforming_fuzzy.transform_trees_trees pattern shorter
              (* TODO, maybe could get multiple matching env *)
              (List.hd matches_with_env);

            k rest
          end
        end
        else
          (* at least recurse *)
          k xs
      );
    }
  in
  (V.mk_visitor hook) ast;
  !was_modifed
