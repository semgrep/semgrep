(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
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

module O = OCaml
module PPI = Pretty_printer_indent

open PPI (* for the --> infix operators *)

(*
 * alternatives:
 * - use camlp4, so will even get static checking that produce value
 *   ocaml code, and also nice pretty printing of generated code, but
 *   arguably harder to test ? and also harder to code ... see my
 *   experience with pa_map.ml
 * - use camlmix
 *
 * TODO: handle Constructor of (int * int),  those extra tuple of tuple ...
 *)

let parenize_and_comma xs =
  if null xs
  then ""
  else "(" ^ (Common.join ", " xs) ^ ")"

let rec call_to_matcher t =
  match t with
  | O.Var s -> spf "m_%s" s

  | O.Apply (s, t) ->
      spf "(m_%s " s ^ call_to_matcher t ^ ")"
  | O.List t ->
      let s = "list" in
      spf "(m_%s " s ^ call_to_matcher t ^ ")"
  | O.Option t ->
      let s = "option" in
      spf "(m_%s " s ^ call_to_matcher t ^ ")"

  | O.Unit   -> "m_unit"
  | O.Bool   -> "m_bool"
  | O.Float  -> "m_float"
  | O.Char   -> "m_char"
  | O.String -> "m_string"

  | _ ->
      (* pr2 (OCaml.string_sexp_of_t t); *)
      pr2_gen t;
      raise Todo

let (gen_matcher: string * OCaml.t -> unit) = fun (s, t) ->
  (* pr2 (OCaml.string_sexp_of_t t); *)

  let e = PPI.init_printer stdout in

  e += "let m_%s a b = " $ s;
  e --> (fun e ->
    e+= "match a, b with";

    let aux t =
      match t with
      | O.Sum xs ->
          xs |> List.iter (fun (s, args) ->

            let aa = Common.index_list_1 args |> List.map (fun (t, i) ->
              spf "a%d" i, t
            ) in
            let bb = Common.index_list_1 args |> List.map (fun (t, i) ->
              spf "b%d" i, t
            ) in

            let aas = aa |> List.map fst |> parenize_and_comma in
            let bbs = bb |> List.map fst |> parenize_and_comma in
            e+= "| A.%s%s, B.%s%s ->" $ s $ aas $ s $ bbs;
            e --> (fun e ->
              (* ... *)
              Common2.zip aa bb |> List.iter (fun ((aa1, t), (aa2, _t2)) ->
                e.p (spf "%s %s %s >>= (fun (%s, %s) -> "
                       (call_to_matcher t) aa1 aa2 aa1 aa2);
              );

              e+= "return (";
              e --> (fun e ->
                e+= " A.%s%s," $ s $ aas;
                e+= " B.%s%s" $ s $ bbs;
              );
              e+= (")");
              if not (null args)
              then e.p (Common2.repeat ")" (List.length args) |> Common.join "");
            )
          );
          if List.length xs > 1 then begin
            xs |> List.iter (fun (s, args) ->
              e+= "| A.%s%s, _" $ s $ (if null args then "" else " _");
            );
            e+= " -> fail ()";
          end


      (* TODO factorize code ? a tuple is a kind of anon single constructor *)
      | O.Tuple args ->
          let aa = Common.index_list_1 args |> List.map (fun (t, i) ->
            spf "a%d" i, t
          ) in
          let bb = Common.index_list_1 args |> List.map (fun (t, i) ->
            spf "b%d" i, t
          ) in

          let aas = aa |> List.map fst |> parenize_and_comma in
          let bbs = bb |> List.map fst |> parenize_and_comma in
          e+= "| %s, %s ->" $ aas $ bbs;
          e --> (fun e ->
            (* ... *)
            Common2.zip aa bb |> List.iter (fun ((aa1, t), (aa2, _t2)) ->
              e.p (spf "%s %s %s >>= (fun (%s, %s) -> "
                     (call_to_matcher t) aa1 aa2 aa1 aa2);
            );

            e+= "return (";
            e --> (fun e ->
              e+= " %s," $ aas;
              e+= " %s" $ bbs;
            );
            e+= (")");
            if not (null args)
            then e.p (Common2.repeat ")" (List.length args) |> Common.join "");
          )
      | O.Var s ->
          e += "(a, b) -> %s a b" $ (call_to_matcher (O.Var s))

      | O.List x ->
          e += "(a, b) -> %s a b" $ (call_to_matcher (O.List x))

      | O.Apply (x,y) ->
          e += "(a, b) -> %s a b" $ (call_to_matcher (O.Apply (x,y)))

      | O.Dict xs ->
          let aa = Common.index_list_1 xs |> List.map (fun (t, i) ->
            spf "a%d" i, t
          ) in
          let bb = Common.index_list_1 xs |> List.map (fun (t, i) ->
            spf "b%d" i, t
          ) in

          e+= "{ A. ";
          aa |> List.iter (fun (aa1, (fld, _, _t)) ->
            e += "%s = %s;" $ fld $ aa1
          );
          e+= "},";
          e+= "{ B. ";
          bb |> List.iter (fun (bb1, (fld, _, _t)) ->
            e += "%s = %s;" $ fld $ bb1
          );
          e+= "} -> ";

          e --> (fun e ->
            (* ... *)
            Common2.zip aa bb |> List.iter (fun ((aa1, (_fld, _, t)), (aa2, _)) ->
              e.p (spf "%s %s %s >>= (fun (%s, %s) -> "
                     (call_to_matcher t) aa1 aa2 aa1 aa2);
            );
            e+= "return (";
            e --> (fun e ->

              e+= "{ A. ";
              aa |> List.iter (fun (aa1, (fld, _, _t)) ->
                e += "%s = %s;" $ fld $ aa1
              );
              e+= "},";
              e+= "{ B.";
              bb |> List.iter (fun (bb1, (fld, _, _t)) ->
                e += "%s = %s;" $ fld $ bb1
              );
              e+= "} ";
            );
            e+= (")");
          );
          e.p (Common2.repeat ")" (List.length xs) |> Common.join "")



      | (O.TTODO _|O.Option _|O.Arrow (_, _)|O.Poly _
        |O.Int|O.String|O.Char|O.Float|O.Bool|O.Unit) ->
          raise Todo

    in
    aux t
  )
