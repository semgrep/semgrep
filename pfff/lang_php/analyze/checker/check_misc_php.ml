(* Yoann Padioleau
 *
 * Copyright (C) 2012 Facebook
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

open Cst_php
module Ast = Cst_php
module V = Visitor_php
module E = Error_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Misc checks
 *  - use of ';' instead of ':', 
 *  - wrong case sensitivity for 'instanceOf'
 *  - right number of arguments in printf type function
 *  - adhoc typing such as using + instead of .
 *)

(*****************************************************************************)
(* Types and Constants *)
(*****************************************************************************)

(* functions that has format string on the n+1th argument *)
let printf_like_functions_list = [
  ("SQL", 0);
  ("SQL_UNSAFE", 0);
  ("exec_manual", 0);
  ("execx", 0);
  ("fprintf", 0);
  ("invariant_violation", 0);
  ("jsprintf", 0);
  ("onloadRegister", 0);
  ("printf", 0);
  ("psprintf", 0);
  ("qsprintf", 0);
  ("sprintf", 0);
  ("usprintf", 0);
  ("invariant", 1);
]

exception Argument_swapping

(*****************************************************************************)
(* Helper *)
(*****************************************************************************)

(* return None for error *)
let rec start_state acc char_list =
  match char_list with
  | [] -> Some acc
  | '\\'::t -> in_escape_state acc t
  | '%'::t -> in_percent_state acc t
  | _::t -> start_state acc t

and in_percent_state acc char_list =
  match char_list with
  | [] -> None
  | '%'::t -> start_state acc t
  | c::'$'::_t when (Common2.is_digit c) ->
    raise Argument_swapping
  | _::t -> start_state (acc+1) t

and in_escape_state acc char_list =
  match char_list with
  | [] -> None
  | _::t -> start_state acc t

let check_format_string args =
  match args with
  | [] -> false
  | ""::_t -> true
  | h::t ->
    let char_list = Common2.list_of_string h in
    let acc = start_state 0 char_list in
    match acc with
    | None -> false
    | Some i -> i = List.length t

(* skip first n arguments *)      
let rec check_format_stringn n args =
  match (n, args) with
  | (n, _) when (n < 0) -> failwith "bad argument for check_format_stringn"
  | (0, args) -> check_format_string args
  | (n, _h::t) -> check_format_stringn (n-1) t
  | (_n, []) -> false

let rec unargs args =
  match args with
  | [] -> []
  | Left(Arg(Sc(C(String((s, _))))))::t -> s::(unargs t)
  | Left _x::t -> ""::(unargs t)
  | _h::t -> unargs t


(* limited typing *)

let expr_is_T_or_F (expr : Ast.expr) =
  match expr with
  | Id(XName([QI(Name((str, _)))])) ->
    let str_l = String.lowercase_ascii str in
    (str_l = "true") || (str_l = "false")
  | _ -> false

(* check if the expression returns boolean *)
let rec expr_is_bool (expr : Ast.expr) =
  match expr with
  | _ when expr_is_T_or_F expr -> true
  | Binary(_, (Logical(_), _), _) -> true
  | ParenExpr(_, new_expr, _) -> expr_is_bool new_expr
  | _ -> false

let expr_is_string e =
  match e with
  | Sc (C (String _)) -> true
  | Sc (C (PreProcess _)) -> true
  | Sc (Guil _ | HereDoc _) -> true
  | Binary (_, (BinaryConcat, _), _) -> true
  | _ -> false

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let check ast = 

  ast |> List.iter (function
  | ClassDef cdef ->
    (match cdef.c_type with
    | Interface _ ->
      cdef.c_body |> Ast.unbrace |> List.iter (function
      | Method mdef ->
        (match mdef.f_type with
        | MethodAbstract -> ()
        | MethodRegular ->
          E.warning mdef.f_tok E.InterfaceMethodWithBody
        | _ -> ()
        )
      | _ -> ()
      )
    | _ -> ()
    )
  | _ -> ()
  );

  let visitor = V.mk_visitor { V.default_visitor with
    V.kstmt = (fun (k, _) st ->
      (match st with
      | Switch (_tok, _expr, cases) ->
        (match cases with
        | CaseList (_obrace, _tok2, cases, _cbrace) ->
          cases |> List.iter (function
          | Case (_, _, case_separator, _)
          | Default (_, case_separator, _) ->
                  (* this is more something that should be fixed by a proper
                   * grammar
                   *)
            let str = Parse_info.str_of_info case_separator in
            (match str with
            | ":" -> ()
            | ";" -> E.warning case_separator E.CaseWithSemiColon
              
            | _ -> raise Impossible
            )
          )
        | _ -> ()
        )
      | _ -> ()
      );
      (* recurse, call continuation *)
      k st
    );
    V.kexpr = (fun (k,_) e ->
      (match e with
      (* could do the case sensitivity check on all keywords, but
       * this one in particular seems to happen a lot
       *)
      | InstanceOf (e, tok, _classname) ->
        let str = Parse_info.str_of_info tok in
        let lower = String.lowercase_ascii str in
        if not (str =$= lower)
        then E.warning tok E.CaseSensitivityKeyword;
        k e
      (* Check the number of argument if the function name is in function_list/function_listn*)
      | Call(Id(XName[QI(Name((func_name, tok)))]), (_, args, _))
          when (List.mem_assoc func_name printf_like_functions_list) ->
        let n = List.assoc func_name printf_like_functions_list in
        (try
           if (not (check_format_stringn n (unargs args)))
           then E.warning tok (E.FormatStringMismatch func_name)
         with Argument_swapping -> ()
        );
        k e
      (* Catch clowny "($x>$y)?true:false" *)
      | CondExpr(cond, tok, Some(expr1),  _, expr2)
          when (expr_is_T_or_F expr1) &&
            (expr_is_T_or_F expr2) &&
            (expr_is_bool cond) ->
        E.warning tok E.UnnecessaryTernaryIf;
      | CondExpr(cond, tok, None,  _, expr2)
          when (expr_is_T_or_F expr2) &&
            (expr_is_bool cond) ->
        E.warning tok E.UnnecessaryTernaryIf;

      | Binary (e1, (Arith Plus, tok), e2) 
          when expr_is_string e1 || expr_is_string e2 ->
        E.fatal tok E.UseOfPlusNotDotForStrings
      | _ -> ()
      );
      (* recurse, call continuation *)
      k e
    );
  }
  in
  visitor (Program ast)
