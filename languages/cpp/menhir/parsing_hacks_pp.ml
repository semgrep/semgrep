(* Yoann Padioleau
 *
 * Copyright (C) 2002-2008 Yoann Padioleau
 * Copyright (C) 2011 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *
 *)
open Common
module TH = Token_helpers_cpp
module TV = Token_views_cpp
module Parser = Parser_cpp
open Parser_cpp
open Token_views_cpp
open Parsing_hacks_lib

[@@@warning "-9"]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This file gathers parsing heuristics related to the C preprocessor cpp.
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ( ==~ ) = Common2.( ==~ )

(* the pair is the status of '()' and '{}', ex: (-1,0)
 * if too much ')' and good '{}'
 * could do for [] too ?
 * could do for ','   if encounter ',' at "toplevel", not inside () or {}
 * then if have ifdef, then certainly can lead to a problem.
 *)
let (count_open_close_stuff_ifdef_clause : ifdef_grouped list -> int * int) =
 fun xs ->
  let cnt_paren, cnt_brace = (ref 0, ref 0) in
  xs
  |> iter_token_ifdef (fun x ->
         match x.t with
         | x when TH.is_opar x -> incr cnt_paren
         | x when TH.is_obrace x -> incr cnt_brace
         | x when TH.is_cpar x -> decr cnt_paren
         | x when TH.is_obrace x -> decr cnt_brace
         | _ -> ());
  (!cnt_paren, !cnt_brace)

(* look if there is a '{' just after the closing ')', and handling the
 * possibility to have nested expressions inside nested parenthesis
 *)
(*
let is_really_foreach xs =
  let rec is_foreach_aux = function
    | [] -> false, []
    | TCPar _::TOBrace _::xs -> true, xs
      (* the following attempts to handle the cases where there is a
	 single statement in the body of the loop.  undoubtedly more
	 cases are needed.
         todo: premier(statement) - suivant(funcall)
      *)
    | TCPar _::TIdent _::xs -> true, xs
    | TCPar _::Tif _::xs -> true, xs
    | TCPar _::Twhile _::xs -> true, xs
    | TCPar _::Tfor _::xs -> true, xs
    | TCPar _::Tswitch _::xs -> true, xs

    | TCPar _::xs -> false, xs
    | TOPar _::xs ->
        let (_, xs') = is_foreach_aux xs in
        is_foreach_aux xs'
    | x::xs -> is_foreach_aux xs
  in
  is_foreach_aux xs +> fst
*)

(* TODO: set_ifdef_parenthize_info ?? from parsing_c/ *)

let filter_pp_or_comment_stuff xs =
  (* Tail-recursive to prevent stack overflows. *)
  let rec aux acc xs =
    match xs with
    | [] -> List.rev acc
    | x :: xs -> (
        match x.TV.t with
        | tok when TH.is_comment tok -> aux acc xs
        (* don't want drop the define, or if drop, have to drop
         * also its body otherwise the line heuristics may be lost
         * by not finding the TDefine in column 0 but by finding
         * a TDefineIdent in a column > 0
         *
         * todo? but define often contain some unbalanced {
         *)
        | Parser.TDefine _ -> aux (x :: acc) xs
        | tok when TH.is_pp_instruction tok -> aux acc xs
        | _ -> aux (x :: acc) xs)
  in
  aux [] xs

(*****************************************************************************)
(* Ifdef keeping/passing *)
(*****************************************************************************)

(* #if 0, #if 1,  #if LINUX_VERSION handling *)
let rec find_ifdef_bool xs =
  xs
  |> List.iter (function
       | NotIfdefLine _ -> ()
       | Ifdefbool (is_ifdef_positif, xxs, info_ifdef_stmt) -> (
           if is_ifdef_positif then
             pr2_pp "commenting parts of a #if 1 or #if LINUX_VERSION"
           else pr2_pp "commenting a #if 0 or #if LINUX_VERSION or __cplusplus";

           match xxs with
           | [] -> raise Impossible
           | firstclause :: xxs ->
               info_ifdef_stmt
               |> List.iter (set_as_comment Token_cpp.CppDirective);

               if is_ifdef_positif then
                 xxs
                 |> List.iter
                      (iter_token_ifdef (set_as_comment Token_cpp.CppOther))
               else (
                 firstclause
                 |> iter_token_ifdef (set_as_comment Token_cpp.CppOther);
                 match List.rev xxs with
                 (* keep only last *)
                 | _last :: startxs ->
                     startxs
                     |> List.iter
                          (iter_token_ifdef (set_as_comment Token_cpp.CppOther))
                 | [] -> (* not #else *) ()))
       | Ifdef (xxs, _info_ifdef_stmt) -> xxs |> List.iter find_ifdef_bool)

let thresholdIfdefSizeMid = 6

(* infer ifdef involving not-closed expressions/statements *)
let rec find_ifdef_mid xs =
  xs
  |> List.iter (function
       | NotIfdefLine _ -> ()
       | Ifdef (xxs, info_ifdef_stmt) ->
           (match xxs with
           | [] -> raise Impossible
           | [ _first ] -> ()
           | _first :: second :: rest ->
               (* don't analyse big ifdef *)
               if
                 xxs
                 |> List.for_all (fun xs ->
                        List.length xs <= thresholdIfdefSizeMid)
                 && (* don't want nested ifdef *)
                 xxs
                 |> List.for_all (fun xs ->
                        xs
                        |> List.for_all (function
                             | NotIfdefLine _ -> true
                             | _ -> false))
               then
                 let counts =
                   xxs |> List_.map count_open_close_stuff_ifdef_clause
                 in
                 let cnt1, cnt2 =
                   match counts with
                   | x :: _ -> x
                   | [] -> assert false
                 in
                 if
                   cnt1 <> 0 || cnt2 <> 0
                   (*???? && counts +> List.for_all (fun x -> x = (cnt1, cnt2)) *)
                   (*
                if counts +> List.exists (fun (cnt1, cnt2) ->
                cnt1 <> 0 || cnt2 <> 0
                )
              *)
                 then (
                   pr2_pp "found ifdef-mid-something";
                   (* keep only first, treat the rest as comment *)
                   info_ifdef_stmt
                   |> List.iter (set_as_comment Token_cpp.CppDirective);
                   second :: rest
                   |> List.iter
                        (iter_token_ifdef (set_as_comment Token_cpp.CppOther))));
           List.iter find_ifdef_mid xxs
       (* no need complex analysis for ifdefbool *)
       | Ifdefbool (_, xxs, _info_ifdef_stmt) -> List.iter find_ifdef_mid xxs)

let thresholdFunheaderLimit = 4

(* ifdef defining alternate function header, type *)
let rec find_ifdef_funheaders = function
  | [] -> ()
  | NotIfdefLine _ :: xs -> find_ifdef_funheaders xs
  (* ifdef-funheader if ifdef with 2 lines and a '{' in next line *)
  | Ifdef
      ( [
          NotIfdefLine (({ col = 0 } as _xline1) :: _line1) :: ifdefblock1;
          NotIfdefLine (({ col = 0 } as xline2) :: line2) :: ifdefblock2;
        ],
        info_ifdef_stmt )
    :: NotIfdefLine ({ t = TOBrace _i; col = 0 } :: _line3)
    :: xs
    when List.length ifdefblock1 <= thresholdFunheaderLimit
         && List.length ifdefblock2 <= thresholdFunheaderLimit ->
      find_ifdef_funheaders xs;
      info_ifdef_stmt |> List.iter (set_as_comment Token_cpp.CppDirective);
      let all_toks = [ xline2 ] @ line2 in
      all_toks |> List.iter (set_as_comment Token_cpp.CppOther);
      ifdefblock2 |> iter_token_ifdef (set_as_comment Token_cpp.CppOther)
      (* ifdef with nested ifdef *)
  | Ifdef
      ( [
          [ NotIfdefLine (({ col = 0 } as _xline1) :: _line1) ];
          [
            Ifdef
              ( [
                  [ NotIfdefLine (({ col = 0 } as xline2) :: line2) ];
                  [ NotIfdefLine (({ col = 0 } as xline3) :: line3) ];
                ],
                info_ifdef_stmt2 );
          ];
        ],
        info_ifdef_stmt )
    :: NotIfdefLine ({ t = TOBrace _i; col = 0 } :: _line4)
    :: xs ->
      find_ifdef_funheaders xs;
      info_ifdef_stmt |> List.iter (set_as_comment Token_cpp.CppDirective);
      info_ifdef_stmt2 |> List.iter (set_as_comment Token_cpp.CppDirective);
      let all_toks = [ xline2; xline3 ] @ line2 @ line3 in
      all_toks |> List.iter (set_as_comment Token_cpp.CppOther)
      (* ifdef with elseif *)
  | Ifdef
      ( [
          [ NotIfdefLine (({ col = 0 } as _xline1) :: _line1) ];
          [ NotIfdefLine (({ col = 0 } as xline2) :: line2) ];
          [ NotIfdefLine (({ col = 0 } as xline3) :: line3) ];
        ],
        info_ifdef_stmt )
    :: NotIfdefLine ({ t = TOBrace _i; col = 0 } :: _line4)
    :: xs ->
      find_ifdef_funheaders xs;
      info_ifdef_stmt |> List.iter (set_as_comment Token_cpp.CppDirective);
      let all_toks = [ xline2; xline3 ] @ line2 @ line3 in
      all_toks |> List.iter (set_as_comment Token_cpp.CppOther)
  | Ifdef (xxs, _) :: xs
  | Ifdefbool (_, xxs, _) :: xs ->
      List.iter find_ifdef_funheaders xxs;
      find_ifdef_funheaders xs

(*
let adjust_inifdef_include xs =
  xs +> List.iter (function
  | NotIfdefLine _ -> ()
  | Ifdef (xxs, info_ifdef_stmt) | Ifdefbool (_, xxs, info_ifdef_stmt) ->
      xxs +> List.iter (iter_token_ifdef (fun tokext ->
        match tokext.t with
        | Parser.TInclude (s1, s2, ii) ->
            (* todo: inifdef_ref := true; *)
            ()
        | _ -> ()
      ));
  )
*)

(*****************************************************************************)
(* Builtin macros using standard.h or other defs *)
(*****************************************************************************)
(* now in pp_token.ml *)

(*****************************************************************************)
(* Stringification *)
(*****************************************************************************)

let rec find_string_macro_paren xs =
  match xs with
  | [] -> ()
  | Parenthised (xxs, _) :: xs ->
      xxs
      |> List.iter (fun xs ->
             if
               xs
               |> List.exists (function
                    | PToken { t = TString _ } -> true
                    | _ -> false)
               && xs
                  |> List.for_all (function
                       | PToken { t = TString _ }
                       | PToken { t = TIdent _ } ->
                           true
                       | _ -> false)
             then
               xs
               |> List.iter (fun tok ->
                      match tok with
                      | PToken ({ t = TIdent (_s, _) } as id) ->
                          change_tok id
                            (TIdent_MacroString (TH.info_of_tok id.t))
                      | _ -> ())
             else find_string_macro_paren xs);
      find_string_macro_paren xs
  | PToken _ :: xs -> find_string_macro_paren xs

(*****************************************************************************)
(* Macros *)
(*****************************************************************************)

(* don't forget to recurse in each case.
 * note that the code below is called after the ifdef phase simplification,
 * so if this previous phase is buggy, then it may pass some code that
 * could be matched by the following rules but will not.
 * *)
let rec find_macro_paren xs =
  match xs with
  | [] -> ()
  (* attribute *)
  | PToken ({ t = Tattribute _ } as id) :: Parenthised (xxs, info_parens) :: xs
    ->
      pr2_pp "MACRO: __attribute detected ";
      [ Parenthised (xxs, info_parens) ]
      |> iter_token_paren (set_as_comment Token_cpp.CppAttr);
      set_as_comment Token_cpp.CppAttr id;
      find_macro_paren xs
  (* stringification
   *
   * the order of the matching clause is important
   *
   *)
  (* string macro with params, before case *)
  | PToken { t = TString _ }
    :: PToken ({ t = TIdent (_s, _) } as id)
    :: Parenthised (xxs, info_parens)
    :: xs ->
      change_tok id (TIdent_MacroString (TH.info_of_tok id.t));
      [ Parenthised (xxs, info_parens) ]
      |> iter_token_paren (set_as_comment Token_cpp.CppMacro);
      find_macro_paren xs
  (* after case *)
  | PToken ({ t = TIdent (_s, _) } as id)
    :: Parenthised (xxs, info_parens)
    :: PToken { t = TString _ }
    :: xs ->
      change_tok id (TIdent_MacroString (TH.info_of_tok id.t));
      [ Parenthised (xxs, info_parens) ]
      |> iter_token_paren (set_as_comment Token_cpp.CppMacro);
      find_macro_paren xs
  (* for the case where the string is not inside a funcall, but
   * for instance in an initializer.
   *)
  (* string macro variable, before case *)
  | PToken { t = TString (str, _) }
    :: PToken ({ t = TIdent (_s, _) } as id)
    :: xs ->
      (* c++ext: *)
      if str <> "C" then (
        change_tok id (TIdent_MacroString (TH.info_of_tok id.t));
        find_macro_paren xs (* bugfix, forgot to recurse in else case too ... *))
      else find_macro_paren xs
  (* after case *)
  | PToken ({ t = TIdent (_s, _) } as id) :: PToken { t = TString _ } :: xs ->
      change_tok id (TIdent_MacroString (TH.info_of_tok id.t));
      find_macro_paren xs
  (* TODO: cooperating with standard.h *)
  | PToken ({ t = TIdent (s, _i1) } as id) :: xs when s = "MACROSTATEMENT" ->
      change_tok id (TIdent_MacroStmt (TH.info_of_tok id.t));
      find_macro_paren xs
  (* recurse *)
  | PToken _x :: xs -> find_macro_paren xs
  | Parenthised (xxs, _) :: xs ->
      xxs |> List.iter find_macro_paren;
      find_macro_paren xs

(* don't forget to recurse in each case *)
let rec find_macro_lineparen xs =
  match xs with
  | [] -> ()
  (* firefoxext: ex: NS_DECL_NSIDOMNODELIST *)
  | Line [ PToken ({ t = TIdent (s, _) } as macro) ] :: xs
    when s ==~ regexp_ns_decl_like ->
      set_as_comment Token_cpp.CppMacro macro;

      find_macro_lineparen xs
  (* firefoxext: ex: NS_DECL_NSIDOMNODELIST; *)
  | Line [ PToken ({ t = TIdent (s, _) } as macro); PToken { t = TPtVirg _ } ]
    :: xs
    when s ==~ regexp_ns_decl_like ->
      set_as_comment Token_cpp.CppMacro macro;

      find_macro_lineparen xs
  (* firefoxext: ex: NS_IMPL_XXX(a) *)
  | Line
      [
        PToken ({ t = TIdent (s, _) } as macro); Parenthised (xxs, info_parens);
      ]
    :: xs
    when s ==~ regexp_ns_decl_like ->
      [ Parenthised (xxs, info_parens) ]
      |> iter_token_paren (set_as_comment Token_cpp.CppMacro);
      set_as_comment Token_cpp.CppMacro macro;

      find_macro_lineparen xs
  (* linuxext: ex: static [const] DEVICE_ATTR(); *)
  | Line
      [
        PToken { t = Tstatic _ };
        PToken ({ t = TIdent (s, _) } as macro);
        Parenthised (_xxs, _);
        PToken { t = TPtVirg _ };
      ]
    :: xs
    when s ==~ regexp_macro ->
      let info = TH.info_of_tok macro.t in
      change_tok macro (TIdent_MacroDecl (Tok.content_of_tok info, info));

      find_macro_lineparen xs
  (* the static const case *)
  | Line
      [
        PToken { t = Tstatic _ };
        PToken ({ t = Tconst _ } as const);
        PToken ({ t = TIdent (s, _) } as macro);
        Parenthised (_xxs, _info_parens);
        PToken { t = TPtVirg _ } (*as line1*);
      ]
    :: xs
    when s ==~ regexp_macro ->
      let info = TH.info_of_tok macro.t in
      change_tok macro (TIdent_MacroDecl (Tok.content_of_tok info, info));

      (* need retag this const, otherwise ambiguity in grammar
         21: shift/reduce conflict (shift 121, reduce 137) on Tconst
         	 decl2 : Tstatic . TMacroDecl TOPar argument_list TCPar ...
         	 decl2 : Tstatic . Tconst TMacroDecl TOPar argument_list TCPar ...
         	 storage_class_spec : Tstatic .  (137)
      *)
      change_tok const (Tconst_MacroDeclConst (TH.info_of_tok const.t));

      find_macro_lineparen xs
  (* same but without trailing ';'
   *
   * I do not put the final ';' because it can be on a multiline and
   * because of the way mk_line is coded, we will not have access to
   * this ';' on the next line, even if next to the ')' *)
  | Line
      [
        PToken { t = Tstatic _ };
        PToken ({ t = TIdent (s, _) } as macro);
        Parenthised (_xxs, _);
      ]
    :: xs
    when s ==~ regexp_macro ->
      let info = TH.info_of_tok macro.t in
      change_tok macro (TIdent_MacroDecl (Tok.content_of_tok info, info));

      find_macro_lineparen xs
  (* on multiple lines *)
  | Line [ PToken { t = Tstatic _ } ]
    :: Line
         [
           PToken ({ t = TIdent (s, _) } as macro);
           Parenthised (_, _);
           PToken { t = TPtVirg _ };
         ]
    :: xs
    when s ==~ regexp_macro ->
      let info = TH.info_of_tok macro.t in
      change_tok macro (TIdent_MacroDecl (Tok.content_of_tok info, info));

      find_macro_lineparen xs
  (* linuxext: ex: DECLARE_BITMAP();
   *
   * Here I use regexp_declare and not regexp_macro because
   * Sometimes it can be a FunCallMacro such as DEBUG(foo());
   * Here we don't have the preceding 'static' so only way to
   * not have positive is to restrict to .*DECLARE.* macros.
   *
   * but there is a grammar rule for that, so don't need this case anymore
   * unless the parameter of the DECLARE_xxx are wierd and can not be mapped
   * on a argument_list
   *)
  | Line
      [
        PToken ({ t = TIdent (s, _) } as macro);
        Parenthised (_, _);
        PToken { t = TPtVirg _ };
      ]
    :: xs
    when s ==~ regexp_declare ->
      let info = TH.info_of_tok macro.t in
      change_tok macro (TIdent_MacroDecl (Tok.content_of_tok info, info));

      find_macro_lineparen xs
  (* toplevel macros.
   * module_init(xxx)
   *
   * Could also transform the TIdent in a TMacroTop but can have false
   * positive, so easier to just change the TCPar and so just solve
   * the end-of-stream pb of ocamlyacc
   *)
  | Line
      ([
         PToken ({ t = TIdent (_s, _ii); col = col1; where = ctx } as _macro);
         Parenthised (_, info_parens);
       ] as _line1)
    :: xs
    when col1 =|= 0 ->
      let condition =
        (* to reduce number of false positive *)
        match xs with
        | Line (PToken ({ col = col2 } as other) :: _restline2) :: _ -> (
            TH.is_eof other.t
            || col2 =|= 0
               &&
               match other.t with
               | TOBrace _ -> false (* otherwise would match funcdecl *)
               | TCBrace _ when List_.hd_exn "empty context" ctx <> InFunction
                 ->
                   false
               | TPtVirg _
               | TCol _ ->
                   false
               | tok when TH.is_binary_operator tok -> false
               | _ -> true)
        | _ -> false
      in
      (if condition then
         (* just to avoid the end-of-stream pb of ocamlyacc  *)
         let tcpar = Common2.list_last info_parens in
         change_tok tcpar (TCPar_EOL (TH.info_of_tok tcpar.t))
         (*macro.t <- TMacroTop (s, TH.info_of_tok macro.t);*));
      find_macro_lineparen xs
  (* macro with parameters
   * ex: DEBUG()
   *     return x;
   *)
  | Line
      ([
         PToken ({ t = TIdent (_s, _ii); col = col1; where = ctx } as macro);
         Parenthised (xxs, info_parens);
       ] as _line1)
    :: (Line (PToken ({ col = col2 } as other) :: _restline2) as line2)
    :: xs
  (* when s ==~ regexp_macro *) ->
      let condition =
        (col1 =|= col2
        &&
        match other.t with
        | TOBrace _ -> false (* otherwise would match funcdecl *)
        | TCBrace _ when List_.hd_exn "empty context" ctx <> InFunction -> false
        | TPtVirg _
        | TCol _ ->
            false
        | tok when TH.is_binary_operator tok -> false
        | _ -> true)
        || col2 <= col1
           &&
           match other.t with
           | TCBrace _ when List_.hd_exn "empty context" ctx =*= InFunction ->
               true
           | Treturn _ -> true
           | Tif _ -> true
           | Telse _ -> true
           | _ -> false
      in

      if condition then
        if col1 =|= 0 then ()
        else (
          change_tok macro (TIdent_MacroStmt (TH.info_of_tok macro.t));
          [ Parenthised (xxs, info_parens) ]
          |> iter_token_paren (set_as_comment Token_cpp.CppMacro));

      find_macro_lineparen (line2 :: xs)
  (* linuxext:? single macro
   * ex: LOCK
   *     foo();
   *     UNLOCK
   *)
  | Line
      ([ PToken ({ t = TIdent (_s, _ii); col = col1; where = ctx } as macro) ]
       as _line1)
    :: (Line (PToken ({ col = col2 } as other) :: _restline2) as line2)
    :: xs ->
      (* when s ==~ regexp_macro *)
      let condition =
        (col1 =|= col2 && col1 <> 0
        &&
        (* otherwise can match typedef of fundecl*)
        match other.t with
        | TPtVirg _ -> false
        | TOr _ -> false
        | TCBrace _ when List_.hd_exn "empty context" ctx <> InFunction -> false
        | tok when TH.is_binary_operator tok -> false
        | _ -> true)
        || col2 <= col1
           &&
           match other.t with
           | TCBrace _ when List_.hd_exn "empty context" ctx =*= InFunction ->
               true
           | Treturn _ -> true
           | Tif _ -> true
           | Telse _ -> true
           | _ -> false
      in

      if condition then
        change_tok macro (TIdent_MacroStmt (TH.info_of_tok macro.t));

      find_macro_lineparen (line2 :: xs)
  | _x :: xs -> find_macro_lineparen xs

(*****************************************************************************)
(* #Define tobrace init *)
(*****************************************************************************)

let is_init tok2 tok3 =
  match (tok2.t, tok3.t) with
  | TInt _, TComma _ -> true
  | TString _, TComma _ -> true
  | TIdent _, TComma _ -> true
  | _ -> false

let find_define_init_brace_paren xs =
  let rec aux xs =
    match xs with
    | [] -> ()
    (* mainly for firefox *)
    | PToken { t = TDefine _ }
      :: PToken { t = TIdent_Define (_s, _) }
      :: PToken ({ t = TOBrace i1 } as tokbrace)
      :: PToken tok2
      :: PToken tok3
      :: xs ->
        if is_init tok2 tok3 then change_tok tokbrace (TOBrace_DefineInit i1);
        aux xs
    (* mainly for linux, especially in sound/ *)
    | PToken { t = TDefine _ }
      :: PToken { t = TIdent_Define (s, _); col = c }
      :: Parenthised (_, { col = c2; _ } :: _)
      :: PToken ({ t = TOBrace i1 } as tokbrace)
      :: PToken tok2
      :: PToken tok3
      :: xs
      when c2 =|= c + String.length s ->
        if is_init tok2 tok3 then change_tok tokbrace (TOBrace_DefineInit i1);

        aux xs
    (* ugly: for plan9, too general? *)
    | PToken { t = TDefine _ }
      :: PToken { t = TIdent_Define (_s, _) }
      :: Parenthised (_xxx, _)
      :: PToken ({ t = TOBrace i1 } as tokbrace)
           (* can be more complex expression than just an int, like (b)&... *)
      :: Parenthised (_, _)
      :: PToken { t = TAnd _ | TOr _; _ }
      :: xs ->
        change_tok tokbrace (TOBrace_DefineInit i1);
        aux xs
    (* recurse *)
    | PToken _ :: xs -> aux xs
    | Parenthised (_, _) :: xs ->
        (* not need for tobrace init:
         *  xxs +> List.iter aux;
         *)
        aux xs
  in
  aux xs
