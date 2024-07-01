(* Yoann Padioleau
 *
 * Copyright (C) 2002-2008 Yoann Padioleau
 * Copyright (C) 2011-2014 Facebook
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
module TV = Token_views_cpp
module TH = Token_helpers_cpp
open Parser_cpp
open Token_views_cpp
open Parsing_hacks_lib

[@@@warning "-9"]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This file gathers parsing heuristics related to typedefs.
 *
 * C does not have a context-free grammar; C requires the parser to know when
 * an ident corresponds to a typedef or an ident. This normally means that
 * we must call cpp on the file and have the lexer and parser cooperate
 * to remember what is what. In lang_cpp/ we want to parse as-is,
 * which means we need to infer back whether an identifier is
 * a typedef or not.
 *
 * In this module we use a view that is more convenient for
 * typedefs detection. We got rid of:
 *  - template arguments (see find_template_commentize())
 *  - qualifiers (see find_qualifier_commentize)
 *  - differences between & and * (filter_for_typedef() below)
 *  - differences between TIdent and TOperator,
 *  - const, volatile, restrict keywords
 *  - TODO merge multiple ** or *& or whatever
 *
 * history:
 *  - We used to make the lexer and parser cooperate in a lexerParser.ml file.
 *    However, this was not enough because of declarations such as 'acpi acpi;'
 *    and so we had to enable/disable the ident->typedef mechanism
 *    which requires even more lexer/parser cooperation
 *  - This forced-cooperation was ugly so we switched to
 *    a typedef "inference" mechanism
 *  - we refined the typedef inference to sometimes use InParameter hint
 *    and more contextual information from token_views_context.ml
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let look_like_multiplication_context tok_before =
  match tok_before with
  | TEq _
  | TAssign _
  | TWhy _
  | Treturn _
  | TDot _
  | TPtrOp _
  | TPtrOpStar _
  | TDotStar _
  | TOCro _ ->
      true
  | tok when TH.is_binary_operator_except_star tok -> true
  | _ -> false

let look_like_declaration_context tok_before =
  match tok_before with
  | TOBrace _
  | TPtVirg _
  | TCommentNewline_DefineEndOfMacro _
  | TInclude _
  (* no!! | TCBrace _, I think because of nested struct so can have
   * struct { ... } v;
   *) ->
      true
  | _ when TH.is_privacy_keyword tok_before -> true
  | _ -> false

(*****************************************************************************)
(* Better View *)
(*****************************************************************************)

let filter_for_typedef multi_groups =
  (* a sentinel, which helps a few typedef heuristics which look
   * for a token before which would not work for the first toplevel
   * declaration.
   *)
  let multi_groups =
    Tok (mk_token_fake (TPtVirg (Tok.unsafe_fake_tok ";"))) :: multi_groups
  in

  let _template_args = ref [] in

  (* remove template and other things
   * less: right now this is less useful because we actually
   * comment template args in a previous pass, but at some point this
   * will be useful.
   *)
  let rec aux xs =
    xs
    |> List_.filter_map (function
         | TV.Angle (_, _, _) ->
             (* todo: analayze xs!! add in _template_args
              * todo: add the t1,t2 around xs to have
              *  some sentinel for the typedef heuristics patterns
              *  who often look for the token just before the typedef.
              *)
             None
         | TV.Braces (t1, xs, t2) -> Some (TV.Braces (t1, aux xs, t2))
         | TV.Parens (t1, xs, t2) -> Some (TV.Parens (t1, aux xs, t2))
         (* remove other noise for the typedef inference *)
         | TV.Tok t1 -> (
             match t1.TV.t with
             (* const is a strong signal for having a typedef, so why skip it?
              * because it forces to duplicate rules. We need to infer
              * the type anyway even when there is no const around.
              * todo? maybe could do a special pass first that infer typedef
              * using only const rules, and then remove those const so
              * have best of both worlds.
              *)
             | Tconst _
             | Tvolatile _
             | Trestrict _ ->
                 None
             | Tregister _
             | Tstatic _
             | Tauto _
             | Textern _
             | Ttypedef _ ->
                 None
             | Tvirtual _
             | Tfriend _
             | Tinline _
             | Tmutable _ ->
                 None
             (* let's transform all '&' into '*'
              * todo: need propagate also the where?
              *)
             | TAnd ii -> Some (TV.Tok (mk_token_extended (TMul ii)))
             | TAndLog ii -> Some (TV.Tok (mk_token_extended (TMul ii)))
             (* and operator into TIdent
              * TODO: skip the token just after the operator keyword?
              * could help some heuristics too
              *)
             | Toperator ii ->
                 Some (TV.Tok (mk_token_extended (TIdent ("operator", ii))))
             | _ -> Some (TV.Tok t1)))
  in
  let xs = aux multi_groups in
  (* todo: look also for _template_args *)
  [ TV.tokens_of_multi_grouped xs ]

(*****************************************************************************)
(* Heuristics *)
(*****************************************************************************)
(* Below we assume a view without:
 *  - comments and cpp-directives
 *  - template stuff and qualifiers (but not TIdent_ClassnameAsQualifier)
 *  - const/volatile/restrict
 *  - & => *
 *
 * With such a view we can write less patterns.
 *
 * Note that qualifiers are slightly less important to filter because
 * most of the heuristics below look for tokens after the ident
 * and qualifiers are usually before.
 *
 * todo: do it on multi view? all those rules with TComma and TOPar
 * are ugly.
 *)
let find_typedefs xxs =
  let rec aux xs =
    match xs with
    | [] -> ()
    (* struct x ...
     * those identifiers (called tags) must not be transformed in typedefs *)
    | { t = Tstruct _ | Tunion _ | Tenum _ | Tclass _ }
      :: { t = TIdent _ }
      :: xs ->
        aux xs
    (* xx yy *)
    | ({ t = TIdent (s, i1) } as tok1) :: { t = TIdent _ } :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux xs
    (* xx ( *yy )( *)
    | ({ t = TIdent (s, i1) } as tok1)
      :: { t = TOPar _ }
      :: { t = TMul _ }
      :: { t = TIdent _ }
      :: { t = TCPar _ }
      :: ({ t = TOPar _ } as tok2)
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux (tok2 :: xs)
    (* xx* ( *yy )( *)
    | ({ t = TIdent (s, i1) } as tok1)
      :: { t = TMul _ }
      :: { t = TOPar _ }
      :: { t = TMul _ }
      :: { t = TIdent _ }
      :: { t = TCPar _ }
      :: ({ t = TOPar _ } as tok2)
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux (tok2 :: xs)
    (* xx ( *yy[x] )( *)
    | ({ t = TIdent (s, i1) } as tok1)
      :: { t = TOPar _ }
      :: { t = TMul _ }
      :: { t = TIdent _ }
      :: { t = TOCro _ }
      :: _
      :: { t = TCCro _ }
      :: { t = TCPar _ }
      :: ({ t = TOPar _ } as tok2)
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux (tok2 :: xs)
    (* xx* ( *yy[x] )( *)
    | ({ t = TIdent (s, i1) } as tok1)
      :: { t = TMul _ }
      :: { t = TOPar _ }
      :: { t = TMul _ }
      :: { t = TIdent _ }
      :: { t = TOCro _ }
      :: _
      :: { t = TCCro _ }
      :: { t = TCPar _ }
      :: ({ t = TOPar _ } as tok2)
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux (tok2 :: xs)
    (* xx ( *yy[]) *)
    | ({ t = TIdent (s, i1) } as tok1)
      :: { t = TOPar _ }
      :: { t = TMul _ }
      :: { t = TIdent _ }
      :: { t = TOCro _ }
      :: { t = TCCro _ }
      :: { t = TCPar _ }
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux xs
    (* + xx * yy *)
    | { t = tok_before }
      :: { t = TIdent (_s, _) }
      :: { t = TMul _ }
      :: { t = TIdent _ }
      :: xs
      when look_like_multiplication_context tok_before ->
        aux xs
    (* { xx * yy *)
    | { t = tok_before }
      :: ({ t = TIdent (s, i1) } as tok1)
      :: { t = TMul _ }
      :: { t = TIdent _ }
      :: xs
      when look_like_declaration_context tok_before ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux xs
    (* } xx * yy *)
    (* because TCBrace is not anymore in look_like_declaration_context *)
    | { t = TCBrace _ }
      :: ({ t = TIdent (s, i1) } as tok1)
      :: { t = TMul _ }
      :: { t = TIdent _ }
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux xs
    (* xx * yy
     * could be a multiplication too, so need InParameter guard/
     * less: the InParameter has some FPs, so maybe better to
     * rely on the spacing hint, see the rule below.
     *)
    | ({ t = TIdent (s, i1); where = InParameter :: _ } as tok1)
      :: { t = TMul _ }
      :: { t = TIdent _ }
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux xs
    (* xx *yy *)
    | ({ t = TIdent (s, i1); col = c0 } as tok1)
      :: { t = TMul _; col = c1 }
      :: { t = TIdent _; col = c2 }
      :: xs
      when c2 =|= c1 + 1 && c1 >= c0 + String.length s + 1 ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux xs
    (* xx* yy *)
    | ({ t = TIdent (s, i1); col = c0 } as tok1)
      :: { t = TMul _; col = c1 }
      :: { t = TIdent _; col = c2 }
      :: xs
      when c1 =|= c0 + String.length s && c2 >= c1 + 2 ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux xs
    (* xx ** yy
     * less could be a multiplication too, but with less probability
     * TODO: now that do & -> * and && -> *, make sure it's not an FP!
     * xx * *yy is probably a valid multiplication!
     * xx && * yy is probably an and
     *)
    | ({ t = TIdent (s, i1) } as tok1)
      :: { t = TMul _ }
      :: { t = TMul _ }
      :: { t = TIdent _ }
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux xs
    (* (xx) yy   and not a if/while before '('  (and yy can also be a constant) *)
    | { t = tok1 }
      :: { t = TOPar _ }
      :: ({ t = TIdent (s, i1) } as tok3)
      :: { t = TCPar _ }
      :: {
           t =
             ( TIdent _ | TInt _ | TString _ | TFloat _ | Tnullptr _ | TTilde _
             | TOPar _ );
         }
      :: xs
      when not (TH.is_stuff_taking_parenthized tok1) (*  && line are the same?*)
      ->
        change_tok tok3 (TIdent_Typedef (s, i1));
        (* todo? recurse on bigger ? *)
        aux xs
    (* todo:  = (xx) ..., |= (xx) ...,   (xx)~, ... *)
    (* (xx){  gccext: kenccext:  *)
    | { t = tok1 }
      :: { t = TOPar _ }
      :: ({ t = TIdent (s, i1) } as tok3)
      :: { t = TCPar _ }
      :: ({ t = TOBrace _ } as tok5)
      :: xs
      when not (TH.is_stuff_taking_parenthized tok1) ->
        change_tok tok3 (TIdent_Typedef (s, i1));
        aux (tok5 :: xs)
    (* (xx * ),    not that pointer function are ( *xx ), so star before.
     * TODO: does not really need the closing paren?
     * TODO: check that not InParameter or InArgument?
     *)
    | { t = TOPar _ }
      :: ({ t = TIdent (s, i1) } as tok3)
      :: { t = TMul _ }
      :: { t = TCPar _ }
      :: xs ->
        change_tok tok3 (TIdent_Typedef (s, i1));
        aux xs
    (* (xx ** ) *)
    | { t = TOPar _ }
      :: ({ t = TIdent (s, i1) } as tok3)
      :: { t = TMul _ }
      :: { t = TMul _ }
      :: { t = TCPar _ }
      :: xs ->
        change_tok tok3 (TIdent_Typedef (s, i1));
        aux xs
    (* xx* [,)]
     * don't forget to recurse by reinjecting the comma or closing paren
     *)
    | ({ t = TIdent (s, i1) } as tok1)
      :: { t = TMul _ }
      :: ({ t = TComma _ | TCPar _ } as x)
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux (x :: xs)
    (* xx** [,)] *)
    | ({ t = TIdent (s, i1) } as tok1)
      :: { t = TMul _ }
      :: { t = TMul _ }
      :: ({ t = TComma _ | TCPar _ } as x)
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux (x :: xs)
    (* xx*** [,)] *)
    | ({ t = TIdent (s, i1) } as tok1)
      :: { t = TMul _ }
      :: { t = TMul _ }
      :: { t = TMul _ }
      :: ({ t = TComma _ | TCPar _ } as x)
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux (x :: xs)
    (* xx*[] [,)] *)
    | ({ t = TIdent (s, i1) } as tok1)
      :: { t = TMul _ }
      :: { t = TOCro _ }
      :: { t = TCCro _ }
      :: ({ t = TComma _ | TCPar _ } as x)
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux (x :: xs)
    (* [(,] xx [),] where InParameter *)
    (* hmmm: todo: some false positives on InParameter, see mini/constants.c,
     * so now simpler to add a TIdent in the parameter_decl rule
     * Use -token_views_cpp to debug InParameter false positives issues.
     *)
    | { t = TOPar _ | TComma _ }
      :: ({ t = TIdent (s, i1); where = InParameter :: _ } as tok1)
      :: ({ t = TCPar _ | TComma _ } as tok2)
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux (tok2 :: xs)
    (* [(,] xx[X] [),] where InParameter *)
    | { t = TOPar _ | TComma _ }
      :: ({ t = TIdent (s, i1); where = InParameter :: _ } as tok1)
      :: { t = TOCro _ }
      :: _
      :: { t = TCCro _ }
      :: ({ t = TCPar _ | TComma _ } as tok2)
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux (tok2 :: xs)
    (* [(,] xx[...]  could be a array access, so need InParameter guard *)
    | { t = TOPar _ | TComma _ }
      :: ({ t = TIdent (s, i1); where = InParameter :: _ } as tok1)
      :: { t = TOCro _ }
      :: _tok
      :: { t = TCCro _ }
      :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux xs
    (* kencc-ext: xx;  where InStruct *)
    | { t = tok_before }
      :: ({ t = TIdent (s, i1) } as tok1)
      :: ({ t = TPtVirg _ } as tok2)
      :: xs
      when look_like_declaration_context tok_before ->
        (match tok1.where with
        | InClassStruct _ :: _ -> change_tok tok1 (TIdent_Typedef (s, i1))
        | _ -> ());
        aux (tok2 :: xs)
    (* sizeof(xx)    sizeof expr does not require extra parenthesis, but
     * in practice people do, so guard it with what looks_like_typedef
     *)
    | { t = Tsizeof _ }
      :: { t = TOPar _ }
      :: ({ t = TIdent (s, i1) } as tok1)
      :: { t = TCPar _ }
      :: xs
      when Token_views_context.look_like_typedef s || s =~ "^[A-Z].*" ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux xs
    (* new Xxx *)
    | { t = Tnew _ } :: ({ t = TIdent (s, i1) } as tok1) :: xs ->
        change_tok tok1 (TIdent_Typedef (s, i1));
        aux xs
    (* recurse *)
    | _ :: xs -> aux xs
  in
  xxs |> List.iter aux
