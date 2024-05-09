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
module Flag_cpp = Flag_parsing_cpp
module TH = Token_helpers_cpp
open Parser_cpp
open Token_views_cpp
module Log = Log_parser_cpp.Log

(*****************************************************************************)
(* Helpers  *)
(*****************************************************************************)

(*
 * In the following, there are some harcoded names of types or macros
 * but they are not used by our heuristics! They are just here to
 * enable to detect false positive by printing only the typedef/macros
 * that we don't know yet. If we print everything, then we can easily
 * get lost with too much verbose tracing information. So those
 * functions "filter" some messages. So our heuristics are still good,
 * there is no more (or not that much) hardcoded linux stuff.
 *)
let msg_gen is_known printer s =
  if not !Flag_cpp.filter_msg then printer s
  else if not (is_known s) then printer s

let pos ii = Tok.stringpos_of_tok ii

(*****************************************************************************)
(* Some debugging functions  *)
(*****************************************************************************)

let pr2_pp s = if !Flag_cpp.debug_pp then Log.debug (fun m -> m "PP-%s" s)

let pr2_cplusplus s =
  if !Flag_cpp.debug_cplusplus then Log.debug (fun m -> m "C++-%s" s)

let pr2_typedef s =
  if !Flag_cpp.debug_typedef then Log.debug (fun m -> m "TYPEDEF-%s" s)

let msg_change_tok tok =
  match tok with
  (* mostly in parsing_hacks_define.ml *)
  | TIdent_Define (_s, _ii) -> ()
  | TOPar_Define _ii -> ()
  | TCommentNewline_DefineEndOfMacro _ -> ()
  (* mostly in parsing_hacks.ml *)
  | TIdent_Typedef (s, ii) ->
      (* todo? also do LP.add_typedef_root s ??? *)
      s
      |> msg_gen
           (fun s ->
             match s with
             | "u_char"
             | "u_short"
             | "u_int"
             | "u_long"
             | "u8"
             | "u16"
             | "u32"
             | "u64"
             | "s8"
             | "s16"
             | "s32"
             | "s64"
             | "__u8"
             | "__u16"
             | "__u32"
             | "__u64" ->
                 true
             | "acpi_handle"
             | "acpi_status" ->
                 true
             | "FILE"
             | "DIR" ->
                 true
             | s when s =~ ".*_t$" -> true
             | _ -> false)
           (fun s -> pr2_typedef (spf "promoting %s at %s " s (pos ii)))
  (* mostly in parsing_hacks_pp.ml *)
  (* cppext: *)
  | TComment_Pp (directive, ii) -> (
      let s = Tok.content_of_tok ii in
      match (directive, s) with
      | Token_cpp.CppMacro, _ -> pr2_pp (spf "MACRO: commented at %s" (pos ii))
      | Token_cpp.CppDirective, _ when s =~ "#define.*" ->
          pr2_pp (spf "DEFINE: commented at %s" (pos ii))
      | Token_cpp.CppDirective, _ when s =~ "#include.*" ->
          pr2_pp (spf "INCLUDE: commented at %s" (pos ii))
      | Token_cpp.CppDirective, _ when s =~ "#if.*" ->
          pr2_pp (spf "IFDEF: commented at %s" (pos ii))
      | Token_cpp.CppDirective, _ when s =~ "#undef.*" ->
          pr2_pp (spf "UNDEF: commented at %s" (pos ii))
      | Token_cpp.CppDirective, _ ->
          pr2_pp (spf "OTHER: commented directive at %s" (pos ii))
      | _ ->
          (* todo? *)
          ())
  | TOBrace_DefineInit ii -> pr2_pp (spf "DEFINE: initializer at %s" (pos ii))
  | TIdent_MacroString ii ->
      let s = Tok.content_of_tok ii in
      s
      |> msg_gen
           (fun s ->
             match s with
             | "REVISION"
             | "UTS_RELEASE"
             | "SIZE_STR"
             | "DMA_STR" ->
                 true
             (* s when s =~ ".*STR.*" -> true  *)
             | _ -> false)
           (fun s -> pr2_pp (spf "MACRO: string-macro %s at %s " s (pos ii)))
  | TIdent_MacroStmt ii -> pr2_pp (spf "MACRO: stmt-macro at %s" (pos ii))
  | TIdent_MacroDecl (s, ii) ->
      s
      |> msg_gen
           (fun s ->
             match s with
             | "DECLARE_MUTEX"
             | "DECLARE_COMPLETION"
             | "DECLARE_RWSEM"
             | "DECLARE_WAITQUEUE"
             | "DECLARE_WAIT_QUEUE_HEAD"
             | "DEFINE_SPINLOCK"
             | "DEFINE_TIMER"
             | "DEVICE_ATTR"
             | "CLASS_DEVICE_ATTR"
             | "DRIVER_ATTR"
             | "SENSOR_DEVICE_ATTR"
             | "LIST_HEAD"
             | "DECLARE_WORK"
             | "DECLARE_TASKLET"
             | "PORT_ATTR_RO"
             | "PORT_PMA_ATTR"
             | "DECLARE_BITMAP" ->
                 true
                 (*
              | s when s =~ "^DECLARE_.*" -> true
              | s when s =~ ".*_ATTR$" -> true
              | s when s =~ "^DEFINE_.*" -> true
              | s when s =~ "NS_DECL.*" -> true
            *)
             | _ -> false)
           (fun _s -> pr2_pp (spf "MACRO: macro-declare at %s" (pos ii)))
  | Tconst_MacroDeclConst ii -> pr2_pp (spf "MACRO: retag const at %s" (pos ii))
  | TAny_Action ii -> pr2_pp (spf "ACTION: retag at %s" (pos ii))
  | TCPar_EOL ii -> pr2_pp (spf "MISC: retagging ) %s" (pos ii))
  (* mostly in parsing_hacks_cpp.ml *)
  (* c++ext: *)
  | TComment_Cpp (directive, ii) -> (
      let s = Tok.content_of_tok ii in
      match (directive, s) with
      | Token_cpp.CplusplusTemplate, _ ->
          pr2_cplusplus (spf "COM-TEMPLATE: commented at %s" (pos ii))
      | Token_cpp.CplusplusQualifier, _ ->
          pr2_cplusplus (spf "COM-QUALIFIER: commented at %s" (pos ii)))
  | TOPar_CplusplusInit ii ->
      pr2_cplusplus (spf "constructor initializer at %s" (pos ii))
  | TOCro_new ii
  | TCCro_new ii ->
      pr2_cplusplus (spf "new [] at %s" (pos ii))
  | TInf_Template ii
  | TSup_Template ii ->
      pr2_cplusplus (spf "template <> at %s" (pos ii))
  | Tchar_Constr ii
  | Tint_Constr ii
  | Tfloat_Constr ii
  | Tdouble_Constr ii
  | Tshort_Constr ii
  | Tlong_Constr ii
  | Tbool_Constr ii
  | Tunsigned_Constr ii
  | Tsigned_Constr ii ->
      pr2_cplusplus (spf "constructed object builtin at %s" (pos ii))
  | TIdent_TypedefConstr (s, ii) ->
      pr2_cplusplus (spf "constructed object %s at %s" s (pos ii))
  | TIdent_ClassnameInQualifier (s, ii) ->
      pr2_cplusplus (spf "CLASSNAME: in qualifier context %s at %s " s (pos ii))
  | TIdent_Constructor (s, ii) ->
      pr2_cplusplus (spf "CONSTRUCTOR: found %s at %s " s (pos ii))
  | TIdent_Templatename (s, ii) ->
      pr2_cplusplus (spf "TEMPLATENAME: found %s at %s" s (pos ii))
  | TColCol_BeforeTypedef ii ->
      pr2_typedef (spf "RECLASSIF colcol to colcol2 at %s" (pos ii))
  | TIdent_ClassnameInQualifier_BeforeTypedef (s, ii) ->
      pr2_typedef (spf "RECLASSIF class in qualifier %s at %s" s (pos ii))
  | TIdent_TemplatenameInQualifier_BeforeTypedef (s, ii) ->
      pr2_typedef (spf "RECLASSIF template in qualifier %s at %s" s (pos ii))
  | _ -> raise Todo

let msg_context t ctx =
  let ctx_str =
    match ctx with
    | InParameter -> "InParameter"
    | InArgument -> "InArgument"
    | _ -> raise Impossible
  in
  pr2_cplusplus (spf "CONTEXT: %s at %s" ctx_str (pos (TH.info_of_tok t)))

let change_tok extended_tok tok =
  msg_change_tok tok;

  (* otherwise parse_c will be lost if don't find a EOF token
   * why? because paren detection had a pb because of
   * some ifdef-exp?
   *)
  if TH.is_eof extended_tok.t then
    Log.warn (fun m -> m "WEIRD, I try to tag an EOF token as something else")
  else extended_tok.t <- tok

let fresh_tok tok =
  msg_change_tok tok;
  tok

(* normally the caller have first filtered the set of tokens to have
 * a clearer "view" to work on
 *)
let set_as_comment cppkind x =
  assert (not (TH.is_real_comment x.t));
  change_tok x (TComment_Pp (cppkind, TH.info_of_tok x.t))

(*****************************************************************************)
(* The regexp and basic view definitions *)
(*****************************************************************************)

(*
val regexp_macro: Str.regexp
val regexp_annot: Str.regexp
val regexp_declare: Str.regexp
val regexp_foreach: Str.regexp
val regexp_typedef: Str.regexp
*)

(* opti: better to built then once and for all, especially regexp_foreach *)

let regexp_macro = Str.regexp "^[A-Z_][A-Z_0-9]*$"

(* linuxext: *)
let regexp_declare = Str.regexp ".*DECLARE.*"

(* firefoxext: *)
let regexp_ns_decl_like =
  Str.regexp
    ("\\(" ^ "NS_DECL_\\|NS_DECLARE_\\|NS_IMPL_\\|"
   ^ "NS_IMPLEMENT_\\|NS_INTERFACE_\\|NS_FORWARD_\\|NS_HTML_\\|"
   ^ "NS_DISPLAY_\\|NS_IMPL_\\|"
   ^ "TX_DECL_\\|DOM_CLASSINFO_\\|NS_CLASSINFO_\\|IMPL_INTERNAL_\\|"
   ^ "ON_\\|EVT_\\|NS_UCONV_\\|NS_GENERIC_\\|NS_COM_" ^ "\\).*")
