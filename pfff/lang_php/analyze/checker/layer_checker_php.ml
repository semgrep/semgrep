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

module E = Error_php
module Ent = Entity_code
open Error_php

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Structure similar to other layer generator.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* ugly: there is some duplication with Error_php.error
 * coupling: with the Error_php.error type 
 *)
let properties = [

  "eUseOfUndefinedVariable", "red" ;
  "eUnusedVariable-Local", "purple";
  "eDeadStatement", "salmon";

  (* ugly: coupling with scope_code.ml *)
(* commented for now, less important
  "eUnusedVariable-Global", "green";
  "eUnusedVariable-Local", "green";
  "eUnusedVariable-Param", "green";
  "eUnusedVariable-Static", "green";
  "eUnusedVariable-Class", "green";
  "eUnusedVariable-LocalExn", "green";
  "eUnusedVariable-LocalIterator", "green";
  "eUnusedVariable-ListBinded", "green";
  "eUnusedVariable-NoScope", "green";
*)

  (* ugly: coupling with entity_php.ml
   * don't forget to copy php_stdlib/ to the directory
   * you want to analyze, otherwise you will get lots
   * of such undefined-Xxx errors
   *)
  "eUndefinedEntity-function",    "blue";
  "eUndefinedEntity-class",    "blue";
(*
  "eUndefinedEntity-method",    "blue";
*)

(*
  "eMultiDefinedEntity-function", "blue2";
  "eMultiDefinedEntity-class", "blue2";
  "eMultiDefinedEntity-method", "blue2";
*)

  "eTooManyArguments", "blue3";
  "eNotEnoughArguments", "blue4";

  (* ugly: coupling with error_code.ml *)
  "eWrongKeywordArgument-Bad", "yellow";
  "eWrongKeywordArgument-ReallyBad", "yellow3";
  "eWrongKeywordArgument-ReallyReallyBad", "yellow4";

(* commented for now
  "eUseOfUndefinedMember", "cyan";
*)
  "eUglyGlobalDynamic", "cyan";
  "eWeirdForeachNoIteratorVar", "cyan";
  "eCallingStaticMethodWithoutQualifier", "cyan";
  "eCallingMethodWithQualifier", "cyan";

  "eDeadBreak", "tan1";
  "eDeadReturn", "tan2";
  "eCfgError", "tan";

  "eFileNotFound", "tan";
  "eInjection", "tan";

  "eAssignInBooleanContext", "cyan";

  "eOther", "red";
]

(*****************************************************************************)
(* Code *)
(*****************************************************************************)

let info_of_error_and_kind err =

  let kind = 
    match err.E.typ with
  | UndefinedEntity (kind, _) -> 
      "eUndefinedEntity-" ^ Entity_php.string_of_id_kind kind
  | MultiDefinedEntity (kind, _, _) ->
      "eMultiDefinedEntity-" ^ Entity_php.string_of_id_kind kind
  | UndefinedClassWhileLookup (_) -> 
      "eUndefinedEntity-" ^ 
        Entity_php.string_of_id_kind (Ent.Class)
  | UndefinedMethodInAbstractClass (_) -> 
      "eUndefinedEntity-" ^ 
        Entity_php.string_of_id_kind (Ent.Method)

  | TooManyArguments _ ->"eTooManyArguments"
  | NotEnoughArguments _ ->"eNotEnoughArguments"

  | WrongKeywordArgument (_, _, severity) ->
      "eWrongKeywordArgument-" ^ (Error_php.string_of_severity2 severity)
  | CallingStaticMethodWithoutQualifier _ ->
      "eCallingStaticMethodWithoutQualifier"
  | CallingMethodWithQualifier _ ->
      "eCallingMethodWithQualifier"
  | PassingUnexpectedRef -> 
      "eOther"
  | FormatStringMismatch _ ->
      "eOther"
  | UnnecessaryTernaryIf ->
      "eOther"
  | MicroCloneCondExp _ ->
      "eMicroCloneCondExp"
  | UndefinedRequiredField _ ->
      "eOther"
  | UseOfUndefinedVariable _ 
  | UseOfUndefinedVariableInLambda _
    -> 
      "eUseOfUndefinedVariable"
  | UnusedVariable (_, scope) ->
      "eUnusedVariable-" ^ Scope_code.string_of_scope scope

  | UseOfUndefinedMember _ ->"eUseOfUndefinedMember"
  | UglyGlobalDynamic -> "eUglyGlobalDynamic"
  | DynamicCode -> "eUglyGlobalDynamic"
  | WeirdForeachNoIteratorVar -> "eWeirdForeachNoIteratorVar"

  | CfgError (Controlflow_build_php.DeadCode node_kind) ->
      (match node_kind with
      | Controlflow_php.Break -> "eDeadBreak"
      | Controlflow_php.Return _ -> "eDeadReturn"
      | _ -> "eDeadStatement"
      )
  | CfgError ( _) ->
      "eCfgError"
(*  | CfgPilError ( _) -> "eCfgError" *)
  | FileNotFound ( _) ->
      "eFileNotFound"
  | AssignInBooleanContext ->
      "eAssignInBooleanContext"
  | Injection (_) ->
      "eInjection"
  | CaseWithSemiColon | CaseSensitivityKeyword
  | InterfaceMethodWithBody
  | KeywordArgumentForRef
  | IncludeUnresolved
  | WrongLvalue
  | UseOfPlusNotDotForStrings
      ->
      "eOther"
  in
  err.loc, kind

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let gen_layer ~root ~output errors = 

  let infos = errors |> List.map info_of_error_and_kind in

  let layer = Layer_code.simple_layer_of_parse_infos 
    ~title:"PHP Bugs"
    ~description:"Use scheck_php; many false positives though"
    ~root infos properties in
  pr2 ("generating layer in " ^ output);
  Layer_code.save_layer layer output;
  ()
