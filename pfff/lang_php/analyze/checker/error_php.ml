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

open Cst_php
module Ast = Cst_php
module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Centralize PHP errors report functions (they did the same in c--)
 * 
 * TODO: move more of the code of lint_php.mli here
 * todo: factorize in errors_code.ml
 *)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)

let strict = ref false

(* see also _errors below *)

(*****************************************************************************)
(* Type *)
(*****************************************************************************)

(* I used to have just type error = error_kind, but then I would need a
 * info_of_error() function to extract the position for each error. It 
 * is simpler to put the error location in a field and have a simple
 * error_kind (having a simple error_kind independent of location also
 * makes it easy to have "stable" errors independent of repository
 * which makes it easy to have cmf --only-new-errors working correctly).
 * 
 *)
type error = {
  typ: error_kind;
  loc: Cst_php.info;
  (* less: maybe severity should be inferred from the error_kind. That
   * way it will also avoid the need for 2 functions fatal()/warning()
   * and just have error().
   *)
  sev: severity; 
}
 (* less: Advice | Noisy | Meticulous/Pedantic ? *)
 and severity = Fatal | Warning

(* Mostly use/def kind of errors
 * (for functions/classes/constants, variables, members, files, etc)
 * 
 * coupling: if you add a constructor here, don't forget to extend
 * layer_checker_php.ml too.
 * 
 * note: try to not put structure that have position information in 
 * the type below (so use string, not Ast_php.name), so that the
 * error_kind is location independent and can be used portably as a key
 * through different repository (cf cmf --only-new-errors).
 *)
 and error_kind = 
  (* entities *)
  | UndefinedEntity of Entity_php.id_kind * string (* name *)
  | MultiDefinedEntity of Entity_php.id_kind * string (* name *) *
      (string * string) (* name * name *)
  | UndefinedClassWhileLookup of string
  | UndefinedMethodInAbstractClass of string

  (* call sites *)
  | TooManyArguments   of string (* name *) (* def *)
  | NotEnoughArguments of string (* name *) (* def *)
  | WrongKeywordArgument of (* erling's idea *)
      string (* dname *) * string (* parameter *) * severity2
  | CallingStaticMethodWithoutQualifier of string
  | CallingMethodWithQualifier of string
  | PassingUnexpectedRef (* alok's idea *)
  | KeywordArgumentForRef (* lovro's idea *)
  | FormatStringMismatch of string

  (* variables *)
  | UseOfUndefinedVariable of string (* dname *) * suggest option
  | UnusedVariable of string (* dname *) * Scope_php.phpscope
  | UseOfUndefinedVariableInLambda of string (* dname *)
  | WrongLvalue

  (* classes (could be put in UndefinedEntity (ClassMember)) *)
  | UseOfUndefinedMember of string (* name *) * suggest option
  | UndefinedRequiredField of string (* name *) * suggest option

  (* wrong include/require *)
  | FileNotFound of Common.filename
  | IncludeUnresolved

  (* types *)
  | UseOfPlusNotDotForStrings

  (* lint *)
  | AssignInBooleanContext
  | UnnecessaryTernaryIf

  (* micro clones *)
  | MicroCloneCondExp of string (* operator *) * string (* expression *)

  (* bail-out constructs *)
  | UglyGlobalDynamic
  | WeirdForeachNoIteratorVar
  | DynamicCode

  (* cfg, mostly DeadCode statements *)
  | CfgError of Controlflow_build_php.error_kind
(*  | CfgPilError of Controlflow_build_pil.error_kind *)


  (* tainting *)
  | Injection of injection_kind (* todo: * explanation (e.g. a path?) *)

  (* some code is using 'case 1;' instead of 'case 1:', ugly *)
  | CaseWithSemiColon
  (* php is case insensitive but it's better to have consistent code
   * that always use the lowercase version of a keyword
   *)
  | CaseSensitivityKeyword

  | InterfaceMethodWithBody

  (* todo: 
   *  - type errors, 
   *  - protocol errors (statistical analysis), 
   *  - etc 
   *)

  and severity2 =
   | Bad
   | ReallyBad
   | ReallyReallyBad
  and suggest = string * int (* edit distance *)
  and injection_kind = XSS | Sql | Shell

exception Error of error

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let string_of_severity2 = function
  | Bad -> "Bad" 
  | ReallyBad -> "ReallyBad"
  | ReallyReallyBad -> "ReallyReallyBad"

let string_of_suggest_opt x =
  match x with
  | None -> ""
  | Some (s, _i) -> 
      spf " (did you mean %s?)" s

let string_of_error_kind error_kind =
  match error_kind with
  | UndefinedEntity(kind, name) ->
      spf "Undefined %s %s" (Entity_php.string_of_id_kind kind) name

  | MultiDefinedEntity(kind, name, (_ex1, _ex2)) ->
     (* todo? one was declared: %s and the other %s    or use tbgs ... *)
      spf "Multiply defined %s %s"(Entity_php.string_of_id_kind kind)
        name
  | UndefinedClassWhileLookup (name) ->
      spf "Undefined class while lookup inheritance tree: %s" name
  | UndefinedMethodInAbstractClass (name) ->
      spf "Undefined method in abstract class: %s" name

  | TooManyArguments _defname ->
     (* todo? function was declared: %s     or use tbgs ... *)
      "Too many arguments"
  | NotEnoughArguments _defname ->
     (* todo? function was declared: %s    or use tbgs *)
      "Not enough arguments"
  | WrongKeywordArgument(dn, param, severity) ->
      spf "Wrong keyword argument, %s <> %s (%s)"
        dn param (string_of_severity2 severity)
  | CallingStaticMethodWithoutQualifier name ->
      spf "Calling static method %s without a qualifier" name
  | CallingMethodWithQualifier name ->
      spf "Calling non static method %s with a qualifier" name
  | PassingUnexpectedRef ->
      "passing a reference to a function not expecting one"
  | KeywordArgumentForRef ->
      "passing a keyword argument to a function expecting a reference"
  | FormatStringMismatch name ->
      spf "Number of arguments in %s does not match the format string" name

  | UseOfUndefinedVariable (dname, x) ->
      spf "Use of undeclared variable %s%s. " dname (string_of_suggest_opt x)
(*
"Declare variables prior to use (even if you are passing them as reference
    parameters). You may have misspelled this variable name.
"
*)
  | UndefinedRequiredField (dname, x) ->
      spf "Undefined required xhp field %s%s. " dname (string_of_suggest_opt x)

  | UseOfUndefinedVariableInLambda (dname) ->
      spf "Use of undeclared variable %s in lambda. " dname ^
"See http://php.net/manual/en/functions.anonymous.php and the 'use' keyword."

  | UnusedVariable (dname, scope) ->
      spf "Unused %s variable %s" (Scope_php.s_of_phpscope scope) dname
  | WrongLvalue ->
      spf "The left side of = does not appear to be an lvalue"

  | UseOfUndefinedMember (name, x) ->
      spf "Use of undefined member $%s%s" name (string_of_suggest_opt x)

  | UglyGlobalDynamic ->
      "Ugly dynamic global declaration"
  | WeirdForeachNoIteratorVar ->
      "Weird, foreach with not a var as iterator"
  | DynamicCode ->
      "This code uses dynamic features of PHP which makes the code very hard
to statically analyze. Please avoid using those features."

  | CfgError err ->
      Controlflow_build_php.string_of_error_kind err
(*  | CfgPilError err ->
      Controlflow_build_pil.string_of_error_kind err
*)

  | FileNotFound s ->
      spf "File not found %s" s
  | IncludeUnresolved ->
      spf "can not resolve statically the include/require"

  | AssignInBooleanContext ->
      "use == or add another set of parens around the assignment"
  | UnnecessaryTernaryIf ->
      "ternary if (\"?:\") is not necessary here, use the condition or its negation."

  | MicroCloneCondExp (op,exp) ->
      spf "Boolean operator %s contains duplicate expression %s." op exp

  | Injection kind ->
      let s =
        match kind with
        | XSS -> "XSS"
        | Sql -> "Sql"
        | Shell -> "Shell"
      in
      spf "%s injection" s
  | CaseWithSemiColon ->
      "Use a colon not a semicolon"
  | CaseSensitivityKeyword ->
      "Use the lowercase version of the keyword"
  | InterfaceMethodWithBody ->
      "The method of an interface should not have a body, use ';' not '{ }'"
  | UseOfPlusNotDotForStrings ->
      "Use '.' not '+' to concatenate strings"

(* note that the output is emacs compile-mode compliant *)
let string_of_error error =
  (* todo? use severity? *)
  let info = Parse_info.token_location_of_info error.loc in
  spf "%s:%d:%d: CHECK: %s" 
    info.Parse_info.file info.Parse_info.line info.Parse_info.column
    (string_of_error_kind error.typ)

let report_error err = 
  pr2 (string_of_error err)

(*****************************************************************************)
(* Global bis *)
(*****************************************************************************)

(* Ugly. Common.save_excursion can help reduce the problems that can
 * come from using a global.
 *)
let _errors = ref []

(* todo? let exn_when_error *)

let fatal loc err =
  Common.push { loc = loc; typ = err; sev = Fatal } _errors
let warning loc err = 
  Common.push { loc = loc; typ = err; sev = Warning } _errors

let report_all_errors () = 
  !_errors |> List.rev |> List.iter report_error

(*****************************************************************************)
(* Ranking *)
(*****************************************************************************)

type rank =
 (* Too many FPs for now. Not applied even in strict mode. *)
 | Never
 (* Usually a few FPs or too many of them. Only applied in strict mode. *)
 | OnlyStrict
 | Less
 | Ok
 | Important
 | ReallyImportant

let score_of_rank = function
  | Never -> 0
  | OnlyStrict -> 1
  | Less -> 2
  | Ok -> 3
  | Important -> 4
  | ReallyImportant -> 5

let rank_of_error_kind err_kind =
  match err_kind with
  (* less: this error requires to inspect the inheritance tree to truly know
   * if the class variable is unused; need lookup parents too.
   *)
  | UnusedVariable (_, Scope_code.Class) -> Never
  (* they should put some _unused ... but don't want to fight this battle *)
  | UnusedVariable (_, Scope_code.Param) -> OnlyStrict
  | UnusedVariable (_, Scope_code.LocalIterator) -> OnlyStrict
  (* some FPs when variable passed by ref I think *)
  | UnusedVariable (_, Scope_code.Closed) -> Never

  (* some FPs with empty *)
  | UnusedVariable (_, Scope_code.Local) -> OnlyStrict

  (* todo: would like to bump that ... but run only in strict mode for now *)
  | UnusedVariable (_, _) -> Less

  (* This used to have many false positives because Lint_php.db_of_defs
   * did not handle require_module_lazy modules. I used to disable it
   * because it was not so important because UndefinedXxx checks were
   * already done by cmf. But once we had the flib-map based
   * entity_finder, this check was useful again to make sure
   * that we check the #args for all functions, and if we're not, then
   * at least that we report the function as an UndefinedEntity. It's just
   * forcing us to have a correct flib-map based entity_finder.
   * 
   * I also used to disable the error for undefined static methods because
   * the resolution was buggy. Indeed self::foo does not necesseraly
   * mean foo is defined in the current class (ugly PHP IMHO). So one
   * needs to lookup method there too in the inheritance tree, which
   * we have now thx to the flib-map based entity_finder.
   * 
   * update: it also actually useful to detect bad code using entities
   * in lib/. Indeed people abuse class_dependency() and other stuff
   * to call code from lib/. This is whitelisted by checkModule
   * but fortunately not by scheck!
   *)
  | UndefinedEntity (kind, _s) -> 
    (match kind with
    (* todo: too many for now *)
    | E.ClassConstant -> Less
    | E.Method -> Less
    (* some FPs about case sensitivity *)
    | _ -> Important
    )

  (* TODO: dont handle interface or traits very well for now *)
  | UndefinedClassWhileLookup _ -> Less

  (* todo: bad, but because of our ugly code, it's hard to fix sometimes
   * as in coreDataType for instance.
  *)
  | UndefinedMethodInAbstractClass _ -> Less

  | UseOfUndefinedMember (s1, suggest) -> 
      (match suggest with
      (* too many for now :( *)
      | None -> Less
      | Some (s2, i) ->
          (match i with
          | 1 -> 
              let s1 = String.lowercase_ascii s1 in
              let s2 = String.lowercase_ascii s2 in
              if (s1 ^ "s" =$= s2) || (s1 =$= s2 ^ "s")
              then Less
              else ReallyImportant
          (* todo? *)
          | 2 -> Less
          | _ -> Less
          )
      )


  (* todo: e.g. apc_fetch in conditional, whitelist those? *)
  | MultiDefinedEntity (_, _, _) -> Important

  (* have false positives when pass variables by reference, but they 
   * can be reduced by using --heavy.
   *)
  | UseOfUndefinedVariable (_s, suggest) -> 
      (match suggest with
      (* todo: still too many fps, even with --heavy for now :( *)
      | None -> Less
      | Some (_s2, i) ->
          (match i with
          | 1 ->  ReallyImportant
          | 2 -> Less
          | _ -> Less
          )
      )
        
  | UndefinedRequiredField (_, _) -> Less
  | UseOfUndefinedVariableInLambda _ -> Important
  | WrongLvalue -> Important
          
  (* giving too many args is kinda ok, it's ignored, but not giving enough can
   * be bad. Those errors happens only when run with --heavy.
   * We have lots of those though because for instance people give the
   * wrong signature for abstract method and forget a default value for
   * instance.
   *)
  | TooManyArguments _ -> Important
  | NotEnoughArguments _ -> Important
  (* many FPs :( todo? edit distance? *)
  | WrongKeywordArgument (_, _, severity) -> 
      (match severity with
      | Bad -> Less
      | ReallyBad -> Important
      | ReallyReallyBad -> ReallyImportant
      )
  (* too many of them ... *)
  | CallingStaticMethodWithoutQualifier _ -> OnlyStrict
  (* too many of them, and FPs, for instance parent::X is ok from a method X *)
  | CallingMethodWithQualifier _ -> OnlyStrict

  | PassingUnexpectedRef -> ReallyImportant
  | KeywordArgumentForRef -> Less
  | FormatStringMismatch _ -> Ok
      
  | CfgError (Controlflow_build_php.DeadCode stmt) ->
      (match stmt with
      (* some dead break and return are ok (too many FP for now) *)
      | Controlflow_php.Break | Controlflow_php.Return _ -> OnlyStrict
      | Controlflow_php.Throw _  -> Never
      | Controlflow_php.SimpleStmt 
          (Controlflow_php.ExprStmt (e,
                                     Controlflow_php.Normal)
          ) ->
          (match e with
          | (Call(Id (XName[QI (Name(
              ( "invariant_violation" | "_piranha_rollback_log"), _))]), _args)
              ) -> Never

          | (Yield(_,
               ArrayExpr(Call(Id (XName[QI (Name(("result", _)))]),
                 (_, [Left(Arg((Id(XName[QI (Name(("null", _)))]))))],
                _))
               ))) -> Never

          | _ -> Ok
          )
      | _ -> Ok
      )
  | CfgError 
      (* (DynamicBreak|NoMethodBody|ColonSyntax|NoEnclosingLoop) *) _ ->
      Ok
  (* | CfgPilError _  -> only_strict *)

  (* need a correct env_php *)
  | FileNotFound _ -> ReallyImportant
  (* need a complete env_php *)
  | IncludeUnresolved -> Less

  | UseOfPlusNotDotForStrings -> Important

  | AssignInBooleanContext -> Less
  | UnnecessaryTernaryIf -> Ok

  | MicroCloneCondExp _ -> Important

  | Injection _ -> ReallyImportant

  | CaseWithSemiColon | CaseSensitivityKeyword -> Less
  | InterfaceMethodWithBody -> Less
  | DynamicCode -> OnlyStrict

  | WeirdForeachNoIteratorVar
  | UglyGlobalDynamic
    -> OnlyStrict


(* ranking errors, inspired by Engler slides *)
let rank_errors errs =
  errs |> List.map (fun x ->
    x,
    rank_of_error_kind x.typ
  ) |> Common.sort_by_val_highfirst |> Common2.map fst

let show_10_most_recurring_unused_variable_names () =

  (* most recurring, probably false positives *)
  let hcount_str = Common2.hash_with_default (fun() -> 0) in

  !_errors |> List.iter (fun err ->
    match err.typ with
    | UnusedVariable (dname, _scope) ->
        hcount_str#update dname (fun old -> old+1);
    | _ -> ()
  );
  pr2 "top 10 most recurring unused variable names";
  hcount_str#to_list |> Common.sort_by_val_highfirst |> Common.take_safe 10
   |> List.iter (fun (s, cnt) ->
        pr2 (spf " %s -> %d" s cnt)
      );
  ()

(*
let filter_false_positives err = 
  err +> Common.exclude (fun x ->
    match x.typ with
    (* this actually requires a global analysis to truly know if the class
     * variable is unused *)
    | UnusedVariable (_, Scope_code.Class) -> true
    | _ -> false
  )
*)

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let (h_already_error: ((Entity_php.id_kind * string, bool) Hashtbl.t)) = 
  Hashtbl.create 101 

let (find_entity_and_warn:
  Entity_php.entity_finder -> (Entity_php.id_kind * Cst_php.name) ->
  (Cst_php.entity -> unit) -> unit) =
 fun find_entity (kind, name) callback ->

   let str = Ast.str_of_name name in
   let ids_ast = find_entity (kind, str) in
   match ids_ast with
   | [x] -> callback x
   | [] ->
       fatal (Ast.info_of_name name) (UndefinedEntity (kind, str));
       
   | x::_::_ ->
       if Hashtbl.mem h_already_error (kind, str)
       then ()
       else begin
         Hashtbl.add h_already_error (kind, str) true;
         (* todo: to give 2 ex of defs *)
         let ex1 = str (* TODO *) in
         let ex2 = str (* TODO *) in
         fatal (Ast.info_of_name name) 
           (MultiDefinedEntity (kind, str, (ex1, ex2)));
       end;
       (* can give the first one ... *)
       callback x
   
