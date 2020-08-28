
val strict: bool ref

type error = {
  typ: error_kind;
  loc: Parse_info.t;
  sev: severity;
}
 and severity = Fatal | Warning

 and error_kind = 
  (* entities *)
  | UndefinedEntity    of Entity_php.id_kind * string (* name *)
  | MultiDefinedEntity of Entity_php.id_kind * string (* name *) *
      (string * string) (* name * name *)
  | UndefinedClassWhileLookup of string
  | UndefinedMethodInAbstractClass of string

  (* call sites *)
  | TooManyArguments   of string (* name *) (* def *)
  | NotEnoughArguments of string (* name *) (* def *)
  | WrongKeywordArgument of
      string (* dname *) * string (* parameter *) * severity2
  | CallingStaticMethodWithoutQualifier of string
  | CallingMethodWithQualifier of string
  | PassingUnexpectedRef
  | KeywordArgumentForRef
  | FormatStringMismatch of string
        
  (* variables *)
  | UseOfUndefinedVariable of string (* dname *) * suggest option
  | UnusedVariable of string (* dname *)  * Scope_php.phpscope
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
  | MicroCloneCondExp of string (*operator *) * string (* expression *)

  (* bail-out constructs *)
  | UglyGlobalDynamic
  | WeirdForeachNoIteratorVar
  | DynamicCode

  | CfgError of Controlflow_build_php.error_kind

  | Injection of injection_kind

  | CaseWithSemiColon
  | CaseSensitivityKeyword
  | InterfaceMethodWithBody

  and severity2 =
    | Bad
    | ReallyBad
    | ReallyReallyBad

  and suggest = string * int (* edit distance *)
  and injection_kind = XSS | Sql | Shell

val string_of_error: error -> string
val string_of_error_kind: error_kind -> string

val string_of_severity2: severity2 -> string

exception Error of error

(* ugly global, but sometimes they are practical *)
val _errors: error list ref

(* modifies _errors by side effect *)
val fatal: Cst_php.info -> error_kind -> unit
val warning: Cst_php.info -> error_kind -> unit

val report_error : error -> unit
val report_all_errors: unit -> unit

type rank
val score_of_rank: rank -> int

val rank_of_error_kind: error_kind -> rank
val rank_errors: error list -> error list

val show_10_most_recurring_unused_variable_names: unit -> unit

(* Small helper function generating Undefined (or MultiDefined) error 
 * if the entity was not found (or defined multiple times). 
 *
 * Note that it memoizes the MultiDefined error so the second time
 * it actually returns one of the definition.
 *)
val find_entity_and_warn: 
  Entity_php.entity_finder ->
  (Entity_php.id_kind * Cst_php.name) ->
   (* callback, will be passed the found entity *)
   (Cst_php.entity -> unit) ->
   unit
