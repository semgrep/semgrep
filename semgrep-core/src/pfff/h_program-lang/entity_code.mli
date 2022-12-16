
type entity_kind =
  (* very high level entities *)
  | Package | Dir
  | Module | File

  (* toplevel entities *)
  | Function
  | Class (* used also for struct, interfaces, traits, see class_kind below *)
  | Type
  | Constant
  | Global
  | Macro
  | Exception
  | TopStmts

  (* class member entities *)
  | Field
  | Method
  | ClassConstant
  (* ocaml variants (not oo ctor, see Method for that *)
  | Constructor

  (* misc *)
  | Prototype | GlobalExtern
  | MultiDirs (* computed on the fly from many Dir by codemap *)
  | Other of string

val string_of_entity_kind: entity_kind -> string
val entity_kind_of_string: string -> entity_kind

type property =
  (* mostly for Function|Method kind, for codemap to highlight! *)
  | ContainDynamicCall | ContainReflectionCall

  | TakeArgNByRef of int (* the argument position taken by ref *)
  | UseGlobal of string
  | ContainDeadStatements

  | DeadCode (* the function itself is dead, e.g. never called *)
  | CodeCoverage of int list (* e.g. covered lines by unit tests *)

  (* for class *)
  | ClassKind of class_kind

  | Privacy of privacy
  | Abstract | Final
  | Static

  (* facebook specific: used for the xhp @required fields for now *)
  | Required | Async

and privacy = Public | Protected | Private
and class_kind =
  | Struct | Class_ | Interface
  | Trait
  (* in Scala, Java, and now PHP enums are actually closer to class
   * than C enums.
  *)
  | Enum
