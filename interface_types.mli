(** interface.proto Types *)

(** {2 Types} *)

type position = { line : int32; col : int32; offset : int32 }

type location = {
  path : string;
  start : position option;
  end_ : position option;
  lines : string list;
}

(** {2 Default values} *)

val default_position :
  ?line:int32 -> ?col:int32 -> ?offset:int32 -> unit -> position
(** [default_position ()] is the default value for type [position] *)

val default_location :
  ?path:string ->
  ?start:position option ->
  ?end_:position option ->
  ?lines:string list ->
  unit ->
  location
(** [default_location ()] is the default value for type [location] *)
