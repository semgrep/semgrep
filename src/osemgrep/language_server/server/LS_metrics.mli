type t = {
  machineId : string option;
  isNewAppInstall : bool;
  sessionId : string option;
  extensionVersion : string option;
  extensionType : string;
  enabled : bool;
}

val default : t
val t_of_yojson : Yojson.Safe.t -> (t, string) result
val yojson_of_t : t -> Yojson.Safe.t
val pp : Format.formatter -> t -> unit
