type client_metrics = {
  machineId : string option;
  isNewAppInstall : bool;
  sessionId : string option;
  extensionVersion : string option;
  extensionType : string;
  enabled : bool;
}
[@@deriving yojson]

type t = {
  client_metrics : client_metrics;
  (* # of autofix code actions *)
  autofix_count : int;
  (* # of ignore code actions *)
  ignore_count : int;
}

val t_of_yojson : Yojson.Safe.t -> (t, string) result
val yojson_of_t : t -> Yojson.Safe.t
val default : t
val client_metrics_default : client_metrics
val pp : Format.formatter -> t -> unit
