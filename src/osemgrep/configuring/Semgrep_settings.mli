type t = {
  has_shown_metrics_notification : bool option;
  api_token : string option;
  anonymous_user_id : Uuidm.t;
}

val save : t -> unit
val get : unit -> t
