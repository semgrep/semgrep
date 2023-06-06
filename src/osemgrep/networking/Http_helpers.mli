val get : ?headers:(string * string) list -> Uri.t -> (string, string) result

val post :
  body:string ->
  ?headers:(string * string) list ->
  Uri.t ->
  (string, int * string) result
