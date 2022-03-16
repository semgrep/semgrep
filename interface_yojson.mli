(** interface.proto YoJSON Encoding *)


(** {2 Protobuf YoJson Encoding} *)

val encode_position : Interface_types.position -> Yojson.Basic.json
(** [encode_position v encoder] encodes [v] to to json*)

val encode_location : Interface_types.location -> Yojson.Basic.json
(** [encode_location v encoder] encodes [v] to to json*)


(** {2 JSON Decoding} *)

val decode_position : Yojson.Basic.json -> Interface_types.position
(** [decode_position decoder] decodes a [position] value from [decoder] *)

val decode_location : Yojson.Basic.json -> Interface_types.location
(** [decode_location decoder] decodes a [location] value from [decoder] *)
