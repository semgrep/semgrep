(** interface.proto Binary Encoding *)

(** {2 Protobuf Encoding} *)

val encode_position : Interface_types.position -> Pbrt.Encoder.t -> unit
(** [encode_position v encoder] encodes [v] with the given [encoder] *)

val encode_location : Interface_types.location -> Pbrt.Encoder.t -> unit
(** [encode_location v encoder] encodes [v] with the given [encoder] *)

(** {2 Protobuf Decoding} *)

val decode_position : Pbrt.Decoder.t -> Interface_types.position
(** [decode_position decoder] decodes a [position] value from [decoder] *)

val decode_location : Pbrt.Decoder.t -> Interface_types.location
(** [decode_location decoder] decodes a [location] value from [decoder] *)
