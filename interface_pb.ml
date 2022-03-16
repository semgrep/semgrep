[@@@ocaml.warning "-27-30-39"]

type position_mutable = {
  mutable line : int32;
  mutable col : int32;
  mutable offset : int32;
}

let default_position_mutable () : position_mutable =
  { line = 0l; col = 0l; offset = 0l }

type location_mutable = {
  mutable path : string;
  mutable start : Interface_types.position option;
  mutable end_ : Interface_types.position option;
  mutable lines : string list;
}

let default_location_mutable () : location_mutable =
  { path = ""; start = None; end_ = None; lines = [] }

let rec decode_position d =
  let v = default_position_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        ();
        continue__ := false
    | Some (1, Pbrt.Varint) -> v.line <- Pbrt.Decoder.int32_as_varint d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(position), field(1)" pk
    | Some (2, Pbrt.Varint) -> v.col <- Pbrt.Decoder.int32_as_varint d
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(position), field(2)" pk
    | Some (3, Pbrt.Varint) -> v.offset <- Pbrt.Decoder.int32_as_varint d
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(position), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Interface_types.line = v.line;
     Interface_types.col = v.col;
     Interface_types.offset = v.offset;
   }
    : Interface_types.position)

let rec decode_location d =
  let v = default_location_mutable () in
  let continue__ = ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None ->
        v.lines <- List.rev v.lines;
        continue__ := false
    | Some (1, Pbrt.Bytes) -> v.path <- Pbrt.Decoder.string d
    | Some (1, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(location), field(1)" pk
    | Some (2, Pbrt.Bytes) ->
        v.start <- Some (decode_position (Pbrt.Decoder.nested d))
    | Some (2, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(location), field(2)" pk
    | Some (3, Pbrt.Bytes) ->
        v.end_ <- Some (decode_position (Pbrt.Decoder.nested d))
    | Some (3, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(location), field(3)" pk
    | Some (4, Pbrt.Bytes) -> v.lines <- Pbrt.Decoder.string d :: v.lines
    | Some (4, pk) ->
        Pbrt.Decoder.unexpected_payload "Message(location), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
     Interface_types.path = v.path;
     Interface_types.start = v.start;
     Interface_types.end_ = v.end_;
     Interface_types.lines = v.lines;
   }
    : Interface_types.location)

let rec encode_position (v : Interface_types.position) encoder =
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder;
  Pbrt.Encoder.int32_as_varint v.Interface_types.line encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder;
  Pbrt.Encoder.int32_as_varint v.Interface_types.col encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder;
  Pbrt.Encoder.int32_as_varint v.Interface_types.offset encoder;
  ()

let rec encode_location (v : Interface_types.location) encoder =
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder;
  Pbrt.Encoder.string v.Interface_types.path encoder;
  (match v.Interface_types.start with
  | Some x ->
      Pbrt.Encoder.key (2, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_position x) encoder
  | None -> ());
  (match v.Interface_types.end_ with
  | Some x ->
      Pbrt.Encoder.key (3, Pbrt.Bytes) encoder;
      Pbrt.Encoder.nested (encode_position x) encoder
  | None -> ());
  List.iter
    (fun x ->
      Pbrt.Encoder.key (4, Pbrt.Bytes) encoder;
      Pbrt.Encoder.string x encoder)
    v.Interface_types.lines;
  ()
