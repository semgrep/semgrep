[@@@ocaml.warning "-27-30-39"]

type position_mutable = {
  mutable line : int32;
  mutable col : int32;
  mutable offset : int32;
}

let default_position_mutable () : position_mutable = {
  line = 0l;
  col = 0l;
  offset = 0l;
}

type location_mutable = {
  mutable path : string;
  mutable start : Interface_types.position option;
  mutable end_ : Interface_types.position option;
  mutable lines : string list;
}

let default_location_mutable () : location_mutable = {
  path = "";
  start = None;
  end_ = None;
  lines = [];
}


let rec decode_position d =
  let v = default_position_mutable () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function
    | ("line", json_value) ->
      v.line <- Pbrt_yojson.int32 json_value "position" "line"
    | ("col", json_value) ->
      v.col <- Pbrt_yojson.int32 json_value "position" "col"
    | ("offset", json_value) ->
      v.offset <- Pbrt_yojson.int32 json_value "position" "offset"

    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    Interface_types.line = v.line;
    Interface_types.col = v.col;
    Interface_types.offset = v.offset;
  } : Interface_types.position)

let rec decode_location d =
  let v = default_location_mutable () in
  let assoc = match d with
    | `Assoc assoc -> assoc
    | _ -> assert(false)
  in
  List.iter (function
    | ("path", json_value) ->
      v.path <- Pbrt_yojson.string json_value "location" "path"
    | ("start", json_value) ->
      v.start <- Some ((decode_position json_value))
    | ("end", json_value) ->
      v.end_ <- Some ((decode_position json_value))
    | ("lines", `List l) -> begin
      v.lines <- List.map (function
        | json_value -> Pbrt_yojson.string json_value "location" "lines"
      ) l;
    end

    | (_, _) -> () (*Unknown fields are ignored*)
  ) assoc;
  ({
    Interface_types.path = v.path;
    Interface_types.start = v.start;
    Interface_types.end_ = v.end_;
    Interface_types.lines = v.lines;
  } : Interface_types.location)

let rec encode_position (v:Interface_types.position) =
  let open !Interface_types in
  let assoc = [] in
  let assoc = ("line", Pbrt_yojson.make_int (Int32.to_int v.line)) :: assoc in
  let assoc = ("col", Pbrt_yojson.make_int (Int32.to_int v.col)) :: assoc in
  let assoc = ("offset", Pbrt_yojson.make_int (Int32.to_int v.offset)) :: assoc in
  `Assoc assoc

let rec encode_location (v:Interface_types.location) =
  let open !Interface_types in
  let assoc = [] in
  let assoc = ("path", Pbrt_yojson.make_string v.path) :: assoc in
  let assoc = match v.start with
    | None -> assoc
    | Some v -> ("start", encode_position v) :: assoc
  in
  let assoc = match v.end_ with
    | None -> assoc
    | Some v -> ("end", encode_position v) :: assoc
  in
  let assoc =
    let l = v.lines |> List.map Pbrt_yojson.make_string in
    ("lines", `List l) :: assoc
  in
  `Assoc assoc
