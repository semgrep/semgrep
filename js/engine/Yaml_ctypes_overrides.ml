(* Semgrep uses ocaml-yaml, which uses ocaml-ctypes' stub generation to build a binding for the libyaml C library.
   This is problematic because our build architecture (64-bit) doesn't match WebAssembly's (32-bit), so memory alignment
   is not quite right. The long-term fix is to cross-compile against an arch that's compatible with WebAssembly,
   but for now, let's do some dirty js_of_ocaml tricks to overwrite the memory offsets *)
external override_yaml_ctypes_field_offset_bytes :
  ('a, 'b) Yaml_bindings.T.field -> int -> unit
  = "override_yaml_ctypes_field_offset_bytes"

let apply () =
  let open Yaml_types.M.Mark in
  override_yaml_ctypes_field_offset_bytes index 0;
  override_yaml_ctypes_field_offset_bytes line 4;
  override_yaml_ctypes_field_offset_bytes column 8;

  let open Yaml_types.M.Event in
  (override_yaml_ctypes_field_offset_bytes _type 0;
   override_yaml_ctypes_field_offset_bytes data 4;
   override_yaml_ctypes_field_offset_bytes start_mark 32;
   override_yaml_ctypes_field_offset_bytes end_mark 44;

   let open Alias in
   override_yaml_ctypes_field_offset_bytes anchor 0;

   let open Document_end in
   override_yaml_ctypes_field_offset_bytes implicit 0;

   let open Document_start in
   (override_yaml_ctypes_field_offset_bytes version_directive 0;
    override_yaml_ctypes_field_offset_bytes tag_directives 4;
    override_yaml_ctypes_field_offset_bytes implicit 12;

    let open Tag_directives in
    override_yaml_ctypes_field_offset_bytes start 0;
    override_yaml_ctypes_field_offset_bytes _end 4);

   let open Mapping_start in
   override_yaml_ctypes_field_offset_bytes anchor 0;
   override_yaml_ctypes_field_offset_bytes tag 4;
   override_yaml_ctypes_field_offset_bytes implicit 8;
   override_yaml_ctypes_field_offset_bytes style 12;

   let open Scalar in
   override_yaml_ctypes_field_offset_bytes anchor 0;
   override_yaml_ctypes_field_offset_bytes tag 4;
   override_yaml_ctypes_field_offset_bytes value 8;
   override_yaml_ctypes_field_offset_bytes length 12;
   override_yaml_ctypes_field_offset_bytes plain_implicit 16;
   override_yaml_ctypes_field_offset_bytes quoted_implicit 20;
   override_yaml_ctypes_field_offset_bytes style 24;

   let open Sequence_start in
   override_yaml_ctypes_field_offset_bytes anchor 0;
   override_yaml_ctypes_field_offset_bytes tag 4;
   override_yaml_ctypes_field_offset_bytes implicit 8;
   override_yaml_ctypes_field_offset_bytes style 12;

   let open Stream_start in
   override_yaml_ctypes_field_offset_bytes encoding 0);

  let open Yaml_types.M.Version_directive in
  override_yaml_ctypes_field_offset_bytes major 0;
  override_yaml_ctypes_field_offset_bytes minor 4
