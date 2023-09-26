//Provides: wasm
var wasm;

//Provides: set_parser_wasm_module
//Requires: wasm
function set_parser_wasm_module(value) {
  wasm = value;
}

//Provides: lazy_instantiate_parser
function lazy_instantiate_parser(func) {
  var ptr;
  return {
    get parser_ptr() {
      if (!ptr) {
        ptr = func();
      }
      return ptr;
    },
  };
}

//Provides: octs_parser_parse_string
//Requires: caml_jsstring_of_string, wasm
function octs_parser_parse_string(vParser, vSource) {
  const { parser_ptr } = vParser;
  var source_str = Buffer.from(caml_jsstring_of_string(vSource)).toString(
    "utf-8"
  );
  var source_len = wasm.lengthBytesUTF8(source_str);
  var source_code_ptr = wasm._malloc(source_len + 1);
  wasm.stringToUTF8(source_str, source_code_ptr, source_len + 1);
  var tree_ptr = wasm._ts_parser_parse_string(
    parser_ptr,
    0,
    source_code_ptr,
    source_len
  );
  return { tree_ptr };
}

//Provides: octs_tree_root_node
//Requires: wasm
function octs_tree_root_node(vTree) {
  const { tree_ptr } = vTree;
  var node_ptr = wasm._malloc(24);
  wasm._ts_tree_root_node(node_ptr, tree_ptr);
  return { node_ptr };
}

//Provides: octs_node_start_point
//Requires: wasm
function octs_node_start_point(vNode) {
  const { node_ptr } = vNode;
  var ptr = wasm._malloc(8);
  wasm._ts_node_start_point(ptr, node_ptr);
  var result = [0, wasm.getValue(ptr, "i32"), wasm.getValue(ptr + 4, "i32")];
  wasm._free(ptr);
  return result;
}

//Provides: octs_node_end_point
//Requires: wasm
function octs_node_end_point(vNode) {
  const { node_ptr } = vNode;
  var ptr = wasm._malloc(8);
  wasm._ts_node_end_point(ptr, node_ptr);
  var result = [0, wasm.getValue(ptr, "i32"), wasm.getValue(ptr + 4, "i32")];
  wasm._free(ptr);
  return result;
}

//Provides: octs_node_is_missing
//Requires: wasm
function octs_node_is_missing(vNode) {
  return wasm._ts_node_is_missing(vNode.node_ptr) != 0;
}

//Provides: octs_node_is_named
//Requires: wasm
function octs_node_is_named(vNode) {
  return wasm._ts_node_is_named(vNode.node_ptr) != 0;
}

//Provides: octs_node_child_count
//Requires: wasm
function octs_node_child_count(vNode) {
  return wasm._ts_node_child_count(vNode.node_ptr);
}

//Provides: octs_node_child
//Requires: wasm
function octs_node_child(vNode, vX) {
  const { node_ptr } = vNode;
  var child_node_ptr = wasm._malloc(24);
  wasm._ts_node_child(child_node_ptr, node_ptr, vX);
  return { node_ptr: child_node_ptr };
}

//Provides: octs_tree_delete
//Requires: wasm
function octs_tree_delete(vTree) {
  const { tree_ptr } = vTree;
  wasm._ts_tree_delete(tree_ptr);

  // TODO: are there any additional free()s needed?
}

//Provides: octs_node_type
//Requires: caml_string_of_jsstring, wasm
function octs_node_type(vNode) {
  const { node_ptr } = vNode;
  return caml_string_of_jsstring(
    wasm.AsciiToString(wasm._ts_node_type(node_ptr))
  );
}
