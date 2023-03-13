//Provides: octs_parser_parse_string
//Requires: caml_jsstring_of_string
function octs_parser_parse_string(vParser, vSource) {
  const { wasm, parser_ptr } = vParser;
  var source = caml_jsstring_of_string(vSource);
  var source_code_ptr = wasm._malloc(source.length + 1); // TODO: do we need to worry about utf-8?
  wasm.stringToAscii(source, source_code_ptr);
  var tree_ptr = wasm._ts_parser_parse_string(
    parser_ptr,
    0,
    source_code_ptr,
    source.length
  );
  return { wasm, tree_ptr };
}

//Provides: octs_tree_root_node
function octs_tree_root_node(vTree) {
  const { wasm, tree_ptr } = vTree;
  var node_ptr = wasm._malloc(24);
  wasm._ts_tree_root_node(node_ptr, tree_ptr);
  return { wasm, node_ptr };
}

//Provides: octs_node_start_point
function octs_node_start_point(vNode) {
  const { wasm, node_ptr } = vNode;
  var ptr = wasm._malloc(8);
  wasm._ts_node_start_point(ptr, node_ptr);
  var result = [0, wasm.getValue(ptr, "i32"), wasm.getValue(ptr + 4, "i32")];
  wasm._free(ptr);
  return result;
}

//Provides: octs_node_end_point
function octs_node_end_point(vNode) {
  const { wasm, node_ptr } = vNode;
  var ptr = wasm._malloc(8);
  wasm._ts_node_end_point(ptr, node_ptr);
  var result = [0, wasm.getValue(ptr, "i32"), wasm.getValue(ptr + 4, "i32")];
  wasm._free(ptr);
  return result;
}

//Provides: octs_node_is_named
function octs_node_is_named(vNode) {
  return vNode.wasm._ts_node_is_named(vNode.node_ptr) != 0;
}

//Provides: octs_node_child_count
function octs_node_child_count(vNode) {
  return vNode.wasm._ts_node_child_count(vNode.node_ptr);
}

//Provides: octs_node_child
function octs_node_child(vNode, vX) {
  const { wasm, node_ptr } = vNode;
  var child_node_ptr = wasm._malloc(24);
  wasm._ts_node_child(child_node_ptr, node_ptr, vX);
  return { wasm, node_ptr: child_node_ptr };
}

//Provides: octs_tree_delete
function octs_tree_delete(vTree) {
  const { wasm, tree_ptr } = vTree;
  wasm._ts_tree_delete(tree_ptr);

  // TODO: are there any additional free()s needed?
}

//Provides: octs_node_type
//Requires: caml_string_of_jsstring
function octs_node_type(vNode) {
  const { wasm, node_ptr } = vNode;
  return caml_string_of_jsstring(
    wasm.AsciiToString(wasm._ts_node_type(node_ptr))
  );
}
