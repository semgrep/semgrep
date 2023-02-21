//Provides: tree_sitter_wasm_module const
const tree_sitter_wasm_module = (() => {
    if (typeof Module !== "object") {
        throw new Error("semgrep-languages.js must be imported before using this module");
    }
    return globalThis.Module;
})()

//Provides: octs_parser_parse_string
//Requires: tree_sitter_wasm_module
function octs_parser_parse_string(vParser, vSource) {
    const source_code_ptr = tree_sitter_wasm_module._malloc(vSource.length*4);
    tree_sitter_wasm_module.stringToAscii(vSource, source_code_ptr);
    const tree_ptr = tree_sitter_wasm_module._ts_parser_parse_string(vParser, 0, source_code_ptr, vSource.length)
    return {tree_ptr}
}

//Provides: octs_tree_root_node
//Requires: tree_sitter_wasm_module
function octs_tree_root_node(vTree) {
    const node_ptr = tree_sitter_wasm_module._malloc(24)
    tree_sitter_wasm_module._ts_tree_root_node(node_ptr, vTree.tree_ptr);
    return {node_ptr}
}

//Provides: octs_node_start_point
//Requires: tree_sitter_wasm_module
function octs_node_start_point(vNode) {
    const ptr = tree_sitter_wasm_module._malloc(8)
    tree_sitter_wasm_module._ts_node_start_point(ptr, vNode.node_ptr);
    const result = [tree_sitter_wasm_module.getValue(ptr, 'i32'), tree_sitter_wasm_module.getValue(ptr+4, 'i32')]
    tree_sitter_wasm_module._free(ptr);
    return result
}

//Provides: octs_node_end_point
//Requires: tree_sitter_wasm_module
function octs_node_end_point(vNode) {
    const ptr = tree_sitter_wasm_module._malloc(8)
    tree_sitter_wasm_module._ts_node_end_point(ptr, vNode.node_ptr);
    const result = [tree_sitter_wasm_module.getValue(ptr, 'i32'), tree_sitter_wasm_module.getValue(ptr+4, 'i32')]
    tree_sitter_wasm_module._free(ptr);
    return result
}

//Provides: octs_node_is_named
//Requires: tree_sitter_wasm_module
function octs_node_is_named(vNode) {
    return tree_sitter_wasm_module._ts_node_is_named(vNode.node_ptr) != 0
}

//Provides: octs_node_child_count
//Requires: tree_sitter_wasm_module
function octs_node_child_count(vNode) {
    return tree_sitter_wasm_module._ts_node_child_count(vNode.node_ptr)
}

//Provides: octs_node_child
//Requires: tree_sitter_wasm_module
function octs_node_child(vNode, vX) {
    const node_ptr = tree_sitter_wasm_module._malloc(24)
    tree_sitter_wasm_module._ts_node_child(node_ptr, vNode.node_ptr, vX)
    return {node_ptr}
}

//Provides: octs_tree_delete
//Requires: tree_sitter_wasm_module
function octs_tree_delete(vTree) {
    tree_sitter_wasm_module._ts_tree_delete(vTree.tree_ptr)
}

//Provides: octs_node_type
//Requires: tree_sitter_wasm_module
function octs_node_type(vNode) {
    return tree_sitter_wasm_module.AsciiToString(tree_sitter_wasm_module._ts_node_type(vNode.node_ptr))
}