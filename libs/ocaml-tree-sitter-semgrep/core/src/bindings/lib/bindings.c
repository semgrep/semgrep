/*
  Copyright (c) 2019 onivim

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
*/

#include <string.h>
#include <tree_sitter/api.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

typedef struct _parser {
  TSParser *parser;
} parser_W;

typedef struct _tree {
  TSTree *tree;
} tree_W;

static void finalize_tree(value v) {
  // Originally, we freed the free when we finalized it by doing:

  // tree_W *t = (tree_W *)Data_custom_val(vTree);
  // ts_tree_delete(t->tree);

  // However, finalize_tree is called before we are done with the
  // underlying tree. Therefore, instead we moved this code to
  // octs_tree_delete, which we expose in Tree_sitter_API as the
  // delete function for Tree.
}

static struct custom_operations tree_custom_ops = {
  .identifier = "tree handling",
  .finalize = finalize_tree,
  .compare = custom_compare_default,
  .hash = custom_hash_default,
  .serialize = custom_serialize_default,
  .deserialize = custom_deserialize_default
};

static struct custom_operations TSNode_custom_ops = {
  .identifier = "TSNode handling",
  .finalize = custom_finalize_default,
  .compare = custom_compare_default,
  .hash = custom_hash_default,
  .serialize = custom_serialize_default,
  .deserialize = custom_deserialize_default
};

const char *octs_read(void *payload, uint32_t byte_offset, TSPoint position,
                      uint32_t *bytes_read) {
  value closure = *caml_named_value("octs__parse_read");
  value result =
      caml_callback3(closure, Val_int(byte_offset), Val_int(position.row),
                     Val_int(position.column));

  *bytes_read = 0;
  if (Is_block(result)) {
    value strVal = Field(result, 0);

    const char *str = String_val(strVal);
    *bytes_read = strlen(str);
    return str;
  }
  else
    return NULL;
}

CAMLprim value octs_parser_parse(value vParser, value vTree, value vRead) {
  CAMLparam3(vParser, vTree, vRead);
  CAMLlocal1(ret);

  parser_W *p = Data_custom_val(vParser);
  TSParser *tsparser = p->parser;

  TSTree *oldTree = NULL;
  // Some(tree)
  if (Is_block(vTree)) {
    tree_W *t = Data_custom_val(Field(vTree, 0));
    oldTree = t->tree;
  }

  TSInput input;
  input.payload = (void *)vRead;
  input.read = &octs_read;
  input.encoding = TSInputEncodingUTF8;

  TSTree *tree = ts_parser_parse(tsparser, oldTree, input);

  tree_W treeWrapper;
  treeWrapper.tree = tree;
  ret = caml_alloc_custom(&tree_custom_ops, sizeof(tree_W), 0, 1);
  memcpy(Data_custom_val(ret), &treeWrapper, sizeof(tree_W));

  CAMLreturn(ret);
};

CAMLprim value octs_parser_parse_string(value vParser, value vSource) {
  CAMLparam2(vParser, vSource);
  CAMLlocal1(v);

  parser_W *p = Data_custom_val(vParser);
  TSParser *tsparser = p->parser;

  const char *source_code = String_val(vSource);
  TSTree *tree =
      ts_parser_parse_string(tsparser, NULL, source_code, caml_string_length(vSource));

  tree_W treeWrapper;
  treeWrapper.tree = tree;
  v = caml_alloc_custom(&tree_custom_ops, sizeof(tree_W), 0, 1);
  memcpy(Data_custom_val(v), &treeWrapper, sizeof(tree_W));

  CAMLreturn(v);
};

void octs_tree_delete(value vTree) {
  CAMLparam1(vTree);

  tree_W *t = (tree_W *)Data_custom_val(vTree);
  ts_tree_delete(t->tree);

  CAMLreturn0;
}

CAMLprim value octs_tree_root_node(value vTree) {
  CAMLparam1(vTree);
  CAMLlocal1(v);

  tree_W *t = Data_custom_val(vTree);
  TSTree *tree = t->tree;

  TSNode node = ts_tree_root_node(tree);
  v = caml_alloc_custom(&TSNode_custom_ops, sizeof(TSNode), 0, 1);
  memcpy(Data_custom_val(v), &node, sizeof(TSNode));
  CAMLreturn(v);
};

CAMLprim value octs_tree_edit_native(value vTree, value vStartByte,
                                     value vOldEndByte, value vNewEndByte,
                                     value vStartLine, value vOldEndLine,
                                     value vNewEndLine) {
  CAMLparam5(vTree, vStartByte, vOldEndByte, vNewEndByte, vStartLine);
  CAMLxparam2(vOldEndLine, vNewEndLine);

  CAMLlocal1(v);

  tree_W *t = Data_custom_val(vTree);
  TSTree *tree = t->tree;

  TSInputEdit edit;
  edit.start_byte = Long_val(vStartByte);
  edit.old_end_byte = Long_val(vOldEndByte);
  edit.new_end_byte = Long_val(vNewEndByte);

  edit.start_point.row = Long_val(vStartLine);
  edit.start_point.column = 0;
  edit.old_end_point.row = Long_val(vOldEndLine);
  edit.old_end_point.column = 0;
  edit.new_end_point.row = Long_val(vNewEndLine);
  edit.new_end_point.column = 0;

  /*printf("SENDING EDIT:\n start_byte: %ld\n old_end_byte: %ld\n new_end_byte:
    %ld\n, start_row: %ld\n start_col: %ld\n old_end_row: %ld\n old_end_col:
    %ld\n new_end_row: %ld\n new_end_col: %ld\n", edit.start_byte,
    edit.old_end_byte,
    edit.new_end_byte,
    edit.start_point.row,
    edit.start_point.column,
    edit.old_end_point.row,
    edit.old_end_point.column,
    edit.new_end_point.row,
    edit.new_end_point.column);*/

  TSTree *ret = ts_tree_copy(tree);

  ts_tree_edit(ret, &edit);
  tree_W treeWrapper;
  treeWrapper.tree = ret;
  v = caml_alloc_custom(&tree_custom_ops, sizeof(tree_W), 0, 1);
  memcpy(Data_custom_val(v), &treeWrapper, sizeof(tree_W));

  CAMLreturn(v);
};

CAMLprim value octs_tree_edit_bytecode(value *argv, int argn) {
  return octs_tree_edit_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                               argv[5], argv[6]);
}

CAMLprim value octs_node_string(value vNode) {
  CAMLparam1(vNode);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);
  char *sz = ts_node_string(*node);

  v = caml_copy_string(sz);
  free(sz);

  CAMLreturn(v);
};

CAMLprim value octs_node_type(value vNode) {
  CAMLparam1(vNode);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);
  const char *sz = ts_node_type(*node);

  v = caml_copy_string(sz);
  CAMLreturn(v);
};

CAMLprim value octs_node_symbol(value vNode) {

  CAMLparam1(vNode);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);
  TSSymbol sym = ts_node_symbol(*node);
  CAMLreturn(Val_int(sym));
}

CAMLprim value octs_node_is_error(value vNode) {
  CAMLparam1(vNode);

  TSNode *node = Data_custom_val(vNode);
  TSSymbol sym = ts_node_symbol(*node);
  int isError = sym == ((TSSymbol)-1);
  CAMLreturn(Val_bool(isError));
}

CAMLprim value octs_node_has_changes(value vNode) {
  CAMLparam1(vNode);

  TSNode *node = Data_custom_val(vNode);
  int hasChanges = ts_node_has_changes(*node);

  CAMLreturn(Val_bool(hasChanges));
}

CAMLprim value octs_node_has_error(value vNode) {
  CAMLparam1(vNode);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);
  int hasError = ts_node_has_error(*node);

  CAMLreturn(Val_bool(hasError));
}

CAMLprim value octs_node_is_missing(value vNode) {
  CAMLparam1(vNode);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);
  int isMissing = ts_node_is_missing(*node);

  CAMLreturn(Val_bool(isMissing));
}

CAMLprim value octs_node_is_null(value vNode) {
  CAMLparam1(vNode);

  TSNode *node = Data_custom_val(vNode);
  int isNull = ts_node_is_null(*node);

  CAMLreturn(Val_bool(isNull));
}

CAMLprim value octs_node_is_named(value vNode) {
  CAMLparam1(vNode);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);
  int isNamed = ts_node_is_named(*node);

  CAMLreturn(Val_bool(isNamed));
}

CAMLprim value octs_node_is_extra(value vNode) {
  CAMLparam1(vNode);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);
  int isExtra = ts_node_is_extra(*node);

  CAMLreturn(Val_bool(isExtra));
}

CAMLprim value octs_node_child_count(value vNode) {
  CAMLparam1(vNode);

  TSNode *node = Data_custom_val(vNode);
  uint32_t c = ts_node_child_count(*node);

  CAMLreturn(Val_int(c));
};

CAMLprim value octs_node_end_byte(value vNode) {
  CAMLparam1(vNode);

  TSNode *node = Data_custom_val(vNode);
  uint32_t endByte = ts_node_end_byte(*node);

  CAMLreturn(Val_int(endByte));
};

CAMLprim value octs_node_start_point(value vNode) {
  CAMLparam1(vNode);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);
  TSPoint startPoint = ts_node_start_point(*node);

  v = caml_alloc(2, 0);
  Store_field(v, 0, Val_int(startPoint.row));
  Store_field(v, 1, Val_int(startPoint.column));
  CAMLreturn(v);
};

CAMLprim value octs_node_end_point(value vNode) {
  CAMLparam1(vNode);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);
  TSPoint endPoint = ts_node_end_point(*node);

  v = caml_alloc(2, 0);
  Store_field(v, 0, Val_int(endPoint.row));
  Store_field(v, 1, Val_int(endPoint.column));
  CAMLreturn(v);
};

CAMLprim value octs_node_start_byte(value vNode) {
  CAMLparam1(vNode);

  TSNode *node = Data_custom_val(vNode);
  uint32_t startByte = ts_node_start_byte(*node);

  CAMLreturn(Val_int(startByte));
};

CAMLprim value octs_node_child(value vNode, value vX) {
  CAMLparam2(vNode, vX);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);
  uint32_t idx = Int_val(vX);

  TSNode child = ts_node_child(*node, idx);

  v = caml_alloc_custom(&TSNode_custom_ops, sizeof(TSNode), 0, 1);
  memcpy(Data_custom_val(v), &child, sizeof(TSNode));
  CAMLreturn(v);
};

CAMLprim value octs_node_next_sibling(value vNode) {
  CAMLparam1(vNode);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);
  TSNode nextSibling = ts_node_next_sibling(*node);

  v = caml_alloc_custom(&TSNode_custom_ops, sizeof(TSNode), 0, 1);
  memcpy(Data_custom_val(v), &nextSibling, sizeof(TSNode));
  CAMLreturn(v);
};

CAMLprim value octs_node_prev_sibling(value vNode) {
  CAMLparam1(vNode);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);
  TSNode prevSibling = ts_node_prev_sibling(*node);

  v = caml_alloc_custom(&TSNode_custom_ops, sizeof(TSNode), 0, 1);
  memcpy(Data_custom_val(v), &prevSibling, sizeof(TSNode));
  CAMLreturn(v);
};

CAMLprim value octs_node_descendant_for_point_range(value vNode,
                                                    value vStartRow,
                                                    value vStartColumn,
                                                    value vEndRow,
                                                    value vEndColumn) {
  CAMLparam5(vNode, vStartRow, vStartColumn, vEndRow, vEndColumn);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);

  TSPoint start;
  TSPoint end;
  start.row = Int_val(vStartRow);
  start.column = Int_val(vStartColumn);

  end.row = Int_val(vEndRow);
  end.column = Int_val(vEndColumn);

  TSNode descendant = ts_node_descendant_for_point_range(*node, start, end);
  v = caml_alloc_custom(&TSNode_custom_ops, sizeof(TSNode), 0, 1);
  memcpy(Data_custom_val(v), &descendant, sizeof(TSNode));
  CAMLreturn(v);
};

CAMLprim value octs_node_parent(value vNode) {
  CAMLparam1(vNode);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);
  TSNode parent = ts_node_parent(*node);

  v = caml_alloc_custom(&TSNode_custom_ops, sizeof(TSNode), 0, 1);
  memcpy(Data_custom_val(v), &parent, sizeof(TSNode));
  CAMLreturn(v);
};

CAMLprim value octs_node_bounded_named_index(value vNode) {
  CAMLparam1(vNode);

  TSNode *node = Data_custom_val(vNode);
  TSNode prev = ts_node_prev_named_sibling(*node);

  uint32_t c = 0;
  while (!ts_node_is_null(prev) && c < 2) {
    c++;
    prev = ts_node_prev_named_sibling(prev);
  }

  CAMLreturn(Val_int(c));
};

CAMLprim value octs_node_named_index(value vNode) {
  CAMLparam1(vNode);

  TSNode *node = Data_custom_val(vNode);
  TSNode prev = ts_node_prev_named_sibling(*node);

  uint32_t c = 0;
  while (!ts_node_is_null(prev)) {
    c++;
    prev = ts_node_prev_named_sibling(prev);
  }

  CAMLreturn(Val_int(c));
};

CAMLprim value octs_node_index(value vNode) {
  CAMLparam1(vNode);

  TSNode *node = Data_custom_val(vNode);
  uint32_t c = 0;
  TSNode prev = ts_node_prev_sibling(*node);
  while (!ts_node_is_null(prev)) {
    c++;
    prev = ts_node_prev_sibling(prev);
  }

  CAMLreturn(Val_int(c));
};

CAMLprim value octs_node_named_child_count(value vNode) {
  CAMLparam1(vNode);

  TSNode *node = Data_custom_val(vNode);
  uint32_t c = ts_node_named_child_count(*node);
  CAMLreturn(Val_int(c));
};

CAMLprim value octs_node_named_child(value vNode, value vX) {
  CAMLparam2(vNode, vX);
  CAMLlocal1(v);

  TSNode *node = Data_custom_val(vNode);
  uint32_t idx = Int_val(vX);

  TSNode child = ts_node_named_child(*node, idx);
  v = caml_alloc_custom(&TSNode_custom_ops, sizeof(TSNode), 0, 1);
  memcpy(Data_custom_val(v), &child, sizeof(TSNode));
  CAMLreturn(v);
};
