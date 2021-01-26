
let (conv_op: AST_generic_.operator -> AST_generic.operator) = fun _op ->
  failwith "TODO"

let (conv_incr: AST_generic_.incr_decr -> AST_generic.incr_decr) = fun _op ->
  failwith "TODO"

let (conv_prepost: AST_generic_.prefix_postfix -> AST_generic.prefix_postfix) = fun _op ->
  failwith "TODO"

let (conv_incdec:
       (AST_generic_.incr_decr * AST_generic_.prefix_postfix) ->
     (AST_generic.incr_decr * AST_generic.prefix_postfix)) = fun _op ->
  failwith "TODO"

let (conv_class_kind: AST_generic_.class_kind * Parse_info.t -> AST_generic.class_kind * Parse_info.t) =
  fun _x -> failwith "TODO"
