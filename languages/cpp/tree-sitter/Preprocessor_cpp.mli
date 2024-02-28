module CST = Tree_sitter_cpp.CST

type ifdef_token =
  [ `Ifdef of Tree_sitter_run.Token.t (* pattern #[ 	]*ifdef *)
  | `Ifndef of Tree_sitter_run.Token.t (* pattern #[ 	]*ifndef *) ]

type elifdef_token =
  [ `Elifdef of Tree_sitter_run.Token.t (* pattern #[ 	]*elifdef *)
  | `Elifndef of Tree_sitter_run.Token.t (* pattern #[ 	]*elifndef *) ]

type 'a preproc_if_poly =
  Tree_sitter_run.Token.t (* pattern #[ 	]*if *)
  * CST.preproc_expression
  * Tree_sitter_run.Token.t (* "\n" *)
  * 'a list (* zero or more *)
  * 'a preproc_else_poly option
  * Tree_sitter_run.Token.t (* pattern #[ 	]*endif *)

and 'a preproc_else_poly =
  [ `Prep_else_poly of
    Tree_sitter_run.Token.t (* pattern #[ 	]*else *) * 'a list
  | `Prep_elif_poly of
    Tree_sitter_run.Token.t (* pattern #[ 	]*elif *)
    * CST.preproc_expression
    * Tree_sitter_run.Token.t (* "\n" *)
    * 'a list (* zero or more *)
    * 'a preproc_else_poly option ]

type 'a preproc_ifdef_poly =
  ifdef_token
  * Tree_sitter_run.Token.t (* identifier *)
  * 'a list (* zero or more *)
  * [ `Choice_prep_else_poly of 'a preproc_else_poly
    | `Prep_elif_poly of 'a preproc_elifdef_poly ]
    option
  * Tree_sitter_run.Token.t (* pattern #[ 	]*endif *)

and 'a preproc_elifdef_poly =
  elifdef_token
  * Tree_sitter_run.Token.t (* identifier *)
  * 'a list (* zero or more *)
  * 'a preproc_else_poly option

val preproc_if_to_poly : CST.preproc_if -> CST.block_item preproc_if_poly

val preproc_ifdef_to_poly :
  CST.preproc_ifdef -> CST.block_item preproc_ifdef_poly

val preproc_if_in_field_declaration_list_to_poly :
  CST.preproc_if_in_field_declaration_list ->
  CST.field_declaration_list_item preproc_if_poly

val preproc_ifdef_in_field_declaration_list_to_poly :
  CST.preproc_ifdef_in_field_declaration_list ->
  CST.field_declaration_list_item preproc_ifdef_poly

val preproc_if_in_enumerator_list_to_poly :
  CST.preproc_if_in_enumerator_list ->
  (CST.enumerator * Tree_sitter_run.Token.t) preproc_if_poly

val preproc_ifdef_in_enumerator_list_to_poly :
  CST.preproc_ifdef_in_enumerator_list ->
  (CST.enumerator * Tree_sitter_run.Token.t) preproc_ifdef_poly

val preproc_if_in_enumerator_list_no_comma_to_poly :
  CST.preproc_if_in_enumerator_list_no_comma -> CST.enumerator preproc_if_poly

val preproc_ifdef_in_enumerator_list_no_comma_to_poly :
  CST.preproc_ifdef_in_enumerator_list_no_comma ->
  CST.enumerator preproc_ifdef_poly
