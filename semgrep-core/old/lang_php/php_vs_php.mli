(*****************************************************************************)
(* PHP vs PHP *)
(*****************************************************************************)

module type PARAM = sig
  type tin

  type 'x tout

  type ('a, 'b) matcher = 'a -> 'b -> tin -> ('a * 'b) tout

  val ( >>= ) :
    (tin -> ('a * 'b) tout) ->
    ('a * 'b -> tin -> ('c * 'd) tout) ->
    tin ->
    ('c * 'd) tout

  val ( >||> ) : (tin -> 'x tout) -> (tin -> 'x tout) -> tin -> 'x tout

  (* The classical monad combinators *)
  val return : 'a * 'b -> tin -> ('a * 'b) tout

  val fail : tin -> ('a * 'b) tout

  (* -------------------------------------------------------------------- *)
  (* Tokens tagging *)
  (* -------------------------------------------------------------------- *)
  val tokenf : (Cst_php.info, Cst_php.info) matcher

  (* -------------------------------------------------------------------- *)
  (* Distr_f functions, to tag a range of tokens *)
  (* -------------------------------------------------------------------- *)

  (* -------------------------------------------------------------------- *)
  (* Environment manipulation. Extract info from tin, the "something" *)
  (* -------------------------------------------------------------------- *)
  val envf : (Metavars_php.mvar Cst_php.wrap, Cst_php.any) matcher

  val envf2 :
    (Metavars_php.mvar Cst_php.wrap, Cst_php.any * Cst_php.any) matcher
end

(*****************************************************************************)
(* Config *)
(*****************************************************************************)

val case_sensitive : bool ref

(*****************************************************************************)
(* The functor itself *)
(*****************************************************************************)

module PHP_VS_PHP : functor (X : PARAM) -> sig
  type ('a, 'b) matcher = 'a -> 'b -> X.tin -> ('a * 'b) X.tout

  val m_expr : (Cst_php.expr, Cst_php.expr) matcher

  val m_stmt : (Cst_php.stmt, Cst_php.stmt) matcher

  val m_xhp_html : (Cst_php.xhp_html, Cst_php.xhp_html) matcher

  val m_hint_type : (Cst_php.hint_type, Cst_php.hint_type) matcher

  (* there are far more functions in this functor but they
   * do not have to be exported
   *)
end
