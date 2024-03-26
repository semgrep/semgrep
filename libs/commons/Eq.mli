(* Physical (shallow) equality, normally available as (==) *)
val phys_equal : 'a -> 'a -> bool

(* Physical (shallow) inequality, normally available as (!=) *)
val phys_not_equal : 'a -> 'a -> bool

type hidden_by_your_nanny

module Operators : sig
  (* This was in Common, but we'd like to use this in some utilities that Common
   * depends on. Normally you should just `open Common` instead of using this
   * directly. *)
  val ( = ) : string -> string -> bool

  (* If you need to use '=', at least use the more precise operators below. *)
  val ( =|= ) : int -> int -> bool
  val ( =$= ) : char -> char -> bool
  val ( =:= ) : bool -> bool -> bool

  (* if you really really need to use the polymorphic '=', at least use
   * the operator below so it's easier to grep for it if one needs to refactor
   * the code to use 'deriving eq' instead.
   *)
  val ( =*= ) : 'a -> 'a -> bool

  (*
     Disable the use of (==) since some people confuse it with structural
     equality. We do this here since we're disabling in with semgrep anyway
     and it's quicker if the compiler can report it.
  *)

  val ( == ) : hidden_by_your_nanny
  val ( != ) : hidden_by_your_nanny
end
