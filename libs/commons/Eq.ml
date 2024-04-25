let phys_equal = ( == )
let phys_not_equal = ( != )

type hidden_by_your_nanny = unit

module Operators = struct
  let ( =|= ) : int -> int -> bool = ( = )
  let ( =$= ) : char -> char -> bool = ( = )
  let ( =:= ) : bool -> bool -> bool = ( = )

  (* dangerous, do not use, see the comment in Common.mli *)
  let ( =*= ) = ( = )

  (* To forbid people to use the polymorphic '='.
   * See https://blog.janestreet.com/the-perils-of-polymorphic-compare/
   *)
  let ( = ) = String.equal
  let ( == ) : hidden_by_your_nanny = ()
  let ( != ) : hidden_by_your_nanny = ()
end
