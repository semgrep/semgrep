(* Do a 'open Fullcommon' to access most of the functions in commons/
 * without needing to qualify them.
 *
 * update: Jane Street use a similar trick, to have a more complete
 * Pervasives, but for far more. They can define a module Std that
 * correspond to old std lib and a module Std_internal that instead
 * include all their extensions over the standard lib (a more complete
 * List module, Arg, etc)
*)
include Common2

include Oset
include Oassoc
include Oarray
include Ograph

include Osetb
include Oassoch
include Oassocb
include Oseti
include Ograph2way
include Oassoc_buffer
