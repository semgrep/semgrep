(*s: interfaces.ml *)
(* Yoann Padioleau
 *
 * Copyright (C) 2010 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)

open Common2.BasicType

(*****************************************************************************)
(* Type classes via module signature. *)
(*****************************************************************************)
(*
 * This module has nothing to do with graphical user interface (GUI). See
 * gui.ml for that.
 *
 * I use this module not so much for functors, I hate functors, but
 * more to force me to have consistent naming of stuff.
 *
 * It's related to objet.ml in some way, but use a different scheme.
 *
 * src: (strongly) inspired by Jane Street core lib, which in turn
 * may have been strongly inspired by Java Interfaces or Haskell
 * type classes.
 *
 *
 *
 * Example of use in .mli:
 *
 *   open Interfaces
 *   include Stringable with type stringable = t
 *   include Comparable with type comparable = t
 *
 * Example of use in .ml:
 *
 *   type xxx
 *   type stringable = xxx
 *   let of_string = bool_of_string
 *   let to_string = string_of_bool
 *
 *
 * todo? but as in type class, or object, can not have default method
 * with this scheme ?
 *)



(*****************************************************************************)
(* Basic *)
(*****************************************************************************)

(* note: less need for cloneable, copyable as in Java. Only needed
 * when use ref, but refs should be avoided anyway so better not to
 * encourage it.
 *
 * Often found this in haskell:
 *
 *    data x = ... deriving (Read, Show, Eq, Ord, Enum, Bounded)
 *
 * Apparently this is what is considered basic by haskell.
*)


module type Check_able = sig
  type checkable
  val invariant: checkable -> unit (* raise exception *)
end



(* Normally should not use the '=' of ocaml. cf common.mli on this issue. *)
module type Eq_able = sig
  type eqable
  val equal : eqable -> eqable -> bool
  (* Jane Street have far more (complex) stuff for this typeclass *)

  val (=*=): eqable -> eqable -> bool
end



(* Same, should not use compare normally, dangerous when evolve code.
 * Called Ord in haskell. Inherit Eq normally.
*)
module type Compare_able = sig
  type compareable
  val compare: compareable -> compareable -> bool
end
(* Jane street have also some binable, sexpable *)


(* Haskell have lots of related type class after Num such as
 * Real, Fractional, Integral, RealFrac, Floating, RealFloat
*)
module type Num_able = sig
  type numable
  (* +, -, etc *)
end



(*****************************************************************************)
(* Show/read related *)
(*****************************************************************************)


(* Called show/read in haskell *)
module type String_able = sig
  type stringable
  val of_string : string -> stringable
  val to_string : stringable -> string
end

module type Debug_able = sig
  type debugable
  val debug: debugable -> string
end


module type XML_able = sig
  type xmlable
  val of_xml: string -> xmlable
  val to_xml: xmlable -> string
end
(* Jane street have also some BIN_able, and SEXP_able (but no sex_able) *)

module type File_able = sig
  type fileable
  val load: filename -> fileable
  val save: fileable -> filename -> unit
end

(* a.k.a Marshall_able *)
module type Serialize_able = sig
  type serializeable
  val serialize: serializeable -> string
  val unserialize: string -> serializeable
end


module type Open_able = sig
  type openable
  val openfile: filename -> openable
  val close: openable -> unit
end

(*****************************************************************************)
(* Other *)
(*****************************************************************************)

(* This is related to ocollection.ml in some way, but use a different scheme *)

(* Require Constructor class ? So can not do it ? apparently can. Note the
 * 'b which is not declareted but seems to pose no problem to ocamlc.
*)
module type Map_able = sig
  type 'a mapable
  val map: ('a -> 'b) -> 'a mapable -> 'b mapable
end

module type Iter_able = sig
  type 'a iterable
  val iter: ('a -> unit) -> 'a iterable -> unit
end


(* testable ? actionable ? *)

(* *)

(* monad ? functor *)



(*****************************************************************************)
(* Idea taken from Jane Street Core library, slightly changed.
 *
 * It's another way to organize data structures, module instead of objects.
 * It's also the Java way.
 *
 * It makes some code looks a little bit like Haskell* typeclass.
 *
*)

(* In Jane Street they put each interface in its own file but then have to
 * do that:
 *
 * module type Stringable = Stringable.S
 * module type Comparable = Comparable.S
 * module type Floatable = Floatable.S
 * module type Hashable = Hashable.S
 * module type Infix_comparators = Comparable.Infix
 * module type Monad = Monad.S
 * module type Robustly_comparable = Robustly_comparable.S
 * module type Setable = Setable.S
 * module type Sexpable = Sexpable.S
 * module type Binable = Binable.S
 *
 * And I dont like having too much files, especially as all those xxable
 * end with able, not start, so don't see them together in the directory.
*)

(*e: interfaces.ml *)
