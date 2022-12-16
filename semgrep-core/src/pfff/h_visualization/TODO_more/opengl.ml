open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* alternatives:
 * - lablGL
 *   * lablGL + glut
 *   * lablGL + gtk
 * - glMLite
 * - camlgl ?
 * - ?
 *
 * lablGL seems better integrated with lablgtk and have IMHO nicer
 * function names (e.g. GlMat.load_identity so you know glLoadIdentity
 * is actually a function that operates on the matrix system of opengl).
 * The glMlite actually provides some script to translate programs using
 * lablGL or glMlite in one another.
*)


(*
 * ref: Jon Harrop article and books
 *)

(* todo: take stuff from otimetracker *)

(* src: Jon Harrop *)
let pi = 4. *. atan 1.

let rec nest n f x =
  match n with
  | 0 -> x
  | n -> nest (n - 1) f (f x)
