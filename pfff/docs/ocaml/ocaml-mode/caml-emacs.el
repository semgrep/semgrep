;(***********************************************************************)
;(*                                                                     *)
;(*                           Objective Caml                            *)
;(*                                                                     *)
;(*            Didier Remy, projet Cristal, INRIA Rocquencourt          *)
;(*                                                                     *)
;(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
;(*  en Automatique.  All rights reserved.  This file is distributed    *)
;(*  under the terms of the GNU General Public License.                 *)
;(*                                                                     *)
;(***********************************************************************)

;(* $Id: caml-emacs.el 10662 2010-08-30 15:15:33Z doligez $ *)

;; for caml-help.el
(defalias 'caml-info-other-window 'info-other-window)

;; for caml-types.el

(defalias 'caml-line-beginning-position 'line-beginning-position)

(defalias 'caml-read-event 'read-event)
(defalias 'caml-window-edges 'window-edges)
(defun caml-mouse-vertical-position ()
  (cddr (mouse-position)))
(defalias 'caml-ignore-event-p 'integer-or-marker-p)
(defalias 'caml-mouse-movement-p 'mouse-movement-p)
(defalias 'caml-sit-for 'sit-for)

(defmacro caml-track-mouse (&rest body) (cons 'track-mouse body))

(defun caml-event-window (e) (posn-window (event-start e)))
(defun caml-event-point-start (e) (posn-point (event-start e)))
(defun caml-event-point-end (e) (posn-point (event-end e)))

(defun caml-release-event-p (original event)
  (and (equal (event-basic-type original) (event-basic-type event))
       (let ((modifiers  (event-modifiers event)))
         (or (member 'drag modifiers)
             (member 'click modifiers)))))

(if (fboundp 'string-to-number)
   (defalias 'caml-string-to-int 'string-to-number)
 (defalias 'caml-string-to-int 'string-to-int))

(provide 'caml-emacs)
