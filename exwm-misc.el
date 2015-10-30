;;; exwm-misc.el --- Misc EXWM subroutines.

;; Copyright (C) 2015 by Zajcev Evegny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Oct 29 17:55:13 2015
;; Keywords: 

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun exwm--nth-arg (arg nlist &optional el)
  (when (< arg 0)
    (setq nlist (nreverse nlist))
    (setq arg (- arg)))

  (while (> arg 0)
    (setq el (or (cadr (memq el nlist)) (car nlist)))
    (decf arg))
  el)

;;; Some useful operations on lists
(defun exwm-list--insert-after (list aft-el el)
  "In LIST after AFT-EL insert EL."
  (push el (cdr (member aft-el list)))
  list)

(defun exwm-list--insert-before (list bef-el el)
  "In LIST before BEF-EL insert EL."
  (nreverse (exwm-list--insert-after (nreverse list) bef-el el)))

(defun exwm-list--set-element (list old-el new-el)
  "In LIST set OLD-EL to NEW-EL."
  (setcar (or (memq old-el list) '(dummy)) new-el)
  list)

(defun exwm-list--exchange-els (list el1 el2)
  "In LIST exchange places of EL1 and EL2."
  (when (and (memq el1 list) (memq el2 list)
             (not (eq el1 el2)))
    (exwm-list--set-element list el1 'this-fake-name1-should-not-be-in-list)
    (exwm-list--set-element list el2 el1)
    (exwm-list--set-element list 'this-fake-name1-should-not-be-in-list el2))
  list)

(provide 'exwm-misc)

;;; exwm-misc.el ends here
