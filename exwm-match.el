;;; exwm.el --- Emacs X Window Manager  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Maintainer: Chris Feng <chris.w.feng@gmail.com>
;; Version: 0
;; Package-Requires: ((xelb "0.3"))
;; Keywords: unix
;; URL: https://github.com/ch11ng/exwm

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

;; Module to match X clients managed by exwm

;;; Code:

(defvar exwm--qualifiers nil "List of qualifiers.")

(defmacro define-exwm-qualifier (name args doc &rest body)
  "Define new qualifier named by NAME.
DOC - Documentation for matcher.
PREDICATE - Function to call with single argument - PARAM."
  `(push (cons (quote ,name)
               (list ,doc #'(lambda ,args ,@body)))
         exwm--qualifiers))

;;;###exwm-autoload
(defun exwm--match-p (qualifier &optional buf)
  "Check whether buffer BUF matches QUALIFIER.
Here is QUALIFIER format in EBNF:

  QUALIFIER ::= <SIMPLE-QUALIFIER> | ( {or|and} <QUALIFIER>* )
  SIMPLE-QUALIFIER ::= ( <TYPE> <PARAM> )
  TYPE ::= one of defined in `exwm--qualifiers'
  PARAM := any data
"
  (with-current-buffer (or buf (current-buffer))
    (let ((case-fold-search nil)          ; case sensivity searching
          (ao-type nil))                  ; and-or matching

      ;; Setup global matching, default to 'and
      (when (memq (car qualifier) '(or and))
        (setq ao-type (car qualifier)
              qualifier (cdr qualifier)))

      (if (not ao-type)
          ;; Check simple qualifier for matching
          (let* ((type (car qualifier))
                 (param (cadr qualifier))
                 (dq (assq type exwm--qualifiers)))
            (unless dq
              (error "Unknown qualifier type: %S" type))
            (funcall (third dq) param))

      (let (ao-res or)
        ;; Scan QUALIFIER for matching
        (while qualifier
          (setq ao-res (exwm--match-p (car qualifier) buf))
          (if (or (and (eq ao-type 'and) (null ao-res))
                  (and (eq ao-type 'or) ao-res))
              ;; Break conditions, not-match or already matches
              (setq qualifier nil)
            ;; Continue traversing
            (setq qualifier (cdr qualifier))))

        ao-res)))))

(define-exwm-qualifier not (param)
  "Return non-nil if PARAM is false."
  (not (exwm--match-p param)))

(define-exwm-qualifier class-inst (param)
  "Return non-nil if PARAM matches class instance."
  (or (null param)
      (and exwm-instance-name
           (string-match param exwm-instance-name))))

(define-exwm-qualifier class-name (param)
  "Return non-nil if PARAM matches class name."
  (or (null param)
      (and exwm-class-name
           (string-match param exwm-class-name))))

(define-exwm-qualifier name (param)
  "Return non-nil if PARAM matches title."
  (or (null param)
      (and exwm-title
           (string-match param exwm-title))))

(define-exwm-qualifier workspace (param)
  "Return non-nil if buffer's workspace is PARAM.
Null PARAM matches any workspace."
  (or (null param)
      (eq param exwm--frame)))

(define-exwm-qualifier command (param)
  "Return non-nil if PARAM matches WM_COMMAND."
  (or (null param)
      (and exwm-command
           (string-match param exwm-command))))

(define-exwm-qualifier property (param)
  "Return non-nil if CL's property PARAM is set.
PARAM could be either symbol or cons cell.
If PARAM is symbol then return non-nil if PARAM property is set.
If PARAM is cons cell then return non-nil if property named by PARAM's
car is set and `equal' to PARAM's cdr."
  (cond ((symbolp param)
         (exwm--get-prop param))
        ((and (consp param)
              (symbolp (car param)))
         (equal (exwm--get-prop param)
                (cdr param)))))

(define-exwm-qualifier predicate (param)
  "Return non-nil if PARAM predicate function return non-nil.
Predicate called without arguments."
  (funcall param))

(define-exwm-qualifier t (param)
  "Always return non-nil."
  t)

(define-exwm-qualifier nil (param)
  "Always return nil."
  nil)

(define-exwm-qualifier buffer-major-mode (param)
  "Return non-nil if buffer's major-mode is PARAM."
  (eq major-mode param))

(define-exwm-qualifier exwm-mode (param)
  "Return non-nil if buffer in `exwm-mode'."
  (eq major-mode 'exwm-mode))

(define-exwm-qualifier buffer-name (param)
  "Return non-nil if buffer's name matches PARAM."
  (string-match param (buffer-name)))

(define-exwm-qualifier buffer-filename (param)
  "Return non-nil if buffer's filename matches PARAM."
  (and param
       (string-match param (buffer-file-name))))


(defun exwm--guest-qualifier (&optional buf)
  (with-current-buffer (or buf (current-buffer))
    (if (eq major-mode 'exwm-mode)
        `(and (class-name ,(concat "^" exwm-class-name "$"))
              (class-inst ,(concat "^" exwm-instance-name "$")))
      `(buffer-major-mode ,major-mode))))

(defun exwm--list (qual &optional buffers)
  "List buffers matching qualifier QUAL."
  (remove-if-not #'(lambda (buf)
                     (exwm--match-p qual buf))
                 (or buffers (buffer-list))))

(defun exwm--x-list (qual)
  "List only X clients buffers matching QUAL.
Fast version of `exwm--list' when many buffers are open."
  (exwm--list qual (mapcar #'cdr exwm--id-buffer-alist)))

;;;###autoload
(defun exwm--forward-app (arg)
  (interactive "p")
  (switch-to-buffer
   (exwm--nth-arg arg (exwm--list (exwm--guest-qualifier))
                  (current-buffer))))

;;;###autoload
(defun exwm--backward-app (arg)
  (interactive "p")
  (exwm--forward-app (- arg)))


(provide 'exwm-match)

;; exwm-match.el ends here
