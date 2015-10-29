;;; exwm-wconf.el --- window configurations for exwm.

;; Copyright (C) 2015 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Thu Oct 29 17:18:58 2015
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
(require 'exwm-core)
(require 'exwm-misc)
(require 'winner)

(defcustom exwm-wconf-switch-hook nil
  "Functions to run when wconf is switched to."
  :type 'hook
  :group 'exwm-wconf)


(defun exwm-wconf--list (&optional frame)
  (frame-parameter frame 'exwm-win-configurations))

(defsetf exwm-wconf--list (&optional frame) (wlist)
  `(set-frame-parameter ,frame 'exwm-win-configurations ,wlist))

(defun exwm-wconf--selected (&optional frame)
  (frame-parameter frame 'exwm-wconf))

(defsetf exwm-wconf--selected (&optional frame) (wconf)
  `(set-frame-parameter ,frame 'exwm-wconf ,wconf))


(defun exwm-wconf--recency-sorter (wc1 wc2)
  (time-less-p
   (buffer-local-value 'buffer-display-time (car wc2))
   (buffer-local-value 'buffer-display-time (car wc1))))

(defun exwm-wconf--current ()
  (cons (current-buffer) (winner-conf)))

(defun exwm-wconf--switch (wconf &optional fast-p)
  "Switch to WCONF."
  (unless (eq wconf (exwm-wconf--selected))
    (unless fast-p
      (set-window-configuration (cadr wconf)))

    (setf (exwm-wconf--selected) wconf)
    (run-hook-with-args 'exwm-wconf-switch-hook wconf)))

(defun exwm-wconf--update ()
  "Update current wconf.
Do nothing if no wconf is currently selected."
  (let ((wconf (exwm-wconf--selected)))
    (when wconf
      (let ((newwc (exwm-wconf--current)))
        (exwm-list--set-element (exwm-wconf--list) wconf newwc)
        (exwm-wconf--switch newwc :fast)))))


;;;###autoload
(defun exwm-wconf-push ()
  "Push current window configuration to list."
  (interactive)

  (let ((wconf (exwm-wconf--current)))
    (push wconf (exwm-wconf--list))

    ;; Switch to newly created wconf
    (exwm-wconf--switch wconf)))

;;;###autoload
(defun exwm-wconf-remove ()
  "Remove selected window configuration from wconf list."
  (interactive)
  (setf (exwm-wconf--list)
        (delq (exwm-wconf--selected) (exwm-wconf--list)))

  (run-hook-with-args 'exwm-wconf-switch-hook))

;;;###autoload
(defun exwm-wconf-next (arg)
  "Switch to ARG next wconf."
  (interactive "p")

  (let ((wc (exwm-wconf--selected))
        (cws (exwm-wconf--list))
        wcinx num nwc)
    (unless cws
      (user-error "[EXWM] No wconfs, use M-x exwm-wconf-push RET"))

    (setq wcinx (- (length cws) (length (memq wc cws))))
    (setq num (% (+ wcinx arg) (length cws)))
    (setq nwc (nth (if (natnump num) num (+ (length cws) num)) cws))

    (unless (eq wc nwc)
      (exwm-wconf-update)
      (exwm-wconf--switch nwc))))

;;;###autoload
(defun exwm-wconf-prev (arg)
  "Switch to ARG prev wconf."
  (interactive "p")
  (exwm-wconf-next (- arg)))

;;;###autoload
(defun exwm-wconf-other (arg)
  "Switch to ARG's other wconf."
  (interactive "p")
  (let* ((wcs (sort (copy-list (exwm-wconf--list)) 'exwm-wconf--recency-sorter))
         (wsel (exwm-wconf--selected))
         (nwc (exwm--nth-arg arg wcs wsel)))

    (message "BUFS: %s" (mapcar 'buffer-name (mapcar 'car wcs)))
    (unless (eq wsel nwc)
      (exwm-wconf-update)
      (exwm-wconf--switch (exwm--nth-arg arg wcs wsel)))))

;;;###autoload
(defun exwm-wconf-transpose (arg)
  "Transpose selected wconf with the next one.
If ARG is given, then transpose with previous one."
  (interactive "P")
  (let* ((wcs (exwm-wconf--list))
         (wsel (exwm-wconf--selected))
         (tai (cadr (memq wsel (funcall (if arg 'reverse 'identity) wcs)))))
    (when tai
      (exwm-list--exchange-els wcs wsel tai)
      (run-hook-with-args 'exwm-wconf-switch-hook))))


(provide 'exwm-wconf)

;;; exwm-wconf.el ends here
