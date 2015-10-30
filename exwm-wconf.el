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

;; To enable tabbing, use:
;;   (exwm-wconf-tabs-mode 1)
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
  (cons (current-buffer) (cons (selected-window) (winner-conf))))

(defun exwm-wconf--switch (wconf)
  "Switch to WCONF."
  (unless (eq wconf (exwm-wconf--selected))
    ;; If switching to new wconf then simple push, otherwise need to
    ;; update selected wconf
    (if (memq wconf (exwm-wconf--list))
        (exwm-list--set-element
         (exwm-wconf--list) (exwm-wconf--selected) (exwm-wconf--current))
      (pushnew wconf (exwm-wconf--list)))

    (set-window-configuration (caddr wconf))
    ;; `set-window-buffer' updates buffer-display-time!
    (set-window-buffer (selected-window) (car wconf))
    (setf (exwm-wconf--selected) wconf)

    (run-hook-with-args 'exwm-wconf-switch-hook wconf)))


;;;###autoload
(defun exwm-wconf-push ()
  "Push current window configuration to list."
  (interactive)
  (exwm-wconf--switch (exwm-wconf--current)))

;;;###autoload
(defun exwm-wconf-remove ()
  "Remove selected window configuration from wconf list."
  (interactive)
  (setf (exwm-wconf--list) (delq (exwm-wconf--selected) (exwm-wconf--list))
        (exwm-wconf--selected) nil)

  (run-hook-with-args 'exwm-wconf-switch-hook))

;;;###autoload
(defun exwm-wconf-next (arg)
  "Switch to ARG next wconf."
  (interactive "p")

  (let ((wsel (exwm-wconf--selected))
        (wcs (exwm-wconf--list))
        wcinx num nwc)
    (unless wcs
      (user-error "[EXWM] No wconfs, use M-x exwm-wconf-push RET"))

    (setq wcinx (- (length wcs) (length (memq wsel wcs))))
    (setq num (% (+ wcinx arg) (length wcs)))
    (setq nwc (nth (if (natnump num) num (+ (length wcs) num)) wcs))

    (exwm-wconf--switch nwc)))

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

    (exwm-wconf--switch (exwm--nth-arg arg wcs wsel))))

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

;;; Tabbing
(defvar exwm-wconf--tabs-enabled nil)

(defface exwm-wconf-active-face
  '((t :inherit header-line
       :background "#008a00"
       :weight bold
     ))
  "Face for active wconf."
  :group 'exwm-wconf)

(defvar exwm-wconf--header-line-format '("%e" (:eval (exwm-wconf--header-line))))

(defun exwm-wconf--header-line ()
  "Generate header line format for current wconf."
  (let* ((wcs (exwm-wconf--list))
         (wsel (exwm-wconf--selected))
         (wpsize (window-pixel-width))
         (hlsize (window-font-width nil 'header-line))
         (tsize (/ (/ wpsize hlsize) (length wcs))))
    (mapconcat 'identity
               (mapcar #'(lambda (wc)
                           (let ((fs (format (format " %%-%ds" (1- tsize))
                                             (buffer-name (if (eq wc wsel) (current-buffer) (car wc))))))
                             (if (eq wc wsel)
                                 (propertize fs 'face 'exwm-wconf-active-face)
                               fs)))
                       wcs)
               (char-to-string #x2502))))

(defun exwm-wconf--tabs-refresh ()
  "Refresh header-mode-line, show header-line only in topmost window."
  (let* ((fwin (frame-first-window))
         (wbuf (window-buffer fwin)))

    (mapc #'(lambda (w)
              (with-current-buffer (window-buffer w)
                (if (eq (current-buffer) wbuf)
                    (setq-local header-line-format exwm-wconf--header-line-format)
                  (setq-local header-line-format nil))))
          (window-list))))

;;;###autoload
(defun exwm-wconf-tabs-mode (arg)
  "Toggle tabbing mode."
  (interactive "p")
  (if (> arg 0)
      (progn
        (add-hook 'exwm-wconf-switch-hook 'force-mode-line-update)
        (add-hook 'window-configuration-change-hook 'exwm-wconf--tabs-refresh)
        (exwm-wconf--tabs-refresh))

    (remove-hook 'exwm-wconf-switch-hook 'force-mode-line-update)
    (remove-hook 'window-configuration-change-hook 'exwm-wconf--tabs-refresh)
    ))
  

(provide 'exwm-wconf)

;;; exwm-wconf.el ends here
