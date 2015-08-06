;;; exwm-layout.el --- Layout Module for EXWM  -*- lexical-binding: t -*-

;; Copyright (C) 2015 Chris Feng

;; Author: Chris Feng <chris.w.feng@gmail.com>
;; Keywords: unix

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module is responsible for keeping X client window properly displayed.

;;; Code:

(defun exwm-layout--show (id &optional window)
  "Show window ID exactly fit in the Emacs window WINDOW."
  (xcb:+request exwm--connection (make-instance 'xcb:MapWindow :window id))
  (xcb:+request exwm--connection
      (make-instance 'xcb:icccm:set-WM_STATE
                     :window id :state xcb:icccm:WM_STATE:NormalState
                     :icon xcb:Window:None))
  (let* ((edges (or (exwm--with-current-id id exwm--floating-edges)
                    (window-inside-pixel-edges window)))
         (x (elt edges 0))
         (y (elt edges 1))
         (width (- (elt edges 2) (elt edges 0)))
         (height (- (elt edges 3) (elt edges 1))))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window id
                       :value-mask (logior xcb:ConfigWindow:X
                                           xcb:ConfigWindow:Y
                                           xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height
                                           xcb:ConfigWindow:StackMode)
                       :x x :y y :width width :height height
                       ;; In order to put non-floating window at bottom
                       :stack-mode xcb:StackMode:Below))
    (xcb:+request exwm--connection
        (make-instance 'xcb:SendEvent
                       :propagate 0 :destination id
                       :event-mask xcb:EventMask:StructureNotify
                       :event (xcb:marshal
                               (make-instance 'xcb:ConfigureNotify
                                              :event id :window id
                                              :above-sibling xcb:Window:None
                                              :x x :y y
                                              :width width :height height
                                              :border-width 0
                                              :override-redirect 0)
                               exwm--connection))))
  (xcb:flush exwm--connection))

(defun exwm-layout--hide (id)
  "Hide window ID."
  (unless (eq xcb:icccm:WM_STATE:IconicState ;already hidden
              (with-current-buffer (exwm--id->buffer id) exwm-state))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeWindowAttributes
                       :window id :value-mask xcb:CW:EventMask
                       :event-mask xcb:EventMask:NoEvent))
    (xcb:+request exwm--connection (make-instance 'xcb:UnmapWindow :window id))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeWindowAttributes
                       :window id :value-mask xcb:CW:EventMask
                       :event-mask exwm--client-event-mask))
    (xcb:+request exwm--connection
        (make-instance 'xcb:icccm:set-WM_STATE
                       :window id
                       :state xcb:icccm:WM_STATE:IconicState
                       :icon xcb:Window:None))
    (xcb:flush exwm--connection)))

(defun exwm-layout-set-fullscreen (&optional id)
  "Make window ID fullscreen."
  (interactive)
  (with-current-buffer (if id (exwm--id->buffer id) (window-buffer))
    ;; Set the floating frame fullscreen first when the client is floating
    (when exwm--floating-frame
      (let* ((outer-id (frame-parameter exwm--floating-frame 'exwm-outer-id))
             (geometry (xcb:+request-unchecked+reply exwm--connection
                           (make-instance 'xcb:GetGeometry
                                          :drawable outer-id))))
        (setq exwm--floating-frame-geometry
              (vector (slot-value geometry 'x) (slot-value geometry 'y)
                      (slot-value geometry 'width)
                      (slot-value geometry 'height)))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window outer-id
                           :value-mask (logior xcb:ConfigWindow:X
                                               xcb:ConfigWindow:Y
                                               xcb:ConfigWindow:Width
                                               xcb:ConfigWindow:Height)
                           :x 0 :y 0
                           :width (x-display-pixel-width)
                           :height (x-display-pixel-height))))
      (xcb:flush exwm--connection))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window exwm--id
                       :value-mask (logior xcb:ConfigWindow:X
                                           xcb:ConfigWindow:Y
                                           xcb:ConfigWindow:Width
                                           xcb:ConfigWindow:Height)
                       :x 0 :y 0
                       :width (x-display-pixel-width)
                       :height (x-display-pixel-height)))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                       :window exwm--id
                       :data (vector xcb:Atom:_NET_WM_STATE_FULLSCREEN)))
    (xcb:flush exwm--connection)
    (cl-assert (not exwm--fullscreen))
    (setq exwm--fullscreen t)
    (exwm-input-release-keyboard)))

(defun exwm-layout-unset-fullscreen (&optional id)
  "Restore window from fullscreen state."
  (interactive)
  (with-current-buffer (if id (exwm--id->buffer id) (window-buffer))
    ;; Restore the floating frame if the client is floating
    (when exwm--floating-frame
      (xcb:+request exwm--connection
          (make-instance 'xcb:ConfigureWindow
                         :window (frame-parameter exwm--floating-frame
                                                  'exwm-outer-id)
                         :value-mask (logior xcb:ConfigWindow:X
                                             xcb:ConfigWindow:Y
                                             xcb:ConfigWindow:Width
                                             xcb:ConfigWindow:Height)
                         :x (elt exwm--floating-frame-geometry 0)
                         :y (elt exwm--floating-frame-geometry 1)
                         :width (elt exwm--floating-frame-geometry 2)
                         :height (elt exwm--floating-frame-geometry 3))))
    (exwm-layout--show exwm--id)
    (xcb:+request exwm--connection
        (make-instance 'xcb:ewmh:set-_NET_WM_STATE :window exwm--id :data []))
    (xcb:flush exwm--connection)
    (cl-assert exwm--fullscreen)
    (setq exwm--fullscreen nil)
    (exwm-input-grab-keyboard)))

(defvar exwm-layout--window-configuration (current-window-configuration)
  "Last saved window configuration, for avoiding unnecessary refreshes.")

(defun exwm-layout--refresh ()
  "Refresh layout."
  (unless (compare-window-configurations exwm-layout--window-configuration
                                         (current-window-configuration))
    (setq exwm-layout--window-configuration (current-window-configuration))
    (let ((frame (selected-frame))
          windows)
      (if (not (memq frame exwm-workspace--list))
          ;; Refresh a floating frame
          (when (eq major-mode 'exwm-mode)
            (with-current-buffer (window-buffer (frame-first-window frame))
              (exwm-layout--show exwm--id (frame-first-window frame))))
        ;; Refresh the whole workspace
        (dolist (pair exwm--id-buffer-alist)
          (with-current-buffer (cdr pair)
            ;; Exclude windows on other workspaces and floating frames
            (when (and (eq frame exwm--frame) (not exwm--floating-frame))
              (setq windows (get-buffer-window-list (current-buffer) 0))
              (if (not windows)
                  (exwm-layout--hide exwm--id)
                (exwm-layout--show exwm--id (car windows))
                (dolist (i (cdr windows))
                  (set-window-buffer i "*scratch*"))))))))))

(defun exwm-layout--init ()
  "Initialize layout module."
  ;; Auto refresh layout
  (add-hook 'window-configuration-change-hook 'exwm-layout--refresh))



(provide 'exwm-layout)

;;; exwm-layout.el ends here