;;; ecb-speedbar.el --- 

;; $Id: ecb-speedbar.el,v 1.6 2001/12/15 10:11:35 burtonator Exp $

;; Copyright (C) 2000-2003 Free Software Foundation, Inc.
;; Copyright (C) 2000-2003 Kevin A. Burton (burton@openprivacy.org)

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: 
;; Version: 1.0.0

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provide speedbar integration for the ECB.
;;
;; This allows you to:
;;
;; - Sync up to the speedbar with the current buffer.
;;
;; - Files opened with the speedbar are displayed in the ecb source window.
;;
;; Note that this is only known to work under Speedbar 0.14beta2

;; If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; Design:

;; There are two major issues we have with the speedbar-frame variable.
;;
;; 1. If we set this value to the (selected-frame), when set change buffers,
;; the current buffers point is reset to (point-min)
;;
;; 2. If we set this to a newly created frame, say an invisible frame, we have
;; the following problems:
;;
;;   - all the glphys in the speedbar window are NOT set.
;;
;;   - if we hit [ENTER] in the speedbar window the invisible frame is made
;;     visible  :(

;;; NOTE: it could be possible to fix this.  We could setup two variables.
;;; ecb-speedbar-invisible-frame and ecb-frame (actually already defined).
;;; Within ecb-current-buffer-sync-hook we could then set speedbar-frame and
;;; dframe-attached-frame to ecb-speedbar-invisible-frame and then restore these
;;; variables to ecb-frame when complete.

;;; History:
;;
;; - Fri Dec 14 2001 10:11 PM (burton@openprivacy.org): when we hit <ENTER> on a
;; file in the speedbar window, a new window is created.
;;
;; - Sun Nov 18 2001 01:46 AM (burton@openprivacy.org): BUG: we need to set
;; dframe-activate-frame to the current frame and NOT use an invisible frame.
;; This is important because when I select a buffer in ECB it can't use the
;; invisible frame.  :(
;;
;; Sat Nov 10 2001 09:30 PM (burton@openprivacy.org): implementation of
;; ecb-delete-other-windows-in-editwindow-20
;;

;;; TODO:

;; - only sync up the eshell if the current file is in a different dir than the
;; speedbar.  

;; - instead of ecb-layout-function-20 use ecb-layout-function-speedbar-1

;; we need to be able to goto the speedbar window via C-c . d
;; (ecb-speedbar-goto-window)

;;; Code:

(require 'ecb)
(require 'speedbar)

(defvar ecb-speedbar-buffer-name " SPEEDBAR" "Name of the ECB speedbar buffer.")

(defun ecb-set-speedbar-buffer()
  "Set the speedbar buffer within ECB."
  
  (ecb-speedbar-activate)
  
  (set-window-dedicated-p (selected-window) nil)
  
  (set-window-buffer (selected-window) (get-buffer ecb-speedbar-buffer-name))

  (set-window-dedicated-p (selected-window) t))

(defun ecb-layout-function-20()

  (when ecb-compile-window-height
    (ecb-split-ver (* -1 ecb-compile-window-height) t)
    (setq ecb-compile-window (next-window)))

  (ecb-split-hor (- ecb-windows-width) t)

  (setq ecb-edit-window (selected-window))

  (select-window (next-window))

  (ecb-set-speedbar-buffer)

  (ecb-split-ver 0.5)
  (ecb-set-methods-buffer))

(defun ecb-delete-other-windows-in-editwindow-20 (split)
  (cond ((equal (ecb-point-in-edit-window) 2)
         (setq ecb-edit-window (selected-window))
         (delete-window (previous-window))
         t)
        ((equal (ecb-point-in-edit-window) 1)
         (delete-window (next-window))
         t)
        (t nil)))

(defun ecb-delete-window-in-editwindow-20 (split)
  (cond ((equal (ecb-point-in-edit-window) 2)
         (delete-window)
         (ecb-select-edit-window)
         t)
        ((equal (ecb-point-in-edit-window) 1)
         (setq ecb-edit-window (next-window))
         (delete-window)
         t)
        (t nil)))

(defun ecb-speedbar-activate()
  "Make sure the speedbar is running.  WARNING: This is very dependend on the
current speedbar implementation.  If the speedbar impl is changed this
will/could break."

  (if (not (buffer-live-p speedbar-buffer))
      (save-excursion
        (setq speedbar-buffer (get-buffer-create ecb-speedbar-buffer-name))
        (set-buffer speedbar-buffer)
        (speedbar-mode)))

  ;;Start up the timer

  (speedbar-reconfigure-keymaps)
  (speedbar-update-contents)
  (speedbar-set-timer 1)

  (set (make-local-variable 'automatic-hscrolling) nil) ;;Emacs 21

  ;;Set the frame that the speedbar should use.  This should be the selected
  ;;frame.  AKA the frame that ECB is running in.

  (setq speedbar-frame ecb-frame)
  (setq speedbar-attached-frame ecb-frame)
  (setq dframe-attached-frame ecb-frame)
  
  ;;this needs to be 0 because we can't have the speedbar too chatty in the
  ;;current frame because this will mean that the minibuffer will be updated too
  ;;much.
  (setq speedbar-verbosity-level 0)
  
  ;;reset the selection variable
  (setq speedbar-last-selected-file nil))

(defun ecb-speedbar-current-buffer-sync()
  "Update the speedbar so that we sync up with the current file."
  (interactive)

  (if ecb-minor-mode

      (save-excursion

        (if (and speedbar-buffer
                 (buffer-live-p speedbar-buffer))
            (speedbar-update-contents)))))

(defun ecb-speedbar-goto-speedbar()
  (interactive)

  (select-window (get-buffer-window ecb-speedbar-buffer-name)))

(add-hook 'ecb-current-buffer-sync-hook 'ecb-speedbar-current-buffer-sync)

(provide 'ecb-speedbar)

;;; ecb-speedbar.el ends here