;;; ecb-speedbar.el --- 

;; $Id: ecb-speedbar.el,v 1.1 2001/11/18 05:34:36 burtonator Exp $

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
;; Note that if you use this package it will break the exiting ECB.  IE don't
;; use the 'speedbar' function because you will get mixed results.

;; NOTE: If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; History:
;;
;; Sat Nov 10 2001 09:30 PM (burton@openprivacy.org): implementation of
;; ecb-delete-other-windows-in-editwindow-20
;;

;;; TODO:

;; - instead of ecb-layout-function-20 use ecb-layout-function-speedbar-1

;; we need to be able to goto the speedbar window via C-c . d
;; (ecb-speedbar-goto-window)

;;; Code:

(require 'ecb)
(require 'speedbar)

(defun ecb-set-speedbar-buffer()

  (ecb-speedbar-activate)
  
  (set-window-dedicated-p (selected-window) nil)
  
  (set-window-buffer (selected-window) (get-buffer " SPEEDBAR"))

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
        (setq speedbar-buffer (get-buffer-create " SPEEDBAR"))
        (set-buffer speedbar-buffer)
        (speedbar-mode)))

  ;;Start up the timer

  (speedbar-reconfigure-keymaps)
  (speedbar-update-contents)
  (speedbar-set-timer 1)

  (set (make-local-variable 'automatic-hscrolling) nil) ;;Emacs 21

  ;;set the frame that the speedbar should use.  Right now we are just using an
  ;;invisible frame.

  (setq speedbar-frame (make-frame '((visibility . nil))))

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

        (message "ecb is updating speedbar...done")
    
        (if (and speedbar-buffer
                 (buffer-live-p speedbar-buffer))
            (speedbar-update-contents))

        (message "ecb is updating speedbar...done"))))
  
(add-hook 'ecb-current-buffer-sync-hook 'ecb-speedbar-current-buffer-sync)

(provide 'ecb-speedbar)

;;; ecb-speedbar.el ends here