;;; ecb-speedbar.el --- 

;; $Id: ecb-speedbar.el,v 1.16 2002/10/16 16:44:17 berndl Exp $

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
;;
;; WARNING: currently ecb-speedbar depends on patches to the speedbar which I
;; sent to the author.  Without these patches ecb-speedbar will work but your
;; source buffer may recenter itself when you change buffers.  Fully functionaly
;; but very annoying.  Hopefully these patches will make it into a 0.14beta3.
;;
;;   - UPDATE:  the patches are in his queue but have not made it in yet.

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

;;; History:
;;
;; - Sat Dec 15 2001 03:10 AM (burton@openprivacy.org): only sync up the eshell
;; if the current file is in a different dir than the speedbar.
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

;; - BUG: when I sync to a buffer in the ECB frame, the speedbar will show the
;;   correct directory.  Then, when I open another frame, and change to a buffer
;;   there, the buffer in the new frame will be synched with the speedbar.  This
;;   needs to stay in synch with the file currently open in the ECB.
;;
;; - BUG: for some reason if we hit <ENTER> in the ecb-speedbar window,
;;   sometimes a new frame will come up.
;;
;;   - this only comes up the FIRST time I select a buffer.  Could some variable
;;     be changed?  Maybe the `dframe-attached-frame' or
;;     `speedbar-attached-frame' needs to be setup correctly.
;;
;;   - Actually it seems to be a problem if we have one ECB frame and then I
;;     create another frame.
;;    
;; - instead of ecb-layout-function-20 use ecb-layout-function-speedbar-1
;;
;; - RFE: what do we do about integration with the standard JDE?  Could we tell
;;   the existing layout functions to use the speedbar instead of the
;;   directories buffer?
;;
;; we need to be able to goto the speedbar window via C-c . b (AKA bar)
;; (ecb-goto-window-speedbar)
;;
;; - BUG: bug in speedbar.  Need a feature so that the speedbar doesn't require
;;   that we HAVE to have the speedbar in a frame.  If we try to run (speedbar)
;;   when ecb-speedbar is active the ecb-frame will go away :(

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


(defun ecb-speedbar-activate()
  "Make sure the speedbar is running.  WARNING: This is very dependend on the
current speedbar implementation.  If the speedbar impl is changed this
will/could break."

  (when (not (buffer-live-p speedbar-buffer))
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

  ;;only operate if the current frame is the ECB frame.

  (when (equal (window-frame (selected-window)) ecb-frame)
    
    (save-excursion
      (let(speedbar-default-directory ecb-default-directory)

        (setq ecb-default-directory default-directory)

        (save-excursion
      
          (set-buffer ecb-speedbar-buffer-name)
        
          (setq speedbar-default-directory default-directory))

        (when (and (not (string-equal speedbar-default-directory
                                      ecb-default-directory))
                   ecb-minor-mode
                   speedbar-buffer
                   (buffer-live-p speedbar-buffer))

            (speedbar-update-contents))))))


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


(defalias 'ecb-delete-other-windows-in-editwindow-20
  'ecb-delete-other-windows-ecb-windows-right)
(defalias 'ecb-delete-window-in-editwindow-20
  'ecb-delete-window-ecb-windows-right)


(defun ecb-speedbar-goto-speedbar()
  "Goto the speedbar window."
  (interactive)

  (select-window (get-buffer-window ecb-speedbar-buffer-name)))

(add-hook 'ecb-current-buffer-sync-hook 'ecb-speedbar-current-buffer-sync)

;;FIXME: migrate this into ecb-mode-map when speedbar is ready.
(define-key ecb-mode-map "\C-c.b" 'ecb-speedbar-goto-speedbar)

;;disable automatic speedbar updates... let the ECB handle this with
;;ecb-current-buffer-sync
(speedbar-disable-update)

(provide 'ecb-speedbar)

;;; ecb-speedbar.el ends here