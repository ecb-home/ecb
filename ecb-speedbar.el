;;; ecb-speedbar.el --- 

;; $Id: ecb-speedbar.el,v 1.37 2003/01/08 10:28:05 berndl Exp $

;; Copyright (C) 2000-2003 Free Software Foundation, Inc.
;; Copyright (C) 2000-2003 Kevin A. Burton (burton@openprivacy.org)

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: 
;; Version: 1.1.

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
;; Note that this is tested with recent speedbars >= 0.14beta2. If the
;; speedbar implementation changes a lot this could break.
;;
;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: IMHO the following WARNING is not
;; necessary anymore because IMHO we need no patched speedbar at least not
;; when a version >= 0.14beta1 is used.

;; WARNING: currently ecb-speedbar depends on patches to the speedbar which I
;; sent to the author.  Without these patches ecb-speedbar will work but your
;; source buffer may recenter itself when you change buffers.  Fully functionaly
;; but very annoying.  Hopefully these patches will make it into a 0.14beta3.
;;
;;   - UPDATE:  the patches are in his queue but have not made it in yet.

;; If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; History:
;;
;; - Thu DEc 19 2002 6:54 PM (klaus.berndl@sdm.de): Full integrated in ECB and
;;   fixed some bugs. Now the speedbar integration seems to work very well.
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



;;; Code:

(eval-when-compile
  (require 'silentcomp))

(require 'speedbar)

(defconst ecb-speedbar-version-ok (string-match "^0\\.\\(1[4-9]\\|[2-9][0-9]*\\)"
                                                speedbar-version)
  "ECB can only integrate speedbar versions >= 0.14beta1 so the value is only
true for these versions.")

(defconst ecb-speedbar-adviced-functions '((speedbar-click . after)
                                           (speedbar-frame-mode . around)
                                           (speedbar-get-focus . around))
  "These functions of speedbar are always adviced if ECB is active. Each
element of the list is a cons-cell where the car is the function-symbol and
the cdr the advice-class \(before, around or after). If a function should be
adviced with more than one class \(e.g. with a before and an after-advice)
then for every class a cons must be added to this list.")


(defadvice speedbar-click (after ecb)
  "Makes the function compatible with ECB. If ECB is active and the window of
`ecb-speedbar-buffer-name' is visible \(means a layouts uses the
speedbar-integration) then the ECB-edit-window is selected at the end. So
always the edit-window is selected after clicking onto a filename in the
speedbar."
  (if (and (equal (selected-frame) ecb-frame)
           (window-live-p (get-buffer-window ecb-speedbar-buffer-name)))
      (ecb-select-edit-window)))


(defadvice speedbar-frame-mode (around ecb)
  "During running speedbar within ECB this command is disabled!"
  (message "This command is disabled during running speedbar within ECB!"))


(defadvice speedbar-get-focus (around ecb)
  "During running speedbar within ECB this function behaves like follows:
Change window focus to or from the ECB-speedbar-window. If the selected window
is not speedbar-window, then the speedbar-window is selected. If the
speedbar-window is active, then select the edit-window."
  (if (equal (current-buffer) (get-buffer ecb-speedbar-buffer-name))
      (ecb-select-edit-window)
    (ecb-speedbar-select-speedbar-window)))
  

(defun ecb-speedbar-enable-advices ()
  (dolist (elem ecb-speedbar-adviced-functions)
    (ad-enable-advice (car elem) (cdr elem) 'ecb)
    (ad-activate (car elem))))

(defun ecb-speedbar-disable-advices ()
  (dolist (elem ecb-speedbar-adviced-functions)
    (ad-disable-advice (car elem) (cdr elem) 'ecb)
    (ad-activate (car elem))))

(defconst ecb-speedbar-buffer-name " SPEEDBAR"
  "Name of the ECB speedbar buffer.")

(defun ecb-speedbar-select-speedbar-window ()
  (ignore-errors
    (and (window-live-p (get-buffer-window ecb-speedbar-buffer-name))
         (select-window (get-buffer-window ecb-speedbar-buffer-name)))))


(defun ecb-set-speedbar-buffer()
  "Set the speedbar buffer within ECB."
  (ecb-speedbar-activate)
  (set-window-buffer (selected-window)
                     (get-buffer-create ecb-speedbar-buffer-name))
  (if ecb-running-emacs-21
      (set (make-local-variable 'automatic-hscrolling) nil)))


(defvar ecb-speedbar-verbosity-level-old nil)
(defvar ecb-speedbar-select-frame-method-old nil)

(defun ecb-speedbar-activate()
  "Make sure the speedbar is running. WARNING: This could be dependend on the
current speedbar implementation but normally it should work with recent
speedbar versions >= 0.14beta1. But be aware: If the speedbar impl changes in
future this could break."

  (if (not ecb-speedbar-version-ok)
      (error "Speedbar integration needs speedbar-version >= 0.14beta1!")
    ;; enable the advices for speedbar
    (ecb-speedbar-enable-advices)
  
    ;;disable automatic speedbar updates... let the ECB handle this with
    ;;ecb-current-buffer-sync
    (speedbar-disable-update)

    ;;always stay in the current frame
    ;; save the old value but only first time!
    (if (null ecb-speedbar-select-frame-method-old)
        (setq ecb-speedbar-select-frame-method-old speedbar-select-frame-method))
    (setq speedbar-select-frame-method 'attached)

    (when (not (buffer-live-p speedbar-buffer))
      (save-excursion
        (setq speedbar-buffer (get-buffer-create ecb-speedbar-buffer-name))
        (set-buffer speedbar-buffer)
        (speedbar-mode)))

    ;;Start up the timer
    (speedbar-reconfigure-keymaps)
    (speedbar-update-contents)
    (speedbar-set-timer 1)

    ;;Set the frame that the speedbar should use.  This should be the selected
    ;;frame.  AKA the frame that ECB is running in.
    (setq speedbar-frame ecb-frame)
    (setq dframe-attached-frame ecb-frame)
  
    ;;this needs to be 0 because we can't have the speedbar too chatty in the
    ;;current frame because this will mean that the minibuffer will be updated too
    ;;much.
    ;; save the old value but only first time!
    (if (null ecb-speedbar-verbosity-level-old)
        (setq ecb-speedbar-verbosity-level-old speedbar-verbosity-level))
    (setq speedbar-verbosity-level 0)

    (add-hook 'ecb-current-buffer-sync-hook
              'ecb-speedbar-current-buffer-sync)
  
    ;;reset the selection variable
    (setq speedbar-last-selected-file nil)))

(defun ecb-speedbar-deactivate ()
  "Reset things as before activating speedbar by ECB"
  (if (not ecb-speedbar-version-ok)
      (error "Speedbar integration needs speedbar-version >= 0.14beta1!")      
    (ecb-speedbar-disable-advices)
  
    (setq speedbar-frame nil)
    (setq dframe-attached-frame nil)

    (speedbar-enable-update)
  
    (if ecb-speedbar-select-frame-method-old
        (setq speedbar-select-frame-method ecb-speedbar-select-frame-method-old))
    (setq ecb-speedbar-select-frame-method-old nil)

    (if ecb-speedbar-verbosity-level-old
        (setq speedbar-verbosity-level ecb-speedbar-verbosity-level-old))
    (setq ecb-speedbar-verbosity-level-old nil)
  
    (remove-hook 'ecb-current-buffer-sync-hook
                 'ecb-speedbar-current-buffer-sync)

    (when (and speedbar-buffer
               (buffer-live-p speedbar-buffer))
      (kill-buffer speedbar-buffer)
      (setq speedbar-buffer nil))))


(defun ecb-speedbar-current-buffer-sync()
  "Update the speedbar so that we sync up with the current file."
  (interactive)

  ;;only operate if the current frame is the ECB frame and the
  ;;ecb-speedbar-buffer is visible!
  (ecb-do-if-buffer-visible-in-ecb-frame 'ecb-speedbar-buffer-name
    ;; this macro binds the local variables visible-buffer and visible-window!
    (let ((speedbar-default-directory
           (save-excursion
             (set-buffer visible-buffer)
             (ecb-fix-filename default-directory)))
          (ecb-default-directory (ecb-fix-filename default-directory)))
      
      (when (and (not (string-equal speedbar-default-directory
                                    ecb-default-directory))
                 speedbar-buffer
                 (buffer-live-p speedbar-buffer))
        (speedbar-update-contents)))))

(silentcomp-provide 'ecb-speedbar)

;;; ecb-speedbar.el ends here