;;; ecb-speedbar.el --- 

;; $Id: ecb-speedbar.el,v 1.27 2002/12/19 00:09:05 burtonator Exp $

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

;; (speedbar-current-frame) doesn't seem to work right..
;; 

;;; Code:

(eval-when-compile
  (require 'silentcomp))

(require 'ecb)
(require 'speedbar)

(silentcomp-defvar speedbar-attached-frame)
(silentcomp-defvar dframe-attached-frame)
(silentcomp-defvar speedbar-select-frame-method)

(defvar ecb-speedbar-buffer-name " SPEEDBAR" "Name of the ECB speedbar buffer.")

(defun ecb-set-speedbar-buffer()
  "Set the speedbar buffer within ECB."
  (ecb-speedbar-activate)
  (set-window-dedicated-p (selected-window) nil)
  (set-window-buffer (selected-window) (get-buffer-create ecb-speedbar-buffer-name))
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

;;TODO: Klaus Berndl <klaus.berndl@sdm.de>: The following redefinitions have
;;to be implemented by an advice which is active during active ECB and not
;;active if ECB is not active! Otherwise speedbar will never work correct
;;after deactivating ECB!

;;TODO: Klaus Berndl <klaus.berndl@sdm.de>: Damn. What is the reason that
;;point does NOT stay in the edit-window after a click onto a filename in the
;;speedbar-window. The file is opened in the edit-window but the window is not
;;selected. ecb-find-file-and-display should do this. Seems that
;;speedbar/dframe does something magic, so this can not happen...
;; I have also tried redefining speedbar-find-file and
;; speedbar-do-function-pointer in senseful ways but no success :-(( Maybe we
;; have to ask Eric....

;;  TODO: Kevin Burton <burton@peerfear.org> Klaus.  Your problem is due to the
;;  speedbar bugs I was talking about.  I can give you a copy of my speedbar.  I
;;  will try to get a decent patch setup and at least documented.  My changes
;;  were made to 0.14b2.  I do not seem to have this problem with 0.14b4 but it
;;  might just be my installation.
;; 

;; (defun speedbar-find-file-in-frame(file)
;;   "This will load FILE into the speedbar attached frame.  If the file is being
;; displayed in a different frame already, then raise that frame instead.  Note
;; that this is a reimplemntation of this for the ECB that does no frame
;; selection"
;;   (ecb-find-file-and-display file nil))

;; (defun speedbar-find-file (text token indent)
;;   "Speedbar click handler for filenames.
;; TEXT, the file will be displayed in the attached frame.
;; TOKEN is unused, but required by the click handler.  INDENT is the
;; current indentation level."
;;   (let ((cdd (speedbar-line-path indent)))
;;     (message "I clicked at %s" (concat cdd text))
;;     (ecb-find-file-and-display (concat cdd text) nil)))

;; (defun speedbar-do-function-pointer ()
;;   "Look under the cursor and examine the text properties.
;; From this extract the file/tag name, token, indentation level and call
;; a function if appropriate"
;;   (let* ((speedbar-frame (speedbar-current-frame))
;; 	 (fn (get-text-property (point) 'speedbar-function))
;; 	 (tok (get-text-property (point) 'speedbar-token))
;; 	 ;; The 1-,+ is safe because scaning starts AFTER the point
;; 	 ;; specified.  This lets the search include the character the
;; 	 ;; cursor is on.
;; 	 (tp (previous-single-property-change
;; 	      (1+ (point)) 'speedbar-function))
;; 	 (np (next-single-property-change
;; 	      (point) 'speedbar-function))
;; 	 (txt (buffer-substring-no-properties (or tp (point-min))
;; 					      (or np (point-max))))
;; 	 (dent (save-excursion (beginning-of-line)
;; 			       (string-to-number
;; 				(if (looking-at "[0-9]+")
;; 				    (buffer-substring-no-properties
;;                                      (match-beginning 0) (match-end 0))
;; 				  "0")))))
;;     ;;(speedbar-message "%S:%S:%S:%s" fn tok txt dent)
;;     (and fn (funcall fn txt tok dent))))


;;TODO: Klaus Berndl <klaus.berndl@sdm.de>: It should not be easy introducing
;;a new option ecb-use-speedbar-for-directories and evaluating it in
;;ecb-set-directories-buffer like follows:
;;
;; (defun ecb-set-directories-buffer ()
;;   (if ecb-use-speedbar-for-directories
;;       (ecb-set-speedbar-buffer)
;;     (ecb-set-buffer ecb-directories-buffer-name)))
;;
;; So we would not need extra layouts for the speedbar-integration but we could
;; use speedbar for all layouts which have a directories-window in its layout.
;; So the following layout-definition would be superfluous...

;; TODO: Kevin Burton <burton@peerfear.org> +1 I am in favor of this option. I
;; haven't looked back at setting up alternative window layouts.  Since it
;; wasn't stable I never went back and look at providing an alternative to the
;; directories buffer.

;; the special speedbar layout.
(ecb-layout-define "speedbar1" right
  "ECB layout with integrated speedbar."  
  (let ((edit-win (previous-window (selected-window) 0)))
    (ecb-set-speedbar-buffer)
    (ecb-split-ver 0.5)
    (ecb-set-methods-buffer)
    (select-window edit-win)))


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

;;always stay in the current frame
(setq speedbar-select-frame-method 'attached)
(setq dframe-activity-change-focus-flag t)

(silentcomp-provide 'ecb-speedbar)

;;; ecb-speedbar.el ends here