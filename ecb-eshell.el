;;; ecb-eshell.el --- eshell integration for the ECB.

;; $Id: ecb-eshell.el,v 1.58 2003/01/02 14:09:46 berndl Exp $

;; Copyright (C) 2000-2003 Free Software Foundation, Inc.
;; Copyright (C) 2000-2003 Kevin A. Burton (burton@openprivacy.org)

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: 
;; Version: 1.1.0

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

;; This package provides eshell integration for the ECB.  This basically allows
;; you to jump to the eshell in the compilation window, synch up the current
;; eshell with the current ECB buffer and run commands without getting in the
;; way.
;;
;; It provides the following features:
;;
;; - ability to jump to the eshell buffer within the compilation window ( C-.e )
;;   If the eshell isn't running it will be started
;;
;; - expands the compilation window when you run commands.  So for example it
;;   allows you to view the eshell in minimized mode and then when you run 'ls'
;;   the window automatically expands.
;;
;; - Sychronizes the current directory of the eshell with the current buffer
;;   with each every ECB buffer.
;;
;; - Provides smart window layout of the eshell buffer.  This makes sure that
;;   the eshell is taking up the exact amount of space and that nothing is
;;   hidden.
;; 
;; The goal is to make it easy to jump to a command prompt to run OS level
;; commands.  
;; 
;; If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; History:

;; - Mon Dec 30 2002 6:57 PM (klaus.berndl@sdm.de):
;;   Added ecb-eshell-start. See docstring.
;; - Son Dec 29 2002 9:43 AM (klaus.berndl@sdm.de):
;;   Fixes some bugs and cleans up the code:
;;   + ecb-eshell-save-buffer-history: In macros with a let-clause we must
;;     generate the local variables with make-symbol!!
;;   + ecb-eshell-current-buffer-sync:
;;     - Now only executed if called in ecb-compile-window; see comment in the
;;       function itself.
;;     - Normalizing directories before comparing it
;;   + ecb-eshell-cleanse: removed (end-of-buffer) because this always sets
;;     the mark. This should not be used in elisp-programs!
;;   + Added to all functions ecb-eshell-running-p
;;   + Removed a lot of superfluous save-excursion, save-window-excursion
;;     etc...
;;   + Removed ecb-eshell-enlarge-when-starting; eshell is always implicit
;;     started and selected by `ecb-eshell-goto-eshell' so the option
;;     `ecb-eshell-enlarge-when-selecting' should be enough.
;;   + Removed the ecb-eshell-buffer-name.
;;   + Added new option ecb-eshell-synchronize
;;   + Moved all add-hook into ecb-eshell-activate
;;   + Added new function ecb-eshell-deactivate
;;   + Made ecb-eshell-activate idempotent.
;;   
;; - Mon Feb 04 2002 10:44 PM (burton@openprivacy.org): BUG: I think at certain
;; conditions, we might be able to get the eshell window to expand after we
;; automatically change directories.  I think this has to be:
;;
;; - Mon Feb 04 2002 10:43 PM (burton@openprivacy.org): BUGFIX: make sure the
;; eshell is not buffer-read-only.  This is manifested by desktop.el for some
;; reason.
;;
;; - Tue Jan 29 2002 03:33 AM (burton@openprivacy.org): Include the ability to
;; startup the eshell when the ECB is started.
;;
;;
;; - Tue Jan 29 2002 03:31 AM (burton@openprivacy.org): RFE: make the
;; auto-enlargement on command execution optional, true by default.
;;
;; - Tue Jan 22 2002 11:38 PM (burton@openprivacy.org): enable customization
;; 
;; - Tue Dec 25 2001 07:54 PM (burton@openprivacy.org): We now enlarge the ecb
;;   window when you go to it.
;;    
;; - Sat Dec 15 2001 01:50 AM (burton@openprivacy.org): We are not being much
;;   smarter about saving selected windows.
;;
;; - Fri Dec 14 2001 05:57 PM (burton@openprivacy.org): fixed a bug which caused 
;;
;; - Fri Dec 14 2001 04:48 PM (burton@openprivacy.org): only run eshell/cd if
;; the current directory is different than the eshell/pwd (this is a performance
;; issue and needs to be added in ecb-eshell-current-buffer-sync
;;
;;   - we can't do this.  eshell/pwd does't return a string.  Instead we should
;;     change to the eshell-buffer and see what the directory default-directory
;;     is there.
;;
;; - Fri Dec 14 2001 03:24 PM (burton@openprivacy.org): use
;; eshell-pre-command-hook to increase the size of the window if we are in an
;; ECB layout (and the ecb is activated)...
;;
;; - Sun Nov 18 2001 07:20 PM (burton@openprivacy.org): putting the cursor one
;;   line from the bottom of the window.

;;; Design:
;;
;; Synching the current buffer with the eshell is done two ways.  If the buffer
;; is visible in a window, we always resynch.  If it is not visible then
;; ecb-eshell-goto-eshell will synch up when the user goes to the eshell buffer.

;;; TODO:
;;
;; - BUG: enable just-in-time current-buffer-sync... only execute if the current
;; buffer's directlry is not equal to the ecb directory.
;;

;;; Code:

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-util)
(require 'ecb-compilation)

(silentcomp-defvar eshell-buffer-name)
(silentcomp-defun eshell)
(silentcomp-defun eshell/cd)
(silentcomp-defun eshell-send-input)
(silentcomp-defun eshell-bol)

(defgroup ecb-eshell nil
  "Settings for eshell integration within the ECB."
  :group 'ecb
  :prefix "ecb-eshell-")

(defcustom ecb-eshell-enlarge-when-selecting t
  "*Enlarge the compile-window if it displays the eshell and
if it is selected by `ecb-eshell-goto-eshell'."
  :group 'ecb-eshell
  :type 'boolean)

(defcustom ecb-eshell-auto-activate nil
  "*Startup the eshell when the ECB is activated and display it in the
compile-window. If current layout does not display a compile-window \(see
`ecb-compile-window-height') then nothing is done."
  :group 'ecb-eshell
  :type 'boolean)

(defcustom ecb-eshell-synchronize t
  "*Synchronize the eshell with the default-directory of current
source-buffer. The synchronization is done by `ecb-eshell-current-buffer-sync'
which can be called interactively but normally it is called autom. by the
`ecb-current-buffer-sync-hook'."
  :group 'ecb-eshell
  :type 'boolean)

(defvar ecb-eshell-pre-command-point nil
  "Point in the buffer we are at before we executed a command.")

(defvar ecb-eshell-pre-window-enlarged nil
  "True if we enlarged the window before we executed a command.")

(defun ecb-eshell-start ()
  "Create an interactive Eshell buffer.
This function is used instead of the original `eshell' cause of a mysterious
behavior of XEmacs which always starts eshell in the edit-window even if
called from the compile-window. This functions ensures that eshell is started
in that window where this function is called from!"
  (require 'eshell)
  (ecb-with-original-functions
   (display-buffer (get-buffer-create eshell-buffer-name)))
  (eshell-mode))

(defun ecb-eshell-current-buffer-sync()
  "Synchronize the eshell with the current buffer. This is only done if the
eshell is currently visible and if either this function is called
interactively or `ecb-eshell-synchronize' is not nil."
  (interactive)

  (when (and (or ecb-eshell-synchronize (interactive-p))
             (ecb-eshell-running-p))

    ;;only do this if the user is looking at the eshell buffer; for this we
    ;;have the macro `ecb-do-if-buffer-visible-in-ecb-frame'.

    (ecb-do-if-buffer-visible-in-ecb-frame 'eshell-buffer-name
      
      ;; here we can be sure that the eshell is visible in a window of
      ;; `ecb-frame'.

      (let ((source-buffer-directory nil)
            (ecb-buffer-directory nil)
            (window (get-buffer-window eshell-buffer-name)))
      
        ;; we synchronize only if the eshell is displayed in the
        ;; compile-window otherwise the following ecb-eshell-cleanse would
        ;; prevent from inserting any command if the eshell is displayed in
        ;; the edit-window (e.g. by calling `eshell' in the edit-window)
        (when (equal window ecb-compile-window)
          (ecb-eshell-save-buffer-history
           ;;make sure we are clean.
           (ecb-eshell-cleanse)
           
           ;;get copies of the current source directory.
           
           (setq source-buffer-directory default-directory)
           
           (save-excursion
             (set-buffer (get-buffer eshell-buffer-name))
             (setq buffer-read-only nil)
             (setq ecb-buffer-directory default-directory))
           
           ;; at this point source-buffer-directory is a snapshot of the
           ;; source buffer window and default directory is the directory
           ;; in the eshell window

           (when (not (string-equal (ecb-fix-filename source-buffer-directory)
                                    (ecb-fix-filename ecb-buffer-directory)))
             (save-excursion
               (set-buffer eshell-buffer-name)
               ;;change the directory without showing the cd command
               (eshell/cd source-buffer-directory)
               
               ;;execute the command
               (save-selected-window
                 (select-window window)
                 (eshell-send-input)))
             
             (ecb-eshell-recenter))))))))

(defmacro ecb-eshell-save-buffer-history (&rest body)
  "Protect the buffer-list so that the eshell buffer name is not placed early
in the buffer list or at all if it currently doesn't exist."
  (let ((eshell-buffer-list (make-symbol "my-buffer-list")))
    `(let ((,eshell-buffer-list (ecb-frame-parameter (selected-frame)
                                                    'buffer-list)))
       (unwind-protect
           (progn
             ,@body)
         (modify-frame-parameters nil (list (cons 'buffer-list
                                                  ,eshell-buffer-list)))))))

(defun ecb-eshell-recenter(&optional display-errors)
  "Recenter the eshell window so that the prompt is at the end of the buffer."
  (interactive (list t))

  (let ((window (and (ecb-eshell-running-p)
                     (get-buffer-window eshell-buffer-name))))
    (if (and window
             (window-live-p window)
             (equal window ecb-compile-window))
        (save-selected-window
          (select-window window)
          (goto-char (point-max))
          (recenter -2))
      (when display-errors
        (ecb-error "Eshell not running or compile-window not live!")))))

(defun ecb-eshell-running-p()
  "Return true if eshell is currently running."
  (and (boundp 'eshell-buffer-name)
       (get-buffer eshell-buffer-name)))

(defun ecb-eshell-goto-eshell()
  "Go to the eshell buffer in the compile-window. If eshell is not alive then
start it."
  (interactive)
  
  ;; This is idempotent!
  (ecb-eshell-activate)

  (if ecb-eshell-enlarge-when-selecting
      (ecb-eshell-enlarge)
    ;;else just recenter
    (ecb-eshell-recenter))

  ;;sync to the current buffer
  (ecb-eshell-current-buffer-sync))

(defun ecb-eshell-activate()
  "Startup the eshell in the compile window. If no compile-window is visible
then an error is reported!"
 (ecb-eshell-save-buffer-history
   (save-excursion
     (when (ecb-compile-window-live-p 'display-msg)
       (select-window ecb-compile-window)
       (if (not (ecb-eshell-running-p))
           (ecb-eshell-start))
       (if (ecb-eshell-running-p)
           (set-window-buffer ecb-compile-window
                              (get-buffer eshell-buffer-name))
         (ecb-error "Can not start eshell. Please check eshell installation!")))))
  
 (when (and (ecb-compile-window-live-p 'display-msg)
            (ecb-eshell-running-p))
   (add-hook 'ecb-current-buffer-sync-hook 'ecb-eshell-current-buffer-sync)
   
   (add-hook 'eshell-post-command-hook 'ecb-eshell-recenter)
   (add-hook 'eshell-post-command-hook 'ecb-eshell-shrink-if-necessary)
   (add-hook 'eshell-pre-command-hook 'ecb-eshell-enlarge)
   
   (add-hook 'window-size-change-functions 'ecb-eshell-window-size-change)
   
   (add-to-list 'ecb-compilation-buffer-names-internal
                (cons eshell-buffer-name nil))))

(defun ecb-eshell-deactivate ()
  ;;TODO: Klaus Berndl <klaus.berndl@sdm.de>: Should we also try to exit the
  ;;eshell itself? IMHO we should not...
  (remove-hook 'ecb-current-buffer-sync-hook 'ecb-eshell-current-buffer-sync)
  (remove-hook 'eshell-post-command-hook 'ecb-eshell-recenter)
  (remove-hook 'eshell-post-command-hook 'ecb-eshell-shrink-if-necessary)
  (remove-hook 'eshell-pre-command-hook 'ecb-eshell-enlarge)
  (remove-hook 'window-size-change-functions 'ecb-eshell-window-size-change))  

(defun ecb-eshell-enlarge()
  "Enlarge the eshell so more information is visible.  This is usually done so
that the eshell has more screen space after we execute a command. "
  (interactive)

  ;;use the eshell-pre-command-hook to see the point and then only enlarge if
  ;;we enlarge past the maximum amount of lines we can use.
  (setq ecb-eshell-pre-command-point (point))
  
  (when (ecb-eshell-running-p)
    (let((window (get-buffer-window eshell-buffer-name)))
      
      (when (and window
                 (window-live-p window)
                 (equal (selected-window) ecb-compile-window)
                 ecb-minor-mode)

        ;;is there a better way to do this? It seems that there should be a
        ;;way to have emacs split or expand a window by 50% like it is done in
        ;;a lot of other places (display-buffer, etc)

        ;;determine if we should actually go ahead and do this.
        (when (< (window-height ecb-compile-window)
                 (/ (frame-height) 2))
          
          (setq ecb-eshell-pre-window-enlarged t)
        
          (ecb-enlarge-window window))))
    (ecb-eshell-recenter)))

(defun ecb-eshell-shrink-if-necessary()
  "If we have expanded the compile buffer after a command, but there was no need
to because the command didn't output much text, go ahead and shrink it again."

  (when (and (ecb-eshell-running-p)
             ecb-minor-mode
             (equal (selected-window) ecb-compile-window)
             (equal ecb-compile-window (get-buffer-window eshell-buffer-name)))
        
    ;; only shrink up if we expanded... we don't want to shrink if we just
    ;; happend to be runnning in large mode
    (when (and ecb-eshell-pre-command-point ecb-eshell-pre-window-enlarged
               (< (count-lines ecb-eshell-pre-command-point
                               (point))
                  ecb-compile-window-height))
      (ecb-toggle-enlarged-compilation-window -1)
      (ecb-eshell-recenter))
        
    ;;reset
    (setq ecb-eshell-pre-command-point nil)
    (setq ecb-eshell-pre-window-enlarged nil)))

(defun ecb-eshell-cleanse()
  "If the user has entered text in the eshell, we need to clean it. If we
don't do this we could end up executing a strange command resulting in a
'command not found'."
  (if (ecb-eshell-running-p)
      (save-excursion
        (set-buffer eshell-buffer-name)
        ;; go to the buffer-end without clobbering the mark!!
        (goto-char (point-max))
        (eshell-bol)
        (delete-region (point) (point-at-eol)))))

(defun ecb-eshell-auto-activate-hook()
  "Activate the eshell when ECB is activated. See `ecb-eshell-auto-activate'."
  (when ecb-eshell-auto-activate
    (ignore-errors (ecb-eshell-activate))))

(defun ecb-eshell-window-size-change(frame)
  "Called when we change window sizes so that the eshell can resize."
  (when (and ecb-minor-mode
             (equal frame ecb-frame))
    (ignore-errors (ecb-eshell-recenter))))

(add-hook 'ecb-activate-hook 'ecb-eshell-auto-activate-hook)

(silentcomp-provide 'ecb-eshell)

;;; ecb-eshell.el ends here
