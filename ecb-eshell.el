;;; ecb-eshell.el --- eshell integration for the ECB.

;; $Id: ecb-eshell.el,v 1.35 2002/02/22 08:30:43 berndl Exp $

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
;; - Right now ecb-eshell doesn't work with dired.  Why?  Try to setup a hook
;; and an ecb-eshell-dired-buffer-sync function that will take care of this.
;;
;; - BUG: when we exit the eshell, we enlarge the compilation buffer.... there
;; is not needed since the buffer is about to exit.
;;
;;    - this might be fixed by using an eshell post command hook.
;;
;; - RFE: could we possibly find a way to avoid window enlargement if we launch
;; a background process like 'xterm &'.  This would require determining what the
;; command is and then making sure it is acceptable.
;;
;; - BUG: when ecb-eshell-enlarge-when-selecting is nil we need to recenter.  If
;; we don't we just end up with the point in the middle of the eshell.
;;
;; - BUG: enable just-in-time current-buffer-sync... only execute if the current
;; buffer's directlry is not equal to the ecb directory.

;;; Code:

(defgroup ecb-eshell nil
  "Settings for eshell integration within the ECB."
  :group 'ecb
  :prefix "ecb-eshell-")

(defcustom ecb-eshell-enlarge-when-selecting t
  "*Enlarge the `ecb-eshell-buffer-name' buffer if it has been selected."
  :group 'ecb-eshell
  :type 'boolean)

(defcustom ecb-eshell-enlarge-when-starting t
  "*When starting the eshell, enlarge the buffer if non-nil."
  :group 'ecb-eshell
  :type 'boolean)

(defcustom ecb-eshell-auto-activate nil
  "*Startup the eshell when the ECB is activated."
  :group 'ecb-eshell
  :type 'boolean)

(defvar ecb-eshell-buffer-name "*eshell*"
  "Buffer name for the eshell.  We define it here so that we don't need to have
the eshell loaded for ecb-eshell to function properly.")

(defun ecb-eshell-current-buffer-sync()
  "Synchronize the eshell with the current buffer.  This is only done if the
eshell is currently visible."
  (interactive)

  ;;only do this if the user is looking at the eshell buffer

  (if (ecb-eshell-running-p)      
      (let((source-buffer-directory nil)
           (ecb-buffer-directory nil)
           (window nil))

        ;;get copies of the current source directory.
        
        (setq source-buffer-directory default-directory)

        (setq window (get-buffer-window ecb-eshell-buffer-name))

        (save-excursion
          (set-buffer (get-buffer-create ecb-eshell-buffer-name))

          (setq buffer-read-only nil)
          
          (setq ecb-buffer-directory default-directory))

        ;;at this point source-buffer-directory is a snapshot of the source
        ;;buffer window and default directory is the directory in the eshell
        ;;window
        
        (when (and window
                   (window-live-p window)
                   (not (string-equal source-buffer-directory ecb-buffer-directory)))
          (save-excursion

            (set-buffer ecb-eshell-buffer-name)
            
            ;;change the directory without showing the cd command
            (eshell/cd source-buffer-directory)
            
            ;;execute the command
            (save-selected-window
              (select-window window)
              
              (eshell-send-input)))
          
          (ecb-eshell-recenter)))))

(defun ecb-eshell-recenter()
  "Recenter the eshell window so that the prompt is at the end of the buffer."
  (interactive)

  (save-selected-window
  
    (let(window)
      
      (setq window (get-buffer-window ecb-eshell-buffer-name))
      
      (when (and (ecb-eshell-running-p)
                 window
                 (window-live-p window)
                 (equal window ecb-compile-window))
        
        (select-window window)

        ;;this needs to be present under GNU Emacs or recenter will fail.
        (end-of-buffer) 
        
        (recenter -2)))))

(defun ecb-eshell-running-p()
  "Return true if eshell is currently running."

  (and (boundp 'ecb-eshell-buffer-name)
       ecb-eshell-buffer-name
       (get-buffer ecb-eshell-buffer-name)))
  
(defun ecb-eshell-goto-eshell()
  "Go ot the eshell buffer"
  (interactive)
  
  ;;first... make sure that we change the compilation window to the eshell
  ;;buffer.

  (if (ecb-eshell-running-p)
      (when (ecb-compile-window-live-p 'display-msg)
        (set-window-buffer ecb-compile-window ecb-eshell-buffer-name)
        (select-window ecb-compile-window)
        (if ecb-eshell-enlarge-when-selecting
            (ecb-eshell-enlarge)
          ;;else just recenter
          (ecb-eshell-recenter)))

    ;;we auto start the eshell here?  I think so..
    (ecb-eshell-activate)
    (when ecb-eshell-enlarge-when-starting
      (ecb-eshell-enlarge))))

(defun ecb-eshell-activate()
  "Startup the eshell in the compile window."

  (when (ecb-compile-window-live-p 'display-msg)
    (select-window ecb-compile-window)
    (eshell)))
  
(defun ecb-eshell-enlarge()
  "Enlarge the eshell so more information is visible.  This is usually done so
that the eshell has more screen space after we execute a command. "
  (interactive)

  (let(window)
  
    (setq window (get-buffer-window ecb-eshell-buffer-name))
    
    (when (and (ecb-eshell-running-p)
               (window-live-p window)
               (equal (selected-window) ecb-compile-window)
               ecb-minor-mode)

      ;;is there a better way to do this?  It seems that there should be a way
      ;;to have emacs split or expand a window by 50% like it is done in a lot
      ;;of other places (display-buffer, etc)

      (ecb-enlarge-window window)))
  (ecb-eshell-recenter))

(defun ecb-eshell-auto-activate-hook()
  "Activate the eshell when ECB is activated.  See `ecb-eshell-auto-activate'."

  (when ecb-eshell-auto-activate
    (ecb-eshell-activate)))

(add-hook 'ecb-activate-hook 'ecb-eshell-auto-activate-hook)
  
(add-hook 'ecb-current-buffer-sync-hook 'ecb-eshell-current-buffer-sync)

(add-hook 'ecb-redraw-layout-hook 'ecb-eshell-recenter)

(add-hook 'eshell-pre-command-hook 'ecb-eshell-enlarge)

(provide 'ecb-eshell)

;;; ecb-eshell.el ends here
