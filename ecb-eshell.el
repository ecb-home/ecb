;;; ecb-eshell.el --- eshell integration for the ECB.

;; $Id: ecb-eshell.el,v 1.4 2001/11/20 23:59:02 burtonator Exp $

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

;; Provides eshell integration for the ECB.  Basically allows you to jump to the
;; eshell in the compilation window, synch up the current eshell with the
;; current ECB buffer, etc.

;; If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; History:

;; - Sun Nov 18 2001 07:20 PM (burton@openprivacy.org): putting the cursor one
;;   line from the bottom of the window.

;;; TODO:

;; - should I use eshell-pre-command-hook to increase the size of the window if
;; we are in an ECB layout?? (and the ecb is activated)...

;; - only run eshell/cd if the current directory is different than the
;; eshell/pwd.
;;
;;

;;; Code:

(defun ecb-eshell-current-buffer-sync()
  "Synchronize the eshell with the current buffer."
  (interactive)

  ;;only do this if the user is looking at the eshell buffer

  (if (ecb-eshell-running-p)      
      (let((new-directory default-directory))
    
        (set-buffer (get-buffer-create eshell-buffer-name))
        
        (end-of-buffer)
        
        ;;change the directory without showing the cd command
        (eshell/cd new-directory)
        
        ;;execute the command
        (eshell-send-input)

        (ecb-eshell-recenter)

        (set-window-point (get-buffer-window eshell-buffer-name) (point-max)))))

(defun ecb-eshell-recenter()
  "Recenter the eshell window so that the prompt is at the end of the buffer."
  (interactive)
  
  (let((window-start nil)
       (eshell-window nil))

    (setq eshell-window (get-buffer-window eshell-buffer-name))
    
    (save-excursion
      
      (set-buffer eshell-buffer-name)
      
      (end-of-buffer)
      
      (forward-line (* -1 (- (window-height eshell-window) 3)))
      
      (beginning-of-line)
      
      (setq window-start (point)))
    
    (set-window-start eshell-window window-start)))

(defun ecb-eshell-running-p()
  "Return true if eshell is currently running."

  (and (boundp 'eshell-buffer-name)
       eshell-buffer-name
       (get-buffer eshell-buffer-name)))
  
(defun ecb-eshell-goto-eshell()
  (interactive)
  
  ;;TODO: first... make sure that we change the compilation window to the eshell
  ;;buffer.

  (if (ecb-eshell-running-p)
      (progn 
        (select-window ecb-compile-window)

        (switch-to-buffer eshell-buffer-name))

      ;;FIXME: should we auto start the eshell here?  I think so..
    (error "The eshell is not running")))

(add-hook 'ecb-current-buffer-sync-hook 'ecb-eshell-current-buffer-sync)
          
(provide 'ecb-eshell)

;;; ecb-eshell.el ends here