;;; ecb-multiframe.el --- 

;; $Id: ecb-multiframe.el,v 1.2 2002/10/29 10:19:25 burtonator Exp $

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

;; This is am implementation of multiple frame support for the ECB.  The
;; original design was fairly easy to implement and since it will be a long time
;; until ECB 2.0 it seems obvious that this should be done as a temporary
;; solution.
;;
;; In order to use this just create a new frame and run 'ecb-activate.  You can
;; create as many frames and run ECB in any of them.
;;

;;; Notes:
;;
;; Because ECB is now global it is never really deactivated.  You can deactivate
;; ECB in a frame if you want but the advice will still be around.
;;
;; You have a separate ECB methods, directory, and source buffer for each ECB
;; frame.
;;

;;; Install:
;;
;; Place a (require 'ecb-multiframe) at the end of your normal ECB
;; initialization

;;; TODO:
;;
;; - Should we have a separeate speedbar buffer?  What about eshell?
;;
;; - I should allocate my own ecb-compile-window for each frame.
;;
;; - ECB deactivation isn't currently supported.
;;
;; - Make sure I don't have any hooks that might run on deleted buffers.
;;
;; - Make sure we clean up when a frame is deleted.
;;
;; - Is it possible to migrate some of this code into default-frame-alist
;; instead of using a hook?


;; NOTE: If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; Code:

;;make certain variables frame local

(defvar ecb-multiframe-variables (list 'ecb-last-edit-window-with-point
                                       'ecb-edit-window
                                       'ecb-compile-window
                                       'ecb-frame
                                       'ecb-windows-hidden
                                       'ecb-toggle-layout-state
                                       'ecb-minor-mode
                                       'ecb-activated-window-configuration)
  "List of ecb variables that are required to be nil in new frames and frame local.")

(defun ecb-multiframe-make-frame-hook(frame)
  "Create a hook so that we can enable the default variables within new frames."
  (interactive
   (list
    (selected-frame)))

  ;;make variables frame local in this frame.

  ;;reset everything to the default value?
  
  (dolist(variable ecb-multiframe-variables)
    (make-variable-frame-local variable)

    (modify-frame-parameters frame (list (cons variable nil))))

  (let((index (number-to-string (length (frame-list)))))

    ;;ecb-eshell-buffer-name ?
    ;;ecb-speedbar-buffer-name ?

    ;;set ECB special buffer names

    (make-variable-frame-local 'ecb-history-buffer-name-buffer-name)
    (modify-frame-parameters frame (list (cons 'ecb-history-buffer-name
                                               (concat " *ECB History <" index ">*"))))

    (make-variable-frame-local 'ecb-sources-buffer-name)
    (modify-frame-parameters frame (list (cons 'ecb-sources-buffer-name
                                               (concat " *ECB Sources <" index ">*"))))

    (make-variable-frame-local 'ecb-directories-buffer-name)
    (modify-frame-parameters frame (list (cons 'ecb-directories-buffer-name
                                               (concat " *ECB Directories <" index ">*"))))

    (make-variable-frame-local 'ecb-methods-buffer-name)
    (modify-frame-parameters frame (list (cons 'ecb-methods-buffer-name
                                               (concat " *ECB Methods <" index ">*")))))

  ;;fix speedbar by binding the given speedbar frame value with the current frame
  (mapcar (lambda(sframe)
            (when (boundp sframe)
              (make-variable-frame-local sframe)
              (modify-frame-parameters frame (list (cons sframe frame)))))
          '(speedbar-frame speedbar-attached-frame dframe-attached-frame)))

(defun ecb-deactivate-internal ()
  "Deactivates the ECB and kills all ECB buffers and windows."
  (unless (not ecb-minor-mode)
    
    (setq ecb-minor-mode nil))
  (message "The ECB is now deactivated.")
  ecb-minor-mode)

;;we need to modify frame parameters for new frames
(add-hook 'after-make-frame-functions 'ecb-multiframe-make-frame-hook)

(provide 'ecb-multiframe)

;;; ecb-multiframe.el ends here