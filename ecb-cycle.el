;;; ecb-cycle.el --- cycle buffers through ecb windows.

;; $Id: ecb-cycle.el,v 1.5 2002/01/26 09:37:07 burtonator Exp $

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

;; NOTE: If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; History:

;; Fri Jan 25 2002 05:07 PM (burton@openprivacy.org): init

;;; TODO:
;;
;; - Add completion support so that I can do an ecb-cycle-set-compilation-buffer
;; which will use completion to prompt for the buffer.  This will require us to
;; get the index and compilation buffer list interactively.
;;
;; - What is the pattern we should use for cycling through other windows?
;;
;;   - ecb-cycle-X-buffers (select the next X buffer)
;;   - ecb-cycle-set-X-buffer (set the X buffer using completion)
;;
;; - How do we setup the menubar?
;;
;;          - ECB
;;                Cycle
;;                     - Forward Compilation Buffer
;;                     - Set Compilation Buffer
;;
;; - What do we use for key bindings?
;;
;; - We need an easier way to setup completion and a better way to get the
;;   index.
;;
;; - if possible, try to put fit the buffer so that the end of buffer is at the
;;   end of the window... if necessary.
;;
;; - Maybe this should be ecb-cycle-switch-to-compilation-buffer.

;;; Code:

(defgroup ecb-cycle nil
  "Setting for cycling through misc ECB buffers."
  :group 'ecb
  :prefix "ecb-cycle-")

(defcustom ecb-cycle-enlarge-compile-window t
  "Enlarge the compilation buffer when we switch to it."
  :group 'ecb-cycle
  :type 'boolean)

(defun ecb-cycle-compilation-buffers()
  "Cycle through all compilation buffers currently open and display them within
the compilation window `ecb-compile-window'.  If the currently opened buffer
within the compilation window is not a compilation buffer, we jump to the first
compilation buffer.  If not we try to loop through all compilation buffers.  If
we hit the end we go back to the beginning.  See `ecb-compilation-buffer-p'."
  (interactive)

  (let*((compilation-buffers (ecb-get-compilation-buffers))
        (current-buffer (window-buffer ecb-compile-window))
        (current-buffer-name (buffer-name current-buffer)))

    (when (null compilation-buffers)
      (error "No compilation buffers"))

    (when ecb-cycle-enlarge-compile-window
      (ecb-enlarge-window ecb-compile-window))

    (if (not (ecb-compilation-buffer-p current-buffer))
        ;;if the current bufffer is not a compilation buffer, goto the first
        ;;compilation buffer.
        
        (ecb-cycle-set-compilation-buffer 0 compilation-buffers)

      ;;else... we need to determine what buffer to display.

      (let(current index)

        (setq current (assoc current-buffer-name compilation-buffers))

        (setq index (cdr current))

        (if (= (1+ index) (length compilation-buffers))
            ;;go back to the first buffer.
            (ecb-cycle-set-compilation-buffer 0 compilation-buffers)
          (ecb-cycle-set-compilation-buffer (1+ index) compilation-buffers))))))

(defun ecb-cycle-set-compilation-buffer(index compilation-buffers)
  "Set the buffer in the compilation window."

  (let((buffer-name (car (nth index compilation-buffers))))

    (message "ECB: setting compilation buffer %d/%d - %s" (1+ index) (length compilation-buffers) buffer-name)

    (set-window-buffer ecb-compile-window buffer-name)))

(defun ecb-cycle-switch-to-compilation-buffer(buffer)
  "Switch to the given compilation buffer in the compilation window."
  (interactive
   (list
    (completing-read "Compilation buffer: " (ecb-get-compilation-buffers))))

  (when ecb-cycle-enlarge-compile-window
    (ecb-enlarge-window ecb-compile-window))

  (set-window-buffer ecb-compile-window buffer))

(defun ecb-get-compilation-buffers()
  "Get all known compilation buffer names.  See `ecb-compilation-buffer-p'."

  (let((buffer-names '())
       (buffer-list (buffer-list))
       (index 0))

    (setq buffer-list (sort buffer-list (lambda(first second)
                                          (string-lessp (buffer-name first)
                                                        (buffer-name second)))))

    (dolist(buffer buffer-list)

      (when (ecb-compilation-buffer-p buffer)

        (add-to-list 'buffer-names (cons (buffer-name buffer) index) t)
        
        (setq index (1+ index))))

    buffer-names))
  
(defun ecb-compilation-buffer-p(buffer)
  "Test if the given buffer is a compilation buffer.  Note that in this case we
define 'compilation buffer' as a buffer that should ideally be displayed in the
`ecb-compile-window'.  This means that in some situations this might not be the
result of a `compile-internal'.  A good example would be the *Help* buffer or
the `ecb-eshell-buffer-name'.  See `compilation-buffer-p'."
  
  (let((buffer-name (buffer-name buffer)))

    (or (string-equal buffer-name ecb-eshell-buffer-name)
        (string-equal buffer-name "*Apropos*")
        (string-equal buffer-name "*Help*")
        (string-equal buffer-name "*Backtrace*")
        (compilation-buffer-p buffer))))

(provide 'ecb-cycle)

;;; ecb-cycle.el ends here