;;; ecb-cycle.el --- cycle buffers through ecb windows.

;; $Id: ecb-cycle.el,v 1.17 2002/11/05 15:14:07 berndl Exp $

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
;; - What is the pattern we should use for cycling through other windows?
;;
;;   - ecb-cycle-through-X-buffers (select the next X buffer)
;;   - ecb-cycle-switch-to-X-buffer (set the X buffer using completion)
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
;; - If possible, try to put fit the buffer so that the end of buffer is at the
;; end of the window... if necessary.

;;; Code:

(eval-when-compile
  (or load-in-progress
      (let ((load-path
             (if (and (boundp 'byte-compile-dest-file)
                      (stringp byte-compile-dest-file))
                 (cons (file-name-directory byte-compile-dest-file)
                       load-path)
               load-path)))
        (load "ecb-bytecomp" nil t))))

(require 'ecb-compilation)
(require 'ecb-layout)

(defgroup ecb-cycle nil
  "Setting for cycling through misc ECB buffers."
  :group 'ecb
  :prefix "ecb-cycle-")


(defun ecb-cycle-through-compilation-buffers(&optional choose-buffer)
  "Cycle through all compilation buffers currently open and display them
within the compilation window `ecb-compile-window'. If the currently opened
buffer within the compilation window is not a compilation buffer, we jump to
the first compilation buffer. If not we try to loop through all compilation
buffers. If we hit the end we go back to the beginning.

If CHOOSE-BUFFER is not nil then the user will be prompted for the
compilation-buffer to swtich to.

See also the option `ecb-layout-switch-to-compilation-window'! The difference
is that this cycling-function offers only compilation-buffers in the sense of
`ecb-compilation-buffer-p' whereas the adviced version of `switch-to-buffer'
offers any buffer but switches to `ecb-compile-window' if a compilation-buffer!"
  
  (interactive "P")

  (if choose-buffer
      (when (ecb-compile-window-live-p 'display-msg)
        (let ((ecb-layout-switch-to-compilation-window '(switch-to-buffer)))
          (switch-to-buffer (completing-read "ECB compilation buffer: "
                                             (ecb-compilation-get-buffers)))))
    (when (ecb-compile-window-live-p 'display-msg)
      (let*((compilation-buffers (ecb-compilation-get-buffers))
            (current-buffer (window-buffer ecb-compile-window))
            (current-buffer-name (buffer-name current-buffer)))
      
        (when (null compilation-buffers)
          (ecb-error "No compilation buffers"))
      
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
              (ecb-cycle-set-compilation-buffer (1+ index)
                                                compilation-buffers))))))))

(defun ecb-cycle-set-compilation-buffer(index compilation-buffers)
  "Set the buffer in the compilation window."

  (when (ecb-compile-window-live-p 'display-msg)
    (let((buffer-name (car (nth index compilation-buffers)))
         (ecb-layout-switch-to-compilation-window '(switch-to-buffer)))
      (message "ECB: setting compilation buffer %d/%d - %s"
               (1+ index) (length compilation-buffers) buffer-name)
      (switch-to-buffer buffer-name))))

(if (featurep 'ecb-bytecomp)
    (ecb-provide 'ecb-cycle)
  (provide 'ecb-cycle))

;;; ecb-cycle.el ends here
