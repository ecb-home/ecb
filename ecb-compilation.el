;;; ecb-compilation.el --- 

;; $Id: ecb-compilation.el,v 1.8 2002/11/06 11:25:38 berndl Exp $

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

;;; Code:

(eval-when-compile
  (require 'ecb-bytecomp))

(defgroup ecb-compilation nil
  "Settings for all things displayed in the compile window of ECB."
  :group 'ecb
  :prefix "ecb-compilation-")

(defcustom ecb-compilation-buffer-names (list ecb-eshell-buffer-name
                                              "*Apropos*"
                                              "*Help*"
                                              "*Backtrace*"
                                              "*shell*"
                                              "*bsh*"
                                              "*Messages*")
  "*Additional buffer names that should be displayed in compilation
window even if `compilation-buffer-p' says nil."
  :group 'ecb-compilation
  :type '(repeat (string :tag "Buffer name")))

(defcustom ecb-compilation-major-modes (list 'eshell-mode 'compilation-mode)
  "*Additional major-mode that should be displayed in compilation window even if
`compilation-buffer-p' says nil."
  :group 'ecb-compilation
  :type '(repeat (symbol :tag "major-mode name")))

(defun ecb-compilation-get-buffers()
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
  "Test if the given buffer is a compilation buffer. Note that in this case we
define 'compilation buffer' as a buffer that should ideally be displayed in the
`ecb-compile-window'. This means that in some situations this might not be the
result of a `compile-internal'. A good example would be the *Help* buffer or the
`ecb-eshell-buffer-name'.

BUFFER can be the name of a buffer or a buffer-objekt.

This function non-nil if the name of BUFFER is either contained in
`ecb-compilation-buffer-names', or its `major-mode' is in
`ecb-compilation-major-modes', or if `compilation-buffer-p' returns true."

  (let ((buf (cond ((stringp buffer)
                    (get-buffer buffer))
                   ((bufferp buffer)
                    buffer)
                   (t
                    nil))))
    (if buf
        (or (member (buffer-name buf) ecb-compilation-buffer-names)
            (save-excursion
              (set-buffer buffer)
              (member major-mode ecb-compilation-major-modes))
            (compilation-buffer-p buf)))))

(ecb-provide 'ecb-compilation)

;;; ecb-compilation.el ends here