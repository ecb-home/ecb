;;; ecb-test.el --- 

;; $Id: ecb-test.el,v 1.1 2001/11/20 23:59:02 burtonator Exp $

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

(defun ecb-redraw-layout()
  ""
  (interactive)

  ;;save copies of:
  ;;
  ;; - the current buffer in the main window
  ;; - the current buffer in the compilation window

  (let((main-window-buffer nil)
       (compilation-window-buffer nil))

    (setq main-window-buffer (window-buffer ecb-edit-window))

    (setq compilation-window-buffer (window-buffer ecb-compile-window))
    
    (set-window-configuration ecb-activated-window-configuration)

    ;;ok... now restore the buffers in the compile and edit windows..

    (set-window-buffer ecb-edit-window main-window-buffer)

    (set-window-buffer ecb-compile-window compilation-window-buffer)))

(provide 'ecb-test)

;;; ecb-test.el ends here