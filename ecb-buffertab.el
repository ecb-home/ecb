;;; ecb-buffertab.el --- 

;; $Id: ecb-buffertab.el,v 1.6 2002/12/06 20:41:25 berndl Exp $

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

;;; TODO: make sure we don't do this TOO many times.
;;
;; - we need to define a property with 'local-map set correctly.
;;
;; - write a function that generates a popup menu
;;
;; - the popup menu should allow the user to check a buffer view to set it as
;;   the default when opening new files.
;;
;;   - is this possible?  I think it might but I would need to set it up
;;   correctly.

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-compilation)

(defface ecb-buffertab-face '((t (:bold t :foreground "lightyellow")))
  "Face used to highlight the annotation lines to the left of the annotate buffer.")

(defcustom ecb-buffertab-map (make-sparse-keymap)
  "Key map used for buffertab navigation")

(define-key ecb-buffertab-map [mode-line down-mouse-1] 'ecb-buffertab-popup-menu)

(defun ecb-buffertab-popup-menu()
  ""
  (interactive)
  
  (popup-menu (ecb-buffertab-make-menu "Compilation Buffers")))

(defun ecb-buffertab-make-menu(name)
  "Make a menu for use on the buffertab."

  (let((menu (list 'keymap name)))

    (dolist(entry (ecb-compilation-get-buffers))

      (add-to-list 'menu (list 'menu-item 'menu-item (car entry) (car entry)) t))

    menu))

(defun ecb-buffertab-setup-modeline()
  ""
  (interactive)

  (let((modeline-tab ""))

    ;;FIXME: figure out what modeline tab to use
    (setq modeline-tab "   ECB: ")

    (set-text-properties 0 (length modeline-tab) (list 'local-map ecb-buffertab-map
                                                       'face 'ecb-buffertab-face) modeline-tab)

    (setq mode-line-format modeline-tab)))

(silentcomp-provide 'ecb-buffertab)

;;; ecb-buffertab.el ends here