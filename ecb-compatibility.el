;;; ecb-compatibility.el --- ECB-compatibility for other packages

;; Copyright (C) 2000 - 2003 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
;; Keywords: browser, code, programming, tools
;; Created: 2004

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-compatibility.el,v 1.2 2004/02/13 16:10:06 berndl Exp $

;;; Commentary:
;;
;; Contains compatibility-code for other-packages.
;;
;; Whenever commands of other packages are not fully compatible with ECB then
;; this library should contain the necessary code to make it fully compatible
;; - or at least working acceptable.
;;
;; This file is part of the ECB package which can be found at:
;; http://ecb.sourceforge.net

(eval-when-compile
  (require 'silentcomp))


(require 'ecb-util)
(require 'ecb-layout)

;; To add compatibilty code for packages just do:
;;
;; 1. Add the needed advice(s) to `ecb-compatibility-advices'
;; 2. Add the advice-code below.
;;
;; All advices of `ecb-compatibility-advices' will be autom. enabled when ECB
;; starts and autom. disabled when ECB shuts down.

(defvar ecb-compatibility-advices '((bs-show . before)
                                    (Electric-pop-up-window . around)
                                    (electric-buffer-list . after))
  "Contains all advices needed for package-compatibility.")

(defadvice bs-show (before ecb)
  "Ensures `bs-show' works well when called from another window as an
edit-window. Does nothing if called in another frame as the `ecb-frame'."
  (when (equal (selected-frame) ecb-frame)
    (unless (ecb-point-in-edit-window)
      (ecb-select-edit-window))
    ;; now we handle if bs-show should always display in the compile-window
    (let ((my-bs-buffer (get-buffer-create "*buffer-selection*")))
      ;; ecb-compilation-buffer-p needs a living buffer!
      (if (and (ecb-compilation-buffer-p my-bs-buffer)
               ecb-compile-window-height)
          (ecb-with-adviced-functions
           (display-buffer (buffer-name my-bs-buffer)))))))

(defadvice Electric-pop-up-window (around ecb)
  (ecb-with-ecb-advice 'one-window-p 'around
    ad-do-it))

(defadvice electric-buffer-list (after ecb)
  (if (get-buffer "*Buffer List*")
      (bury-buffer (get-buffer "*Buffer List*"))))

;; we disable the advices at load-time
(ecb-disable-advices ecb-compatibility-advices)

(silentcomp-provide 'ecb-compatibility)

;;; ecb-compatibility.el ends here
