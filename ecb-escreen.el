;; escreen integration - a first test implementation

;; Copyright (C) 2000 - 2003 Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools
;; Created: 2003

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

;; $Id: ecb-escreen.el,v 1.1 2003/09/08 17:31:48 berndl Exp $


;;; Installation
;;
;; 1. Load this library into Emacs (e.g. with (require 'ecb-escreen))
;; 2. Run `ecb-escreen-enable-integration'. This loads the escreen-library itself
;;    and enables the ECB-integration.
;;    *IMPORTANT*: This must be done before the first call to `escreen-install'!!!


;;; Deinstallation
;;
;; Just run `ecb-escreen-disable-integration'


;;; Usage
;;
;; After enabling the escreen-integration for ECB just run `escreen-install'
;; (deactivates ECB if currently running), `escreen-create-screen',
;; `escreen-goto-screen' etc. The later ones activate autom. ECB if creating
;; or selecting the escreen with number `ecb-escreen-number' (default = 1) and
;; deactivate ECB autom. if leaving the ECB-escreen.

;;; BUGS
;;
;; There are some dedicated windows in the non-ECB-escreen which should
;; definitely not be there. I'm not sure if this a bug in ECB itself, in
;; ecb-escreen.el or in escreen.el itself?!

;;; TODO
;;
;; Fixing the bugs
;;
;; A lot of tests and maybe some more robustness of the code. This first
;; implementation is a proof of concept which seems to work.


;;; Code


(require 'ecb-util)

(defgroup ecb-escreen nil
  "Settings for escreen integration within the ECB."
  :group 'ecb
  :prefix "ecb-escreen-")

(defcustom ecb-escreen-number 1
  "*Number of the escreen which is reserved for ECB.
If you go to the escreen with this number you go always to the escreen with
activated ECB. All other escreen-numbers are escreens with deactivated ECB!"
  :group 'ecb-escreen
  :type 'integer)

(defconst ecb-escreen-adviced-functions '((escreen-save-current-screen-configuration . before))
  "These functions of escreen are adviced if escreen is active during ECB is
active. Each element of the list is a cons-cell where the car is the
function-symbol and the cdr the advice-class \(before, around or after). If a
function should be adviced with more than one class \(e.g. with a before and
an after-advice) then for every class a cons must be added to this list.")


(defun ecb-escreen-enable-integration ()
  "Load the escreen-library and enable the ECB-integration for it.
This does not install or activate escreen! For this you have still to call
`escreen-install'!"
  (interactive)
  (when (locate-library "escreen")
    (condition-case nil
        (progn
          (require 'escreen)
          (ecb-enable-advices ecb-escreen-adviced-functions)
          (add-hook 'escreen-goto-screen-hook
                    'ecb-escreen-goto-escreen-hook))
      (error
       (ecb-disable-advices ecb-escreen-adviced-functions)
       (remove-hook 'escreen-goto-screen-hook
                    'ecb-escreen-goto-escreen-hook)))))

(defun ecb-escreen-disable-integration ()
  "Disable the escreen-integration for ECB."
  (interactive)
  (when (featurep 'escreen)
    (ecb-disable-advices ecb-escreen-adviced-functions)
    (remove-hook 'escreen-goto-screen-hook
                 'ecb-escreen-goto-escreen-hook)))
    

(defun ecb-escreen-goto-escreen-hook ()
  "Activate ECB if we go to the escreen with number `ecb-escreen-number'."
  (if (and (boundp 'ecb-minor-mode)
           (not ecb-minor-mode)
           (= escreen-current-screen-number
              ecb-escreen-number))
      (let ((ecb-split-edit-window t))
        (ecb-activate))))

(defadvice escreen-save-current-screen-configuration (before ecb)
  "escreen can only save screen-configurations if ECB is deactivated. So we
deactivate ECB before running this function."
  (if (and (boundp 'ecb-minor-mode)
           ecb-minor-mode)
      (let ((ecb-split-edit-window t))
        (ecb-deactivate))))

(provide 'ecb-escreen)