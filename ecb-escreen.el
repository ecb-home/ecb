;; escreen integration - integration of escreen with ECB

;; Copyright (C) 2000 - 2003 Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, tools, escreen
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

;; $Id: ecb-escreen.el,v 1.2 2003/09/09 09:45:27 berndl Exp $


;;; Installation
;;
;; This library is installed autom. with ECB. But you must enable it
;; explicitly with `ecb-escreen-enable-integration'. This *must* be done
;; *before* the first call to any escreen-command, so also before calling
;; `escreen-install'!


;;; Deinstallation
;;
;; Just run `ecb-escreen-disable-integration'


;;; Usage
;;
;; After enabling the escreen-integration for ECB just go on as described in
;; escreen.el, i.e. run `escreen-install' (deactivates ECB if currently
;; running), `escreen-create-screen', `escreen-goto-screen' etc. The latter
;; ones activate autom. ECB if creating or selecting the escreen with number
;; `ecb-escreen-number' (default = 1) and deactivate ECB autom. if leaving the
;; ECB-escreen.


;;; BUGS
;;
;; Currently not known


;;; TODO
;;
;; A lot of tests and maybe some more robustness of the code. This first
;; implementation is a proof of concept which seems to work.

;; Thanks to Johann <myrkraverk@users.sourceforge.net> for the first trigger
;; for this integration of escreen.

;;; Code

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-util)

(silentcomp-defvar escreen-current-screen-number)


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

(silentcomp-provide 'ecb-escreen)

;;; ecb-escreen.el ends here
