;;; ecb-upgrade.el: Contains all code to upgrade an old ecb-version

;; Copyright (C) 2002 Klaus Berndl

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: java, class, browser
;; Created: Mar 2002

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

;;; Commentary:
;;
;; This file upgrades an old ECB-version best possible to the latest one.

;;; Code

(defconst ecb-upgradable-option-alist nil
  "Alist of all options which are renamed in current-ECB version. The car is
the old option symbol and the cdr is a 2-element-list with:
1. elem: The new option symbol
2. elem: A function which converts the value of the old option to the new
   option. If the type of the option is identical \(i.e. only the option name
   has been changed) then this function should be `identity' otherwise a
   function which gets one argument \(the value of the old option) and returns
   the corresponding value for the new option.")

;; Kriterium:

;;- (get <ecb-old-option-symbol> 'saved-value) ist not nil wobei
;;   <ecb-old-option-symbol> aus `ecb-changed-option-alist' sein muss &&
;;- (boundp <ecb-old-option-symbol>) ist nil &&
;;- (get <ecb-new-option-symbol> 'saved-value) ist nil, wobei
;;  <ecb-new-option-symbol> zu <ecb-old-option-symbol> gehört!
;; genau dann ist ein Value-Upgrade notwendig!

(defun ecb-upgrade-option (old-option)
  (let ((upgrade-elem (cdr (assoc old-option ecb-upgradable-option-alist)))
        (old-value (get old-option 'saved-value)))
    (if (and upgrade-elem
             (or (equal old-option (nth 0 upgrade-elem))
                 (and (not (boundp old-option))
                      (null (get (nth 0 upgrade-elem) 'saved-value))))
             old-value)
        (customize-save-variable (nth 0 upgrade-elem)
                                 (funcall (nth 1 upgrade-elem) old-value)))))
  

(defvar ecb-not-compatible-options nil)

(defun ecb-check-not-compatible-options ()
  "Check for all ECB-options if their current value is compatible to the
defined type. If not store it in `ecb-not-compatible-options'."
  (setq ecb-not-compatible-options nil)

  ;; get all options of ECB
  (let ((ecb-options nil))
    (mapatoms
     (lambda (symbol)
       (when (and (string-match "ecb-" (symbol-name symbol))
                  (get symbol 'custom-type))
         (setq ecb-options (cons symbol ecb-options)))))

    ;; check if all current values of ECB options match their types. Add not
    ;; matching options to `ecb-not-compatible-options'.
    (dolist (option ecb-options)
      (require 'cus-edit)
      (unless (widget-apply (widget-convert (get option 'custom-type))
                            :match (symbol-value option))
        (setq ecb-not-compatible-options
              (cons (cons option
                          (symbol-value option))
                    ecb-not-compatible-options))))))

(defun ecb-reset-not-compatible-options ()
  "Reset all not anymore compatible options of `ecb-not-compatible-options' to
their current default-values."
  ;; For every not compatible option perform an upgrade
  (dolist (option ecb-not-compatible-options)
    (let ((special-upgrade-elem (cdr (assoc (car option)
                                            ecb-upgradable-option-alist))))
      (if special-upgrade-elem
          (ecb-upgrade-option option)
        (let ((default-val (car (get (car option) 'standard-value))))
          (setq default-val (cond ((not (listp default-val)) default-val)
                                  ((equal 'quote (car default-val))
                                   (car (cdr default-val)))
                                  (t (car default-val))))
          (customize-save-variable (car option) default-val))))))


(defun ecb-display-reset-options ()
  "Display a message-buffer which options have been reset."
  (interactive)
  (if ecb-not-compatible-options
      (with-output-to-temp-buffer "*ECB reset options*"
        (princ (format "ECB %s has reset the following options to the new default values\nof current ECB because the old customized values are not compatible.\nPlease re-customize if the new default value is not what you need!"
                       ecb-version))
        (princ "\n\n")
        (dolist (option ecb-not-compatible-options)
          (let ((option-name (symbol-name (car option)))
                (old-value (cdr option))
                (new-value (symbol-value (car option))))
            (princ (concat "+ Option :   " option-name))
            (princ "\n")
            (princ (concat "  Old value: "
                           (if (and (not (equal old-value nil))
                                    (not (equal old-value t))
                                    (or (symbolp old-value)
                                        (listp old-value)))
                               "'")
                           (prin1-to-string old-value)))
            (princ "\n")
            (princ (concat "  New value: "
                           (if (and (not (equal new-value nil))
                                    (not (equal new-value t))
                                    (or (symbolp new-value)
                                        (listp new-value)))
                               "'")
                           (prin1-to-string new-value)))
            (princ "\n\n")))
        (print-help-return-message))
    (message "There were no incompatible options and therefore no resets!")))

(defun ecb-check-and-reset-incompatible-options ()
  "Check for all ECB-options if their current value is compatible to the
defined type. If not reset it to the default-value of current ECB. Displays
all reset options with their old \(before the reset) and new values."
  (interactive)
  (ecb-check-not-compatible-options)
  (ecb-reset-not-compatible-options)
  (ecb-display-reset-options))


(provide 'ecb-upgrade)

;;; ecb-upgrade.el ends here
