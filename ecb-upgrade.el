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
  "Alist of all options which must be upgraded for current ECB-version.
There are several reasons why an option should be contained in this alist:
a) An old option has just be renamed in current-ECB version but has still the
   same type of value so the new option should get the value of the old one.
b) An old option has changed its type and we try to transform the old-typed
   value to the new type.
c) An old option has be renamed and also changed its type so we try to
   transform the value of the old option to the type of the new option and set
   the new option to this transformed value.

If an old option has changed its type and we can not transform the old-value
save to the new type then this option should NOT be contained in this alist!

The car is the old option symbol and the cdr is a 2-element-list with:
1. elem: The new option symbol \(can be equal with the old option symbol, see
   b) above)
2. elem: A function which converts the value of the old option to the new
   option. If the type of the option is identical \(i.e. only the option name
   has been changed) then this function should be `identity' otherwise a
   function which gets one argument \(the value of the old option) and returns
   either the corresponding value for the new option with the new correct type
   or the symbol 'ecb-no-upgrade-conversion if no correct conversion can be
   performed!")


(defun ecb-option-get-value (option type)
  "Return the value of a customizable ECB-option OPTION with TYPE, where TYPE
can either be 'standard-value or 'saved-value."
  (let ((val (car (get option type))))
    (cond ((not (listp val)) val)
          ((equal 'quote (car val)) (car (cdr val)))
          (t (car val)))))

(defun ecb-option-set-default (option)
  "Save the ECB-option OPTION with current default value."
  (customize-save-variable option
                           (ecb-option-get-value option 'standard-value)))

(defun ecb-option-upgrade (old-option)
  "Upgrade the old ECB-option OLD-OPTION if the following conditions are ALL
true:
1. OLD-OPTION is the key of an element of `ecb-upgradable-option-alist'
2. 'saved-value of OLD-OPTION is not nil
3. Either
   + the new-option from `ecb-upgradable-option-alist' has the same name
     as OLD-OPTION and
   + the type of the value of OLD-OPTION is not compatible with the current
     type of OLD-OPTION \(this prevents from doing an upgrade twice!)
   or
   + OLD-OPTION is not a bound and valid option in current ECB and
   + The related new-option `ecb-upgradable-option-alist' is not already
     customized, i.e. the 'saved-value of new-option is nil.
Return nil if no upgrade is necessary because at least one of the conditions
above is not true. Returns the transformed value of OLD-OPTION or
'ecb-no-upgrade-conversion in form of a list, to distinguish a transformed
value nil from the nil-result which indicates that no upgrade was necessary
\(see above). This means the \"real\" new value is the car of this
result-list!"
  (let ((upgrade-elem (cdr (assoc old-option ecb-upgradable-option-alist)))
        new-value)
    ;; check if an upgrade is necessary or allowed
    (when (and upgrade-elem
               (or (and (equal old-option (nth 0 upgrade-elem))
                        (not (ecb-option-compatible-p old-option)))
                   (and (not (boundp old-option))
                        (null (get (nth 0 upgrade-elem) 'saved-value))))
               (get old-option 'saved-value))
      ;; try to transform the old-value in the new type.
      (setq new-value
            (condition-case nil
                (funcall (nth 1 upgrade-elem)
                         (ecb-option-get-value old-option 'saved-value))
              (error 'ecb-no-upgrade-conversion)))
      (when (not (equal new-value 'ecb-no-upgrade-conversion))
        ;; the old-value has been transformed successfully into the new type
        ;; so we can save it.
        (customize-save-variable (nth 0 upgrade-elem) new-value))
      ;; we return the value of the transforming-function even if it is
      ;; 'ecb-no-upgrade-conversion!
      (list new-value))))

(defun ecb-option-compatible-p (option)
  "Return not nil only if the type of the value of OPTION is compatible with
current ECB."
  (require 'cus-edit)
  (widget-apply (widget-convert (get option 'custom-type))
                :match (symbol-value option)))

(defvar ecb-not-compatible-options nil
  "This variable is only set by `ecb-check-not-compatible-options'! It is an
alist with car is the symbol of an incompatible option and the cdr is the not
compatible value of this option.
This option is evaluated by `ecb-upgrade-not-compatible-options' and
`ecb-display-upgraded-options'.")

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
      (unless (ecb-option-compatible-p option)
        (setq ecb-not-compatible-options
              (cons (cons option
                          (symbol-value option))
                    ecb-not-compatible-options))))))

(defun ecb-upgrade-not-compatible-options ()
  "Upgrade all not anymore compatible options of `ecb-not-compatible-options'.
If such an option is contained in `ecb-upgradable-option-alist' then try to
perform a special upgrade with `ecb-option-upgrade'. If no upgrade is done
then the option is reset to the default-value of current ECB-version."
  ;; For every not compatible option perform an upgrade
  (dolist (option ecb-not-compatible-options)
    ;; if the incompatible option is not upgraded by `ecb-option-upgrade' then
    ;; we reset it to the standard-value of current ECB-version.
    (let ((upgrade-result (ecb-option-upgrade (car option))))
      (when (or (null upgrade-result) ;; no upgrade necessary or allowed
                ;; the upgrade has been tried but has failed.
                (equal (car upgrade-result) 'ecb-no-upgrade-conversion))
        (ecb-option-set-default (car option))))))

(defvar ecb-renamed-options nil)

(defun ecb-upgrade-renamed-options ()
  "Upgrade all renamed options of `ecb-upgradable-option-alist' and store
every option in `ecb-renamed-options' if at least an upgrade was necessary and
therefore tried \(see `ecb-option-upgrade').

Note: This function upgrades only the renamed but not the incompatible options
\(i.e. only the type but not the name of the option has changed) of
`ecb-upgradable-option-alist' because the latter ones will be upgraded by
`ecb-upgrade-not-compatible-options'!"
  (setq ecb-renamed-options nil)
  (dolist (option ecb-upgradable-option-alist)
    ;; perform only an upgrade if the option is not contained in
    ;; `ecb-not-compatible-options' too because then ECB has auto. recognized
    ;; that this option is not compatible and the upgrade (or reset) is
    ;; performed by `ecb-upgrade-not-compatible-options'!
    (when (not (assoc (car option) ecb-not-compatible-options))
      (let ((new-value-list (ecb-option-upgrade (car option))))
        ;; if an upgrade was tried then store the option in
        ;; `ecb-renamed-options'.
        (when new-value-list
          (setq ecb-renamed-options
                (cons (list (car option)
                            (ecb-option-get-value (car option) 'saved-value)
                            (car (cdr option))
                            (car new-value-list))
                      ecb-renamed-options)))))))

(defun ecb-display-upgraded-options ()
  "Display a message-buffer which options have been upgraded or reset."
  (interactive)
  (if (or ecb-not-compatible-options ecb-renamed-options)
      (with-output-to-temp-buffer "*ECB upgraded options*"
        (when ecb-not-compatible-options
          (princ "The values of the following options are incompatible with current type.\nECB has tried to transform the old-value to the new type. In cases where\nthis was not possible ECB has reset to the current default-value.")
          (princ "\n\n"))
        (dolist (option ecb-not-compatible-options)
          (let ((option-name (symbol-name (car option)))
                (old-value (cdr option))
                (new-value (symbol-value (car option))))
            (princ (concat "+ Option:   " option-name))
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
        (when ecb-renamed-options
          (princ "The following options are no longer valid and have now new names. ECB has\ntried to transform the old value to the new option. In cases where this\nwas not possible the current default value is active!")
          (princ "\n\n"))
        (dolist (option ecb-renamed-options)
          (let ((old-option-name (symbol-name (nth 0 option)))
                (old-value (nth 1 option))
                (new-option-name (symbol-name (nth 2 option)))
                (new-value (nth 3 option)))
            (princ (concat "+ Old option: " old-option-name))
            (princ "\n")
            (princ (concat "  Old value:  "
                           (if (and (not (equal old-value nil))
                                    (not (equal old-value t))
                                    (or (symbolp old-value)
                                        (listp old-value)))
                               "'")
                           (prin1-to-string old-value)))
            (princ "\n")
            (princ (concat "  New option: " new-option-name))
            (princ "\n")
            (princ (concat "  New value:  "
                           (if (equal new-value 'ecb-no-upgrade-conversion)
                               ;; we print the new default value.
                               (prin1-to-string (symbol-value (nth 2 option)))
                             (if (and (not (equal new-value nil))
                                      (not (equal new-value t))
                                      (or (symbolp new-value)
                                          (listp new-value)))
                                 "'")
                             (prin1-to-string new-value))))
            (if (equal new-value 'ecb-no-upgrade-conversion)
                (princ "\n  (The old value couldn't be transformed - this is the current default!)"))
            (princ "\n\n")))
        (princ "If the new values are not what you want please re-customize!")
        (princ "\n\n")
        (print-help-return-message))
    (message "There were no incompatible or renamed options!")))

(defun ecb-upgrade-options ()
  "Check for all ECB-options if their current value is compatible to the
defined type. If not upgrade it to the new type or reset it to the
default-value of current ECB. Try also to upgrade renamed options. Displays
all upgraded or reset options with their old \(before the upgrade/reset) and
new values."
  (interactive)
  (ecb-check-not-compatible-options)
  (ecb-upgrade-not-compatible-options)
  (ecb-upgrade-renamed-options)
  (ecb-display-upgraded-options))


(provide 'ecb-upgrade)

;;; ecb-upgrade.el ends here
