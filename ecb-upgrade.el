;;; ecb-upgrade.el: Upgrade an old ecb-version to the latest one

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
;;
;; What is the intention of this library:
;;
;; Big packages like ECB will be enhanced and developed continously so
;; sometimes a new version must be released. Such packages offer in general a
;; lot of customizable options so probably some of these options change the
;; type or are renamed because the old type and/or name of the option makes no
;; sense in the new release.
;;
;; Especially options which have changed the type of their value are now a
;; problem for the user which want to upgrade to the latest ECB-version: If
;; the user has saved a certain value for option X in its .emacs-file but the
;; type of this saved value doesn't match the new defined type in the
;; defcustom-form after an ECB-upgrade then there can occur serious problems
;; like ECB can not be started anymore or even Emacs can not be startet
;; without errors.
;;
;; Until now there was only one way to fix these problems: The user must
;; manually edit his .emacs-file and remove all entries for options which have
;; now another type. After this and after restarting Emacs the new
;; default-values of the type-changed options in the new ECB-release are
;; active and the user can go on using Emacs and ECB. But this approach to fix
;; the incompatible-option-problem has two serious drawbacks:
;; 1. The user must manually edit the customize-section in his .emacs-file.
;;    This should normally not be done and if then only by old-handed
;;    Emacs-users.
;; 2. The customized value of the option X in the old-release (with the old
;;    type) is lost because after removing the related entry from the
;;    .emacs-file the new default-value is active, so the user must
;;    re-customize the option X.
;;
;; Ok, this is one half of the option-upgrade-problem but a new ECB-release
;; can also rename a option from name X to name Y because the new name Y makes
;; much more sense and/or is more mnemonic. If only the name has changed but
;; not the type this is not a serious problem like above but also annoying
;; because the customized value of the old-option X takes no effect in the new
;; release but instead the default-value of the new-option Y is now active.
;; But nevertheless this problem has the drawback number 2 (see above).
;;
;; The last category of upgrade-problems is a renamed option which has also
;; changed its type.
;;
;; ecb-upgrade.el is the solution for all these problems:

;; - It checks all customized values of all ECB-optons if they are still
;;   type-compatible. If not then it tries to upgrade the old-value to the new
;;   value-type and if this is not possible then it resets the option to the
;;   new default value and store it via customize in the .emacs-file (or in
;;   any file which is used for customized options).
;; - It offers a special constant `ecb-upgradable-option-alist' which allows
;;   the ECB-maintainers to define special transformings for renamed options
;;   so even the value of an old-option X can be savely transformed to the
;;   new-option Y and the old setting is not lost.
;;
;; All these checks and transformings are done at beginning of activating ECB.
;; If ECB has recognized incompatible or renamed options it does its
;; upgrading/reseting-job so all ECB-options have correct types so ECB can
;; start correct. After ECB is started it displays a list of all upgraded or
;; resetted option with their old and new values.
;;
;; How does this library work:
;;
;; The important functions are `ecb-check-not-compatible-options' and
;; `ecb-upgrade-not-compatible-options':
;;
;; The former one checks if all customized values of ECB-options have still
;; correct type. If not the incompatible options and their old values are
;; stored in an alist `ecb-not-compatible-options'. Only this function is
;; allowed to changed this alist!!
;;
;; The latter one processes now this alist and looks for every incompatible
;; option if there is an entry in `ecb-upgradable-option-alist'. If yes then a
;; special value-transforming is tried by `ecb-option-upgrade'. If no or if
;; the special transforming has been failed for any reason then it resets the
;; option to the default-value of current active ECB-version and save it via
;; `customize-save-variable'.
;;
;; So if the ECB-maintainers define no special transforming in the alist
;; `ecb-upgradable-option-alist' for a re-typed option X then all incompatible
;; options are at least reset to their current default-value and therefore ECB
;; can start correct.
;;
;; But there is another function `ecb-upgrade-renamed-options': This function
;; processes the alist `ecb-upgradable-option-alist' and call for every
;; element-key (an old option-symbol) of this alist `ecb-option-upgrade' but
;; only if this element-key is not also contained in the alist
;; `ecb-not-compatible-options' because in this case this option has been
;; already be upgraded/resetted by `ecb-upgrade-not-compatible-options' (see
;; above).
;;
;; So the calling sequence of these three functions must be:
;; 1. `ecb-check-not-compatible-options'
;; 2. `ecb-upgrade-not-compatible-options'
;;    `ecb-upgrade-renamed-options' or vice versa.
;; 
;; There are also two interactive commands:
;; - `ecb-display-upgraded-options' displays a temp. buffer with all upgraded
;;   or resetted ECB-options with their old and new values.
;; - `ecb-upgrade-options': Does all necessary beginning with the
;;   incompatibility-check and ending with the display of the options.
;;
;; What must an ECB-maintainer do:
;;
;; + If he wants only a save and correct ECB-start with the new release:
;;   NOTHING
;; + If he wants to preserve best possible the customized values of now
;;   type-incompatible and/or renamed options:
;;   - Adding entries to the alist `ecb-upgradable-option-alist' and
;;   - Defining suitable transforming-functions for every of these options.
;;   See the comment of `ecb-upgradable-option-alist'.

;; As an addition to this upgrade feature this library offers a function
;; `ecb-download-ecb' to download a newer version of ECB direct from the
;; website!

;;; Code

(eval-when-compile
  (require 'silentcomp))

(require 'ecb-util)

(silentcomp-defun widget-convert)

;; ----------------------------------------------------------------------
;; define in this defconst all options which should be upgraded
;; ----------------------------------------------------------------------

(defconst ecb-upgradable-option-alist
  '((ecb-compile-window-temporally-enlarge . (ecb-compile-window-temporally-enlarge
                                              ecb-upgrade-compile-window-temporally-enlarge))
    (ecb-window-sync . (ecb-window-sync ecb-upgrade-window-sync))
    (ecb-hide-ecb-windows-hook . (ecb-hide-ecb-windows-before-hook identity))
    (ecb-show-ecb-windows-hook . (ecb-show-ecb-windows-before-hook identity))
    (ecb-layout-nr . (ecb-layout-name ecb-upgrade-layout-nr))
    (ecb-toggle-layout-sequence . (ecb-toggle-layout-sequence
                                   ecb-upgrade-toggle-layout-sequence))
    (ecb-layout-window-sizes . (ecb-layout-window-sizes
                                ecb-upgrade-layout-window-sizes))
    (ecb-major-modes-activate . (ecb-major-modes-activate
                                 ecb-upgrade-major-modes-activate))
    (ecb-cache-directory-contents . (ecb-cache-directory-contents
                                     ecb-upgrade-cache-directory-contents)))
  
  "Alist of all options which should be upgraded for current ECB-version.
There are several reasons why an option should be contained in this alist:
a) An old option has just be renamed in current-ECB version but has still the
   same type of value so the new option should get the value of the old one.
b) An old option has changed its type and we try to transform the old-typed
   value to the new type.
c) An old option has be renamed and also changed its type so we try to
   transform the value of the old option to the type of the new option and set
   the new option to this transformed value.

If an old option has changed its type and we can not savely transform the
old-value to the new type then this option should NOT be contained in this
alist! Such an option is auto. reset to the current default-value by
`ecb-upgrade-not-compatible-options'!

Every element of this alist has the following form:
The car is the old option symbol and the cdr is a 2-element-list with:
1. elem: The new option symbol \(can be equal with the old option symbol, see
   b) above)
2. elem: A function which converts the value of the old option to the new
   option. If the type of the options is identical \(i.e. only the option name
   has been changed, see a) above) then this function should be `identity'
   otherwise a function which gets one argument \(the value of the old option)
   and returns either a corresponding value for the new option with the new
   correct type or the symbol 'ecb-no-upgrade-conversion if no correct
   conversion can be performed! Maybe the function `ecb-option-get-value' can
   be helpful within such a transforming-function.")

;; ----------------------------------------------------------------------
;; define here all necessary upgrade functions
;; ----------------------------------------------------------------------

;; upgrading ecb-compile-window-temporally-enlarge
(defun ecb-upgrade-compile-window-temporally-enlarge (old-val)
  (cond ((equal old-val t)
         'after-compilation)
        ((null old-val)
         nil)
        (t 'ecb-no-upgrade-conversion)))

;; upgrading ecb-window-sync
(defun ecb-upgrade-window-sync (old-val)
  (if (equal old-val t)
      (ecb-option-get-value 'ecb-window-sync 'standard-value)
    nil))

;; upgrading old layout-numbers (ECB <= 1.80) to new layout-names (ECB
;; >= 1.90)
(defun ecb-upgrade-layout-nr2name (number)
  (let ((number-name-alist '((0 . "left1")
                             (1 . "left2")
                             (2 . "left3")
                             (3 . "left4")
                             (4 . "left5")
                             (5 . "right1")
                             (6 . "left6")
                             (7 . "top1")
                             (8 . "left7")
                             (9 . "left8")
                             (10 . "top2")
                             (11 . "left9")
                             (12 . "left10")
                             (13 . "left11")
                             (14 . "left12")
                             (15 . "left13")
                             (16 . "left14")
                             (17 . "left15")
                             (18 . "leftright1")
                             (19 . "leftright2")
                             (20 . "speedbar1"))))
    (cdr (assoc number number-name-alist))))

(defun ecb-upgrade-layout-nr (old-val)
  (let ((name (ecb-upgrade-layout-nr2name old-val)))
    (if (stringp name)
        name
      'ecb-no-upgrade-conversion)))

(defun ecb-upgrade-toggle-layout-sequence (old-val)
  (mapcar (function (lambda (elem)
                      (ecb-upgrade-layout-nr2name elem)))
          old-val))

(defun ecb-upgrade-layout-window-sizes (old-val)
  (let ((l (copy-tree old-val)))
    (dolist (elem l)
      (setcar elem
              (ecb-upgrade-layout-nr2name (car elem))))
    l))

(defun ecb-upgrade-major-modes-activate (old-val)
  (if (not (listp old-val))
      old-val
    (let ((l (copy-tree old-val)))
      (dolist (elem l)
        (if (and (consp elem)
                 (integerp (cdr elem)))
            (setcdr elem (ecb-upgrade-layout-nr2name (cdr elem)))))
      l)))

(defun ecb-upgrade-cache-directory-contents (old-val)
  (mapcar (function (lambda (elem)
                      (cons (nth 0 elem) (nth 1 elem))))
          old-val))

;; ----------------------------------------------------------------------
;; internal functions. Dot change anything below this line
;; ----------------------------------------------------------------------

(defun ecb-option-get-value (option type)
  "Return the value of a customizable ECB-option OPTION with TYPE, where TYPE
can either be 'standard-value \(the default-value of the defcustom) or
'saved-value \(the value stored durable by the user via customize)."
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

If all conditions are true then the value of OLD-OPTION is transformed by the
transforming-function of the related element of `ecb-upgradable-option-alist'
to the correct new type and then the related new option is saved with this new
value.

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
its current defcustom-definition."
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
perform a special upgrade with `ecb-option-upgrade'. If no special upgrade is
done then the option is reset to the default-value of current ECB-version."
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
every option in `ecb-renamed-options' if at least an upgrade was tried \(see
`ecb-option-upgrade').

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
          (princ "The following options are not longer valid and have now new names. ECB has\ntried to transform the old value to the new option. In cases where this\nwas not possible the current default value is active!")
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
                             (concat (if (and (not (equal new-value nil))
                                              (not (equal new-value t))
                                              (or (symbolp new-value)
                                                  (listp new-value)))
                                         "'")
                                     (prin1-to-string new-value)))))
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

(defgroup ecb-download nil
  "Settings for downloading and installing a new ECB from within ECB."
  :group 'ecb
  :prefix "ecb-")

(defcustom ecb-download-url "http://ftp1.sourceforge.net/ecb/"
  "*URL where downloadable ECB-versions are located.
The ECB-archive-file \(e.g. ecb-1.70.tar.gz\) will be appended to this URL and
`ecb-download-ecb' will try to download this archive.

Note: Normally this URL should never change but who knows..."
  :group 'ecb-download
  :type 'string)

(defcustom ecb-download-version "latest"
  "*Which version of ECB should be downloaded by `ecb-download-ecb'.
Valid values are either the string \"latest\" or a version number like
\"1.70\". ECB creates automatically the correct URL for download, see
`ecb-download-ecb'."
  :group 'ecb-download
  :type '(radio (const :tag "Latest ECB version" :value "latest")
                (string :tag "ECB version")))

(defcustom ecb-download-delete-archive 'always
  "*Should the downloaded archive be deleted after successfull
installation or after failure during the installation-process. Possible values
are:
- only-after-success: Archive is only deleted after successfull installation
  but not if a failure occurs during the installation process.
- always: Archive is also deleted if an error occurs.
- nil: Archive will never be deleted."
  :group 'ecb-download
  :type '(choice :tag "Delete archive" :menu-tag "Delete archive"
                 (const :tag "After successfull installation" only-after-success)
                 (const :tag "Always" always)
                 (const :tag "Never" nil)))

(defvar ecb-semantic-eieio-url "http://ftp1.sourceforge.net/cedet/")

(defconst ecb-download-buffername " *ecb-download*")

;; Klaus: Arrghhhhhhhhhhhhhhh... the cygwin version of tar does not accept
;; args in windows-style file-format :-( Therefore we convert it with cygpath.
;; Cause of the need of wget we can assume the the user has cygwin installed!
(defmacro ecb-create-shell-argument (arg)
  `(if (eq system-type 'windows-nt)
       (if (executable-find "cygpath.exe")
           (shell-command-to-string (concat "cygpath -u " ,arg))
         (ecb-error "Cannot find the cygpath utility!"))
     ,arg))

(defun ecb-download-ecb ()
  "Download ECB from the ECB-website and install it. For this the options
`ecb-download-url' and `ecb-download-version' must be set correct, whereas the
default value of the former one should always be correct.

For details about downloading and what requirements must be satisfied see
`ecb-download-package'!

After sucessfull downloading the new ECB will be installed in a directory
parallel to current ECB-directory. After adding this new directory to
`load-path' and restarting Emacs the new ECB version can be activated by
`ecb-activate'.

If current running ECB is installed as regular XEmacs-package and not with the
archive available at the ECB website then this function asks for proceeding!"
  (interactive)
  (let ((proceed t))
    (when ecb-regular-xemacs-package-p
      (with-output-to-temp-buffer "*ECB downloading and installing*"
        (princ "Current ECB is installed as regular XEmacs package and not with the\n")
        (princ "archive available at the ECB-website. So you should use the package-manager\n")
        (princ "of XEmacs to get the latest version of ECB! If you proceed installing from\n")
        (princ "the ECB website then the new ECB is NOT installed as regular XEmacs-package\n")
        (princ "but as \"flat\" package parallel to the current ECB directory!\n\n"))
      (setq proceed (yes-or-no-p "Do you want to proceed installing from the ECB-website? ")))
    (when proceed
      (let ((install-dir (ecb-download-package "ecb" ecb-download-version
                                               ecb-download-url)))
        (when install-dir
          (message "New ECB successfully installed!")
          (with-output-to-temp-buffer "*ECB downloading and installing*"
            (princ "ECB has successfully installed the new ECB version in a directory parallel to\n")
            (princ "current ECB.\n\n")
            (princ (concat "+ Current ECB: " ecb-ecb-dir "\n"))
            (princ (concat "+ New ECB: " install-dir))
            (princ "\n\n")
            (princ "After replacing the current ECB-directory with the new one in your `load-path'\n")
            (princ "and then restarting Emacs the new ECB version can be activated by `ecb-activate'.\n\n")
            (princ "If the value of `ecb-auto-compatibility-check' is not nil then the new version\n")
            (princ "checks at start-time if there are incompatible options! Please read the\n")
            (princ "documentation of this option!")
            (princ "\n\n")))))))
    

(defun ecb-download-package (package version url)
  "Download VERSION of PACKAGE from URL and install it. If no failure occurs
during this process the full path of the directory is returned in which the
new package is installed. Otherwise an error is reported.

For correct downloading and installing the utilities \"wget\", \"tar\" and
\"gzip\" are needed which are available for unix and also for windows with
cygwin. All utilities must reside in your PATH!

If you are behind a firewall and you have to use a proxy you maybe need the
following wget-configuration in your \"~/.wgetrc\"-file:

   # Define your proxies \(where 8080 and 8081 are examples for the portnumbers)
   http_proxy = http://your.proxy.com:8080
   ftp_proxy  = http://your.ftpproxy.com:8081
     
   # If you do not want to use proxy at all, set this to off.
   use_proxy = on

ECB will try to download the file: \"<URL><PACKAGE>-<VERSION>.tar.gz\".
Example: For PACKAGE = \"ecb\", VERSION = \"latest\" and URL =
\"http://ftp1.sourceforge.net/ecb/\" the download-file would be
\"http://ftp1.sourceforge.net/ecb/ecb-latest.tar.gz\".

After sucessfull downloading the new package version will be installed in a
directory parallel to current ECB-directory. After adding this new directory
tp `load-path' and restarting Emacs the new package version can be activated."
  (let ((downloaded-filename (concat ecb-ecb-parent-dir
                                     package "-download.tar.gz"))
        (success t)
        process-result install-dir)

    ;; a first simple check if the new version is already installed - will not
    ;; work for "latest"
    
    (if (not (or (not (file-directory-p (concat ecb-ecb-parent-dir package "-"
                                                version)))
                 (yes-or-no-p
                  (format "%s %s seems to be already installed in directory %s! Force? "
                          package version
                          (concat package "-" version)))))
        ;; we can go back with this install dir
        (concat ecb-ecb-parent-dir package "-" version)
      
      ;; cleaning up

      (if (get-buffer ecb-download-buffername)
          (kill-buffer ecb-download-buffername))
      (ecb-delete-file downloaded-filename)
      (ecb-delete-file (file-name-sans-extension downloaded-filename))

      ;; checking if all necessary tools are available

      (if (not (and (executable-find
                     (if (eq system-type 'windows-nt) "wget.exe" "wget"))
                    (executable-find
                     (if (eq system-type 'windows-nt) "tar.exe" "tar"))
                    (executable-find
                     (if (eq system-type 'windows-nt) "gzip.exe" "gzip"))))
          (ecb-error
           (concat "Cannot find wget, tar and gzip. These utilities are needed "
                   "to download and install ECB or required packages."))

        ;; OK, now we begin....
        
        ;; Downloading with working-display

        (working-status-call-process
         0.1
         (concat "Downloading new " package)
         "done"
         (if (eq system-type 'windows-nt)
             "wget.exe"
           "wget")
         nil
         ecb-download-buffername
         nil
         "-C"
         "off"
         "-O"
         downloaded-filename
         (concat url package "-" version ".tar.gz"))

        ;; checking the download-result

        (save-excursion
          (set-buffer ecb-download-buffername)
          (setq process-result (buffer-string))
          (goto-char (point-min))
          (when (not (and (save-excursion
                            (search-forward-regexp "200" nil t))
                          (search-forward-regexp
                           (concat (regexp-quote downloaded-filename) ".*saved.*")
                           nil t)
                          (file-exists-p downloaded-filename)))
            (setq success nil)))
        (unless success
          (with-output-to-temp-buffer "*ECB-download-failure*"
            (princ (format "The download of %s has failed cause of the following wget-failure:"
                           package))
            (princ "\n")
            (princ "______________________________________________________________________________\n\n")
            (princ process-result)
            (princ "\n______________________________________________________________________________")
            (princ "\n\n")
            (princ "Please check the wget configuration in \"~/.wgetrc\" and also the values\n")
            (princ "of the options in the customize group 'ecb-download'.")
            (princ "ECB has tried to download the following URL:\n\n")
            (princ (concat "  " url package "-" version ".tar.gz"))
            (princ "\n\n")
            (princ "Maybe this URL does not exist...please check this!\n\n")))
        (kill-buffer ecb-download-buffername)

        ;; uncompressing with gzip

        (when success
          (message "Uncompressing new %s..." package)
          (setq process-result
                (shell-command-to-string (concat "gzip -d " downloaded-filename)))
          (when (> (length process-result) 0)
            (setq success nil)
            (with-output-to-temp-buffer "*ECB-uncompressing-failure*"
              (princ (format "Uncompressing of %s has failed cause of the following problems:"
                             package))
              (princ "\n\n")
              (princ process-result))))

        ;; checking the version of the new package

        (when success
          (message "Uncompressing new %s...done" package)
          (message "Checking if already installed...")
          (setq process-result
                (shell-command-to-string
                 (concat "tar"
                         " -tf "
                         (ecb-create-shell-argument
                          (file-name-sans-extension downloaded-filename)))))
          (if (string-match (format "^%s-\\(.+\\)/" package) process-result)
              (let ((downloaded-version (match-string 1 process-result)))
                (setq install-dir (concat package "-" downloaded-version))
                (when (not (or (not (file-directory-p (concat ecb-ecb-parent-dir
                                                              install-dir)))
                               (yes-or-no-p
                                (format "%s %s seems to be already installed in directory %s! Force? "
                                        package downloaded-version install-dir))))
                  ;; we have finished an can go back with this install dir
                  (return install-dir)))
            (setq success nil)
            (with-output-to-temp-buffer "*ECB-archive-failure*"
              (princ (format "Checking the archive of %s has failed cause of the following problems:"
                             package))
              (princ "\n\n")
              (princ process-result))))

        ;; unpacking new package

        (when success
          (message "Checking if already installed...done")
          (message "Unpacking new %s..." package)
          (setq process-result
                (shell-command-to-string
                 (concat "tar -C "
                         (ecb-create-shell-argument ecb-ecb-parent-dir)
                         " -xf "
                         (ecb-create-shell-argument
                          (file-name-sans-extension downloaded-filename)))))
          (when (> (length process-result) 0)
            (setq success nil)
            (with-output-to-temp-buffer "*ECB-unpacking-failure*"
              (princ (format "Unpacking of %s has failed cause of the following problems:"
                             package))
              (princ "\n\n")
              (princ process-result))))

        ;; maybe cleaning up

        (when (or (and success ecb-download-delete-archive)
                  (and (not success) (eq ecb-download-delete-archive 'always)))
          (ecb-delete-file (file-name-sans-extension downloaded-filename))
          (ecb-delete-file downloaded-filename))))
    ;; now we return if we had success or not
    (if success
        (concat ecb-ecb-parent-dir install-dir)
      (ecb-error "Downloading or installing failure for %s %s" package version))))

(silentcomp-provide 'ecb-upgrade)

;;; ecb-upgrade.el ends here
