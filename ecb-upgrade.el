;;; ecb-upgrade.el --- Upgrade an old ecb-version to the latest one

;; Copyright (C) 2000 - 2003 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
;; Keywords: browser, code, programming, tools
;; Created: 2002

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

;; $Id$

;;; Commentary:
;;
;; This file upgrades an old ECB-version best possible to the latest one.
;;
;; What is the intention of this library:
;;
;; Big packages like ECB will be enhanced and developed continuously so
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
;; like ECB can not be started anymore or even Emacs can not be started
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
;; OK, this is one half of the option-upgrade-problem but a new ECB-release
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

;; - It checks all customized values of all ECB-options if they are still
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
;; reseted option with their old and new values.
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
;; already be upgraded/reseted by `ecb-upgrade-not-compatible-options' (see
;; above).
;;
;; So the calling sequence of these three functions must be:
;; 1. `ecb-check-not-compatible-options'
;; 2. `ecb-upgrade-not-compatible-options'
;;    `ecb-upgrade-renamed-options' or vice versa.
;; 
;; There are also two interactive commands:
;; - `ecb-display-upgraded-options' displays a temp. buffer with all upgraded
;;   or reseted ECB-options with their old and new values.
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

;; As an addition to this upgrade feature this library offers a set of
;; functions ecb-package-... for downloading and installing newer versions of
;; packages (ecb itself and the required packages)!

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.


;;; Code

(eval-when-compile
  (require 'silentcomp))

;; IMPORTANT: The version-number is auto-frobbed from the Makefile. Do not
;; change it here!
(defconst ecb-version "2.24"
  "Current ECB version.")

(eval-when-compile
  (require 'cl))

(require 'ecb-util)

(silentcomp-defun widget-convert)

;; -------------------------------------------------------------------------
;; define in this defconst all important NEWS which a user should know after
;; upgrading to the new version.
;; -------------------------------------------------------------------------

;; Each NEWS-string should be a one-liner shorter than 70 chars
(defconst ecb-upgrade-news
  '(
    ("2.24" . ("New \"current-type\"-filter for the Methods-buffer"
               "Now directories are prescanned for emptyness"))
    ("2.23" . ("New cedet1.0beta2 is supported."
               "Distinction between functions and function-prototypes in the Methods-buffer"
               "The command `ecb-toggle-layout' now has a prefix-argument"
               "Default tag-filters for certain files which are applied automatically"
               "Double-clicking the mouse-1-button now works with integrated speedbar"
               "A new hook `ecb-speedbar-before-activate-hook'"))
    ("2.22" . ("New nifty feature for filtering the tags displayed in the Methods-buffer"
               "Much smarter mechanism to highlight the current tag in the methods-buffer"
               "New option `ecb-auto-expand-tag-tree-collapse-other'."
               "Fixed a bug preventing the native Windows-port of XEmacs from working."))
    ("2.21" . ("Advice for `balance-windows' so only the edit-windows are balanced."
               "Gnus, BBDB, VM, Xrefactory etc. work even when ECB-windows are visible."
               "Commands using `Electric-pop-up-window' now work correctly with ECB."
               "Fixed some annoying bugs and one fatal bug."))
    ("2.20" . ("Fixed a bug preventing tree-buffers with expand-symbol \'before\' to work"
               "'ecb-major-modes-\(de)activate' replaced by `ecb-major-modes-show-or-hide'"
               "New keybinding for the online-help: [C-c . h]"
               "The edit-area can be splitted in more than 2 windows."
               "`ecb-other-window-jump-behavior' renamed in `ecb-other-window-behavior'"
               "New option `ecb-maximize-ecb-window-after-selection'"
               "popup-menus of the tree-buffers can be used with the tmm-library"
               "New option `ecb-change-layout-preserves-compwin-state'"
               "`delete-window' and `delete-other-windows' handle the compile-window"
               "Support of the default modeline-mechanisms for deleting other windows"))
    ("2.11" . ("Using semanticdb to jump to type-tags defined in other files"))
    ("2.01" . ("Support for semantic 2.0"
               "The tree-buffers can be displayed graphically with images"
               "Popup-menus of the tree-buffers support submenus"
               "The sources- and the history-buffer can be filtered"
               "Ediff runs per default in the ECB-frame"))
    ("1.96" . ("ECB can work together with the window-managers escreen and winring"
               "Much better support of the ECB-compile-window"))))



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
    (ecb-cache-directory-contents . (ecb-cache-directory-contents
                                     ecb-upgrade-cache-directory-contents))
    (ecb-source-file-regexps . (ecb-source-file-regexps
                                ecb-upgrade-source-file-regexps))
    (ecb-layout-always-operate-in-edit-window . (ecb-layout-always-operate-in-edit-window
                                                 ecb-upgrade-alway-operate-in-edit-window))
    (ecb-truncate-lines . (ecb-truncate-lines
                           ecb-upgrade-truncate-lines))
    (ecb-mode-line-prefixes . (ecb-mode-line-prefixes
                               ecb-upgrade-mode-line-prefixes))
    (ecb-mode-line-data . (ecb-mode-line-data
                               ecb-upgrade-mode-line-data))
    (ecb-use-speedbar-for-directories . (ecb-use-speedbar-instead-native-tree-buffer
                                         ecb-upgrade-use-speedbar-for-directories))

    (ecb-directories-menu-user-extension . (ecb-directories-menu-user-extension
                                            ecb-upgrade-directories-menu-ext))
    (ecb-sources-menu-user-extension . (ecb-sources-menu-user-extension
                                        ecb-upgrade-sources-menu-ext))
    (ecb-methods-menu-user-extension . (ecb-methods-menu-user-extension
                                        ecb-upgrade-methods-menu-ext))
    (ecb-history-menu-user-extension . (ecb-history-menu-user-extension
                                        ecb-upgrade-history-menu-ext))
    (ecb-bucket-token-display . (ecb-bucket-node-display identity))
    (ecb-auto-expand-token-tree . (ecb-auto-expand-tag-tree identity))
    (ecb-font-lock-tokens . (ecb-font-lock-tags identity))
    (ecb-token-jump-sets-mark . (ecb-tag-jump-sets-mark identity))
    (ecb-token-display-function . (ecb-tag-display-function ecb-upgrade-token-display-function))
    (ecb-type-token-display . (ecb-type-tag-display ecb-upgrade-type-token-display))
    (ecb-post-process-semantic-tokenlist . (ecb-post-process-semantic-taglist
                                            ecb-upgrade-post-process-semantic-tokenlist))
    (ecb-show-only-positioned-tokens . (ecb-show-only-positioned-tags identity))
    (ecb-show-tokens . (ecb-show-tags ecb-upgrade-show-tags))
    (ecb-show-tags . (ecb-show-tags ecb-upgrade-show-tags))
    (ecb-highlight-token-with-point . (ecb-highlight-tag-with-point identity))
    (ecb-highlight-token-with-point-delay . (ecb-highlight-tag-with-point-delay identity))
    (ecb-token-visit-post-actions . (ecb-tag-visit-post-actions
                                     ecb-upgrade-token-visit-post-actions))
    (ecb-token-header-face . (ecb-tag-header-face
                              ecb-upgrade-token-header-face))
    (ecb-post-process-semantic-taglist . (ecb-post-process-semantic-taglist
                                          ecb-upgrade-post-process-semantic-taglist))
    (ecb-primary-mouse-jump-destination . (ecb-mouse-click-destination identity))
    (ecb-split-edit-window . (ecb-split-edit-window-after-start ecb-upgrade-split-edit-window))
    (ecb-other-window-jump-behavior . (ecb-other-window-behavior ecb-upgrade-other-window-jump-behavior)))
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
  (cond ((or (equal old-val t)
             (equal old-val 'after-compilation))
         'after-display)
        ((null old-val)
         nil)
        ((member old-val '(after-selection both))
         old-val)
        (t 'ecb-no-upgrade-conversion)))

;; upgrading ecb-window-sync
(defun ecb-upgrade-window-sync (old-val)
  (if (equal old-val t)
      (ecb-option-get-value 'ecb-window-sync 'standard-value)
    nil))

;; upgrading old layout-numbers (ECB <= 1.80) to new layout-names (ECB
;; >= 1.90)
(defun ecb-upgrade-layout-nr2name (number)
  (let ((number-name-alist '((nil . "left8")
                             (0 . "left1")
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

(defun ecb-upgrade-use-speedbar-for-directories (old-val)
  (if old-val
      'dir))

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

(defun ecb-upgrade-source-file-regexps (old-val)
  (list (cons ".*" old-val)))

(defun ecb-upgrade-truncate-lines (old-val)
  (if old-val
      '(t t t t)
    '(nil nil nil nil)))

(defun ecb-upgrade-alway-operate-in-edit-window (old-val)
  (let ((l (copy-tree old-val)))
    (setq l (delete 'switch-to-buffer-other-window l))
    l))

(defun ecb-upgrade-mode-line-prefixes (old-val)
  (list (cons 'ecb-directories-buffer-name
              (nth 0 old-val))
        (cons 'ecb-sources-buffer-name
              (nth 1 old-val))
        (cons 'ecb-methods-buffer-name
              (nth 2 old-val))
        (cons 'ecb-history-buffer-name
              (nth 3 old-val))))

(defun ecb-upgrade-mode-line-data (old-val)
  (list (cons 'ecb-directories-buffer-name
              (if (equal (nth 0 old-val) 'selected)
                  'sel-dir
                (nth 0 old-val)))
        (cons 'ecb-sources-buffer-name
              (if (equal (nth 1 old-val) 'selected)
                  'sel-dir
                (nth 1 old-val)))
        (cons 'ecb-methods-buffer-name
              (if (equal (nth 2 old-val) 'selected)
                  'sel-source
                (nth 2 old-val)))
        (cons 'ecb-history-buffer-name
              (nth 3 old-val))))

(defun ecb-upgrade-menu-extension (old-list)
  (mapcar (function (lambda (i)
                      (reverse i)))
          old-list))

(defun ecb-upgrade-directories-menu-ext (old-val)
  (append (ecb-upgrade-menu-extension old-val)
          (ecb-option-get-value 'ecb-directories-menu-user-extension
                                'standard-value)))

(defun ecb-upgrade-sources-menu-ext (old-val)
  (append (ecb-upgrade-menu-extension old-val)
          (ecb-option-get-value 'ecb-sources-menu-user-extension
                                'standard-value)))

(defun ecb-upgrade-methods-menu-ext (old-val)
  (append (ecb-upgrade-menu-extension old-val)
          (ecb-option-get-value 'ecb-methods-menu-user-extension
                                'standard-value)))

(defun ecb-upgrade-history-menu-ext (old-val)
  (append (ecb-upgrade-menu-extension old-val)
          (ecb-option-get-value 'ecb-history-menu-user-extension
                                'standard-value)))

(defun ecb-upgrade-token-display-function (old-val)
  (let ((l (copy-tree old-val))
        (mapping-list
         '((semantic-name-nonterminal                  . ecb--semantic-format-tag-name)
           (semantic-abbreviate-nonterminal            . ecb--semantic-format-tag-abbreviate)
           (semantic-summarize-nonterminal             . ecb--semantic-format-tag-summarize)
           (semantic-prototype-nonterminal             . ecb--semantic-format-tag-prototype)
           (semantic-concise-prototype-nonterminal     . ecb--semantic-format-tag-concise-prototype)
           (semantic-uml-abbreviate-nonterminal        . ecb--semantic-format-tag-uml-abbreviate)
           (semantic-uml-prototype-nonterminal         . ecb--semantic-format-tag-uml-prototype)
           (semantic-uml-concise-prototype-nonterminal . ecb--semantic-format-tag-uml-concise-prototype)
           (semantic-prin1-nonterminal                 . ecb--semantic-format-tag-prin1)
           (ecb-name-nonterminal                  . ecb-format-tag-name)
           (ecb-abbreviate-nonterminal            . ecb-format-tag-abbreviate)
           (ecb-summarize-nonterminal             . ecb-format-tag-summarize)
           (ecb-prototype-nonterminal             . ecb-format-tag-prototype)
           (ecb-concise-prototype-nonterminal     . ecb-format-tag-concise-prototype)
           (ecb-uml-abbreviate-nonterminal        . ecb-format-tag-uml-abbreviate)
           (ecb-uml-prototype-nonterminal         . ecb-format-tag-uml-prototype)
           (ecb-uml-concise-prototype-nonterminal . ecb-format-tag-uml-concise-prototype)
           (ecb-prin1-nonterminal                 . ecb-format-tag-prin1))))
    (mapc (function (lambda (e)
                      (if (assoc (cdr e) mapping-list)
                          (setcdr e (cdr (assoc (cdr e) mapping-list))))))
          l)
    l))


(defun ecb-upgrade-type-token-display (old-val)
  (let ((val-copy (copy-tree old-val))
        (mapping-list
         '((ecb-type-token-class-face . ecb-type-tag-class-face)
           (ecb-type-token-interface-face . ecb-type-tag-interface-face)
           (ecb-type-token-struct-face . ecb-type-tag-struct-face)
           (ecb-type-token-typedef-face . ecb-type-tag-typedef-face)
           (ecb-type-token-enum-face . ecb-type-tag-enum-face)
           (ecb-type-token-group-face . ecb-type-tag-group-face))))
    (mapc (function (lambda (e)
                      (dolist (l (cdr e))
                        (if (assoc (nth 2 l) mapping-list)
                            (ecb-set-elt l 2
                                         (cdr (assoc (nth 2 l) mapping-list)))))))
          val-copy)
    val-copy))

(defun ecb-upgrade-post-process-semantic-tokenlist (old-val)
  (let ((val-copy (copy-tree old-val))
        (mapping-list
         '((ecb-group-function-tokens-with-parents . ecb-group-function-tags-with-parents))))
    (mapc (function (lambda (e)
                      (if (assoc (cdr e) mapping-list)
                          (setcdr e (cdr (assoc (cdr e) mapping-list))))))
          val-copy)
    val-copy))

(defun ecb-upgrade-token-visit-post-actions (old-val)
  (let ((val-copy (copy-tree old-val))
        (mapping-list
         '((ecb-token-visit-highlight-token-header . ecb-tag-visit-highlight-tag-header)
           (ecb-token-visit-smart-token-start . ecb-tag-visit-smart-tag-start)
           (ecb-token-visit-recenter . ecb-tag-visit-recenter)
           (ecb-token-visit-recenter-top . ecb-tag-visit-recenter-top)
           (ecb-token-visit-goto-doc-start . ecb-tag-visit-goto-doc-start)
           (ecb-token-visit-narrow-token . ecb-tag-visit-narrow-tag))))
    (mapc (function (lambda (e)
                      (dotimes (i (length (cdr e)))
                        (if (assoc (nth i (cdr e)) mapping-list)
                            (ecb-set-elt (cdr e) i
                                         (cdr (assoc (nth i (cdr e))
                                                     mapping-list)))))))
          val-copy)
    val-copy))

(defun ecb-upgrade-token-header-face (old-val)
  (if (equal old-val 'ecb-token-header-face)
      'ecb-tag-header-face
    old-val))

(defun ecb-upgrade-post-process-semantic-taglist (old-val)
  (let ((l (copy-tree old-val)))
    (dolist (elem l)
      (if (cdr elem)
          (setcdr elem (list (cdr elem)))))
    l))

(defun ecb-upgrade-split-edit-window (old-val)
  (if (equal old-val t)
      'before-activation
    old-val))

(defun ecb-upgrade-other-window-jump-behavior (old-val)
  (if (equal old-val 'all)
      'all
    (ecb-option-get-value 'ecb-other-window-behavior
                          'standard-value)))

(defun ecb-upgrade-show-tags (old-val)
  (ecb-option-get-value 'ecb-show-tags
                        'standard-value))

;; ----------------------------------------------------------------------
;; internal functions. Dot change anything below this line
;; ----------------------------------------------------------------------

(defgroup ecb-upgrade-internal nil
  "Only Internal setting for the ECB upgrade-mechanism - no user-options!"
  :group 'ecb-general
  :prefix "ecb-")

(defcustom ecb-options-version ecb-version
  "*DO NOT CUSTOMIZE THIS VALUE - IT IS ONLY FOR INTERNAL USAGE!"
  :group 'ecb-upgrade-internal
  :type 'string)

(defun ecb-custom-file-writeable-p ()
  (ignore-errors (file-writable-p (ecb-custom-file))))

(defun ecb-customize-save-variable (option value)
  (if (ecb-custom-file-writeable-p)
      (customize-save-variable option value)
    (customize-set-variable option value)))

(defun ecb-option-set-default (option)
  "Save the ECB-option OPTION with current default value."
  (ecb-customize-save-variable option
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
   + OLD-OPTION is not a valid option in current ECB and
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
                   (and (not (member old-option ecb-all-options))
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
        (ecb-customize-save-variable (nth 0 upgrade-elem) new-value))
      ;; we return the value of the transforming-function even if it is
      ;; 'ecb-no-upgrade-conversion!
      (list new-value))))

(defun ecb-option-compatible-p (option)
  "Return not nil only if the type of the value of OPTION is compatible with
its current defcustom-definition."
  (require 'cus-edit)
  (widget-apply (widget-convert (get option 'custom-type))
                :match (symbol-value option)))

(defvar ecb-old-ecb-version nil
  "Only not nil if ECB has upgraded the options to a newer options-version
after an ECB-upgrade.")

(defun ecb-store-current-options-version ()
  (when (not (equal (ecb-option-get-value 'ecb-options-version
                                          'saved-value)
                    ecb-version))
    (setq ecb-old-ecb-version (ecb-option-get-value 'ecb-options-version
                                                    'saved-value))
    (ecb-customize-save-variable 'ecb-options-version ecb-version)))
  

(defvar ecb-not-compatible-options nil
  "This variable is only set by `ecb-check-not-compatible-options'! It is an
alist with car is the symbol of an incompatible option and the cdr is the not
compatible value of this option.
This option is evaluated by `ecb-upgrade-not-compatible-options' and
`ecb-display-upgraded-options'.")


(defvar ecb-all-options nil)

(defun ecb-get-all-ecb-options ()
  (or ecb-all-options
      (mapatoms
       (lambda (symbol)
         (when (and (string-match "ecb-" (symbol-name symbol))
                    (get symbol 'custom-type))
           (setq ecb-all-options (cons symbol ecb-all-options)))))))

(defun ecb-check-not-compatible-options ()
  "Check for all ECB-options if their current value is compatible to the
defined type. If not store it in `ecb-not-compatible-options'."
  (setq ecb-not-compatible-options nil)

  ;; get all options of ECB
;;   (let ((ecb-options nil))
  (ecb-get-all-ecb-options)
  
  ;; check if all current values of ECB options match their types. Add not
  ;; matching options to `ecb-not-compatible-options'.
  (dolist (option ecb-all-options)
    (require 'cus-edit)
    (unless (ecb-option-compatible-p option)
      (setq ecb-not-compatible-options
            (cons (cons option
                        (symbol-value option))
                  ecb-not-compatible-options)))))

(defun ecb-upgrade-not-compatible-options ()
  "Upgrade all not anymore compatible options of `ecb-not-compatible-options'.
If such an option is contained in `ecb-upgradable-option-alist' then try to
perform a special upgrade with `ecb-option-upgrade'. If no special upgrade is
done then the option is reset to the default-value of current ECB-version."
  ;; For every not compatible option perform an upgrade
  (let ((is-not-a-downgrade
         (not (ecb-package-version-list<
               (ecb-package-version-str2list ecb-version)
               (ecb-package-version-str2list ecb-options-version)))))
    (dolist (option ecb-not-compatible-options)
      ;; if the incompatible option is not upgraded by `ecb-option-upgrade'
      ;; then we reset it to the standard-value of current ECB-version. If we
      ;; make a downgrade we always reset to the default!
      (let ((upgrade-result
             (if is-not-a-downgrade (ecb-option-upgrade (car option)))))
        (when (or (null upgrade-result) ;; no upgrade necessary or allowed
                  ;; the upgrade has been tried but has failed.
                  (equal (car upgrade-result) 'ecb-no-upgrade-conversion))
          (ecb-option-set-default (car option)))))
    ;; Now we store the version of the options
    (ecb-store-current-options-version)))

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
  (when (not (ecb-package-version-list<
              (ecb-package-version-str2list ecb-version)
              (ecb-package-version-str2list ecb-options-version)))
    (ecb-get-all-ecb-options)
    (dolist (option ecb-upgradable-option-alist)
      ;; perform only an upgrade if the option is not contained in
      ;; `ecb-not-compatible-options' too because then ECB has auto.
      ;; recognized that this option is not compatible and the upgrade (or
      ;; reset) is performed by `ecb-upgrade-not-compatible-options'!
      (when (not (assoc (car option) ecb-not-compatible-options))
        (let ((new-value-list (ecb-option-upgrade (car option))))
          ;; if an upgrade was tried then store the option in
          ;; `ecb-renamed-options'.
          (when (and new-value-list
                     (not (equal (car new-value-list)
                                 'ecb-no-upgrade-conversion)))
            (setq ecb-renamed-options
                  (cons (list (car option)
                              (ecb-option-get-value (car option) 'saved-value)
                              (car (cdr option))
                              (car new-value-list))
                        ecb-renamed-options))))))
    ;; Now we store the version of the options
    (ecb-store-current-options-version)))


(defun ecb-display-upgraded-options ()
  "Display a message-buffer which options have been upgraded or reset."
  (interactive)
  (if (or ecb-not-compatible-options ecb-renamed-options)
      (progn
        (with-output-to-temp-buffer "*ECB upgraded options*"
          (when (and (or ecb-not-compatible-options ecb-renamed-options)
                     (not (ecb-custom-file-writeable-p)))
            (princ "Emacs can not save the upgraded options because the needed file\n")
            (princ (if (ecb-custom-file)
                       (concat (ecb-custom-file) " is not writeable!")
                     "does not exist!"))
            (princ "\nPlease ensure that the new values will be stored!\n\n"))
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
          (princ "For a list of the most important NEWS call `ecb-display-news-for-upgrade'!\n\n")
          (print-help-return-message))
        t)
    (message "There are no incompatible or renamed options!")
    nil))

(defvar ecb-news-for-upgrade-displayed nil)

(defun ecb-display-news-for-upgrade (&optional full-news)
  "Display the most important NEWS after an ECB-upgrade.
If you call this function but no ECB-upgrade has been performed before
starting ECB then nothing is display unless FULL-NEWS is not nil.

If FULL-NEWS is not nil then the NEWS-file is displayed in another window."
  (interactive "P")
  (if full-news
      (find-file-other-window (concat ecb-ecb-dir "NEWS"))
    (if (and ecb-old-ecb-version
             (or (not ecb-news-for-upgrade-displayed)
                 (interactive-p)))
        (progn
          (with-output-to-temp-buffer "*News for the new ECB-version*"
            (princ (format "You have upgraded ECB from version %s to %s.\n\n"
                           ecb-old-ecb-version ecb-version))
            (princ "Here are the most important NEWS:\n\n")
            (mapc (function (lambda (version)
                              (if (ecb-package-version-list<
                                   (ecb-package-version-str2list ecb-old-ecb-version)
                                   (ecb-package-version-str2list (car version)))
                                  (dolist (news (cdr version))
                                    (princ (concat "* " news "\n"))))))
                  ecb-upgrade-news)
            (princ "\nFor more details see the attached NEWS-file."))
          ;; We want this being displayed only once
          (setq ecb-news-for-upgrade-displayed t))
      (message "There are no NEWS to display."))))
    
  
(defun ecb-upgrade-options ()
  "Check for all ECB-options if the current value is compatible to the type.
If not upgrade it to the new type or reset it to the default-value of current
ECB. Try also to upgrade renamed options. Displays all upgraded or reset
options with their old \(before the upgrade/reset) and new values."
  (interactive)
  (ecb-check-not-compatible-options)
  (ecb-upgrade-not-compatible-options)
  (ecb-upgrade-renamed-options)
  (ecb-display-upgraded-options))

;; ----------------------------------------------------------------------
;; all needs for the requirements check
;; ----------------------------------------------------------------------

(defconst ecb-required-semantic-version-min '(1 4 2 0))
(defconst ecb-required-semantic-version-max '(1 4 3 9))
(defconst ecb-required-eieio-version-min '(0 17 2 0))
(defconst ecb-required-eieio-version-max '(0 17 3 9))
(defconst ecb-required-speedbar-version-min '(0 14 1 1))
(defconst ecb-required-speedbar-version-max '(0 15 3 9))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Currently we support the
;; cedet-library by hacking ecb-check-requirements see the TODO there). But
;; when cedet is stable (or a stable beta ;-) then we should add here a
;; cedet-required-version-min|max etc....

(defvar ecb-all-requirements-available nil)

(defun ecb-check-requirements (&optional just-check)
  "Ensure that we use the right `semantic-version' and right `eieio-version'
and offer to download them if not installed.

If JUST-CHECK is not nil then
1. Only the version check is done but no download
2. It returns nil if all requirements are correct, otherwise a list which
   contains the symbol 'semantic if `semantic-version' is incorrect and 'eieio
   if `eieio-version' is incorrect.

If called in non-interactive mode \(e.g. in batch-mode) then JUST-CHECK is
always true."
  (when (and (or (not (boundp 'ecb-version-check)) ecb-version-check)
             (not ecb-all-requirements-available)
             ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: This allows ECB to
             ;; work with current cedet 1.0. Remove this hack when we fully
             ;; support the cedet-library in upgrading.
             (not (boundp 'cedet-version)))
    (let ((semantic-required-version-str-min (ecb-package-version-list2str
                                              ecb-required-semantic-version-min))
          (semantic-required-version-str-max (ecb-package-version-list2str
                                              ecb-required-semantic-version-max))
          (eieio-required-version-str-min (ecb-package-version-list2str
                                           ecb-required-eieio-version-min))
          (eieio-required-version-str-max (ecb-package-version-list2str
                                           ecb-required-eieio-version-max))
          (speedbar-required-version-str-min (ecb-package-version-list2str
                                              ecb-required-speedbar-version-min))
          (speedbar-required-version-str-max (ecb-package-version-list2str
                                              ecb-required-speedbar-version-max))
          (failed-result)
          (version-error nil)
          (semantic-dir nil)
          (semantic-state nil)
          (semantic-installed-version-str nil)
          (eieio-dir nil)
          (eieio-state nil)
          (eieio-installed-version-str nil)
          (speedbar-dir nil)
          (speedbar-state nil)
          (speedbar-installed-version-str nil))
      ;; check if semantic version is correct
      (when (or (not (boundp 'semantic-version))
                (ecb-package-version-list<
                 (ecb-package-version-str2list semantic-version)
                 ecb-required-semantic-version-min)
                (ecb-package-version-list<
                 ecb-required-semantic-version-max
                 (ecb-package-version-str2list semantic-version)))
        (setq version-error (concat "semantic ["
                                    semantic-required-version-str-min
                                    ", "
                                    semantic-required-version-str-max
                                    "]"))
        (setq failed-result (cons 'semantic failed-result)))
      ;; check if eieio version is correct
      (when (or (not (boundp 'eieio-version))
                (ecb-package-version-list<
                 (ecb-package-version-str2list eieio-version)
                 ecb-required-eieio-version-min)
                (ecb-package-version-list<
                 ecb-required-eieio-version-max
                 (ecb-package-version-str2list eieio-version)))
        (setq version-error
              (concat version-error (if version-error " and ")
                      "eieio ["
                      eieio-required-version-str-min
                      ", "
                      eieio-required-version-str-max
                      "]"))
        (setq failed-result (cons 'eieio failed-result)))
      ;; check if speedbar version is correct
      (when (or (not (boundp 'speedbar-version))
                (ecb-package-version-list<
                 (ecb-package-version-str2list speedbar-version)
                 ecb-required-speedbar-version-min)
                (ecb-package-version-list<
                 ecb-required-speedbar-version-max
                 (ecb-package-version-str2list speedbar-version)))
        (setq version-error
              (concat version-error (if version-error " and ")
                      "speedbar ["
                      speedbar-required-version-str-min
                      ", "
                      speedbar-required-version-str-max
                      "]"))
        (setq failed-result (cons 'speedbar failed-result)))
      (if (null failed-result)
          ;; this is the only place where this variable is set
          (setq ecb-all-requirements-available t))
      (if (or just-check (ecb-noninteractive))
          failed-result
        (when failed-result
          (when ecb-regular-xemacs-package-p
            (with-output-to-temp-buffer "*ECB downloading and installing*"
              (princ "Current ECB is installed as regular XEmacs package and not with the\n")
              (princ "archive available at the ECB-website. So you should use the package-manager\n")
              (princ "of XEmacs to get the required versions of semantic, eieio, speedbar and to\n")
              (princ "install them also as regular XEmacs-packages! If you now proceed installing\n")
              (princ "from the CEDET-website then the new versions will NOT be installed as\n")
              (princ "regular XEmacs-package(s) but as \"flat\" package(s) parallel to the current\n")
              (princ "ECB directory!\n\n")))
          (if (not (ecb-confirm (format "ECB requires %s. Download now? "
                                        version-error)))
              (ecb-error "ECB can only be used with %s! Sorry!"
                         version-error)
            (message nil)

            ;; try to download semantic and set state and install dir
            (if (not (member 'semantic failed-result))
                (setq semantic-state "Correct version already loaded!")
              (setq semantic-installed-version-str
                    (ecb-package-get-matching-versions-str
                     "semantic" ecb-cedet-url
                     ecb-required-semantic-version-min
                     ecb-required-semantic-version-max))
              (setq semantic-dir
                    (ecb-package-download "semantic"
                                          semantic-installed-version-str
                                          ecb-cedet-url))
              (setq semantic-state (if (and semantic-dir
                                            semantic-installed-version-str)
                                       (concat "Installed "
                                               semantic-installed-version-str
                                               " in " semantic-dir)
                                     "Download- or installing-failure!")))

            ;; try to download eieio and set state and install dir
            (if (not (member 'eieio failed-result))
                (setq eieio-state "Correct version already loaded!")
              (setq eieio-installed-version-str
                    (ecb-package-get-matching-versions-str
                     "eieio" ecb-cedet-url
                     ecb-required-eieio-version-min
                     ecb-required-eieio-version-max))
              (setq eieio-dir
                    (ecb-package-download "eieio"
                                          eieio-installed-version-str
                                          ecb-cedet-url))
              (setq eieio-state (if (and eieio-dir
                                         eieio-installed-version-str)
                                    (concat "Installed "
                                            eieio-installed-version-str
                                            " in " eieio-dir)
                                  "Download- or installing-failure!")))

            ;; try to download speedbar and set state and install dir
            (if (not (member 'speedbar failed-result))
                (setq speedbar-state "Correct version already loaded!")
              (setq speedbar-installed-version-str
                    (ecb-package-get-matching-versions-str
                     "speedbar" ecb-cedet-url
                     ecb-required-speedbar-version-min
                     ecb-required-speedbar-version-max))
              (setq speedbar-dir
                    (ecb-package-download "speedbar"
                                          speedbar-installed-version-str
                                          ecb-cedet-url))
              (setq speedbar-state (if (and speedbar-dir
                                            speedbar-installed-version-str)
                                       (concat "Installed "
                                               speedbar-installed-version-str
                                               " in " speedbar-dir)
                                     "Download- or installing-failure!")))

            ;; display the success
            (with-output-to-temp-buffer "*ECB downloading and installing*"
              (princ "Current state of the required packages semantic and eieio:\n\n")
              (princ (concat "- semantic author-version must be ["
                             semantic-required-version-str-min
                             ", "
                             semantic-required-version-str-max "]:\n  "))
              (princ semantic-state)
              (princ "\n")
              (princ (concat "- eieio author-version must be ["
                             eieio-required-version-str-min
                             ", "
                             eieio-required-version-str-max "]:\n  "))
              (princ eieio-state)
              (princ "\n")
              (princ (concat "- speedbar author-version must be ["
                             speedbar-required-version-str-min
                             ", "
                             speedbar-required-version-str-max "]:\n  "))
              (princ speedbar-state)
              (princ "\n\n")
              (princ "After adding the new directories to your `load-path' and then restarting\n")
              (princ "Emacs and ECB the new packages will be used.\n\n")
              (princ "\n\n"))
            (ecb-error "Please restart Emacs with the required packages!")))))))

;; ----------------------------------------------------------------------
;; all needs for the package download
;; ----------------------------------------------------------------------

(defgroup ecb-download nil
  "Settings for downloading and installing a new ECB from within ECB."
  :group 'ecb
  :prefix "ecb-")

(defcustom ecb-download-url "http://ftp1.sourceforge.net/ecb/"
  "*URL where download-able ECB-versions are located.
The ECB-archive-file \(e.g. ecb-1.70.tar.gz\) will be appended to this URL and
`ecb-download-ecb' will try to download this archive.

Note: Normally this URL should never change but who knows..."
  :group 'ecb-download
  :type 'string)

(defcustom ecb-download-package-version-type 1
  "*Version type ECB is allowed to download for upgrading.
If you want to upgrade to a newer ECB-version via `ecb-download-ecb' or if you
must upgrade to newer semantic- and/or eieio-versions \(because ECB requires
these newer versions) then this option specifies which version-types are
allowed. ECB checks on the download-sites of ECB/semantic/eieio which versions
are currently available and then downloads always the latest version matching
the specified type:

2: Gets the newest version of all stable versions available.
1: Gets the newest version of all stable and beta versions available.
0: Gets the newest version of all stable, beta and alpha versions
   available.
-1: Ask before downloading in the minibuffer for a version \(TAB-completion
    of all available versions is possible).

So, 2 means stable, 1 means stable and betas, 0 means stable, betas and alphas
and -1 means ask the user for a version.

Per default stable and beta-versions are allowed \(value 1). This comes also
from the fact, that currently speedbar is only available in beta versions
which are very stable.

But all versions must match the restrictions of the specified min- and
max-versions of the required packages. For this see the file README!"
  :group 'ecb-download
  :type '(radio (const :tag "Only stable versions"
                       :value 2)
                (const :tag "Allow beta versions"
                       :value 1)
                (const :tag "Allow alpha versions"
                       :value 0)
                (const :tag "Ask for version"
                       :value -1)))

(defcustom ecb-download-install-parent-dir (or (and (file-writable-p ecb-ecb-parent-dir)
                                                    ecb-ecb-parent-dir)
                                               "~")
  "*Parent directory where downloaded packages are installed.
ECB installs a downloaded package in this directory, i.e. the downloaded
archive X.tar.gz will be extracted in this directory so afterwards this
directory contains a new subdirectory X which contains the downloaded package.

This directory must be write-able!"
  :group 'ecb-download
  :type 'directory)

(defcustom ecb-download-delete-archive 'always
  "*Should the downloaded archive be deleted after success or failure.
Possible values are:
- only-after-success: Archive is only deleted after successful installation
  but not if a failure occurs during the installation process.
- always: Archive is also deleted if an error occurs.
- nil: Archive will never be deleted."
  :group 'ecb-download
  :type '(choice :tag "Delete archive" :menu-tag "Delete archive"
                 (const :tag "After successful installation" only-after-success)
                 (const :tag "Always" always)
                 (const :tag "Never" nil)))

(defcustom ecb-cedet-url "http://ftp1.sourceforge.net/cedet/"
  "*URL where download-able CEDET-libraries are located.
ECB will try to download \(if necessary) required versions of the libraries
needed by ECB: The CEDET libraries semantic, eieio and speedbar.

Note: Normally this URL should never change but who knows..."
  :group 'ecb-download
  :type 'string)

(defconst ecb-download-buffername " *ecb-download*")

;; Klaus: Arrghhhhhhhhhhhhhhh... the cygwin version of tar does not accept
;; args in windows-style file-format :-( Therefore we convert it with cygpath.
;; Cause of the need of wget we can assume the the user has cygwin installed!
(defmacro ecb-create-shell-argument (arg)
  `(if (eq system-type 'windows-nt)
       (progn
         (require 'executable)
         (if (executable-find "cygpath.exe")
             ;; if bash is used as shell-file-name then the command must
             ;; not contain newlines!
             (ecb-trim
              (ecb-subst-char-in-string ?\n 32
                                        (shell-command-to-string
                                         (concat "cygpath -u " ,arg))))
           (ecb-error "Cannot find the cygpath utility!")))
     ,arg))


(defun ecb-package-version-str2list (ver-str)
  "Convert the version-str VER-STR to the internal version-list format with
the following elements of the version-list:
1. Major-version
2. Minor-version
3. 0 = alpha, 1 = beta, 2 = nothing \(e.g. \"1.4\"), 3 = . \(e.g. \"1.4.3\"
4. Subversion after the alpha, beta or .

Return nil if ver-str has not the required syntax:
<major>.<minor>\[.|beta|alpha]\[<sub-stable/beta/alpha-version>]"
  (let ((str ver-str))
    (if (string-match "^\\([0-9]+\\)\\.\\([0-9]+\\)\\(beta\\|alpha\\|\\.\\)?\\([0-9]+\\)?$" str)
        (list (string-to-number (match-string 1 str))
              (string-to-number (match-string 2 str))
              (if (string= (match-string 3 str) "alpha")
                  0
                (if (string= (match-string 3 str) "beta")
                    1
                  (if (string= (match-string 3 str) ".")
                      3
                    2)))
              (if (match-string 4 str)
                  (string-to-number (match-string 4 str))
                0)))))

(defun ecb-package-version-list< (ver1 ver2)
  "Return non-nil if VER1 is less than VER2."
  (let ((v1-0 (nth 0 ver1))
	(v1-1 (nth 1 ver1))
	(v1-2 (nth 2 ver1))
	(v1-3 (nth 3 ver1))
	;; v2
	(v2-0 (nth 0 ver2))
	(v2-1 (nth 1 ver2))
	(v2-2 (nth 2 ver2))
	(v2-3 (nth 3 ver2)))
    (or (< v1-0 v2-0)
        (and (= v1-0 v2-0)
             (< v1-1 v2-1))
        (and (= v1-0 v2-0)
             (= v1-1 v2-1)
             (< v1-2 v2-2))
        (and (= v1-0 v2-0)
             (= v1-1 v2-1)
             (= v1-2 v2-2)
             (< v1-3 v2-3)))))

(defun ecb-package-version-string< (ver1-str ver2-str)
  "Return non nil if VER-STR1 is logically less then VER-STR2."
  (let ((ver1 (ecb-package-version-str2list ver1-str))
        (ver2 (ecb-package-version-str2list ver2-str)))
    (ecb-package-version-list< ver1 ver2)))

(defun ecb-package-version-list2str (ver)
  "Complementary function to `ecb-package-version-str2list'."
  (concat (number-to-string (nth 0 ver))
          "."
          (number-to-string (nth 1 ver))
          (cond ((= (nth 2 ver) 0)
                 "alpha")
                ((= (nth 2 ver) 1)
                 "beta")
                ((= (nth 2 ver) 3)
                 ".")
                (t ""))
          (if (not (= (nth 2 ver) 2))
              (number-to-string (nth 3 ver))
            "")))

(defun ecb-package-get-matching-versions-str (package package-url
                                                      min-list max-list)
  "Get from PACKAGE-URL all available version-numbers of PACKAGE. Remove all
version-numbers which are not between MIN-LIST and MAX-LIST and which do not
match the setting in `ecb-download-package-version-type'.

If `ecb-download-package-version-type' = -1 then let the user choose a version
of the remaining version-numbers \(default is the newest version) otherwise
return autom. the newest version-number as version-string."
  (let* ((full-version-list (ecb-package-get-available-versions
                             package package-url))
         (sorted-matching-ver-list
          (sort (delete nil
                        (mapcar (function
                                 (lambda (ver-str)
                                   (let ((ver-list
                                          (ecb-package-version-str2list ver-str)))
                                     (if (and ver-list
                                              (not (ecb-package-version-list<
                                                    ver-list
                                                    min-list))
                                              (not (ecb-package-version-list<
                                                    max-list
                                                    ver-list))
                                              (<= ecb-download-package-version-type
                                                  (nth 2 ver-list)))
                                         ver-list))))
                                full-version-list))
                'ecb-package-version-list<))
         (sorted-matching-ver-str
          (nreverse (mapcar (function (lambda (x)
                                        (list (ecb-package-version-list2str x)
                                              t)))
                            sorted-matching-ver-list))))
    (if sorted-matching-ver-str
        (if (= ecb-download-package-version-type -1)
            (completing-read "Choose a version: "
                             sorted-matching-ver-str
                             nil t nil nil (caar sorted-matching-ver-str))
          (caar sorted-matching-ver-str))
      (ecb-error "No matching versions available for %s at %s."
                 package package-url))))



(defun ecb-download-ecb ()
  "Download ECB from the ECB-website and install it.
For this the option `ecb-download-url' must be set correct, whereas the
default value of this option should always be correct.

If `ecb-download-package-version-type' is set to -1 \(means asking for a
version) then you will be ask in the minibuffer for the version to download.
Otherwise ECB downloads autom. the latest version available for the type
specified in `ecb-download-package-version-type'. If no newer version than the
current one is available no download will be done.

For details about downloading and what requirements must be satisfied see
function `ecb-package-download' and option `ecb-download-package-version-type'!

After successful downloading the new ECB will be installed in a subdirectory
of `ecb-download-install-parent-dir'. After adding this new subdirectory to
`load-path' and restarting Emacs the new ECB version can be activated by
`ecb-activate'.

If current running ECB is installed as regular XEmacs-package and not with the
archive available at the ECB website then this function asks for proceeding!"
  (interactive)
  (when (or (not ecb-regular-xemacs-package-p)
            (ecb-package-display-xemacs-package-info "ecb"))
    (ecb-package-download-ecb/semantic "ecb"
                                       ecb-version
                                       ecb-download-url)))

(defun ecb-download-semantic ()
  "Download semantic from the semantic-website and install it.
For this the variable `ecb-cedet-url' must be set correct, whereas the default
value of this variable should always be correct.

If `ecb-download-package-version-type' is set to -1 \(means asking for a
version) then you will be ask in the minibuffer for the version to download.
Otherwise ECB downloads autom. the latest version available for the type
specified in `ecb-download-package-version-type'. If no newer version than the
current one is available no download will be done.

For details about downloading and what requirements must be satisfied see
function `ecb-package-download' and option `ecb-download-package-version-type'!

After successful downloading the new semantic will be installed in a
subdirectory of `ecb-download-install-parent-dir'. After adding this new
subdirectory to `load-path' and restarting Emacs the new semantic version is
loaded and is used after next start of ECB.

If current running semantic is installed as regular XEmacs-package and not
with the archive available at the semantic website then this function asks for
proceeding!"
  (interactive)
  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Remove this test, and offer to
  ;; download full cedet suite instead of just semantic when cedet 1.0 is
  ;; stable!
  (if (boundp 'cedet-version)
      (ecb-error "When new cedet 1.0 is loaded then no semantic upgrade possible!")
    (when (or (not ecb-semantic-regular-xemacs-package-p)
              (ecb-package-display-xemacs-package-info "semantic"))
      (ecb-package-download-ecb/semantic "semantic"
                                         semantic-version
                                         ecb-cedet-url))))

(defun ecb-package-display-xemacs-package-info (package)
  "Displays a warning if PACKAGE is a standard xemacs-package and ask if to
proceed with downloading. Return not nil if proceeding."
  (with-output-to-temp-buffer "*ECB downloading and installing*"
    (princ (concat "Current "
                   package
                   " is installed as regular XEmacs package and not with the\n"))
    (princ (concat "archive available at the "
                   package
                   "-website. So you should use the package-manager\n"))
    (princ (concat "of XEmacs to get the latest version of "
                   package
                   "! If you proceed installing from\n"))
    (princ (concat "the "
                   package
                   "-website then the new "
                   package
                   " is NOT installed as regular XEmacs-\n"))
    (princ "package but as \"flat\" package into `ecb-download-package-version-type'!\n\n"))
  (ecb-confirm (concat "Do you want to proceed installing from the "
                       package
                       "-website? ")))


(defun ecb-package-download-ecb/semantic (package curr-version url)
  "Download PACKAGE from URL. CURR-VERSION must be the current version of
current active version of PACKAGE."
  (let ((ver (ecb-package-get-matching-versions-str
              package url
              (if (= ecb-download-package-version-type -1)
                  '(0 0 0 0) ;; smallest possible version-number
                (ecb-package-version-str2list curr-version))
              '(100 99 3 99))) ;; this version-number should be the biggest;-) 
        (install-dir nil))
    (if (string= ver curr-version)
        (ecb-error "You tried to download an already installed version %s - Stop!"
                   ver))
    (setq install-dir (ecb-package-download package ver url))
    (when install-dir
      (message "New %s successfully installed!" package)
      (with-output-to-temp-buffer "*ECB downloading and installing*"
        (princ (concat "New "
                       package
                       " version is installed.\n\n"))
        (princ (concat "+ Current " package ": "
                       (file-name-directory (locate-library package))
                       "\n"))
        (princ (concat "+ New " package ": " install-dir))
        (princ "\n\n")
        (princ "After replacing the current directory with the new one in your `load-path'\n")
        (princ (concat "and then restarting Emacs the new version of "
                       package
                       " is loaded."))
        (when (string= package "ecb")
          (princ "\n\nIf the value of `ecb-auto-compatibility-check' is not nil then the new version\n")
          (princ "checks at start-time if there are incompatible options! Please read the\n")
          (princ "documentation of this option!"))
        (princ "\n\n")))))


(defun ecb-package-download (package version url)
  "Download VERSION of PACKAGE from URL and install it in a new subdirectory
of `ecb-download-install-parent-dir'. If no failure occurs during this process
the full path of the directory is returned in which the new package is
installed. Otherwise an error is reported.

For correct downloading and installing the utilities \"wget\", \"tar\" and
\"gzip\" are needed which are available for Unix and also for windows with
cygwin. All utilities must reside in your PATH!

If you are behind a firewall and you have to use a proxy you maybe need the
following wget-configuration in your \"~/.wgetrc\"-file:

   # Define your proxies \(where 8080 and 8081 are examples for the portnumbers)
   http_proxy = http://your.proxy.com:8080
   ftp_proxy  = http://your.ftpproxy.com:8081
     
   # If you do not want to use proxy at all, set this to off.
   use_proxy = on

ECB will try to download the file: \"<URL><PACKAGE>-<VERSION>.tar.gz\".
Example: For PACKAGE = \"ecb\", VERSION = \"1.90\" and URL =
\"http://ftp1.sourceforge.net/ecb/\" the download-file would be
\"http://ftp1.sourceforge.net/ecb/ecb-1.90.tar.gz\".

After successful downloading the new package version will be installed in a
new subdirectory of `ecb-download-install-parent-dir'. After adding this new
subdirectory to `load-path' and restarting Emacs the new package version can be
activated."
  (let* ((download-install-dir (file-name-as-directory
                                ecb-download-install-parent-dir))
         (downloaded-filename (concat download-install-dir
                                      package "-download.tar.gz"))
         (success t)
         process-result)

    ;; a first simple check if the new version is already installed
    
    (if (not (or (not (file-directory-p (concat download-install-dir
                                                package "-"
                                                version)))
                 (ecb-confirm
                  (format "%s %s seems to be already installed in directory %s! Force? "
                          package version
                          (concat package "-" version)))))
        ;; we can go back with this install dir
        (concat download-install-dir package "-" version)
      
      ;; cleaning up

      (if (get-buffer ecb-download-buffername)
          (kill-buffer ecb-download-buffername))
      (ecb-delete-file downloaded-filename)
      (ecb-delete-file (file-name-sans-extension downloaded-filename))

      ;; checking if all necessary tools are available

      ;; Emacs 20.X does not autoload executable-find :-(
      (require 'executable)
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

        (ecb-working-status-call-process
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

        ;; unpacking new package

        (when success
          (message "Uncompressing new %s...done" package)
          (message "Unpacking new %s..." package)
          (setq process-result
                (shell-command-to-string
                 (concat "tar -C "
                         (ecb-create-shell-argument download-install-dir)
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
          (ecb-delete-file downloaded-filename))
    
        ;; now we return if we had success or not
        (if success
            (concat download-install-dir package "-" version)
          (ecb-error "Downloading or installing failure for %s %s" package version))))))

(defun ecb-package-get-available-versions (package package-url)
  "Get a list of available versions of PACKAGE download-able at PACKAGE-URL.
This is done with the utility \"wget\", so please see `ecb-package-download'
for details about using \"wget\"."
  (let ((downloaded-filename (concat ecb-temp-dir "package-index.html"))
        (success t)
        (version-list nil)
        process-result)

    (require 'executable)
    (if (not (executable-find
              (if (eq system-type 'windows-nt) "wget.exe" "wget")))
        (ecb-error
         (concat "Cannot find wget. This utility is needed "
                 "to get available-package-list."))

      ;; OK, now we begin....

      (let (
            (ecb-window-sync nil)
            (kill-buffer-hook nil)
            (semantic-after-toplevel-cache-change-hook nil)
            (semantic-after-partial-cache-change-hook nil)
            (auto-mode-alist nil)
            )
        (if (get-buffer ecb-download-buffername)
            (kill-buffer ecb-download-buffername))
        (ecb-delete-file downloaded-filename))
        
      ;; Downloading with working-display

      (ecb-working-status-call-process
       0.1
       (concat "Getting list of available versions of package " package)
       "done"
       (if (eq system-type 'windows-nt)
           "wget.exe"
         "wget")
       nil
       ecb-download-buffername
       nil
       "-O"
       downloaded-filename
       package-url)

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
          (princ (format "Checking available versions for %s has failed cause of the following\nwget-failure:"
                         package))
          (princ "\n")
          (princ "______________________________________________________________________________\n\n")
          (princ process-result)
          (princ "\n______________________________________________________________________________")
          (princ "\n\n")
          (princ "Please check the wget configuration in \"~/.wgetrc\" and also the value\n")
          (princ (format "of the option %s." (if (string= package "ecb")
                                                 "`ecb-download-url'"
                                               "`ecb-cedet-url'")))
          (princ " ECB has tried to get informations from\nthe following URL:\n\n")
          (princ (concat "   " package-url))
          (princ "\n\n")
          (princ "Maybe this URL does not exist...please check this!\n\n")))
;;       (princ "______________________________________________________________________________\n\n")
;;           (princ process-result)
;;           (princ "\n______________________________________________________________________________")
;;           (princ "\n\n")))
      (kill-buffer ecb-download-buffername)

      ;; getting the list from downloaded-filename.

      (when success
        (let (
              (ecb-window-sync nil)
              (kill-buffer-hook nil)
              (semantic-after-toplevel-cache-change-hook nil)
              (semantic-after-partial-cache-change-hook nil)
              (auto-mode-alist nil)
              )
          (save-excursion
            (set-buffer (find-file-noselect downloaded-filename t t))
            (goto-char (point-min))
            (while (re-search-forward
                    (concat "<A HREF=\""
                            package
                            "-\\([^<>]+\\)\\.tar\\.gz\">")
                    nil t)
              (add-to-list 'version-list (match-string 1)))
            (kill-buffer (current-buffer)))))
        
      ;; maybe cleaning up
      (let (
            (ecb-window-sync nil)
            (kill-buffer-hook nil)
            (semantic-after-toplevel-cache-change-hook nil)
            (semantic-after-partial-cache-change-hook nil)
            (auto-mode-alist nil)
            )
        (if (get-buffer ecb-download-buffername)
            (kill-buffer ecb-download-buffername))
        (ecb-delete-file downloaded-filename))

      ;; now we return the version-list
      version-list)))


(silentcomp-provide 'ecb-upgrade)

;;; ecb-upgrade.el ends here
