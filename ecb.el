;;; ecb.el --- a code browser for Emacs

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
;; Created: 2000

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

;; $Id: ecb.el,v 1.331 2003/09/05 07:27:34 berndl Exp $

;;; Commentary:
;;
;; ECB stands for "Emacs Code Browser" and is a source code browser for
;; (X)Emacs. It is a global minor-mode which displays a couple of windows that
;; can be used to browse directories, files and file-contents like methods and
;; variables. It supports source-code parsing for semantic-supported languages
;; like Java, C, C++, Elisp and Scheme as well as for source-types supported
;; "only" by imenu or etags (e.g. perl, TeX, LaTeX etc.).

;;; Installation
;;
;; To use the Emacs code browser add the ECB files to your load path and add the
;; following line to your .emacs file:
;;
;; If you want autoloading ECB after first start:
;;
;;    (require 'ecb-autoloads)
;;
;; or if you want loading the complete ECB:
;;
;;    (require 'ecb)
;;
;; Optional: You can byte-compile ECB with `ecb-byte-compile' after the
;;           ECB-package is loaded


;;; Requirements
;;
;; - Semantic, author-version between 1.4 and 1.4.9
;;   (http://cedet.sourceforge.net/semantic.shtml).
;; - Eieio, author-version between 0.17 and 0.17.9
;;   (http://cedet.sourceforge.net/eieio.shtml).
;; - speedbar, author version 0.14beta1 or higher
;;   (http://cedet.sourceforge.net/speedbar.shtml)
;; - Optional: If Java code is edited the ECB works best when the JDEE package
;;   (http://sunsite.auc.dk/jde) is installed.


;;; Activation
;;
;; ECB is either activated and started by calling
;;    M-x ecb-activate
;; or
;;    via the menu "Tools --> Start Code Browser (ECB)"
;;
;; ECB can also be (de)activated/toggled by M-x ecb-minor-mode.
;;
;; After activating ECB you should call `ecb-show-help' to get a detailed
;; description of what ECB offers to you and how to use ECB.


;;; Availability
;;
;; The latest version of the ECB is available at http://ecb.sourceforge.net

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code:

;; IMPORTANT: The version-number is auto-frobbed from the Makefile. Do not
;; change it here!
(defconst ecb-version "1.95.2"
  "Current ECB version.")

(eval-when-compile
  (require 'silentcomp))


;; We need this libraries already here if we miss some requirements and we
;; want to offer the user to download them.
(require 'ecb-upgrade)
(require 'ecb-util)


;; if we miss some of the requirements we offer the user to download and
;; install them if Emacs is started interactive or - in batch mode - we
;; report an error.
(let* ((semantic-load-ok (condition-case nil
                             (require 'semantic)
                           (error nil)))
       (eieio-load-ok (condition-case nil
                          (require 'eieio)
                        (error nil)))
       (speedbar-load-ok (condition-case nil
                          (require 'speedbar)
                        (error nil)))
       (missing-msg (concat (if (not semantic-load-ok) "the package semantic")
                            (when (not eieio-load-ok)
                              (concat (if (not semantic-load-ok) " and the ")
                                      "package eieio"))
                            (when (not speedbar-load-ok)
                              (concat (if (or (not semantic-load-ok)
                                              (not eieio-load-ok)) " and the ")
                                      "package speedbar")))))
  (when (not (and semantic-load-ok eieio-load-ok speedbar-load-ok))
    (if (ecb-noninteractive)
        (ecb-error "ECB is missing %s!" missing-msg)
      (ecb-check-requirements))))

;; If we are here we can load ECB because at least we have installed and
;; loaded all required packages. If they have correct version will be checked
;; at start- or byte-compile-time


(message "ECB %s uses semantic %s, eieio %s and speedbar %s." ecb-version
         (or (and (boundp 'semantic-version)
                  semantic-version)
             "<unknown version>")
         (or (and (boundp 'eieio-version)
                  eieio-version)
             "<unknown version>")
         (or (and (boundp 'speedbar-version)
                  speedbar-version)
             "<unknown version>"))

(require 'semantic-load)

;; rest of ecb loads
(require 'tree-buffer)
(require 'ecb-jde)
(require 'ecb-layout)
(require 'ecb-create-layout)
(require 'ecb-mode-line)
(require 'ecb-help)
(require 'ecb-navigate)
(require 'ecb-eshell)
(require 'ecb-compilation)
(require 'ecb-cycle)
(require 'ecb-face)
(require 'ecb-tod)
(require 'ecb-speedbar)
(require 'ecb-autogen)
;;(require 'ecb-profile)

;; various loads
(require 'easymenu)
(require 'assoc)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))




;; XEmacs
(silentcomp-defun redraw-modeline)
;; Emacs
(silentcomp-defun force-mode-line-update)

(silentcomp-defvar dired-directory)
(silentcomp-defun add-submenu)
(silentcomp-defun semanticdb-minor-mode-p)
(silentcomp-defun semanticdb-find-nonterminal-by-name)
(silentcomp-defun semanticdb-full-filename)
(silentcomp-defun ediff-cleanup-mess)
(silentcomp-defvar ediff-quit-hook)
(silentcomp-defvar tar-subfile-mode)
(silentcomp-defvar archive-subfile-mode)
(silentcomp-defun hs-minor-mode)
(silentcomp-defun hs-show-block)
(silentcomp-defun hs-hide-block)
(silentcomp-defvar hs-minor-mode)
(silentcomp-defvar hs-block-start-regexp)

;; ecb-speedbar is are first loaded if
;; ecb-use-speedbar-instead-native-tree-buffer is set to not nil or if
;; non-semantic-sources are opened and ecb-process-non-semantic-files is not
;; nil.
(silentcomp-defun ecb-speedbar-active-p)
(silentcomp-defun ecb-speedbar-deactivate)
(silentcomp-defvar ecb-speedbar-buffer-name)
(silentcomp-defun ecb-speedbar-update-contents)
(silentcomp-defun ecb-get-tags-for-non-semantic-files)
(silentcomp-defun ecb-create-non-semantic-tree)


;;====================================================
;; Variables
;;====================================================
(defvar ecb-tree-buffers nil
  "The names of the tree-buffers of ECB.")

(defvar ecb-selected-method-start 0
  "The currently selected method.")
(defvar ecb-path-selected-directory nil
  "Path to currently selected directory.")
(defvar ecb-path-selected-source nil
  "Path to currently selected source.")
(defvar ecb-major-mode-selected-source nil
  "Major-mode of currently selected source.")
(defvar ecb-selected-token nil
  "The currently selected Semantic token.")
(make-variable-buffer-local 'ecb-selected-token)
(defvar ecb-methods-root-node nil
  "Path to currently selected source.")
(defvar ecb-item-in-tree-buffer-selected nil
  "Only true if any item in any tree-buffer has been selected in recent
command.")

(defun ecb-initialize-internal-vars ()
  (setq ecb-tree-buffers nil
        ecb-selected-method-start 0
        ecb-path-selected-directory nil
        ecb-path-selected-source nil
        ecb-major-mode-selected-source nil
        ecb-selected-token nil
        ecb-methods-root-node nil))

(defvar ecb-minor-mode nil
  "Do not set this variable directly. Use `ecb-activate' and
`ecb-deactivate' or `ecb-minor-mode'.!")

(defvar ecb-activated-window-configuration nil
  "Window configuration used after the ECB is activated.")

;;====================================================
;; Customization
;;====================================================

(defgroup ecb nil
  "Emacs code browser."
  :group 'tools
  :prefix "ecb-")

(defgroup ecb-general nil
  "General settings for the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-directories nil
  "Settings for the directories buffer in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-sources nil
  "Settings for the source buffers in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-methods nil
  "Settings for the methods buffer in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-history nil
  "Settings for the history buffer in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-non-semantic nil
  "Settings for parsing and displaying non-semantic files."
  :group 'ecb
  :prefix "ecb-")

(defcustom ecb-use-recursive-edit nil
  "*Tell ECB to use a recursive edit.
If set then it can easily be deactivated by \(keyboard-escape-quit)."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-auto-activate nil
  "*Automatically startup ECB when Emacs starts up.
This should only be true if you always want to run `ecb-activate'."
  :group 'ecb-general
  :type
  'boolean)

(defcustom ecb-major-modes-activate 'none
  "*List of major-modes for which ECB should be activated or shown.
Do not mistake this option with `ecb-auto-activate'. The latter one is for
activating ECB after Emacs-startup \(even without opening a buffer) and this
one is for defining for which major-modes ECB should be activated if the mode
goes active!

The behavior is like follows: If a mode is contained in this option ECB is
activated after activating this mode \(if ECB was deactivated before) or the
ECB-windows are shown if ECB was already active but its windows were hidden.
In every case ECB is activated with visible ECB-windows afterwards!

For every major mode there can be specified an `ecb-layout-name':
- default: The value customized with `ecb-layout-name' is chosen.
- a string: ECB is activated with this layout-name. This changes the value of
  `ecb-layout-name' but only for current emacs-session!
But the layout is only changed if ECB was activated, if just the ECB-windows
were shown, the current layout is used!

There are two additional options:
- none: No major-modes should activate ECB automatically.
- a regexp: This means all major-modes which are not listed in
  `ecb-major-modes-deactivate' activate ECB except the `symbol-name' of
  `major-mode' matches this regexp. If this option is set then the default
  regexp excludes all Info- and customize-buffers because this buffers should
  not change anything in the ECB-activation state.

Any auto. activation is only done if the current-frame is unsplitted to avoid
changing unnecessarily or unintentionally the frame-layout if the user just
jumps between different windows."
  :group 'ecb-general
  :type '(radio :tag "Modes for activation"
                (const :tag "None" none)
                (regexp :tag "All except deactivated but not"
                        :value "\\(Info\\|custom\\)-mode")
                (repeat :tag "Mode list"
                        (cons (symbol :tag "Major-mode")
                              (choice :tag "Layout" :menu-tag "Layout"
                                      :value default
                                      (const :tag "Default" default)
                                      (string :tag "Layout-name"))))))

(defcustom ecb-major-modes-deactivate 'none
  "*List of major-modes for which ECB should be deactivated or hidden.
Specify if ECB should be deactivated or at least hidden if a `major-mode' is
active. For each `major-mode' there must be added an action what should be done:
- hide: ECB just hides all the ECB windows like with `ecb-hide-ecb-windows'.
- deactivate: ECB is completely deactivated after activating the `major-mode'.

There are two additional options:
- none: No major-modes should deactivate/hide ECB automatically.
- A cons for the meaning \"All except the activated modes of
  `ecb-major-modes-activate'\". The car of this cons can be:
   + hide-all-except-activated: All major-modes which are not listed in
     `ecb-major-modes-activate' hide the ECB-windows.
   + deactivate-all-except-activated: All major-modes which are not listed in
     `ecb-major-modes-activate' deactivate ECB.
  The cdr of this cons is a regexp: This means all major-modes which are not
  listed in `ecb-major-modes-activate' deactivate/hide ECB except the
  `symbol-name' of `major-mode' matches this regexp. If this option is set then
  the default regexp excludes all Info- and customize-buffers because this
  buffers should not change anything in the ECB-activation state.

If a `major-mode' is listed in `ecb-major-modes-activate' as well as in
`ecb-major-modes-deactivate' then ECB is activated!

Any auto. deactivation/hiding is only done if the edit-window of ECB is
unsplitted and point is in the edit-window to avoid changing unnecessarily or
unintentionally the frame-layout if the user just jumps between different
edit-windows, the tree-windows and the compile-window of ECB.
There is one exception from this rule: If the edit-window is splitted and one
window contains a dired-buffer and a \"ECB-deactivating\"-file is opened via
dired then opening this file deactivates ECB \(rsp. hides the ECB-windows)."
  :group 'ecb-general
  :type '(radio :tag "Modes for deactivation"
                (const :tag "None" none)
                (cons :tag "All except activated"
                      (choice :tag "Action"
                              :menu-tag "Action"
                              :value hide-all-except-activated
                              (const :tag "Hide ECB-windows"
                                     hide-all-except-activated)
                              (const :tag "Deactivate ECB"
                                     deactivate-all-except-activated))
                      (regexp :tag "But not for these modes"
                              :value "\\(Info\\|custom\\)-mode"))
                (repeat :tag "Mode list"
                        (cons (symbol :tag "Major-mode")
                              (choice :tag "Action" :menu-tag "Action"
                                      :value hide
                                      (const :tag "Just Hide" hide)
                                      (const :tag "Deactivate" deactivate))))))

(defcustom ecb-source-path nil
  "*Paths where to find code sources.
Each path can have an optional alias that is used as it's display name. If no
alias is set, the path is used as display name."
  :group 'ecb-directories
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (if (and ecb-minor-mode
			    (functionp 'ecb-update-directories-buffer))
		       (ecb-update-directories-buffer))))
  :type '(repeat (choice :tag "Display type"
                         :menu-tag "Display type"
			 (directory :tag "Path")
			 (list :tag "Path with alias"
			       (directory :tag "Path")
			       (string :tag "Alias")))))

(defcustom ecb-add-path-for-not-matching-files '(t . nil)
  "*Add path of a file to `ecb-source-path' if not already contained.
This is done during the auto. windows synchronization which happens if a file
is opened not via the file/directory-browser of ECB. In such a situation ECB
adds the path of the new file auto. to `ecb-source-path' at least temporally
for the current Emacs session. This option defines two things:
1. Should only the root-part \(which means for Unix-like systems always '/'
   and for windows-like systems the drive) of the new file be added as
   source-path to `ecb-source-path' or the whole directory-part?
2. Should this path be added for future sessions too?

The value of this option is a cons-cell where the car is a boolean for 1. and
the cdr is a boolean for 2.

A value of not nil for the car \(1.) is reasonably if a user often opens files
not via the ECB-browser which are not located in any of the paths of
`ecb-source-path' because then only one path for each drive \(windows) or the
root-path \(unix) is added to the directory buffer of ECB."
  :group 'ecb-directories
  :type '(cons (boolean :tag "Add only root path")
               (boolean :tag "Ask for saving for future sessions")))

(defvar ecb-source-path-functions nil
  "List of functions to call for finding sources.
Each time the function `ecb-update-directories-buffer' is called, the
functions in this variable will be evaluated. Such a function must return
either nil or a list of strings where each string is a path.")

(defcustom ecb-display-default-dir-after-start t
  "*Automatically display current default-directory after activating ECB.
If a file-buffer is displayed in the edit-window then ECB synchronizes its
tree-buffers to this file-buffer - at least if the option `ecb-window-sync' it
not nil. So for this situation `ecb-display-default-dir-after-start' takes no
effect but this option is for the case if no file-buffer is displayed in the
edit-window after startup:

If true then ECB selects autom. the current default-directory after activation
even if no file-buffer is displayed in the edit-window. This is useful if ECB
is autom. activated after startup of Emacs and Emacs is started without a
file-argument. So the directory from which the startup has performed is auto.
selected in the ECB-directories buffer and the ECB-sources buffer displays the
contents of this directory."
  :group 'ecb-directories
  :type 'boolean)

(defcustom ecb-show-sources-in-directories-buffer '("left7" "left13"
                                                    "left14" "left15")
  "*Show source files in directories buffer.
The value is either 'always or 'never or a list of layout-names for which
layouts sources should be displayed in the directories window."
  :group 'ecb-directories
  :type '(radio (const :tag "Always" :value always)
                (const :tag "Never" :value never)
                (repeat :tag "With these layouts"
                        (string :tag "Layout name"))))


(defcustom ecb-use-speedbar-instead-native-tree-buffer nil
  "*If true then uses speedbar for directories, sources or methods.
This means that speedbar is integrated in the ECB-frame and is displayed in
that window normally displaying the standard ECB-directories-buffer,
ECB-sources-buffer or ECB-methods-buffer.

This option takes effect in all layouts which contain either a directory
window, a sources window or a method window.

This option can have four valid values:
- nil: Do not use speedbar \(default)
- dir: Use speedbar instead of the standard directories-buffer
- source: Use speedbar instead of the standard sources-buffer
- method: Use speedbar instead of the standard methods-buffer

Note: For directories and sources a similar effect and usability is available
by setting this option to nil \(or 'method) and setting
`ecb-show-sources-in-directories-buffer' to not nil, because this combination
displays also directories and sources in one window.

`ecb-use-speedbar-instead-native-tree-buffer' is for people who like the
speedbar way handling directories and source-files or methods and want it in
conjunction with ECB."
  :group 'ecb-general
  :group 'ecb-directories
  :group 'ecb-sources
  :group 'ecb-methods
  :type '(radio (const :tag "Do not use speedbar" :value nil)
                (const :tag "For directories" :value dir)
                (const :tag "For sources" :value source)
                (const :tag "For methods" :value method))
  :set (function (lambda (sym val)
                   (set sym val)
                   (let ((ecb-redraw-layout-quickly nil))
                     (ecb-redraw-layout-full)))))

(defcustom ecb-directories-update-speedbar 'auto
  "*Update an integrated speedbar after selecting a directory.
If not nil then an integrated speedbar will be updated after selecting a
directory in the ECB-directories-buffer so the speedbar displays the contents
of that directory.

Of course this option makes only sense if the integrated speedbar is displayed
in addition to the ECB-directories-buffer.

This option can have the following values:
- t: Always update speedbar.
- nil: Never update speedbar.
- auto: Update when senseful \(see scenarios below)
- <function>: A user-defined function. The function is called after a
  directory is selected, gets the selected directory as argument and has to
  return nil if the the integrated speedbar should NOT be updated.

Two example-scenarios where different values for this option can be senseful:

If `ecb-show-sources-in-directories-buffer' is not nil or you have a layout
where an ECB-sources-buffer is visible then you probably want to use the
ECB-directories-buffer \(and/or the ECB-sources-buffer) for directory- and
file-browsing. If you have in addition an integrated speedbar running then you
probably want to use speedbar instead of the ECB-methods-buffer for
source-content-browsing. In this case you probably want the speedbar not be
updated because you do not need speedbar reflecting the current-directory
contents but only the contents of the currently selected source-file and the
integrated speedbar updates itself autom. for the latter one!

If `ecb-show-sources-in-directories-buffer' is nil and there is also no
ECB-sources-buffer visible in the current layout then you probably want to use
an integrated speedbar for browsing directory-contents \(i.e. the files) and
file-contents \(instead of the ECB-methods-buffer for example). In this case
you probably want the speedbar updated because you need speedbar reflecting
the current-directory contents so you can select files.

The value 'auto \(see above) takes exactly these two scenarios into account."
  :group 'ecb-directories
  :type '(radio (const :tag "Always" :value t)
                (const :tag "Never" :value nil)
                (const :tag "Automatic" :value auto)
                (function :tag "Custom function")))

(defun ecb-show-sources-in-directories-buffer-p ()
  (cond ((equal ecb-show-sources-in-directories-buffer 'never)
         nil)
        ((equal ecb-show-sources-in-directories-buffer 'always)
         t)
        (t
         (and (listp ecb-show-sources-in-directories-buffer)
              (member ecb-layout-name
                      ecb-show-sources-in-directories-buffer)))))

(defcustom ecb-cache-directory-contents nil
  "*Cache contents of directories.
This can be useful if `ecb-source-path' contains directories with many files
and subdirs, especially if these directories are mounted net-drives \(\"many\"
means here something > 1000, dependent of the speed of the net-connection and
the machine). For these directories actualizing the sources- and/or directories-
buffer of ECB \(if displayed in current layout!) can slow down dramatically so
a caching increases speed a lot.
 
The value of this option is a list where the each element is a cons-cell and
looks like:
  \(<dir-regexp> . <filenumber threshold>)
<dir-regexp>: Regular expression a directory must match to be cached.
<filenumber threshold>: Number of directory contents must exceed this number.

A directory will we only be cached if and only if the directory-name matches
one regexp of this option and its content-number exceeds the related
threshold.

The cache entry for a certain directory will be refreshed and actualized only
by using the POWER-click \(see `ecb-primary-secondary-mouse-buttons') in the
directories-buffer of ECB.

Examples:

A value of \(\"~/bigdir*\" . 1000) means the contents of every subdirectory of
the home-directory will be cached if the directory contains more than 1000
entries and its name begins with \"bigdir\".

A value of \(\".*\" . 1000) caches every directory which has more than 1000
entries.

A value of \(\".*\" . 0) caches every directory regardless of the number of
entries."
  :group 'ecb-directories
  :type `(repeat (cons (regexp :tag "Directory-regexp")
                       (integer :tag "Filenumber threshold" :value 1000))))

(defcustom ecb-directories-buffer-name " *ECB Directories*"
  "*Name of the ECB directory buffer.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB Directories*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object of
the ECB-directory-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-directories
  :type 'string)

(defcustom ecb-excluded-directories-regexp "^\\(CVS\\|\\..*\\)$"
  "*Directories that should not be included in the directories list.
The value of this variable should be a regular expression."
  :group 'ecb-directories
  :type 'regexp)

(defcustom ecb-auto-expand-directory-tree 'best
  "*Automatically expand the directory tree to the current source file.
There are three options:
- best: Expand the best-matching source-path
- first: Expand the first matching source-path
- nil: Do not automatically expand the directory tree."
  :group 'ecb-directories
  :type '(radio (const :tag "Best matching"
                       :value best)
                (const :tag "First matching"
                       :value first)
                (const :tag "No auto. expand"
                       :value nil)))

(defcustom ecb-grep-function (if (fboundp 'igrep) 'igrep 'grep)
  "*Function used for performing a grep.
The popup-menu of the tree-buffers \"Directories\", \"Sources\" and
\"History\" offer to grep the \"current\" directory:
- Directory-buffer: The grep is performed in the current popup-directory after
  clicking the right mouse-button onto a node.
- Sources-buffer: The grep is performed in the current selected directory.
- History-buffer: The grep is performed in the directory of the current
  popup-source after clicking the right mouse-button onto a node.

Conditions for such a function:
- The function is called interactively via `call-interactively'
- During the function-call the `default-directory' is temp. set to that
  directory mentioned above with \"... is performed in ...\", i.e. the
  function can use the value of `default-directory' to determine the directory
  to grep.
- The function must read all it's arguments itself.
- The function is completely responsible for performing the grep itself and
  displaying the results.

Normally one of the standard-grepping functions like `grep' or `igrep' \(or
some wrappers around it) should be used!"
  :group 'ecb-directories
  :type 'function)

(defcustom ecb-grep-find-function (if (fboundp 'igrep-find)
                                      'igrep-find 'grep-find)
  "*Function used for performing a recursive grep.
For more Details see option `ecb-grep-function' and replace \"grep\" with
\"recursive grep\" or \"grep-find\"."
  :group 'ecb-directories
  :type 'function)

(defcustom ecb-sources-buffer-name " *ECB Sources*"
  "*Name of the ECB sources buffer.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB Sources*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object of
the ECB-sources-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-sources
  :type 'string)


(defcustom ecb-sources-exclude-cvsignore nil
  "*Specify if files contained in a .cvsignore should be excluded.
Value is a list of regular expressions or nil. If you want to exclude files
listed in a .cvsignore-file from being displayed in the ecb-sources-buffer
then specify a regexp for such a directory.

If you want to exclude the contents of .cvsignore-files for every directory
then you should add one regexp \".*\" which matches every directory.

If you never want to exclude the contents of .cvsignore-files then set this
option to nil. This is the default."
  :group 'ecb-sources
  :group 'ecb-directories
  :type '(repeat (regexp :tag "Directory-regexp")))


(defcustom ecb-source-file-regexps
  '((".*" . ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\)$\\)\\)"
             "^\\.\\(emacs\\|gnus\\)$")))
  "*Specifies which files are shown as source files.
This is done on directory-base, which means for each directory-regexp the
files to display can be specified. If more than one directory-regexp matches
the current selected directory then always the first one \(and its related
file-exclude/include-regexps) is used! If no directory-regexp matches then all
files are displayed for the currently selected directory.

Important note: It is recommended that the *LAST* element of this list should
contain an always matching directory-regexp \(\".*\")!

So the value of this option is a list of cons-cells where the car is a
directory regexp and the cdr is a 2 element list where the first element is a
exclude regexp and the second element is a include regexp. A file is displayed
in the sources-buffer of ECB iff: The file does not match the exclude regexp
OR the file matches the include regexp.

But regardless of the value of this option a file F is never displayed in the
sources-buffer if the directory matches `ecb-sources-exclude-cvsignore'
and the directory contains a file .cvsignore which contains F as an entry!

There are three predefined and useful combinations of an exclude and include
regexp:
- All files
- All, but no backup, object, lib or ini-files \(except .emacs and .gnus). This
  means all files except those starting with \".\", \"#\" or ending with
  \"~\", \".elc\", \".obj\", \".o\", \".lib\", \".dll\", \".a\", \".so\".
  (but including .emacs and .gnus)
- Common source file types (.c, .java etc.)
In addition to these predefined values a custom exclude and include
combination can be defined.

Tips for the directory- and file-regexps: \"$^\" matches no files/directories,
\".*\" matches all files/directories."
  :group 'ecb-sources
  :type '(repeat (cons :tag "Directory file-spec"
                       (regexp :tag "Directory regexp")
                       (choice :tag "Files to display"
                               :menu-tag "Files to display"
                               (const :tag "All files"
                                      :value ("" ""))
                               (const :tag "All, but no backups, objects, etc..."
                                      :value ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\)$\\)\\)" "^\\.\\(emacs\\|gnus\\)$"))
                               (const :tag "Common source file types"
                                      :value ("" "\\(\\(M\\|m\\)akefile\\|.*\\.\\(java\\|el\\|c\\|cc\\|h\\|hh\\|txt\\|html\\|texi\\|info\\|bnf\\)\\)$"))
                               (list :tag "Custom"
                                     (regexp :tag "Exclude regexp"
                                             :value "")
                                     (regexp :tag "Include regexp"
                                             :value ""))))))

(defcustom ecb-show-source-file-extension t
  "*Show the file extension of source files."
  :group 'ecb-sources
  :type 'boolean)

(defcustom ecb-sources-sort-method 'name
  "*Defines how the source files are sorted.
- 'name: Sorting by name.
- 'extension: Sorting first by name and then by extension.
- nil: No sorting, means source files are displayed in the sequence returned by
  `directory-files' \(called without sorting)."
  :group 'ecb-sources
  :type '(radio (const :tag "By name"
                       :value name)
                (const :tag "By extension"
                       :value extension)
                (const :tag "No sorting"
                       :value nil)))

(defcustom ecb-history-buffer-name " *ECB History*"
  "*Name of the ECB history buffer.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB History*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object of
the ECB-history-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-history
  :type 'string)

(defcustom ecb-sort-history-items nil
  "*Sorts the items in the history buffer."
  :group 'ecb-history
  :type 'boolean)

(defcustom ecb-clear-history-behavior 'not-existing-buffers
  "*The entries of the history buffer to delete with `ecb-clear-history'.
Three options are available:
- not-existing-buffers: All entries which represent a buffer-name not existing
  anymore in the bufferlist will be cleared. Probably the most senseful value.
- existing-buffers: The opposite of 'not-existing-buffers.
- all: The whole history will be cleared."
  :group 'ecb-history
  :type '(radio (const :tag "Not existing buffers"
                       :value not-existing-buffers)
                (const :tag "Existing buffers"
                       :value existing-buffers)
                (const :tag "All entries"
                       :value all)))

(defcustom ecb-kill-buffer-clears-history nil
  "*Define if `kill-buffer' should also clear the history.
There are three options:
- auto: Removes automatically the corresponding history-entry after the buffer
  has been killed.
- ask: Asks, if the history-entry should be removed after the kill.
- nil: `kill-buffer' does not affect the history \(this is the default)."
  :group 'ecb-history
  :type '(radio (const :tag "Remove history entry automatically"
                       :value auto)
                (const :tag "Ask if history entry should be removed"
                       :value ask)
                (const :tag "Do not clear the history"
                       :value nil)))

(defcustom ecb-history-item-name 'buffer-name
  "*The name to use for items in the history buffer."
  :group 'ecb-history
  :type '(radio (const :tag "Buffer name"
                       :value buffer-name)
                (const :tag "File name"
                       :value file-name)))
                
(defcustom ecb-methods-buffer-name " *ECB Methods*"
  "*Name of the ECB methods buffer.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB Methods*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object of
the ECB-methods-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-methods
  :type 'string)

(defcustom ecb-auto-expand-token-tree 'expand-spec
  "*Expand the methods-token-tree automatically if node invisible.
This option has only an effect if option `ecb-highlight-token-with-point' is
switched on too. There are three possible choices:
- nil: No auto. expanding of the method buffer.
- expand-spec: Auto expand the method-buffer nodes if the node belonging to
  current token under point is invisible because its parent-node is collapsed.
  But expanding is only done if the type of the token under point in the
  edit-buffer is contained in `ecb-methods-nodes-expand-spec'.
- all: Like expand-spec but expands all tokens regardless of the setting in
  `ecb-methods-nodes-expand-spec'.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "No auto. expand" :value nil)
                (const :tag "Expand as specified" :value expand-spec)
                (const :tag "Expand all" :value all)))

(defcustom ecb-expand-methods-switch-off-auto-expand t
  "*Switch off auto expanding in the ECB-method buffer.
If on then auto expanding is switched off after explicit expanding or
collapsing by `ecb-expand-methods-nodes'.

This is done with `ecb-toggle-auto-expand-token-tree' so after the switch off
the auto expanding feature can again switched on quickly.

But after explicitly expanding/collapsing the methods-buffer to a certain
level the auto. expanding could undo this when the node belonging to current
token under point in the edit-window is invisible after
`ecb-expand-methods-nodes' - then the auto. expand feature would make this
node immediately visible and destroys the explicitly set expand-level."
  :group 'ecb-methods
  :type 'boolean)

(defvar ecb-tree-RET-selects-edit-window--internal nil
  "Only set by customizing `ecb-tree-RET-selects-edit-window' or calling
`ecb-toggle-RET-selects-edit-window'!
Do not set this variable directly, it is only for internal uses!")

(defcustom ecb-tree-RET-selects-edit-window
  (list ecb-directories-buffer-name
        ecb-sources-buffer-name
        ecb-methods-buffer-name
        ecb-history-buffer-name)
  "*In which tree-buffers RET should finally select an edit-window.
If a name of an ECB tree-buffer is contained in this list then hitting RET in
this tree-buffer selects as last action the right edit-window otherwise only
the right action is performed \(opening a new source, selecting a method etc.)
but point stays in the tree-buffer.

A special remark for the `ecb-directories-buffer-name': Of course here the
edit-window is only selected if the name of the current layout is contained in
`ecb-show-sources-in-directories-buffer' or if the value of
`ecb-show-sources-in-directories-buffer' is 'always \(otherwise this would not
make any sense)!

The setting in this option is only the default for each tree-buffer. With
`ecb-toggle-RET-selects-edit-window' the behavior of RET can be changed fast
and easy in a tree-buffer without customizing this option, but of course not
for future Emacs sessions!"
  :group 'ecb-general
  :set (function (lambda (sym val)
                   (set sym val)
                   (setq ecb-tree-RET-selects-edit-window--internal
                         (copy-list val))))
  :type `(set (const :tag ,ecb-directories-buffer-name
                     :value ,ecb-directories-buffer-name)
              (const :tag ,ecb-sources-buffer-name
                     :value ,ecb-sources-buffer-name)
              (const :tag ,ecb-methods-buffer-name
                     :value ,ecb-methods-buffer-name)
              (const :tag ,ecb-history-buffer-name
                     :value ,ecb-history-buffer-name)))

(defcustom ecb-auto-update-methods-after-save t
  "*Automatically updating the ECB method buffer after saving a source."
  :group 'ecb-methods
  :type 'boolean)

(defcustom ecb-font-lock-tokens t
  "*Adds font-locking \(means highlighting) to the ECB-method buffer.
This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-token-tree-cache)))
  :type 'boolean
  :initialize 'custom-initialize-default)

(defcustom ecb-token-jump-sets-mark t
  "*Set the mark after jumping to a token from the ECB-method buffer.
If set the user can easily jump back."
  :group 'ecb-methods
  :type 'boolean)

(defconst ecb-token->text-functions
  (mapcar (lambda (fkt)
            (cons (intern
                   (concat "ecb-"
                           (mapconcat 'identity
                                      (cdr (split-string (symbol-name fkt) "-"))
                                      "-")))
                  fkt))
          semantic-token->text-functions)
  "Alist containing one element for every member of
`semantic-token->text-functions' where the value is the member of
`semantic-token->text-functions' with name \"semantic-XYZ\" and the key is a
symbol with name \"ecb-XYZ\".")

(defcustom ecb-bucket-token-display '("" "" ecb-bucket-token-face)
  "*How ECB displays bucket tokens in the ECB methods buffer.
Bucket tokens are tokens like \"[+] Variables\", \"[+] Dependencies\" etc. The
name of the bucket-token comes from semantic but you can define a prefix, a
suffix and a special face for the bucket token.

The default are empty prefix/suffix-strings and 'ecb-bucket-token-face'. But
an alternative can be for example '\(\"[\" \"]\" nil) which means no special
face and a display like \"[+] [<bucket-name>]\".

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-token-tree-cache)))
  :type '(list (string :tag "Bucket-prefix" :value "[")
               (string :tag "Bucket-suffix" :value "]")
               (choice :tag "Bucket-face" :menu-tag "Bucket-face"
                       (const :tag "No special face" :value nil)
                       (face :tag "Face" :value ecb-bucket-token-face)))
  :initialize 'custom-initialize-default)

(defcustom ecb-token-display-function '((default . ecb-prototype-nonterminal))
  "*Function to use for displaying tokens in the methods buffer.
This functionality is set on major-mode base, i.e. for every major-mode a
different function can be used. The value of this option is a list of
cons-cells:
- The car is either a major-mode symbol or the special symbol 'default which
  means if no function for a certain major-mode is defined then the cdr of
  the 'default cons-cell is used.
- The cdr is the function used for displaying a token in the related
  major-mode.
Every function is called with 3 arguments:
1. The token
2. The parent-token of token \(can be nil)
3. The value of `ecb-font-lock-tokens'.
Every function must return the display of the token as string, colorized if
the third argument is not nil.

The following functions are predefined:
- All functions of `semantic-token->text-functions'.
- For every function in `semantic-token->text-functions' with name
  \"semantic-XYZ\" a function with name \"ecb-XYC\" is predefined. The
  differences between the semantic- and the ECB-version are:
  + The ECB-version displays for type tokens only the type-name and nothing
    else \(exception: In c++-mode a template specifier is appended to the
    type-name if a template instead a normal class).
  + The ECB-version displays type-tokens according to the setting in
    `ecb-type-token-display'. This is useful for better recognizing
    different classes, structs etc. in the ECB-method window.
  For all tokens which are not types the display of the ECB-version is
  identical to the semantic version. Example: For `semantic-name-nonterminal'
  the pendant is `ecb-name-nonterminal'.

This functionality also allows the user to display tokens as UML. To enable
this functionality set the function for a major-mode \(e.g. `jde-mode') to
`semantic-uml-concise-prototype-nonterminal',
`semantic-uml-prototype-nonterminal', or
`semantic-uml-abbreviate-nonterminal' or the ECB-versions of these functions.

If the value is nil, i.e. neither a function for a major-mode is defined nor
the special 'default, then `semantic-prototype-nonterminal' is used for
displaying the tokens.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-token-tree-cache)))
  :type (list 'repeat ':tag "Display functions per mode"
              (list 'cons ':tag "Mode token display"
                    '(symbol :tag "Major mode")
                    (nconc (list 'choice ':tag "Display function"
                                 ':menu-tag '"Display function")
                           (append
                            (mapcar (lambda (f)
                                      (list 'const ':tag
                                            (symbol-name (car f)) (car f)))
                                    ecb-token->text-functions)
                            (mapcar (lambda (f)
                                      (list 'const ':tag
                                            (symbol-name (cdr f)) (cdr f)))
                                    ecb-token->text-functions)
                            (list '(function :tag "Function"))))))
  :initialize 'custom-initialize-default)

(defcustom ecb-type-token-display nil
  "*How to display semantic type-tokens in the methods buffer.
Normally all token displaying, colorizing and facing is done by semantic
according to the value of `semantic-face-alist' and the semantic
display-function \(e.g. one from `semantic-token->text-functions'). But
sometimes a finer distinction in displaying the different type specifiers of
type-tokens can be useful. For a description when this option is evaluated
look at `ecb-token-display-function'!

This functionality is set on a major-mode base, i.e. for every major-mode a
different setting can be used. The value of this option is a list of
cons-cells:
- The car is either a major-mode symbol or the special symbol 'default which
  means if no setting for a certain major-mode is defined then the cdr of
  the 'default cons-cell is used.
- The cdr is a list of 3-element-lists:
  1. First entry is a semantic type specifier in string-form. Current
     available type specifiers are for example \"class\", \"interface\",
     \"struct\", \"typedef\" and \"enum\". In addition to these ones there is
     also a special ECB type specifier \"group\" which is related to grouping
     tokens \(see `ecb-post-process-semantic-tokenlist' and
     `ecb-group-function-tokens-with-parents'). Any arbitrary specifier can be
     set here but if it is not \"group\" or not known by semantic it will be
     useless.
  2. Second entry is a flag which indicates if the type-specifier string from
     \(1.) itself should be removed \(if there is any) from the display.
  3. Third entry is the face which is used in the ECB-method window to display
     type-tokens with this specifier. ECB has some predefined faces for this
     \(`ecb-type-token-class-face', `ecb-type-token-struct-face',
     `ecb-type-token-typedef-face', `ecb-type-token-enum-face' and
     `ecb-type-token-group-face') but any arbitrary face can be set here. This
     face is merged with the faces semantic already uses to display a token,
     i.e. the result is a display where all face-attributes of the ECB-face
     take effect plus all face-attributes of the semantic-faces which are not
     set in the ECB-face \(with XEmacs this merge doesn't work so here the
     ECB-face replaces the semantic-faces; this may be fixed in future
     versions).

The default value is nil means there is no special ECB-displaying of
type-tokens in addition to the displaying and colorizing semantic does. But a
value like the following could be a useful setting:

  \(\(default
     \(\"class\" t ecb-type-token-class-face)
     \(\"group\" nil ecb-type-token-group-face))
    \(c-mode
     \(\"struct\" nil ecb-type-token-struct-face)
     \(\"typedef\" nil ecb-type-token-typedef-face)))

This means that in `c-mode' only \"struct\"s and \"typedef\"s are displayed
with special faces \(the specifiers itself are not removed) and in all other
modes \"class\"es and grouping-tokens \(see `ecb-token-display-function',
`ecb-group-function-tokens-with-parents') have special faces and the \"class\"
specifier-string is removed from the display.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-token-tree-cache)))
  :type '(repeat (cons (symbol :tag "Major-mode")
                       (repeat :tag "Display of type specifiers"
                               (list (choice :tag "Specifier list"
                                             :menu-tag "Specifier list"
                                             (const :tag "class"
                                                    :value "class")
                                             (const :tag "interface"
                                                    :value "interface")
                                             (const :tag "struct"
                                                    :value "struct")
                                             (const :tag "typedef"
                                                    :value "typedef")
                                             (const :tag "enum"
                                                    :value "enum")
                                             (const :tag "group"
                                                    :value "group")
                                             (string :tag "Any specifier"))
                                     (boolean :tag "Remove the type-specifier" t)
                                     (face :tag "Any face"
                                           :value ecb-type-token-class-face)))))
  :initialize 'custom-initialize-default)

(defun ecb-get-face-for-type-token (type-specifier)
  "Return the face set in `ecb-type-token-display' for current major-mode and
TYPE-SPECIFIER or nil."
  (let ((mode-display (cdr (assoc major-mode ecb-type-token-display)))
        (default-display (cdr (assoc 'default ecb-type-token-display))))
    (or (nth 2 (assoc type-specifier mode-display))
        (nth 2 (assoc type-specifier default-display)))))

(defun ecb-get-remove-specifier-flag-for-type-token (type-specifier)
  "Return the remove-specifier-flag set in `ecb-type-token-display' for
current major-mode and TYPE-SPECIFIER or nil."
  (let ((mode-display (cdr (assoc major-mode ecb-type-token-display)))
        (default-display (cdr (assoc 'default ecb-type-token-display))))
    (or (nth 1 (assoc type-specifier mode-display))
        (nth 1 (assoc type-specifier default-display)))))

(defun ecb-merge-face-into-text (text face)
  "Merge FACE to the already precolored TEXT so the values of all
face-attributes of FACE take effect and but the values of all face-attributes
of TEXT which are not set by FACE are preserved.
For XEmacs this merge does currently not work therefore here FACE replaces all
faces of TEXT!"
  (if (null face)
      text
    (let ((newtext (concat text)))
      (if ecb-running-xemacs
          (add-text-properties 0 (length newtext) (list 'face face) newtext)
        (alter-text-property 0 (length newtext) 'face
                             (lambda (current-face)
                               (let ((cf
                                      (cond ((facep current-face)
                                             (list current-face))
                                            ((listp current-face)
                                             current-face)
                                            (t nil)))
                                     (nf
                                      (cond ((facep face)
                                             (list face))
                                            ((listp face)
                                             face)
                                            (t nil))))
                                 ;; we must add the new-face in front of
                                 ;; current-face to get the right merge!
                                 (append nf cf)))
                             newtext))
      newtext)))

(dolist (elem ecb-token->text-functions)
  (fset (car elem)
        `(lambda (token &optional parent-token colorize)
           (if (eq 'type (semantic-token-token token))
               (let* ( ;; we must here distinguish between UML- and
                      ;; not-UML-semantic functions because for UML we must
                      ;; preserve some semantic facing added by semantic (e.g.
                      ;; italic for abstract classes)!
                      (text (funcall (if (string-match "-uml-" (symbol-name (quote ,(car elem))))
                                         'semantic-uml-abbreviate-nonterminal
                                       'semantic-name-nonterminal)
                                     token parent-token colorize))
                      (type-specifier (if (or (semantic-token-get token
                                                                  'ecb-group-token)
                                            ;; marking done my semantic itself
                                              (semantic-token-get token 'faux))
                                          "group"
                                        (semantic-token-type token)))
                      (face (ecb-get-face-for-type-token type-specifier))
                      (remove-flag (ecb-get-remove-specifier-flag-for-type-token
                                    type-specifier)))
                 (save-match-data
                   ;; the following is done to replace the "struct" from
                   ;; grouping tokens (see
                   ;; ecb-group-function-tokens-with-parents) with "group".
                   ;; This code can be removed (or changed) if semantic allows
                   ;; correct protection display for function-tokens with
                   ;; parent-token.
                   (when (or (semantic-token-get token 'ecb-group-token)
                             (semantic-token-get token 'faux))
                     (if (string-match (concat "^\\(.+"
                                               semantic-uml-colon-string
                                               "\\)\\("
                                               (if (semantic-token-get token 'faux)
                                                   semantic-orphaned-member-metaparent-type
                                                 "struct")
                                               "\\)") text)
                         (let ((type-spec-text "group"))
                           (put-text-property 0 (length type-spec-text)
                                              'face
                                              (get-text-property
                                               0 'face
                                               (match-string 2 text))
                                              type-spec-text)
                           (setq text (concat (match-string 1 text)
                                              type-spec-text)))))
                   ;; Now we must maybe add a template-spec in c++-mode and
                   ;; maybe remove the type-specifier string.
                   (let (col-type-name col-type-spec template-text)
                     (if (string-match (concat "^\\(.+\\)\\("
                                               semantic-uml-colon-string
                                               type-specifier "\\)")
                                       text)
                         (setq col-type-name (match-string 1 text)
                               col-type-spec (if (not remove-flag)
                                                 (match-string 2 text)))
                       (setq col-type-name text))
                     (when (and (equal major-mode 'c++-mode)
                                (fboundp 'semantic-c-template-string))
                       (setq template-text (semantic-c-template-string
                                            token parent-token colorize))
                       (put-text-property 0 (length template-text)
                                          'face
                                          (get-text-property
                                           (1- (length col-type-name)) 'face
                                           col-type-name)
                                          template-text))
                     (setq text (concat col-type-name template-text
                                        col-type-spec))))
                 ;; now we add some own colorizing if necessary
                 (if face
                     (setq text (ecb-merge-face-into-text text face)))
                 text)
             (funcall (quote ,(cdr elem)) token parent-token colorize)))))

(defcustom ecb-post-process-semantic-tokenlist
  '((c++-mode . ecb-group-function-tokens-with-parents)
    (emacs-lisp-mode . ecb-group-function-tokens-with-parents))
  "*Define mode-dependent post-processing for the semantic-tokenlist.
This is an alist where the car is a major-mode symbol and the cdr is a
function-symbol of a function which should be used for post-processing the
tokenlist \(returned by `semantic-bovinate-toplevel') for a buffer in this
major-mode. Such a function is called with current semantic tokenlist of
current buffer and must return a valid tokenlist again.

For oo-programming languages where the methods of a class can be defined
outside the class-definition \(e.g. C++, Eieio) the function
`ecb-group-function-tokens-with-parents' can be used to get a much better
method-display in the methods-window of ECB, because all method
implementations of a class are grouped together.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(repeat (cons (symbol :tag "Major-mode")
                       (function :tag "Post-process function"))))

(defcustom ecb-show-only-positioned-tokens t
  "*Show only nodes in the method-buffer which are \"jump-able\".
If not nil then ECB displays in the method-buffer only nodes which are
\"jump-able\", i.e. after selecting it by clicking or with RET then ECB jumps
to the corresponding location in the edit-window.
Example: With CLOS or Eieio source-code there can exist some position-less
nodes like variable-attributes in a `defclass' form which are only displayed
if this option is nil. Displaying such nodes can be senseful even if they can
not be jumped.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type 'boolean)

(defcustom ecb-show-tokens '((include collapsed nil)
			     (parent collapsed nil)
                             (type flattened nil)
                             (variable collapsed access)
                             (function flattened access)
                             (rule flattened name)
                             (section flattened nil)
                             (def collapsed name)
                             (t collapsed name))
  "*How to show tokens in the methods buffer first time after find-file.
This variable is a list where each element represents a type of tokens:

\(<token type> <display type> <sort method>)

The tokens in the methods buffer are displayed in the order as they appear in
this list.

Token Type
----------

A Semantic token type symbol \(for all possible type symbols see documentation
of semantic):
- include
- type
- variable
- function
- rule
- section \(chapters and sections in `info-mode')
- def \(definitions in `info-mode')

or one of the following:

- t:      All token types not specified anywhere else in the list.
- parent: The parents of a type.

Display Type
------------

A symbol which describes how the tokens of this type shall be shown:

- expanded:  The tokens are shown in an expanded node.
- collapsed: The tokens are shown in a collapsed node.
- flattened: The tokens are added to the parent node.
- hidden:    The tokens are not shown.

Sort Method
-----------

A symbol describing how to sort the tokens of this type:

- name:   Sort by the token name.
- access: Sort by token access (public, protected, private) and then by name.
- nil:    Don't sort tokens. They appear in the same order as in the source
          buffer.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-token-tree-cache)))
  :type '(repeat (list (symbol :tag "Token symbol")
		       (choice :tag "Display type" :value collapsed
			       (const :tag "Expanded" expanded)
			       (const :tag "Collapsed" collapsed)
			       (const :tag "Flattened" flattened)
			       (const :tag "Hidden" hidden))
		       (choice :tag "Sort by" :value nil
			       (const :tag "Name" name)
			       (const :tag "Access then name" access)
			       (const :tag "No sort" nil))))
  :initialize 'custom-initialize-default)

(defcustom ecb-methods-nodes-expand-spec '(type variable function section)
  "*Semantic token-types expanded by `ecb-expand-methods-nodes'.
The value of this option is either the symbol 'all \(all tokens are expanded
regardless of their type) or a list of symbols where each symbol is a valid
semantic token-type. For a description of semantic token types see option
`ecb-show-tokens'.

But this option also defines if bucket-nodes in the ECB-method-buffer \(e.g.
\"\[Variables\]\") should be expanded. Therefore valid symbols for this list
are also all cars of the variable `semantic-symbol->name-assoc-list'.

If there is a bucket-name \(the node-name stripped of the settings in
`ecb-bucket-token-display') which is not contained as cdr in
`semantic-symbol->name-assoc-list' then the symbol with this bucket-name as
name is also a valid symbol for this list. Example: In ECB there are buckets
\"\[Parents\]\". The bucket-name is \"Parents\" and the valid symbol-name is
then 'Parents.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "All node-types" :value all)
                (repeat :tag "Node-type list"
                        (symbol :tag "Node-type"))))

(defcustom ecb-methods-nodes-collapse-spec 'all
  "*Semantic token-types collapsed by `ecb-expand-methods-nodes'.
For valid values of this option see `ecb-methods-nodes-expand-spec'!

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "All node-types" :value all)
                (repeat :tag "Node-type list"
                        (symbol :tag "Node-type"))))

(defcustom ecb-exclude-parents-regexp nil
  "*Regexp which parent classes should not be shown in the methods buffer.
If nil then all parents will be shown if `ecb-show-parents' is not nil.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-token-tree-cache)))
  :type '(radio (const :tag "Do not exclude any parents"
                       :value nil)
                (regexp :tag "Parents-regexp to exclude"))
  :initialize 'custom-initialize-default)

(defcustom ecb-highlight-token-with-point 'highlight-scroll
  "*How to highlight the method or variable under the cursor.
- highlight-scroll: Always scroll the method buffer, so the current method of the
  edit-window is highlighted in the method-window.
- highlight: Only highlight the current method of the edit window in the
  method window if the method is visible in the method-window.
- nil: No highlighting is done.
See also `ecb-highlight-token-with-point-delay'.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "Highlight and scroll window"
                       :value highlight-scroll)
                (const :tag "Just highlight"
                       :value highlight)
                (const :tag "Do not highlight"
                       :value nil)))

(defcustom ecb-highlight-token-with-point-delay 0.25
  "*Time Emacs must be idle before current token is highlighted.
If nil then there is no delay, means current token is highlighted immediately.
A small value of about 0.25 seconds saves CPU resources and you get even
though almost the same effect as if you set no delay. But such a delay
prevents also \"jumping backward/forward\" during scrolling within
java-classes if point goes out of method-definition into class-definition.
Therefore the default value is a delay of 0.25 seconds.

This options takes only effect for semantic-sources - means sources supported
by semantic!"
  :group 'ecb-methods
  :type '(radio (const :tag "No highlighting delay"
                       :value nil)
                (number :tag "Idle time before highlighting"
                        :value 0.25))
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if ecb-minor-mode
                       (ecb-activate-ecb-sync-functions value 'ecb-token-sync))))
  :initialize 'custom-initialize-default)

(defvar ecb-method-overlay (make-overlay 1 1)
  "Internal overlay used for the first line of a method.")
(overlay-put ecb-method-overlay 'face ecb-token-header-face)


(defcustom ecb-token-visit-post-actions '((default . (ecb-token-visit-smart-token-start
                                                      ecb-token-visit-highlight-token-header))
                                          (java-mode . (ecb-token-visit-goto-doc-start))
                                          (jde-mode . (ecb-token-visit-goto-doc-start)))
  "*Actions to perform after visiting a token from the Method-buffer.
With this option actions can be added which will be performed after visiting
the start of the token in the source-buffer.

This functionality is set on a `major-mode' base, i.e. for every `major-mode' a
different setting can be used. The value of this option is a list of
cons-cells:
- The car is either a `major-mode' symbol or the special symbol 'default.
- The cdr is a list of action-functions or nil.

ECB first performs all actions defined for the special symbol 'default \(if
any) and then all actions defined for current `major-mode' \(if any).

ECB offers some predefined senseful action-functions. Currently there are:
- `ecb-token-visit-highlight-token-header'
- `ecb-token-visit-smart-token-start'
- `ecb-token-visit-recenter'
- `ecb-token-visit-recenter-top'
- `ecb-token-visit-goto-doc-start'
- `ecb-token-visit-narrow-token'
See the documentation of these function for details what they do.

But you can add any arbitrary function if the following conditions are
fulfilled:
- The function gets the semantic token as argument and
- the function returns the \(new) point after finishing its job."
  :group 'ecb-methods
  :type '(repeat (cons :value (nil . (ecb-token-visit-recenter))
                       (symbol :tag "Major-mode or default")
                       (repeat (choice :tag "Post action" :menu-tag "Post action"
                                       (const :tag "ecb-token-visit-smart-token-start"
                                              :value ecb-token-visit-smart-token-start)
                                       (const :tag "ecb-token-visit-highlight-token-header"
                                              :value ecb-token-visit-highlight-token-header)
                                       (const :tag "ecb-token-visit-goto-doc-start"
                                              :value ecb-token-visit-goto-doc-start)
                                       (const :tag "ecb-token-visit-narrow-token"
                                              :value ecb-token-visit-narrow-token)
                                       (const :tag "ecb-token-visit-recenter-top"
                                              :value ecb-token-visit-recenter-top)
                                       (const :tag "ecb-token-visit-recenter"
                                              :value ecb-token-visit-recenter)
                                       (function :tag "Function"))))))

(defun ecb-token-visit-function-member-p (fnc)
  (or (member fnc (cdr (assoc 'default ecb-token-visit-post-actions)))
      (member fnc (cdr (assoc major-mode ecb-token-visit-post-actions)))))

(defcustom ecb-tree-indent 2
  "*Indent size for tree buffer.
If you change this during ECB is activated you must deactivate and activate
ECB again to take effect."
  :group 'ecb-general
  :type 'integer)

(defcustom ecb-tree-expand-symbol-before nil
  "*Show the expand symbol before the items in a tree."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-tree-use-image-icons t
  "*Use icons for expand/collapse tokens instead of the ascii-strings.
If true the ECB displays in its tree-buffers the expand- and collapse symbols
with appropriate icons - the icons are the same as used by speedbar."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-truncate-lines '(t t t t)
  "*Truncate lines in ECB buffers.
If you change this during ECB is activated you must deactivate and activate
ECB again to take effect."
  :group 'ecb-general
  :type '(list (boolean :tag "Directories buffer")
               (boolean :tag "Sources buffer")
               (boolean :tag "Methods buffer")
               (boolean :tag "History buffer")))

(defcustom ecb-tree-easy-hor-scroll 5
  "*Scroll step for easy hor. scrolling via mouse-click in tree-buffers.
XEmacs has horizontal scroll-bars so invisible parts beyond the right
window-border of a tree-buffer can always made visible very easy.

GNU Emacs does not have hor. scroll-bars so especially with the mouse it is
quite impossible to scroll smoothly right and left. The functions
`scroll-left' and `scroll-right' can be annoying and are also not bound to
mouse-buttons.

If this option is a positive integer S then in all ECB-tree-buffers the keys
\[M-mouse-1] and \[M-mouse-3] are bound to scrolling left rsp. right with
scroll-step S - clicking with mouse-1 or mouse-2 onto the edge of the modeline
has the same effect, i.e. if you click with mouse-1 onto the left \(rsp.
right) edge of the modeline you will scroll left \(rsp. right). Additionally
\[C-M-mouse-1] and \[C-M-mouse-3] are bound to scrolling left rsp. right with
scroll-step `window-width' - 2. Default is a scroll-step of 5. If the value is
nil then no keys for horizontal scrolling are bound."
  :group 'ecb-general
  :type '(radio :value 5
                (const :tag "No hor. mouse scrolling" :value nil)
                (integer :tag "Scroll step")))

(defcustom ecb-truncate-long-names t
  "*Truncate long names that don't fit in the width of the ECB windows.
If you change this during ECB is activated you must deactivate and activate
ECB again to take effect."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-window-sync '(Info-mode dired-mode)
  "*Synchronize the ECB-windows automatically with current edit window.
If 'always then the synchronization takes place always a buffer changes in the
edit window, if nil then never. If a list of major-modes then only if the
`major-mode' of the new buffer belongs NOT to this list.

But in every case the synchronization takes only place if the current-buffer
in the edit-window has a relation to files or directories. Examples for the
former one are all programming-language-modes, `Info-mode' too, an example for
the latter one is `dired-mode'. For all major-modes related to
non-file/directory-buffers like `help-mode', `customize-mode' and others never
an autom. synchronization will be done!

It's recommended to exclude at least `Info-mode' because it makes no sense to
synchronize the ECB-windows after calling the Info help. Per default also
`dired-mode' is excluded but it can also making sense to synchronize the
ECB-directories/sources windows with the current directory in the
dired-buffer.

IMPORTANT NOTE: Every time the synchronization is done the hook
`ecb-current-buffer-sync-hook' is evaluated."
  :group 'ecb-general
  :type '(radio :tag "Synchronize ECB windows"
                (const :tag "Always" :value always)
                (const :tag "Never" nil)
                (repeat :tag "Not with these modes"
                        (symbol :tag "mode"))))

(defcustom ecb-window-sync-delay 0.25
  "*Time Emacs must be idle before the ECB-windows are synchronized.
Synchronizing is done with the current source displayed in the edit window. If
nil then there is no delay, means synchronization takes place immediately. A
small value of about 0.25 seconds saves CPU resources and you get even though
almost the same effect as if you set no delay."
  :group 'ecb-general
  :type '(radio (const :tag "No synchronizing delay"
                       :value nil)
                (number :tag "Idle time before synchronizing"
                        :value 0.25))
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if ecb-minor-mode
                       (ecb-activate-ecb-sync-functions value
                                                        'ecb-window-sync-function))))
  :initialize 'custom-initialize-default)

(defcustom ecb-tree-incremental-search 'prefix
  "*Enable incremental search in the ECB-tree-buffers.
For a detailed explanation see the online help section \"Working with the
keyboard in the ECB buffers\". If you change this during ECB is activated you
must deactivate and activate ECB again to take effect."
  :group 'ecb-general
  :type '(radio (const :tag "Match only prefix"
                       :value prefix)
                (const :tag "Match every substring"
                       :value substring)
                (const :tag "No incremental search"
                       :value nil)))

(defcustom ecb-tree-navigation-by-arrow t
  "*Enable smart navigation in the tree-windows by horizontal arrow-keys.
If not nil then the left- and right-arrow keys work in the ECB tree-window in
the following smart way if onto an expandable node:
+ Left-arrow: If node is expanded then it will be collapsed otherwise point
  jumps to the next \"higher\" node in the hierarchical tree \(higher means
  the next higher tree-level or - if no higher level available - the next
  higher node on the same level).
+ Right-arrow: If node is not expanded then it will be expanded.
Onto a not expandable node the horizontal arrow-keys go one character in the
senseful correct direction.

If this option is changed the new value takes first effect after deactivating
ECB and then activating it again!"
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-show-node-info-in-minibuffer '((if-too-long . path)
                                              (if-too-long . name)
                                              (always . path)
                                              (if-too-long . name+type))
  "*Node info to display in a tree-buffer.
Define which node info should displayed in a tree-buffer after
mouse moving over the node or after a shift click onto the node.

For every tree-buffer you can define \"when\" node info should be displayed:
- always: Node info is displayed by moving with the mouse over a node.
- if-too-long: Node info is only displayed by moving with the mouse over a
  node does not fit into the window-width of the tree-buffer window.
  In the ECB directories buffer this means also if a node is shortend or if
  the node has an alias \(see `ecb-source-path').
- shift-click: Node info is only displayed after a shift click with the
  primary mouse button onto the node.
- never: Node info is never displayed.

For every tree-buffer you can define what info should be displayed:
+ Directory-buffer:
  - name: Only the full node-name is displayed.
  - path: The full-path of the node is displayed.
+ Sources-buffer:
  - name: Only the full node-name is displayed.
  - file-info: File infos for this file are displayed.
  - file-info-full: Fill infos incl. full path for this file are displayed.
+ History-buffer:
  see Directories-buffer.
+ Methods-buffer:
  - name: Only the full node name is displayed.
  - name+type: The full name + the type of the node \(function, class,
    variable) is displayed.

Do NOT set this option directly via setq but use always customize!"
  :group 'ecb-general
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode)
                            ecb-minor-mode)
                       (let ((when-list (mapcar (lambda (elem)
                                                  (car elem))
                                                value)))
                         (if (or (member 'if-too-long when-list)
                                 (member 'always when-list))
                             (tree-buffer-activate-follow-mouse)
                           (tree-buffer-deactivate-follow-mouse)
                           (tree-buffer-deactivate-mouse-tracking))))))
  :type '(list (cons :tag "* Directories-buffer"
                     (choice :tag "When"
                             (const :tag "Always" :value always)
                             (const :tag "If too long" :value if-too-long)
                             (const :tag "After shift click" :value shift-click)
                             (const :tag "Never" :value never))
                     (choice :tag "What"
                             (const :tag "Node-name" :value name)
                             (const :tag "Full path" :value path)))
               (cons :tag "* Sources-buffer"
                     (choice :tag "When"
                             (const :tag "Always" :value always)
                             (const :tag "If too long" :value if-too-long)
                             (const :tag "After shift click" :value shift-click)
                             (const :tag "Never" :value never))
                     (choice :tag "What"
                             (const :tag "Node-name" :value name)
                             (const :tag "File info" :value file-info)
                             (const :tag "File info \(full path)"
                                    :value file-info-full)))
               (cons :tag "* History-buffer"
                     (choice :tag "When"
                             (const :tag "Always" :value always)
                             (const :tag "If too long" :value if-too-long)
                             (const :tag "After shift click" :value shift-click)
                             (const :tag "Never" :value never))
                     (choice :tag "What"
                             (const :tag "Node-name" :value name)
                             (const :tag "Full path" :value path)))
               (cons :tag "* Method-buffer"
                     (choice :tag "When"
                             (const :tag "Always" :value always)
                             (const :tag "If too long" :value if-too-long)
                             (const :tag "After shift click" :value shift-click)
                             (const :tag "Never" :value never))
                     (choice :tag "What"
                             (const :tag "Node-name" :value name)
                             (const :tag "Node-name + type" :value name+type)))))

(defcustom ecb-primary-secondary-mouse-buttons 'mouse-2--C-mouse-2
  "*Primary- and secondary mouse button for using the ECB-buffers.
A click with the primary button causes the main effect in each ECB-buffer:
- ECB Directories: Expanding/collapsing nodes and displaying files in the ECB
  Sources buffer.
- ECB sources/history: Opening the file in that edit-window specified by the
  option `ecb-primary-mouse-jump-destination'.
- ECB Methods: Jumping to the method in that edit-window specified by the
  option `ecb-primary-mouse-jump-destination'.

A click with the primary mouse-button while the SHIFT-key is pressed called
the POWER-click and does the following \(depending on the ECB-buffer where the
POWER-click occurs):
+ Directory-buffer: Refreshing the directory-contents-cache \(see
  `ecb-cache-directory-contents').
+ Sources- and History-buffer: Only displaying the source-contents in the
  method-buffer but not displaying the source-file in the edit-window.
+ Methods-buffer: Narrowing to the clicked method/variable/ect... \(see
  `ecb-token-visit-post-actions'). This works only for sources supported by
  semantic!

In addition always the whole node-name is displayed in the minibuffer after a
POWER-click \(for this see also `ecb-show-node-info-in-minibuffer').

The secondary mouse-button is for opening \(jumping to) the file in the other
window \(see the documentation `ecb-primary-mouse-jump-destination').

The following combinations are possible:
- primary: mouse-2, secondary: C-mouse-2 \(means mouse-2 while CTRL-key is
  pressed). This is the default setting.
- primary: mouse-1, secondary: C-mouse-1
- primary: mouse-1, secondary: mouse-2

If you change this during ECB is activated you must deactivate and activate
ECB again to take effect!"
  :group 'ecb-general
  :type '(radio (const :tag "Primary: mouse-2, secondary: Ctrl-mouse-2"
                       :value mouse-2--C-mouse-2)
                (const :tag "Primary: mouse-1, secondary: Ctrl-mouse-1"
                       :value mouse-1--C-mouse-1)
                (const :tag "Primary: mouse-1, secondary: mouse-2"
                       :value mouse-1--mouse-2)))

;; Thanks to David Hay for the suggestion <David.Hay@requisite.com>
(defcustom ecb-primary-mouse-jump-destination 'left-top
  "*Jump-destination of a primary mouse-button click.
Defines in which edit-window \(if splitted) ECB does the \"right\" action
\(opening the source, jumping to a method/variable) after clicking with the
primary mouse-button \(see `ecb-primary-secondary-mouse-buttons') onto a node.
There are two possible choices:
- left-top: Does the \"right\" action always in the left/topmost edit-window.
- last-point: Does the \"right\" action always in that edit-window which had
  the point before.

If the edit-window is not splitted this setting doesn´t matter.

Note: A click with the secondary mouse-button \(see again
`ecb-primary-secondary-mouse-buttons' does the \"right\" action always in the
\"other\" window related to the setting in this option."
  :group 'ecb-general
  :type '(radio (const :tag "Left/topmost edit-window"
                       :value left-top)
                (const :tag "Last edit-window with point"
                       :value last-point)))

(defcustom ecb-minor-mode-text " ECB"
  "*String to display in the mode line when ECB minor mode is active.
\(When the string is not empty, make sure that it has a leading space.)"
  :group 'ecb-general
  :type 'string)

(defcustom ecb-auto-compatibility-check t
  "*Check at ECB-startup if all ECB-options have correct values.
If not nil then all ECB-options are checked if their current value have the
correct type. It the type is incorrect the option is either auto. upgraded to
the new type or reset to the default-value of current ECB if no upgrade is
possible. This feature can also upgrade options which are renamed in current
ECB and try to transform the old-value to the new named option. After startup
all upgraded or reset options are displayed with their old \(before
upgrade/reset) and new values. See also the commands `ecb-upgrade-options' and
`ecb-display-upgraded-options'. If this option is off then the user can
perform the check and reset manually with `ecb-upgrade-options'."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-version-check t
  "*Checks at start-time if the requirements are fulfilled.
It checks if the required versions of the libraries semantic, eieio and
speedbar are installed and loaded into Emacs.

It is strongly recommended to set this option to not nil!"
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-debug-mode nil
  "*If not nil ECB displays debug-information in the Messages-buffer.
This is done for some critical situations concerning semantic-tokens and their
overlays \(or extends for XEmacs). Normally you should not need this switched
on! But if you get errors like \"destroyed extend\" for XEmacs or
\"wrong-argument-type\" concerning overlays for GNU Emacs then you should
switch on this option and submitting a bug-report to the ecb-mailing-list
\(`ecb-submit-problem-report') after getting the error again!"
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-directories-menu-user-extension nil
  "*User extensions for the popup-menu of the directories buffer.
Value is a list of elements of the following type: Each element defines a new
menu-entry and is a list containing two sub-elements, whereas the first is the
name of the menu-entry and the second the function \(a function symbol or a
lambda-expression) being called if the menu-entry is selected. If there is no
second sub-element and the first one is the string \"---\" then a
non-selectable menu-separator is displayed.

The function must follow the following guidelines:
It takes one argument which is the tree-buffer-node of the selected node \(means
the node for which the popup-menu has been opened). With the function
`tree-node-get-data' the related data of this node is accessible and returns
in case of the directories buffer the directory for which the popup-menu has
been opened. The function can do any arbitrary things with this directory.

Example for such a menu-function:

\(defun ecb-my-special-dir-popup-function \(node)
  \(let \(\(node-data=dir \(tree-node-get-data node)))
     \(message \"Dir under node: %s\" node-data=dir)))

Per default the user-extensions are added at the beginning of the built-in
menu-entries of `ecb-directories-menu' but the whole menu can be re-arranged
with `ecb-directories-menu-sorter'.

If you change this option you have to restart ECB to take effect."
  :group 'ecb-directories
  :type '(repeat (choice :tag "Menu-entry" :menu-tag "Menu-entry"
                         :value ("" ignore)
                         (const :tag "Separator" :value ("---"))
                         (list :tag "Menu-entry"
                               (string :tag "Entry-name")
                               (function :tag "Function" :value ignore)))))

(defcustom ecb-sources-menu-user-extension nil
  "*User extensions for the popup-menu of the sources buffer.
For further explanations see `ecb-directories-menu-user-extension'.

The node-argument of a menu-function contains as data the filename of the
source for which the popup-menu has been opened.

Per default the user-extensions are added at the beginning of the built-in
menu-entries of `ecb-sources-menu' but the whole menu can be re-arranged
with `ecb-sources-menu-sorter'.

If you change this option you have to restart ECB to take effect."
  :group 'ecb-sources
  :type '(repeat (choice :tag "Menu-entry" :menu-tag "Menu-entry"
                         :value ("" ignore)
                         (const :tag "Separator" :value ("---"))
                         (list :tag "Menu-entry"
                               (string :tag "Entry-name")
                               (function :tag "Function" :value ignore)))))

(defcustom ecb-methods-menu-user-extension nil
  "*User extensions for the popup-menu of the methods buffer.
For further explanations see `ecb-directories-menu-user-extension'.

The node-argument of a menu-function contains as data the semantic-token of
the method/variable/token for which the popup-menu has been opened.

Per default the user-extensions are added at the beginning of the built-in
menu-entries of `ecb-methods-menu' but the whole menu can be re-arranged
with `ecb-methods-menu-sorter'.

If you change this option you have to restart ECB to take effect."
  :group 'ecb-methods
  :type '(repeat (choice :tag "Menu-entry" :menu-tag "Menu-entry"
                         :value ("" ignore)
                         (const :tag "Separator" :value ("---"))
                         (list :tag "Menu-entry"
                               (string :tag "Entry-name")
                               (function :tag "Function" :value ignore)))))

(defcustom ecb-history-menu-user-extension nil
  "*User extensions for the popup-menu of the history buffer.
For further explanations see `ecb-directories-menu-user-extension'.

The node-argument of a menu-function contains as data the filename of the
source for which the popup-menu has been opened.

Per default the user-extensions are added at the beginning of the built-in
menu-entries of `ecb-history-menu' but the whole menu can be re-arranged
with `ecb-history-menu-sorter'.

If you change this option you have to restart ECB to take effect."
  :group 'ecb-history
  :type '(repeat (choice :tag "Menu-entry" :menu-tag "Menu-entry"
                         :value ("" ignore)
                         (const :tag "Separator" :value ("---"))
                         (list :tag "Menu-entry"
                               (string :tag "Entry-name")
                               (function :tag "Function" :value ignore)))))


(defcustom ecb-directories-menu-sorter nil
  "*Function which re-sorts the menu-entries of the directories buffer.
If a function then this function is called to re-arrange the menu-entries of
the combined menu-entries of the user-menu-extensions of
`ecb-directories-menu-user-extension' and the built-in-menu
`ecb-directories-menu'. If nil then no special sorting will be done and the
user-extensions are placed in front of the built-in-entries.

The function get one argument, a list of menu-entries. For the format of this
argument see `ecb-directories-menu-user-extension'. The function must return a
new list in the same format. Of course this function can not only re-arrange
the entries but also delete entries or add new entries."
  :group 'ecb-directories
  :type '(choice :tag "Menu-sorter" :menu-tag "Menu-sorter"
                 (const :tag "No special sorting" :value nil)
                 (function :tag "Sort-function" :value identity)))

(defcustom ecb-sources-menu-sorter nil
  "*Function which re-sorts the menu-entries of the directories buffer.
If a function then this function is called to sort the menu-entries of the
combined menu-entries of the user-menu-extensions of
`ecb-sources-menu-user-extension' and the built-in-menu
`ecb-sources-menu'. If nil then no special sorting will be done and the
user-extensions are placed in front of the built-in-entries.

For the guidelines for such a sorter-function see
`ecb-directories-menu-sorter'."
  :group 'ecb-sources
  :type '(choice :tag "Menu-sorter" :menu-tag "Menu-sorter"
                 (const :tag "No special sorting" :value nil)
                 (function :tag "Sort-function" :value identity)))

(defcustom ecb-methods-menu-sorter nil
  "*Function which re-sorts the menu-entries of the directories buffer.
If a function then this function is called to sort the menu-entries of the
combined menu-entries of the user-menu-extensions of
`ecb-methods-menu-user-extension' and the built-in-menu
`ecb-methods-menu'. If nil then no special sorting will be done and the
user-extensions are placed in front of the built-in-entries.

For the guidelines for such a sorter-function see
`ecb-directories-menu-sorter'."
  :group 'ecb-methods
  :type '(choice :tag "Menu-sorter" :menu-tag "Menu-sorter"
                 (const :tag "No special sorting" :value nil)
                 (function :tag "Sort-function" :value identity)))

(defcustom ecb-history-menu-sorter nil
  "*Function which re-sorts the menu-entries of the directories buffer.
If a function then this function is called to sort the menu-entries of the
combined menu-entries of the user-menu-extensions of
`ecb-history-menu-user-extension' and the built-in-menu
`ecb-history-menu'. If nil then no special sorting will be done and the
user-extensions are placed in front of the built-in-entries.

For the guidelines for such a sorter-function see
`ecb-directories-menu-sorter'."
  :group 'ecb-history
  :type '(choice :tag "Menu-sorter" :menu-tag "Menu-sorter"
                 (const :tag "No special sorting" :value nil)
                 (function :tag "Sort-function" :value identity)))

(defcustom ecb-activate-before-layout-draw-hook nil
  "*Hook run at the end of activating ECB by `ecb-activate'.
These hooks run after all the internal setup process but directly before\(!)
drawing the layout specified in `ecb-layout' \(means before dividing the frame
into several windows). A senseful using of this hook can be maximizing the
Emacs-frame for example, because this should be done before the layout is
drawn because ECB computes the size of the ECB-windows with the current frame
size! If you need a hook-option for the real end of the activating process
\(i.e. after the layout-drawing) look at `ecb-activate-hook'.

IMPORTANT: The difference between this hook and
`ecb-redraw-layout-before-hook' is that the latter one is evaluated always
before the layout is redrawn \(for example after calling `ecb-redraw-layout')
whereas the former one \(this hook) is only evaluated exactly once during the
activation-process of ECB. So during the activation process there is the
following sequence of hooks:
1. 'ecb-activate-before-layout-draw-hook' \(this one)
2. `ecb-redraw-layout-before-hook'
3. <Drawing the layout>
4. `ecb-redraw-layout-after-hook'
5. `ecb-activate-hook'"
  :group 'ecb-general
  :type 'hook)


(defcustom ecb-before-activate-hook nil
  "*Hook run at the beginning of activating ECB by `ecb-activate'.
These hooks run before any other tasks of the activating process are
performed. If any of these hooks returns nil then ECB will not be activated!

This can be used to check some conditions and then only start ECB if all
conditions are true. For example a function could be added which returns only
nil if Gnus is running. Then calling `ecb-activate' or `ecb-minor-mode' will
only start ECB if Gnus is not already running."
  :group 'ecb-general
  :type 'hook)


(defcustom ecb-activate-hook nil
  "*Hook run at the end of activating ECB by `ecb-activate'.
These hooks run at the real end of the activating process, means after the
layout has been drawn!. If you need hooks which are run direct before the
layout-drawing look at `ecb-activate-before-layout-draw-hook'."
  :group 'ecb-general
  :type 'hook)

(defcustom ecb-deactivate-hook nil
  "*Hook run at the end of deactivating ECB by `ecb-deactivate'.
These hooks run before the ecb-layout is cleared!"
  :group 'ecb-general
  :type 'hook)

(defcustom ecb-before-deactivate-hook nil
  "*Hook run at the beginning of deactivating ECB by `ecb-deactivate'.
These hooks run before any other tasks of the deactivating process are
performed. If any of these hooks returns nil then ECB will not be deactivated!
See also `ecb-before-activate-hook'."
  :group 'ecb-general
  :type 'hook)

(defcustom ecb-current-buffer-sync-hook nil
  "*Hook run at the end of `ecb-current-buffer-sync'.
See documentation of `ecb-current-buffer-sync' for conditions when
synchronization takes place and so in turn these hooks are evaluated.

Precondition for such a hook:
Current buffer is the buffer of the currently selected edit-window.

Postcondition for such a hook:
Point must stay in the same edit-window as before evaluating the hook.

Important note: If `ecb-window-sync' is not nil `ecb-current-buffer-sync' is
running either every time Emacs is idle or even after every command \(see
`ecb-window-sync-delay'). So these hooks can be really called very often!
Therefore each function of this hook should/must check in an efficient way at
beginning if its task have to be really performed and then do them only if
really necessary! Otherwise performance of Emacs could slow down
dramatically!

It is strongly recommended that each function added to this hook uses the
macro `ecb-do-if-buffer-visible-in-ecb-frame' at beginning! See
`ecb-speedbar-current-buffer-sync' and `ecb-eshell-current-buffer-sync' for
examples how to use this macro!"
  :group 'ecb-general
  :type 'hook)

(defcustom ecb-common-tree-buffer-after-create-hook nil
  "*Local hook running at the end of each tree-buffer creation.
Every function of this hook is called once without arguments direct after
creating a tree-buffer of ECB and it's local key-map. So for example a function
could be added which performs calls of `local-set-key' to define new
key-bindings for EVERY tree-buffer.

The following keys must not be rebind in all tree-buffers:
- <RET> and all combinations with <Shift> and <Ctrl>
- <TAB>
- `C-t'"
  :group 'ecb-general
  :type 'hook)

(defcustom ecb-directories-buffer-after-create-hook nil
  "*Local hook running after the creation of the directories-buffer.
Every function of this hook is called once without arguments direct after
creating the directories-buffer of ECB and it's local key-map. So for example a
function could be added which performs calls of `local-set-key' to define new
key-bindings only for the directories-buffer of ECB.

The following keys must not be rebind in the directories-buffer:
<F2>, <F3> and <F4>"
  :group 'ecb-directories
  :type 'hook)

(defcustom ecb-sources-buffer-after-create-hook nil
  "*Local hook running after the creation of the sources-buffer.
Every function of this hook is called once without arguments direct after
creating the sources-buffer of ECB and it's local key-map. So for example a
function could be added which performs calls of `local-set-key' to define new
key-bindings only for the sources-buffer of ECB."
  :group 'ecb-sources
  :type 'hook)

(defcustom ecb-methods-buffer-after-create-hook nil
  "*Local hook running after the creation of the methods-buffer.
Every function of this hook is called once without arguments direct after
creating the methods-buffer of ECB and it's local key-map. So for example a
function could be added which performs calls of `local-set-key' to define new
key-bindings only for the methods-buffer of ECB."
  :group 'ecb-methods
  :type 'hook)

(defcustom ecb-history-buffer-after-create-hook nil
  "*Local hook running after the creation of the history-buffer.
Every function of this hook is called once without arguments direct after
creating the history-buffer of ECB and it's local key-map. So for example a
function could be added which performs calls of `local-set-key' to define new
key-bindings only for the history-buffer of ECB."
  :group 'ecb-history
  :type 'hook)

(defcustom ecb-process-non-semantic-files (if (locate-library "speedbar")
                                              t)
  "*Display contents of non-semantic-files in the ECB-methods-buffer.
See also `ecb-non-semantic-parsing-function'."
  :group 'ecb-general
  :group 'ecb-non-semantic
  :type 'boolean)

(defcustom ecb-non-semantic-parsing-function nil
  "*Define mode-dependent parsing functions for non-semantic files.
This is an alist where the car is a major-mode symbol and the cdr is a
function-symbol of a function which should be used for parsing a non-semantic
buffer, i.h. a buffer for which no semantic grammar exists. Such a function
gets one argument - the filename of current buffer - and has to generate and
return a token/tag list which is understandable by
`speedbar-insert-generic-list'. speedbar has already included two functions
`speedbar-fetch-dynamic-imenu' and `speedbar-fetch-dynamic-etags' which can be
used for parsing buffers with imenu rsp. etags.

This option takes only effect if `ecb-process-non-semantic-files' is not nil:
Then ECB checks for non-semantic buffers if current `major-mode' is contained
in this option and if yes, then the specified parsing function is called;
if not then the cars of the elements of `speedbar-dynamic-tags-function-list'
are called in that sequence they are listed in this variable. See option
`speedbar-dynamic-tags-function-list' for further details.

In most cases imenu-parsing is preferable over etags-parsing because imenu
operates on Emacs-buffers and needs no external tool and therefore parsing
works also if current contents of a buffer are not saved to disk. But maybe
sometimes etags may return better parsing results.

IMPORTANT: if imenu-parsing should be used then the option
`speedbar-use-imenu-flag' must be set to not nil!"
  :group 'ecb-methods
  :group 'ecb-non-semantic
  :type '(repeat (cons (symbol :tag "Major-mode")
                       (function :tag "Parsing function"))))


(defcustom ecb-non-semantic-methods-initial-expand nil
  "*Initially expand all tokens for not by semantic supported sources.
This option can be customized on a major-mode basis, i.e. if a `major-mode' is
contained in this option then all tokens for this modes will be initially
expanded - otherwise not."
  :group 'ecb-methods
  :group 'ecb-non-semantic
  :type '(repeat :tag "Expand this modes"
                 (symbol :tag "major mode")))

(defcustom ecb-auto-save-before-etags-methods-rebuild t
  "*Automatic saving of current buffer before rebuilding its methods.
This option is only relevant for sources which are supported and parsed by
etags \(see `ecb-process-non-semantic-files'). Because etags is an external
tool a source-buffer can only be reparsed if the buffer is saved to disk. So
the command `ecb-rebuild-methods-buffer' checks for sources which are not
supported by semantic or imenu if either this option is t or if the major-mode
of the source-buffer is contained in this list: In both cases ECB saves the
current source-buffer before it re-runs etags for reparsing the source.
If nil or if the major-mode is not contained then no automatic saving will be
done!

For all source supported by semantic or by imenu this option takes no effect."
  :group 'ecb-methods
  :group 'ecb-non-semantic
  :type '(radio (const :tag "For all etags modes" :value t)
                (repeat :tag "For these modes" (symbol :tag "Major-mode"))))

(defcustom ecb-non-semantic-exclude-modes '(sh-mode fundamental-mode text-mode)
  "*Exclude modes from parsing with imenu or etags.
Per default, ECB tries to parse all file-types not supported by semantic with
imenu or etags or some other method \(for details see the option
`ecb-non-semantic-parsing-function'). If a file-type can not be parsed by
semantic, imenu or etags than this simply results in an empty method-buffer
for this file. But nevertheless you will get a message \"Sorry, no support for
a file of that extension\" which comes from the speedbar-library and can not
switched off. Therefore if a `major-mode' is known as not parse-able by
semantic, imenu or etags it can be added to this option and then it will be
excluded from being tried to parsed."
  :group 'ecb-non-semantic
  :type '(repeat :tag "Modes to exclude"
                 (symbol :tag "Major-mode")))

(defcustom ecb-rebuild-non-semantic-methods-before-hook nil
  "*Hook at beginning of `ecb-rebuild-methods-buffer-for-non-semantic'.
So this function is always called by the command `ecb-rebuild-methods-buffer'
for not semantic supported source-types.

Every function of this hook gets one argument: The complete filename of the
current source-buffer in the edit-window. The Method-buffer is only rebuild by
`ecb-rebuild-methods-buffer-for-non-semantic' if either the hook contains no
function \(the default) or if no function of this hook returns nil! See
`run-hook-with-args-until-failure' for description how these function are
processed."
  :group 'ecb-methods
  :group 'ecb-non-semantic
  :type 'hook)
  
;;====================================================
;; Internals
;;====================================================


(defun ecb-enter-debugger (&rest error-args)
  "If `ecb-debug-mode' is not nil then enter the Emacs-debugger and signal an
error with ERROR-ARGS."
  (when ecb-debug-mode
    (let ((debug-on-error t))
      (apply 'error error-args))))

;; encapsulation all semantic-functions ECB uses if they operate with the
;; semantic-overlays, so we can handle an error if these overlays (extends for
;; XEmacs) are destroyed and invalid cause of some mysterious circumstances.

(defun ecb-semantic-assert-valid-token (token &optional no-reparse)
  "Assert that TOKEN is a valid token. If not valid then `ecb-enter-debugger'
is called. If NO-REPARSE is not nil then the buffer is not autom. reparsed. It
returns nil if the assertion fails otherwise not nil. So the caller can even
check the result if `ecb-debug-mode' is nil in which case the function
`ecb-enter-debugger' is a no-op."
  (if (semantic-token-p token)
      (if (semantic-token-with-position-p token)
          (let ((o  (semantic-token-overlay token)))
            (if (and (semantic-overlay-p o)
                     (not (semantic-overlay-live-p o)))
                (progn
                  (when (not no-reparse)
                    ;; we need this because:
                    ;; 1. After every jump to a token X via the method-buffer of
                    ;;    ECB this token X is added to the navigation history list
                    ;;    as new ecb-nav-token-history-item.
                    ;; 2. Before every select of a source in the sources- or
                    ;;    history-buffer or of a node in the method-buffer
                    ;;    `ecb-nav-save-current' is called which operates onto
                    ;;    the last saved history-item which is often a
                    ;;    token-history-item (see 1.): `ecb-nav-save-current'
                    ;;    saves for token-history-items current-position and
                    ;;    window-start relative to the token position of the
                    ;;    last saved token-history-item which is token X from
                    ;;    1.
                    ;; Now suppose that after 1. and before 2. the overlay of
                    ;; token X has been destroyed cause of some reason. Then
                    ;; the token-history-item of 1. contains now a token with
                    ;; a destroyed overlay. Now step 2. is performed and now
                    ;; we see why from this moment every click onto a node in
                    ;; the source-, history- or method-buffer must fail:
                    ;; During step 2. `ecb-nav-save-current' gets the token
                    ;; from the last token-history-item and calls for this
                    ;; token `semantic-token-start' which fails now because
                    ;; the contained overlay of this token is destroyed in the
                    ;; meanwhile. Therefore we must throw away this last
                    ;; token-history-item containing the token with the
                    ;; destroyed overlay. Then after a complete reparse of the
                    ;; source-buffer and following rebuild of the
                    ;; ECB-method-buffer ECB is in correct state again!
                    (ecb-nav-initialize)
                    (semantic-clear-toplevel-cache)
                    (ecb-update-methods-buffer--internal))
                  (ecb-enter-debugger "Token %S is invalid!" token)
                  nil)
              ;; else, token is OK.
              t))
        ;; Position-less tokens are also OK.
        t)
    ;; For no semantic-tokens a reparse makes no sense!
    (ecb-enter-debugger "Not a semantic token: %S" token)
    nil))


(defun ecb-semantic-token-buffer (token)
  (ecb-semantic-assert-valid-token token)
  ;; if ecb-debug-mode is not nil then the TOKEN is valid if we pass the
  ;; assert. If ecb-debug-mode is nil then we call simply the semantic
  ;; function and see what happens.
  (semantic-token-buffer token))

(defun ecb-semantic-token-start (token)
  (ecb-semantic-assert-valid-token token)
  ;; if ecb-debug-mode is not nil then the TOKEN is valid if we pass the
  ;; assert. If ecb-debug-mode is nil then we call simply the semantic
  ;; function and see what happens.
  (semantic-token-start token))

(defun ecb-semantic-token-end (token)
  (ecb-semantic-assert-valid-token token)
  ;; if ecb-debug-mode is not nil then the TOKEN is valid if we pass the
  ;; assert. If ecb-debug-mode is nil then we call simply the semantic
  ;; function and see what happens.
  (semantic-token-end token))

;; Klaus: We must not reparse the buffer if `semantic-current-nonterminal'
;; returns nil because here this is no error but nil is always returned for
;; example if point stays within a comment. Therefore here we only catch real
;; errors!
(defun ecb-semantic-current-nonterminal ()
  (condition-case nil
      (semantic-current-nonterminal)
    (error (message "semantic-current-nonterminal has problems --> reparsed is performed!")
           (when (ecb-point-in-edit-window)
             (semantic-clear-toplevel-cache)
             (ecb-update-methods-buffer--internal)
             (semantic-current-nonterminal)))))
  
;; macros and functions selecting ecb-windows or operating in ecb-windows

(defmacro ecb-exec-in-directories-window (&rest body)
  `(unwind-protect
       (when (ecb-window-select ecb-directories-buffer-name)
	 ,@body)
     ))

(defmacro ecb-exec-in-sources-window (&rest body)
  `(unwind-protect
       (when (ecb-window-select ecb-sources-buffer-name)
	 ,@body)
     ))

(defmacro ecb-exec-in-methods-window (&rest body)
  `(unwind-protect
       (when (ecb-window-select ecb-methods-buffer-name)
	 ,@body)
     ))

(defmacro ecb-exec-in-history-window (&rest body)
  `(unwind-protect
       (when (ecb-window-select ecb-history-buffer-name)
	 ,@body)
     ))

(defun ecb-window-select (name)
  (let ((window (get-buffer-window name)))
    (if window
	(select-window window)
      nil)))

(defun ecb-goto-window (name)
  (when ecb-minor-mode
    (raise-frame ecb-frame)
    (select-frame ecb-frame)
    (ecb-window-select name)))

(defun ecb-goto-window-directories ()
  "Make the ECB-directories window the current window.
If `ecb-use-speedbar-instead-native-tree-buffer' is 'dir then goto to the
speedbar-window."
  (interactive)
  (or (ecb-goto-window ecb-directories-buffer-name)
      (and (equal ecb-use-speedbar-instead-native-tree-buffer 'dir)
           (ecb-goto-window-speedbar))))

(defun ecb-goto-window-sources ()
  "Make the ECB-sources window the current window.
If `ecb-use-speedbar-instead-native-tree-buffer' is 'source then goto to the
speedbar-window."
  (interactive)
  (or (ecb-goto-window ecb-sources-buffer-name)
      (and (equal ecb-use-speedbar-instead-native-tree-buffer 'source)
           (ecb-goto-window-speedbar))))

(defun ecb-goto-window-methods ()
  "Make the ECB-methods window the current window.
If `ecb-use-speedbar-instead-native-tree-buffer' is 'method then goto to the
speedbar-window."
  (interactive)
  (or (ecb-goto-window ecb-methods-buffer-name)
      (and (equal ecb-use-speedbar-instead-native-tree-buffer 'method)
           (ecb-goto-window-speedbar))))

(defun ecb-goto-window-history ()
  "Make the ECB-history window the current window."
  (interactive)
  (ecb-goto-window ecb-history-buffer-name))

(defun ecb-goto-window-speedbar ()
  "Make the ECB-speedbar window the current window.
This command does nothing if no integrated speedbar is visible in the
ECB-frame."
  (interactive)
  (and (ecb-speedbar-active-p)
       (ecb-goto-window ecb-speedbar-buffer-name)))

(defun ecb-goto-window-edit1 ()
  "Make the \(first) edit-window window the current window."
  (interactive)
  (when ecb-minor-mode
    (raise-frame ecb-frame)
    (select-frame ecb-frame)
    (ecb-select-edit-window nil)))

(defun ecb-goto-window-edit2 ()
  "Make the second edit-window \(if available) window the current window."
  (interactive)
  (when ecb-minor-mode
    (raise-frame ecb-frame)
    (select-frame ecb-frame)
    (ecb-select-edit-window t)))

(defun ecb-goto-window-compilation ()
  "Goto the ecb compilation window `ecb-compile-window'."
  (interactive)
  (when (and ecb-minor-mode
             ecb-compile-window-height
             ecb-compile-window
             (window-live-p ecb-compile-window))
    (raise-frame ecb-frame)
    (select-frame ecb-frame)
    (select-window ecb-compile-window)))

(defun ecb-buffer-select (name)
  (set-buffer (get-buffer name)))

(defun ecb-maximize-window-directories ()
  "Maximize the ECB-directories-window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works also if the
ECB-directories-window is not visible in current layout."
  (interactive)
  (if (equal ecb-use-speedbar-instead-native-tree-buffer 'dir)
      (ecb-maximize-window-speedbar)
    (ecb-display-one-ecb-buffer ecb-directories-buffer-name)))

(defun ecb-maximize-window-sources ()
  "Maximize the ECB-sources-window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works also if the
ECB-sources-window is not visible in current layout."
  (interactive)
  (if (equal ecb-use-speedbar-instead-native-tree-buffer 'source)
      (ecb-maximize-window-speedbar)
    (ecb-display-one-ecb-buffer ecb-sources-buffer-name)))

(defun ecb-maximize-window-methods ()
  "Maximize the ECB-methods-window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works also if the
ECB-methods-window is not visible in current layout."
  (interactive)
  (if (equal ecb-use-speedbar-instead-native-tree-buffer 'method)
      (ecb-maximize-window-speedbar)
    (ecb-display-one-ecb-buffer ecb-methods-buffer-name)))

(defun ecb-maximize-window-history ()
  "Maximize the ECB-history-window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works also if the
ECB-history-window is not visible in current layout."
  (interactive)
  (ecb-display-one-ecb-buffer ecb-history-buffer-name))

(defun ecb-maximize-window-speedbar ()
  "Maximize the ECB-speedbar-window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Does nothing if the
speedbar-window is not visible within the ECB-frame."
  (interactive)
  (if (ecb-speedbar-active-p)
      (ecb-display-one-ecb-buffer ecb-speedbar-buffer-name)))

(defun ecb-toggle-RET-selects-edit-window ()
  "Toggles if RET in a tree-buffer should finally select the edit-window.
See also the option `ecb-tree-RET-selects-edit-window'."
  (interactive)
  (let ((tree-buffer (ecb-point-in-tree-buffer)))
    (if tree-buffer
        (if (member (buffer-name tree-buffer)
                    ecb-tree-RET-selects-edit-window--internal)
            (progn
              (setq ecb-tree-RET-selects-edit-window--internal
                    (delete (buffer-name tree-buffer)
                            ecb-tree-RET-selects-edit-window--internal))
              (message "RET does not select the edit-window."))
          (setq ecb-tree-RET-selects-edit-window--internal
                (append ecb-tree-RET-selects-edit-window--internal
                        (list (buffer-name tree-buffer))))
          (message "RET selects the edit-window."))
      (message "Point must stay in an ECB tree-buffer!"))))

(defun ecb-create-node (parent-node display name data type)
  (if (eq 'hidden display)
      nil
    (if (eq 'flattened display)
	parent-node
      (let ((node (tree-node-new name type data nil parent-node
				 (if ecb-truncate-long-names 'end))))
	(when (eq 'expanded display)
	  (tree-node-set-expanded node t))
	node))))

(defun ecb-get-token-type-display (token-type)
  (let ((display (find token-type ecb-show-tokens :test
		       (function (lambda (a b) (eq a (car b)))))))
    (if display
	display
      (setq display (find t ecb-show-tokens :test
			  (function (lambda (a b) (eq a (car b))))))
      (if display
	  display
	'(t hidden nil)))))

(defun ecb-get-token-parent-names (parents)
  (when parents
    (let* ((parent (car parents))
	   (name (cond
		  ((semantic-token-p parent)
		   (semantic-name-nonterminal parent nil ecb-font-lock-tokens))
		  ((stringp parent)
		   (semantic-colorize-text parent 'type)))))
      (if name
	  (if (and ecb-exclude-parents-regexp
		   (string-match ecb-exclude-parents-regexp name))
	      (ecb-get-token-parent-names (cdr parents))
	    (cons name (ecb-get-token-parent-names (cdr parents))))
	(if (listp parent)
	    (append (ecb-get-token-parent-names parent)
		    (ecb-get-token-parent-names (cdr parents))))))))

(defun ecb-get-token-parents (token)
  (ecb-get-token-parent-names (semantic-token-type-parent token)))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: A first try to display protection
;; symbols with image icons...works fine..but this should really be done by
;; semantic or should it not?!

;; (defun ecb-get-token-name (token &optional
;; parent-token)
;;   "Get the name of TOKEN with the appropriate fcn from
;; `ecb-token-display-function'."
;;   (condition-case nil
;;       (let* ((mode-display-fkt (cdr (assoc major-mode ecb-token-display-function)))
;;              (default-fkt (cdr (assoc 'default ecb-token-display-function)))
;;              (display-fkt (or (and (fboundp mode-display-fkt) mode-display-fkt)
;;                               (and (fboundp default-fkt) default-fkt)
;;                               'semantic-prototype-nonterminal))
;;              (token-name (funcall display-fkt
;;                                   token parent-token ecb-font-lock-tokens)))
;;         (if (equal (semantic-nonterminal-protection token) 'private)
;;             (tree-buffer-add-image-icon-maybe 0 1 token-name 'speedbar-read-only)
;;           token-name))
;;     (error (semantic-prototype-nonterminal token parent-token
;;                                            ecb-font-lock-tokens))))

(defun ecb-get-token-name (token &optional parent-token)
  "Get the name of TOKEN with the appropriate fcn from
`ecb-token-display-function'."
  (condition-case nil
      (let* ((mode-display-fkt (cdr (assoc major-mode ecb-token-display-function)))
             (default-fkt (cdr (assoc 'default ecb-token-display-function)))
             (display-fkt (or (and (fboundp mode-display-fkt) mode-display-fkt)
                              (and (fboundp default-fkt) default-fkt)
                              'semantic-prototype-nonterminal)))
        (funcall display-fkt token parent-token ecb-font-lock-tokens))
    (error (semantic-prototype-nonterminal token parent-token
                                           ecb-font-lock-tokens))))

(defun ecb-find-add-token-bucket (node type display sort-method buckets
                                       &optional parent-token no-bucketize)
  "Finds a bucket containing tokens of the given type, creates nodes for them
and adds them to the given node. The bucket is removed from the buckets list.
PARENT-TOKEN is only propagated to `ecb-add-token-bucket'."
  (when (cdr buckets)
    (let ((bucket (cadr buckets)))
      (if (eq type (semantic-token-token (cadr bucket)))
	  (progn
	    (ecb-add-token-bucket node bucket display sort-method parent-token
                                  no-bucketize)
	    (setcdr buckets (cddr buckets)))
	(ecb-find-add-token-bucket node type display sort-method
				   (cdr buckets) parent-token no-bucketize)))))

(defun ecb-format-bucket-name (name)
  (let ((formatted-name (concat (nth 0 ecb-bucket-token-display)
				name
				(nth 1 ecb-bucket-token-display))))
    (setq formatted-name (ecb-merge-face-into-text formatted-name (nth 2 ecb-bucket-token-display)))
    formatted-name))

(defun ecb-add-token-bucket (node bucket display sort-method
                                  &optional parent-token no-bucketize)
  "Adds a token bucket to a node unless DISPLAY equals 'hidden."
  (when bucket
    (let ((name (ecb-format-bucket-name (car bucket)))
          ;;(type (semantic-token-token (cadr bucket)))
	  (bucket-node node))
      (unless (eq 'hidden display)
	(unless (eq 'flattened display)
	  (setq bucket-node (tree-node-new name 1 nil nil node
					   (if ecb-truncate-long-names 'end)))
	  (tree-node-set-expanded bucket-node (eq 'expanded display)))
	(dolist (token (ecb-sort-tokens sort-method (cdr bucket)))
          ;;           (semantic-token-put token 'parent-token parent-token)
	  (ecb-update-token-node token
                                 (tree-node-new "" 0 token t bucket-node
                                                (if ecb-truncate-long-names 'end))
                                 parent-token no-bucketize))))))

(defun ecb-update-token-node (token node &optional parent-token no-bucketize)
  "Updates a node containing a token."
  (let* ((children (semantic-nonterminal-children
                    token ecb-show-only-positioned-tokens)))
    (tree-node-set-name node (ecb-get-token-name token parent-token))
    ;; Always expand types, maybe this should be customizable and more
    ;; flexible
    (tree-node-set-expanded node (eq 'type (semantic-token-token token)))
    (unless (eq 'function (semantic-token-token token))
      (ecb-add-tokens node children token no-bucketize)
      (tree-node-set-expandable
       node (not (eq nil (tree-node-get-children node)))))))

(defun ecb-post-process-tokenlist (tokenlist)
  "If for current major-mode a post-process function is found in
`ecb-post-process-semantic-tokenlist' then this function is called with
TOKENLIST otherwise TOKENLIST is returned."
  (let ((fcn (cdr (assoc major-mode ecb-post-process-semantic-tokenlist))))
    (if (fboundp fcn)
        (funcall fcn tokenlist)
      tokenlist)))

(defun ecb-group-function-tokens-with-parents (tokenlist)
  "Return a new tokenlist based on TOKENLIST where all function-tokens in
TOKENLIST having a parent token are grouped together under a new faux token
for this parent-token. The new tokenlist contains first all parent-less tokens
and then all grouped tokens.

This is useful for oo-programming languages where the methods of a class can
be defined outside the class-definition, e.g. C++, Eieio."
  (if (fboundp 'semantic-adopt-external-members)
      (semantic-adopt-external-members tokenlist)
    (let ((parent-alist nil)
          (parents nil)
          (parentless nil))
      (while tokenlist
        (cond ((and (eq (semantic-token-token (car tokenlist)) 'function)
                    (semantic-token-function-parent (car tokenlist)))
               ;; Find or Create a faux parent token in `parents'
               ;; and add this token to it.
               (let ((elem (assoc (semantic-token-function-parent (car tokenlist))
                                  parent-alist)))
                 (if elem
                     (setcdr elem (cons (car tokenlist) (cdr elem)))
                   (setq parent-alist
                         (cons (cons (semantic-token-function-parent (car
                                                                      tokenlist))
                                     (list (car tokenlist)))
                               parent-alist)))))
              (t
               (setq parentless (cons (car tokenlist) parentless))))
        (setq tokenlist (cdr tokenlist)))
      ;; now we have an alist with an element for each parent where the key is
      ;; the class-name (string) and the value is a list of all method-tokens
      ;; for this class.

      ;; Now we must build a new token-list
      (dolist (alist-elem parent-alist)
        (let ((group-token (list (concat (car alist-elem) " (Methods)")
                                 'type
                                 ;; if we set "struct" the protection will be
                                 ;; public, with "class" it will be private.
                                 ;; Unfortunately there is no way to display
                                 ;; the right protection, but i think public
                                 ;; is better then private. The best would be
                                 ;; a blank protection symbol but this will
                                 ;; be first available with semantic-1.4beta13.
                                 "struct"
                                 ;; the PART-LIST, means all the methods of
                                 ;; this class. But first we must nreverse
                                 ;; the list because we have build the list
                                 ;; with cons.
                                 (nreverse (cdr alist-elem))
                                 nil nil nil nil nil)))
          ;; now we mark our new group token
          (semantic-token-put group-token 'ecb-group-token t)
          (setq parents (cons group-token parents))))

      ;; We nreverse the parent-less (because build with cons) and append then
      ;; all the parents.
      (append (nreverse parentless) parents))))

(defun ecb-dump-semantic-toplevel ()
  "Dump the current semantic-tokens in special buffer and display them."
  (interactive)
  (let ((tokens (ecb-post-process-tokenlist (semantic-bovinate-toplevel t))))
    (save-selected-window
      (set-buffer (get-buffer-create "ecb-dump"))
      (erase-buffer)
      (ecb-dump-tokens tokens "")
      (switch-to-buffer-other-window (get-buffer-create "ecb-dump"))
      (goto-char (point-min)))))

(defun ecb-dump-type (tok prefix)
  (dolist (parent (ecb-get-token-parents tok))
    (insert prefix "  " parent)))

(defun ecb-dump-tokens (tokens prefix)
  (dolist (tok tokens)
    (if (stringp tok)
	(princ (concat prefix tok))
      (insert prefix
              (semantic-name-nonterminal tok nil ecb-font-lock-tokens)
              ", "
              (symbol-name (semantic-token-token tok))
              ", "
              (if (stringp (semantic-token-type tok))
                  (semantic-token-type tok)
                "<unknown type>")
              "\n")
      (if (eq 'type (semantic-token-token tok))
	  (ecb-dump-type tok prefix))
      (ecb-dump-tokens (semantic-nonterminal-children
                        tok ecb-show-only-positioned-tokens)
                       (concat prefix "  ")))))

(defun ecb-add-tokens (node tokens &optional parent-token no-bucketize)
  "If NO-BUCKETIZE is not nil then TOKENS will not bucketized by
`semantic-bucketize' but must already been bucketized!"
  (ecb-add-token-buckets node parent-token
                         (if no-bucketize
                             tokens
                           (semantic-bucketize tokens))
                         no-bucketize))

(defun ecb-access-order (access)
  (cond
   ((eq 'public access) 0)
   ((eq 'protected access) 1)
   ((eq 'private access) 3)
   (t  2)))

(defun ecb-sort-tokens (sort-method tokens)
  (if sort-method
      (let ((tokens-by-name
	     (sort tokens (function (lambda (a b)
				      (string< (semantic-token-name a)
					       (semantic-token-name b)))))))
	(if (eq 'access sort-method)
	    (sort tokens-by-name
		  (function
		   (lambda (a b)
		     (< (ecb-access-order (semantic-nonterminal-protection a))
			(ecb-access-order (semantic-nonterminal-protection b))))))
	  tokens-by-name))
    tokens))

(defun ecb-add-token-buckets (node parent-token buckets &optional no-bucketize)
  "Creates and adds token nodes to the given node.
The PARENT-TOKEN is propagated to the functions `ecb-add-token-bucket' and
`ecb-find-add-token-bucket'."
  (setq buckets (cons nil buckets))
  (dolist (token-display ecb-show-tokens)
    (let* ((type (car token-display))
           (display (cadr token-display))
           (sort-method (caddr token-display)))
      (cond
       ((eq 'parent type)
 	(when (and parent-token
 		   (eq 'type (semantic-token-token parent-token)))
 	  (let ((parents (ecb-get-token-parents parent-token)))
	    (when parents
	      (let ((node (ecb-create-node node display (ecb-format-bucket-name "Parents") nil 1)))
		(when node
		  (dolist (parent (if sort-method
				      (sort parents 'string<) parents))
		    (tree-node-new (if ecb-font-lock-tokens
				       (semantic-colorize-text parent 'type)
				     parent)
				   2 parent t node
				   (if ecb-truncate-long-names 'end)))))))))
       (t (ecb-find-add-token-bucket node type display sort-method buckets
                                     parent-token no-bucketize)))))
  (let ((type-display (ecb-get-token-type-display t)))
    (dolist (bucket buckets)
      (ecb-add-token-bucket node bucket (cadr type-display)
                            (caddr type-display) parent-token no-bucketize))))

(defun ecb-update-after-partial-reparse (updated-tokens)
  "Updates the method buffer and all internal ECB-caches after a partial
semantic-reparse. This function is added to the hook
`semantic-after-partial-cache-change-hook'."
  ;; TODO: Currently we get simply the whole cache from semantic (already up
  ;; to date at this time!) and then we rebuild the whole tree-buffer with
  ;; this cache-contents. This is for great sources slow. We should implement
  ;; a mechanism where only the UPDATED-TOKENS are used and only this ones are
  ;; updated. But for this we need also a tree-buffer-update which can update
  ;; single nodes without refreshing the whole tree-buffer like now.
  (ecb-rebuild-methods-buffer-with-tokencache (semantic-bovinate-toplevel t)))


(defun ecb-expand-directory-tree (path node)
  (catch 'exit
    (dolist (child (tree-node-get-children node))
      (let ((data (tree-node-get-data child)))
        (when (and (>= (length path) (length data))
                   (string= (substring path 0 (length data)) data)
                   (or (= (length path) (length data))
                       (eq (elt path (length data)) ecb-directory-sep-char)))
          (let ((was-expanded (tree-node-is-expanded child)))
            (tree-node-set-expanded child t)
            (ecb-update-directory-node child)
            (throw 'exit
                   (or (when (> (length path) (length data))
                         (ecb-expand-directory-tree path child))
                       (not was-expanded)))))))))

(defvar ecb-files-and-subdirs-cache nil
  "Cache for every directory all subdirs and files. This is an alist where an
element looks like:
   \(<directory> . \(<file-list> . <subdirs-list>))")

(defun ecb-files-and-subdirs-cache-add (cache-elem)
  (if (not (ecb-files-and-subdirs-cache-get (car cache-elem)))
      (setq ecb-files-and-subdirs-cache
            (cons cache-elem ecb-files-and-subdirs-cache))))

(defun ecb-files-and-subdirs-cache-get (dir)
  (cdr (assoc dir ecb-files-and-subdirs-cache)))

(defun ecb-files-and-subdirs-cache-remove (dir)
  (let ((elem (assoc dir ecb-files-and-subdirs-cache)))
    (if elem
        (setq ecb-files-and-subdirs-cache
              (delete elem ecb-files-and-subdirs-cache)))))

(defun ecb-clear-files-and-subdirs-cache ()
  (setq ecb-files-and-subdirs-cache nil))

(defun ecb-check-directory-for-caching (dir number-of-contents)
  "Return not nil if DIR matches any regexp in `ecb-cache-directory-contents'
and NUMBER-OF-CONTENTS is greater then the related threshold."
  (catch 'exit
    (dolist (elem ecb-cache-directory-contents)
      (let ((case-fold-search t))
        (save-match-data
          (if (and (string-match (car elem) dir)
                   (> number-of-contents (cdr elem)))
              (throw 'exit (car elem))))
        nil))))

(defun ecb-check-directory-for-source-regexps (dir)
  "Return the related source-exclude-include-regexps of
`ecb-source-file-regexps' if DIR matches any directory-regexp in
`ecb-source-file-regexps'."
  (catch 'exit
    (dolist (elem ecb-source-file-regexps)
      (let ((case-fold-search t))
        (save-match-data
          (if (string-match (car elem) dir)
              (throw 'exit (cdr elem))))
        nil))))


(defun ecb-files-from-cvsignore (dir)
  "Return an expanded list of filenames which are excluded by the .cvsignore
file in current directory."
  (let ((cvsignore-content (ecb-file-content-as-string
                            (expand-file-name ".cvsignore" dir)))
        (files nil))
    (when cvsignore-content
      (dolist (f (split-string cvsignore-content))
        (setq files (append (directory-files dir nil (wildcard-to-regexp f) t)
                            files)))
      files)))

(defun ecb-check-directory-for-cvsignore-exclude (dir)
  "Return not nil if DIR matches a regexp in `ecb-sources-exclude-cvsignore'."
  (catch 'exit
    (dolist (elem ecb-sources-exclude-cvsignore)
      (let ((case-fold-search t))
        (save-match-data
          (if (string-match elem dir)
              (throw 'exit elem)))
        nil))))


(defun ecb-get-files-and-subdirs (dir)
  "Return a cons cell where car is a list of all files to display in DIR and
cdr is a list of all subdirs to display in DIR. Both lists are sorted
according to `ecb-sources-sort-method'."
  (or (ecb-files-and-subdirs-cache-get dir)
      ;; dir is not cached
      (let ((files (directory-files dir nil nil t))
            (source-regexps (or (ecb-check-directory-for-source-regexps
                                 (ecb-fix-filename dir))
                                '("" "")))
            (cvsignore-files (if (ecb-check-directory-for-cvsignore-exclude dir)
                                 (ecb-files-from-cvsignore dir)))
            sorted-files source-files subdirs cache-elem)
        ;; if necessary sort FILES
        (setq sorted-files
              (cond ((equal ecb-sources-sort-method 'name)
                     (sort files 'string<))
                    ((equal ecb-sources-sort-method 'extension)
                     (sort files (function
                                  (lambda(a b)
                                    (let ((ext-a (file-name-extension a t))
                                          (ext-b (file-name-extension b t)))
                                      (if (string= ext-a ext-b)
                                          (string< a b)
                                        (string< ext-a ext-b)))))))
                    (t files)))
        ;; divide real files and subdirs. For really large directories ( ~ >=
        ;; 2000 entries) this is the performance-bottleneck in the
        ;; file-browser of ECB.
        (dolist (file sorted-files)
          (if (file-directory-p (ecb-fix-filename dir file))
              (if (not (string-match ecb-excluded-directories-regexp file))
                  (setq subdirs (append subdirs (list file))))
            (if (and (not (member file cvsignore-files))
                     (or (string-match (cadr source-regexps) file)
                         (not (string-match (car source-regexps) file))))
                (setq source-files (append source-files (list file))))))
        
        (setq cache-elem (cons dir (cons source-files subdirs)))
        ;; check if this directory must be cached
        (if (ecb-check-directory-for-caching dir (length sorted-files))
            (ecb-files-and-subdirs-cache-add cache-elem))
        ;; return the result
        (cdr cache-elem))))


(defvar ecb-sources-cache nil
  "Cache for the contents of the buffer `ecb-sources-buffer-name'. This is an
alist where every element is a cons cell which looks like:
 \(directory . \(tree-buffer-root <copy of tree-buffer-nodes> buffer-string)")

(defun ecb-sources-cache-remove (dir)
  (let ((cache-elem (assoc dir ecb-sources-cache)))
    (if cache-elem
        (setq ecb-sources-cache (delete cache-elem ecb-sources-cache)))))

(defun ecb-sources-cache-add (cache-elem)
  (if (not (ecb-sources-cache-get (car cache-elem)))
      (setq ecb-sources-cache (cons cache-elem ecb-sources-cache))))

(defun ecb-sources-cache-get (dir)
  "Return the value of a cached-directory DIR, means the 3-element-list. If no
cache-entry for DIR is available then nil is returned."
  (cdr (assoc dir ecb-sources-cache)))

(defun ecb-sources-cache-clear ()
  (setq ecb-sources-cache nil))


(defun ecb-set-selected-directory (path &optional force)
  "Set the contents of the ECB-directories and -sources buffer correct for the
value of PATH. If PATH is equal to the value of `ecb-path-selected-directory'
then nothing is done unless first optional argument FORCE is not nil."
  (let ((last-dir ecb-path-selected-directory))
    (save-selected-window
      (setq ecb-path-selected-directory (ecb-fix-filename path))
      ;; if ecb-path-selected-directory has not changed then there is no need
      ;; to do anything here because neither the content of directory buffer
      ;; nor the content of the sources buffer can have been changed!
      (when (or force (not (string= last-dir ecb-path-selected-directory)))
        (when (or (not (ecb-show-sources-in-directories-buffer-p))
                  ecb-auto-expand-directory-tree)
          (ecb-exec-in-directories-window
           (let (start)
             (when ecb-auto-expand-directory-tree
               ;; Expand tree to show selected directory
               (setq start
                     (if (equal ecb-auto-expand-directory-tree 'best)
                         ;; If none of the source-paths in the buffer
                         ;; `ecb-directories-buffer-name' matches then nil
                         ;; otherwise the node of the best matching source-path
                         (cdar (sort (delete nil
                                             (mapcar (lambda (elem)
                                                       (let ((data (tree-node-get-data elem)))
                                                         (save-match-data
                                                           (if (string-match
                                                                (concat "^"
                                                                        (regexp-quote data))
                                                                ecb-path-selected-directory)
                                                               (cons data elem)
                                                             nil))))
                                                     (tree-node-get-children (tree-buffer-get-root))))
                                     (lambda (lhs rhs)
                                       (> (length (car lhs)) (length (car rhs))))))
                       ;; we start at the root node
                       (tree-buffer-get-root)))
               (when (and (equal ecb-auto-expand-directory-tree 'best)
                          start)
                 ;; expand the best-match node itself
                 (tree-node-set-expanded start t)
                 (ecb-update-directory-node start))
               ;; start recursive expanding of either the best-matching node or
               ;; the root-node itself.
               (ecb-expand-directory-tree ecb-path-selected-directory
                                          (or start
                                              (tree-buffer-get-root)))
               (tree-buffer-update))
             (when (not (ecb-show-sources-in-directories-buffer-p))
               (tree-buffer-highlight-node-data ecb-path-selected-directory
                                                start)))))

        ;;Here we add a cache-mechanism which caches for each path the
        ;;node-tree and the whole buffer-string of the sources-buffer. A
        ;;cache-elem would be removed from the cache if a directory is
        ;;POWER-clicked in the directories buffer because this is the only way
        ;;to synchronize the sources-buffer with the disk-contents of the
        ;;clicked directory.
        (ecb-exec-in-sources-window
         (let ((cache-elem (ecb-sources-cache-get ecb-path-selected-directory)))
           (if cache-elem
               (progn
                 (tree-buffer-set-root (nth 0 cache-elem))
                 (setq tree-buffer-nodes (nth 1 cache-elem))
                 (let ((buffer-read-only nil))
                   (erase-buffer)
                   (insert (nth 2 cache-elem))))
             (let ((new-tree (tree-node-new "root" 0 nil))
                   (old-children (tree-node-get-children (tree-buffer-get-root)))
                   (new-cache-elem nil))
               ;; building up the new files-tree
               (ecb-tree-node-add-files
                new-tree
                ecb-path-selected-directory
                (car (ecb-get-files-and-subdirs ecb-path-selected-directory))
                0 ecb-show-source-file-extension old-children t)

               ;; updating the buffer itself
               (tree-buffer-set-root new-tree)
               (tree-buffer-update)
               
               ;; check if the sources buffer for this directory must be
               ;; cached: If yes update the cache
               (when (ecb-check-directory-for-caching
                      ecb-path-selected-directory
                      (length tree-buffer-nodes))
                 (setq new-cache-elem (cons ecb-path-selected-directory
                                            (list (tree-buffer-get-root)
                                                  (copy-list tree-buffer-nodes)
                                                  (buffer-string))))
                 (ecb-sources-cache-add new-cache-elem))))
           
           (when (not (string= last-dir ecb-path-selected-directory))
             (tree-buffer-scroll (point-min) (point-min))))))))
  
  ;; set the default-directory of each tree-buffer to current selected
  ;; directory so we can open files via find-file from each tree-buffer.
  ;; is this necessary if neither dir.- nor sources-buffer-contents have been
  ;; changed? I think not but anyway, doesn't matter, costs are very low.
  (save-excursion
    (dolist (buf ecb-tree-buffers)
      (set-buffer buf)
      (setq default-directory
            (concat ecb-path-selected-directory
                    (and (not (= (aref ecb-path-selected-directory
                                       (1- (length ecb-path-selected-directory)))
                                 ecb-directory-sep-char))
                         ecb-directory-sep-string)))))
  ;; set the modelines of all visible tree-buffers new
  (ecb-mode-line-format))


(defun ecb-get-source-name (filename)
  "Returns the source name of a file."
  (let ((f (file-name-nondirectory filename)))
    (if ecb-show-source-file-extension
        f
      (file-name-sans-extension f))))

(defun ecb-semantic-active-for-file (filename)
  "Return not nil if FILENAME is already displayed in a buffer and if semantic
is active for this buffer."
  (and (get-file-buffer filename)
       (save-excursion
         (set-buffer (get-file-buffer filename))
         (semantic-active-p))))

(defun ecb-select-source-file (filename &optional force)
  "Updates the directories, sources and history buffers to match the filename
given. If FORCE is not nil then the update of the directories buffer is done
even if current directory is equal to `ecb-path-selected-directory'."
  (save-selected-window
    (ecb-set-selected-directory (file-name-directory filename) force)
    (setq ecb-path-selected-source filename)
  
    ;; Update directory buffer
    (when (ecb-show-sources-in-directories-buffer-p)
      (ecb-exec-in-directories-window
       (tree-buffer-highlight-node-data ecb-path-selected-source)))
    
    ;; Update source buffer
    (ecb-exec-in-sources-window
     (tree-buffer-highlight-node-data ecb-path-selected-source))

    ;; Update history buffer always regardless of visibility of history window
    (ecb-add-item-to-history-buffer ecb-path-selected-source)
    (ecb-sort-history-buffer)
    ;; Update the history window only if it is visible
    (ecb-update-history-window ecb-path-selected-source)))

(defun ecb-add-item-to-history-buffer (filename)
  "Add a new item for FILENAME to the history buffer."
  (save-excursion
    (ecb-buffer-select ecb-history-buffer-name)
    (tree-node-remove-child-data (tree-buffer-get-root) filename)
    (tree-node-add-child-first
     (tree-buffer-get-root)
     (tree-node-new
      (if (eq ecb-history-item-name 'buffer-name)
          (let ((b (get-file-buffer filename)))
            (if b
                (buffer-name b)
              (ecb-get-source-name filename)))
        (ecb-get-source-name filename))
      0
      filename t))))

(defun ecb-sort-history-buffer ()
  "Sort the history buffer according to `ecb-sort-history-items'."
  (when ecb-sort-history-items
    (save-excursion
      (ecb-buffer-select ecb-history-buffer-name)
      (tree-node-sort-children
       (tree-buffer-get-root)
       (function (lambda (l r) (string< (tree-node-get-name l)
                                        (tree-node-get-name r))))))))

(defun ecb-update-history-window (&optional filename)
  "Updates the history window and highlights the item for FILENAME if given."
  (save-selected-window
    (ecb-exec-in-history-window
     (tree-buffer-update)
     (tree-buffer-highlight-node-data filename))))

(defun ecb-add-all-buffers-to-history ()
  "Add all current file-buffers to the history-buffer of ECB.
If `ecb-sort-history-items' is not nil then afterwards the history is sorted
alphabetically. Otherwise the most recently used buffers are on the top of the
history and the seldom used buffers at the bottom."
  (interactive)
  (when (ecb-window-live-p ecb-history-buffer-name)
    ;; first we remove all not anymore existing buffers
    (let ((ecb-clear-history-behavior 'not-existing-buffers))
      (ecb-clear-history))
    ;; now we add the current existing file-buffers
    (mapc (lambda (buffer)
            (when (buffer-file-name buffer)
              (ecb-add-item-to-history-buffer (buffer-file-name buffer))))
          (reverse (buffer-list)))
    (ecb-sort-history-buffer))
  (ecb-update-history-window (buffer-file-name (current-buffer))))

(defun ecb-update-methods-after-saving ()
  "Updates the methods-buffer after saving if this option is turned on and if
current-buffer is saved."
  (when (and (equal (selected-frame) ecb-frame)
             ecb-auto-update-methods-after-save
             ecb-last-edit-window-with-point
             ;; this prevents updating the method buffer after saving a not
             ;; current buffer (e.g. with `save-some-buffers'), because this
             ;; would result in displaying a method-buffer not belonging to the
             ;; current source-buffer.
             (equal (current-buffer)
                    (window-buffer ecb-last-edit-window-with-point)))
    (ecb-select-source-file ecb-path-selected-source)
    (ecb-rebuild-methods-buffer)))

;; This variable is only set and evaluated by the functions
;; `ecb-update-methods-buffer--internal' and
;; `ecb-rebuild-methods-buffer-with-tokencache'!
(defvar ecb-method-buffer-needs-rebuild t)

(defun ecb-update-methods-buffer--internal (&optional scroll-to-top
                                                      rebuild-non-semantic)
  "Updates the methods buffer with the current buffer. The only thing what
must be done is to start the toplevel parsing of semantic, because the rest is
done by `ecb-rebuild-methods-buffer-with-tokencache' because this function is in
the `semantic-after-toplevel-cache-change-hook'.
If optional argument SCROLL-TO-TOP is non nil then the method-buffer is
displayed with window-start and point at beginning of buffer.

If second optional argument REBUILD-NON-SEMANTIC is not nil then non-semantic
sources are forced to be rescanned and reparsed by
`ecb-rebuild-methods-buffer-with-tokencache'. The function
`ecb-rebuild-methods-buffer-for-non-semantic' is the only one settings this
argument to not nil!"
  (when (and (equal (selected-frame) ecb-frame)
             (get-buffer-window ecb-methods-buffer-name))
    ;; Set here `ecb-method-buffer-needs-rebuild' to t so we can see below if
    ;; `ecb-rebuild-methods-buffer-with-tokencache' was called auto. after
    ;; `semantic-bovinate-toplevel'.
    (setq ecb-method-buffer-needs-rebuild t)

    (let ((current-tokencache (and (semantic-active-p)
                                   ;; if we manually bovinate the buffer we
                                   ;; must widen the source to get all tokens.
                                   ;; But here we must not use the adviced
                                   ;; version of widen!
                                   (save-excursion
                                     (save-restriction
                                       (ecb-with-original-basic-functions
                                        (widen))
                                       (semantic-bovinate-toplevel t))))))
      ;; If the `semantic-bovinate-toplevel' has done no reparsing but only
      ;; used it´s still valid `semantic-toplevel-bovine-cache' then neither
      ;; the hooks of `semantic-after-toplevel-cache-change-hook' nor the
      ;; hooks in `semantic-after-partial-cache-change-hook' are evaluated and
      ;; therefore `ecb-rebuild-methods-buffer-with-tokencache' was not
      ;; called. Therefore we call it here manually.
      ;; `ecb-rebuild-methods-buffer-with-tokencache' is the only function
      ;; which sets `ecb-method-buffer-needs-rebuild' to nil to signalize that
      ;; a "manually" rebuild of the method buffer is not necessary.
      ;;
      ;; `ecb-update-methods-buffer--internal' is called by
      ;; `ecb-current-buffer-sync' and `ecb-set-selected-source' (depending on
      ;; the method switching to current buffer) which both are called also
      ;; for buffers which are not setup for semantic (e.g. text-,
      ;; tex-buffers). current-tokencache is nil for such buffers so we call
      ;; the rebuilding of the method buffer with a nil cache and therefore
      ;; the method-buffer will be cleared out for such buffers. This is what
      ;; we want! For further explanation see
      ;; `ecb-rebuild-methods-buffer-with-tokencache'...
      (if ecb-method-buffer-needs-rebuild
          ;; the hook was not called therefore here manually
          (ecb-rebuild-methods-buffer-with-tokencache
           current-tokencache
           (semantic-active-p)
           nil rebuild-non-semantic)))
    (when scroll-to-top
      (save-selected-window
	(ecb-exec-in-methods-window
	 (tree-buffer-scroll (point-min) (point-min)))))))

(defvar ecb-token-tree-cache nil
  "This is the token-tree-cache for already opened file-buffers. The cache is
a list of cons-cells where the car is the name of the source and the cdr is
the current token-tree for this source. The cache contains exactly one element
for a certain source.")
(setq ecb-token-tree-cache nil)

(defun ecb-clear-token-tree-cache (&optional source-file-name)
  "Clears wither the whole token-tree-cache \(SOURCE-FILE-NAME is nil) or
removes only the token-tree for SOURCE-FILE-NAME from the cache."
  (if (not source-file-name)
      (setq ecb-token-tree-cache nil)
    (setq ecb-token-tree-cache
          (adelete 'ecb-token-tree-cache source-file-name))))


;; The most important function for (re)building the Method-buffer
(defun ecb-rebuild-methods-buffer-with-tokencache (updated-cache
						   &optional no-update-semantic
                                                   force-nil-cache
                                                   non-semantic-rebuild)
  "Rebuilds the ECB-method buffer after toplevel-parsing by semantic. This
function is added to the hook `semantic-after-toplevel-cache-change-hook'.

If NO-UPDATE-SEMANTIC is not nil then the tokens of the ECB-methods-buffer are
not updated with UPDATED-CACHE but the method-buffer is rebuild with these
tokens ECB has already cached in it `ecb-token-tree-cache'. Only relevant for
semantic-parsed sources!

If FORCE-NIL-CACHE is not nil then the method-buffer is even rebuild if
UPDATED-CACHE is nil. Normally a nil cache is ignored if it belongs to a
buffer witch is setup for semantic-parsing; only nil caches for non-semantic
buffers \(like plain text-buffers) are used for updating the method-buffers.
With FORCE-NIL-CACHE the method-buffer is updated with a nil cache too, i.e.
it is cleared.

IF NON-SEMANTIC-REBUILD is not nil then current non-semantic-source is forced
to be rescanned/reparsed and therefore the Method-buffer will be rebuild too."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (get-buffer-window ecb-methods-buffer-name)
             (buffer-file-name (current-buffer))             
             ;; The functions of the hook
             ;; `semantic-after-toplevel-cache-change-hook' are also called
             ;; after clearing the cache to set the cache to nil if a buffer
             ;; is parsed which has no tokens. But buffers with no tokens are
             ;; really seldom so cause of better performance here we do not
             ;; want rebuilding the method-buffer if the cache is nil but the
             ;; current buffer is set up for semantic-parsing, because the
             ;; real rebuild should be done after the cache is filled again.
             ;; If this hook is called "manually" by
             ;; `ecb-update-methods-buffer--internal' then we do an update
             ;; also for a nil cache if the buffer is not setup for semantic
             ;; (like text-buffers or non-semantic-sources) so we can either
             ;; clear out the method-buffer or fill it with parsing
             ;; information of non-semantic-sources!
             (or updated-cache
                 (not (semantic-active-p))
                 force-nil-cache))

    ;; no-update-semantic has to be nil for non-semantic-sources!
    (if (not (semantic-active-p)) (setq no-update-semantic nil))

    ;; the following cache-mechanism MUST use the (buffer-file-name
    ;; (current-buffer)) instead of ecb-path-selected-source because in case
    ;; of opening a buffer not via directory-window but via the
    ;; standard-mechanism of Emacs this function is called via hook BEFORE
    ;; ecb-path-selected-source is set currently by the synchronize-mechanism
    ;; of ECB.
    ;; Also if we create a new cache-element for the token-tree we MUST look
    ;; if in the cache is already an element with this key and if we MUST
    ;; update this cache-element instead of always adding a new one to the
    ;; cache. Otherwise we would get more than one cache-element for the same
    ;; source!.
    
    (let* ((norm-buffer-file-name (ecb-fix-filename
                                   (buffer-file-name (current-buffer))))
           (cached-tree (assoc norm-buffer-file-name ecb-token-tree-cache))
           new-tree non-semantic-handling)
      
      (if ecb-debug-mode
          (dolist (tok updated-cache)
            (ecb-semantic-assert-valid-token tok)))
      
      ;; here we process non-semantic buffers if the user wants this. But only
      ;; if either non-semantic-rebuild is true or no cached-tree exists.
      (when (and ecb-process-non-semantic-files
                 (null updated-cache)
                 (not (semantic-active-p))
                 (buffer-file-name (current-buffer))
                 (or non-semantic-rebuild (null cached-tree)))
        (setq updated-cache (ignore-errors
                              (ecb-get-tags-for-non-semantic-files)))
        (setq non-semantic-handling
              (if updated-cache 'parsed 'parsed-failed)))

      ;; Now non-semantic-handling is only nil either for semantic-sources or
      ;; for non-semantic-sources if already a cached-tree exists and
      ;; non-semantic-rebuild is nil (i.e. no rescan and rebuild is
      ;; necessary). A not-nil value is only possible for non-semantic-sources
      ;; and is then either 'parsed in case the parsing was successful or
      ;; 'parsed-failed.

      ;; We always make a new token-tree with updated-cache except for
      ;; - semantic-sources if no-update-semantic is true and already a
      ;;   cached-tree exists. This means this function is NOT called by
      ;;   `semantic-after-toplevel-cache-change-hook'.
      ;; - non-semantic-sources if non-semantic-handling is false, because
      ;;   then no rescan has been performed and updated-cache contains
      ;;   nothing; see comment above.
      (unless (or (and no-update-semantic cached-tree)  ;; for semantic-sources
                  (and (not (semantic-active-p))    ;; for non-semantic-sources
                       (not non-semantic-handling)
                       ;; for clearing out non-semantic-buffers too after
                       ;; killing one; see `ecb-kill-buffer-hook'.
                       (not force-nil-cache)))
        (setq new-tree (tree-node-new "root" 0 nil))
        (if non-semantic-handling
            (if (equal non-semantic-handling 'parsed)
                (ecb-create-non-semantic-tree new-tree updated-cache))
          (ecb-add-tokens new-tree (ecb-post-process-tokenlist updated-cache)))
        (if cached-tree
            (setcdr cached-tree new-tree)
          (setq cached-tree (cons norm-buffer-file-name new-tree))
          (setq ecb-token-tree-cache (cons cached-tree ecb-token-tree-cache))))

      ;; Now we either update the method-buffer with a newly created
      ;; token-tree or with the token-tree from the cache (with all its
      ;; existing expansions!)
      (save-excursion
        (ecb-buffer-select ecb-methods-buffer-name)
        (tree-buffer-set-root (cdr cached-tree))
        (setq ecb-methods-root-node (cdr cached-tree))
        (setq tree-buffer-indent ecb-tree-indent)
        (tree-buffer-update)))
    
    ;; Klaus Berndl <klaus.berndl@sdm.de>: after a full reparse all overlays
    ;; stored in the dnodes of the navigation-list now are invalid. Therefore
    ;; we have changed the implementation of ecb-navigate.el from storing
    ;; whole tokens to storing buffer and start- and end-markers!
    
    (ecb-mode-line-format)

    ;; signalize that the rebuild has already be done
    (setq ecb-method-buffer-needs-rebuild nil)))


(defun ecb-save-without-auto-update-methods ()
  (let ((ecb-auto-update-methods-after-save nil))
    (save-buffer)))

(defun ecb-rebuild-methods-buffer-for-non-semantic ()
  "Rebuild the ECB-method-buffer for current source-file of the edit-window.
This function does nothing if point stays not in an edit-window of the
ECB-frame or if current source-file is supported by semantic!

Before rebuilding the Methods-buffer the hook
`ecb-rebuild-non-semantic-methods-before-hook' is called. The Method-buffer is
only rebuild if either the hook contains no function \(the default) or if no
function of this hook returns nil! See `run-hook-with-args-until-failure' for
description how these function are pressed.

The option `ecb-auto-save-before-etags-methods-rebuild' is checked before
rescanning the source-buffer and rebuilding the methods-buffer.

This function is called by the command `ecb-rebuild-methods-buffer'."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (not (semantic-active-p))
             (not (member major-mode ecb-non-semantic-exclude-modes))
             (ecb-point-in-edit-window))
    (when (run-hook-with-args-until-failure
           'ecb-rebuild-non-semantic-methods-before-hook
           (buffer-file-name))
      ;; For etags supported non-semantic-sources we maybe have to save the
      ;; buffer first.
      (when (and (buffer-modified-p)
                 (not (and (boundp 'imenu--index-alist)
                           imenu--index-alist))
                 (or (equal ecb-auto-save-before-etags-methods-rebuild t)
                     (member major-mode
                             ecb-auto-save-before-etags-methods-rebuild)))
        ;; to prevent files from being parsed too often we need to temp.
        ;; switch off the auto-method-updating-after-save feature
        (ecb-save-without-auto-update-methods))
      (ecb-update-methods-buffer--internal nil t))))

(defun ecb-rebuild-methods-buffer-for-semantic ()
  "Rebuild the ECB-method-buffer for current source-file of the edit-window.
This function does nothing if point stays not in an edit-window of the
ECB-frame or if current source-file is not supported by semantic!"
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (semantic-active-p)
             (ecb-point-in-edit-window))
    ;; to force a really complete rebuild we must completely clear the
    ;; semantic cache for semantic-files.
    (semantic-clear-toplevel-cache)
    (ecb-update-methods-buffer--internal)))

(defun ecb-rebuild-methods-buffer ()
  "Updates the methods buffer with the current source-buffer.
The complete previous parser-information is deleted before, means no
semantic-cache is used! Point must stay in an edit-window otherwise nothing is
done. This method is merely needed for semantic parsed buffers if semantic
parses not the whole buffer because it reaches a not parse-able code or for
buffers not supported by semantic but by imenu or etags.

Examples when a call to this function can be necessary:

+ If an elisp-file is parsed which contains in the middle a defun X where the
  closing ) is missing then semantic parses only until this defun X is reached
  and you will get an incomplete ECB-method buffer. In such a case you must
  complete the defun X and then call this function to completely reparse the
  elisp-file and rebuild the ECB method buffer!

+ For not semantic supported buffers which can be parsed by imenu or etags
  \(see `ecb-process-non-semantic-files') because for these buffers there is
  no built-in auto-rebuild mechanism. For these buffers this command calls
  `ecb-rebuild-methods-buffer-for-non-semantic'.

For non-semantic-sources supported by etags the option
`ecb-auto-save-before-etags-methods-rebuild' is checked before rescanning the
source-buffer and rebuilding the methods-buffer."
  (interactive)
    (if (semantic-active-p)
        (ecb-rebuild-methods-buffer-for-semantic)
      (ecb-rebuild-methods-buffer-for-non-semantic)))

(defun ecb-set-selected-source (filename other-edit-window
					 no-edit-buffer-selection)
  "Updates all the ECB buffers and loads the file. The file is also
displayed unless NO-EDIT-BUFFER-SELECTION is set to non nil. In such case
the file is only loaded invisible in the background, all semantic-parsing
and ECB-Buffer-updating is done but the content of the main-edit window
is not changed."
  (ecb-select-source-file filename)
  (if no-edit-buffer-selection
      ;; load the selected source in an invisible buffer, do all the
      ;; updating and parsing stuff with this buffer in the background and
      ;; display the methods in the METHOD-buffer. We can not go back to
      ;; the edit-window because then the METHODS buffer would be
      ;; immediately updated with the methods of the edit-window.
        (save-excursion
          (set-buffer (find-file-noselect filename))
          (ecb-update-methods-buffer--internal 'scroll-to-begin))
    ;; open the selected source in the edit-window and do all the update and
    ;; parsing stuff with this buffer
    (ecb-find-file-and-display ecb-path-selected-source
			       other-edit-window)
    (ecb-update-methods-buffer--internal 'scroll-to-begin)
    (setq ecb-major-mode-selected-source major-mode)
    (ecb-token-sync 'force)))

(defun ecb-remove-from-current-tree-buffer (node)
  (when node
    (tree-node-remove-child (tree-buffer-get-root) node)))

(defun ecb-kill-buffer-hook ()
  "Function added to the `kill-buffer-hook' during ECB activation.
It does several tasks:
- Depending on the value in `ecb-kill-buffer-clears-history' the corresponding
  entry in the history-buffer is removed.
- Clearing the method buffer if a file-buffer has been killed.
- The entry of the removed file-buffer is removed from `ecb-token-tree-cache'."
  (let* ((curr-buf (current-buffer))
         (buffer-file (ecb-fix-filename (buffer-file-name curr-buf))))
    ;; 1. clearing the history if necessary
    (when ecb-kill-buffer-clears-history
      (let ((node (if buffer-file
                      (save-selected-window
                        (ecb-exec-in-history-window (tree-buffer-find-node-data buffer-file))))))
        (when node
          (if (or (equal ecb-kill-buffer-clears-history 'auto)
                  (and (equal ecb-kill-buffer-clears-history 'ask)
                       (y-or-n-p "Remove history entry for this buffer? ")))
              (ecb-clear-history-node node)))))

    ;; 2. clearing the method buffer if a file-buffer is killed
    (if buffer-file
        (ecb-rebuild-methods-buffer-with-tokencache nil nil t))

    ;; 3. removing the file-buffer from `ecb-token-tree-cache'. Must be done
    ;;    after 2. because otherwise a new element in the cache would be
    ;;    created again by `ecb-rebuild-methods-buffer-with-tokencache'.
    (if buffer-file
        (ecb-clear-token-tree-cache buffer-file))

    ;; 4. Preventing from killing the special-ecb-buffers by accident
    (when (member curr-buf (ecb-get-current-visible-ecb-buffers))
      (ecb-error "Killing an special ECB-buffer is not possible!"))))

(defun ecb-clear-history (&optional clearall)
  "Clears the ECB history-buffer.
If CLEARALL is nil then the behavior is defined in the option
`ecb-clear-history-behavior' otherwise the user is prompted what buffers
should be cleared from the history-buffer. For further explanation see
`ecb-clear-history-behavior'."
  (interactive "P")
  (unless (or (not ecb-minor-mode)
              (not (equal (selected-frame) ecb-frame)))
    (save-selected-window
      (ecb-exec-in-history-window
       (let ((buffer-file-name-list (mapcar (lambda (buff)
                                              (buffer-file-name buff))
                                            (buffer-list)))
             (tree-childs (tree-node-get-children (tree-buffer-get-root)))
             (clear-behavior
              (or (if clearall
                      (intern (ecb-query-string "Clear from history:"
                                                '("all"
                                                  "not-existing-buffers"
                                                  "existing-buffers"))))
                  ecb-clear-history-behavior))
             child-data)
         (while tree-childs
           (setq child-data (tree-node-get-data (car tree-childs)))
           (if (or (eq clear-behavior 'all)
                   (and (eq clear-behavior 'not-existing-buffers)
                        (not (member child-data buffer-file-name-list)))
                   (and (eq clear-behavior 'existing-buffers)
                        (member child-data buffer-file-name-list)))
               (ecb-remove-from-current-tree-buffer (car tree-childs)))
           (setq tree-childs (cdr tree-childs))))
       (tree-buffer-update)
       (tree-buffer-highlight-node-data ecb-path-selected-source)))))

(defvar ecb-auto-expand-token-tree-old 'expand-spec)
(defun ecb-toggle-auto-expand-token-tree (&optional arg)
  "Toggle auto expanding of the ECB-methods-buffer.
With prefix argument ARG, switch on if positive, otherwise switch off. If the
effect is that auto-expanding is switched off then the current value of
`ecb-auto-expand-token-tree' is saved so it can be used for the next switch on
by this command."
  (interactive "P")
  (let* ((new-value (if (null arg)
                        (if ecb-auto-expand-token-tree
                            (progn
                              (setq ecb-auto-expand-token-tree-old
                                    ecb-auto-expand-token-tree)
                              nil)
                          ecb-auto-expand-token-tree-old)
                      (if (<= (prefix-numeric-value arg) 0)
                          (progn
                            (if ecb-auto-expand-token-tree
                                (setq ecb-auto-expand-token-tree-old
                                      ecb-auto-expand-token-tree))
                            nil)
                        (or ecb-auto-expand-token-tree
                            ecb-auto-expand-token-tree-old)))))
    (setq ecb-auto-expand-token-tree new-value)
    (message "Auto. expanding of the methods-buffer is switched %s \(Value: %s\)."
             (if new-value "on" "off")
             new-value)))

(defun ecb-token-sync (&optional force)
  (when (and ecb-minor-mode
             (ecb-point-in-edit-window))
    (when ecb-highlight-token-with-point
      (let ((tok (ecb-semantic-current-nonterminal)))
        (when (or force (not (equal ecb-selected-token tok)))
          (setq ecb-selected-token tok)
          (save-selected-window
            (ecb-exec-in-methods-window
             (or (tree-buffer-highlight-node-data
                  tok nil (equal ecb-highlight-token-with-point 'highlight))
                 ;; The node representing TOK could not be highlighted be
                 ;; `tree-buffer-highlight-node-data' - probably it is
                 ;; invisible. Let's try to make visible and then highlighting
                 ;; again.
                 (when (and tok ecb-auto-expand-token-tree
                            (or (equal ecb-auto-expand-token-tree 'all)
                                (member (semantic-token-token tok)
                                        (ecb-normalize-expand-spec
                                         ecb-methods-nodes-expand-spec))))
                   (ecb-expand-methods-nodes-internal
                    100
                    (equal ecb-auto-expand-token-tree 'all))
                   (tree-buffer-highlight-node-data
                    tok nil (equal ecb-highlight-token-with-point 'highlight))
                   )))))))))

(defun ecb-current-buffer-archive-extract-p ()
  "Return not nil if current buffer was extracted of an archive which is in
`tar-mode' or `archive-mode'. For this the current buffer has either to be in
minor-mode `tar-subfile-mode' or `archive-subfile-mode'."
  (or (and (boundp 'tar-subfile-mode)
           tar-subfile-mode)
      (and (boundp 'archive-subfile-mode)
           archive-subfile-mode)))

(defun ecb-buffer-or-file-readable-p (&optional filename)
  "Checks if a buffer or a file is a readable file in the sense of ECB which
means either a real physical file or an auto-extracted file from an archive.
See `ecb-current-buffer-archive-extract-p'. FILENAME is either a filename or
nil whereas in the latter case the current-buffer is assumed."
  (let* ((file (or filename (buffer-file-name (current-buffer)))))
    (or (and file (file-readable-p file))
        (and (not ecb-running-xemacs)
             (if filename
                 (save-excursion
                   (set-buffer (find-file-noselect filename))
                   (ecb-current-buffer-archive-extract-p))
               (ecb-current-buffer-archive-extract-p))))))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: In all sources and the texi-file
;; replacing of interactive using of ecb-current-buffer-sync with
;; ecb-window-sync! 
(defun ecb-current-buffer-sync (&optional force)
  "Synchronizes all special ECB-buffers with current buffer.

Depending on the contents of current buffer this function performs different
synchronizing tasks but only if ECB is active and point stays in an
edit-window. If this is true under the following additional conditions some
tasks are performed:

- Current buffer is a file-buffer and either FORCE is not nil or the buffer
  is different from the source-file currently displayed in the
  ECB-tree-buffers:

  Synchronizing all tree-buffers with the current buffer

- Current buffer is a dired-buffer:

  Synchronizing the directory- and sources-tree-buffer if visible

- Always:

  Running the hooks in `ecb-current-buffer-sync-hook'."  
  (when (and ecb-minor-mode
             (not ecb-windows-hidden)
             (ecb-point-in-edit-window))
    (ignore-errors
      (let ((filename (buffer-file-name (current-buffer))))
        (cond (;; synchronizing for real filesource-buffers
               (and filename
                    (ecb-buffer-or-file-readable-p)
                    (or force
                        (not (string= filename ecb-path-selected-source))))
          
               ;; * KB: Problem: seems this little sleep is necessary because
               ;;   otherwise jumping to certain markers in new opened files (e.g.
               ;;   with next-error etc. ) doesn´t work correct. Can´t debug down
               ;;   this mysterious thing! Regardless of the size of the file to
               ;;   load, this 0.1 fraction of a sec is enough!
               ;; * KB: With current ECB implementation this sit-for seems not
               ;;   longer necessary, it works with every Emacs version correct.
               ;;   Therefore i comment out the sit-for until this error occurs
               ;;   again.
               ;;           (sit-for 0.1)
               
               ;; if the file is not located in any of the paths in
               ;; `ecb-source-path' or in the paths returned from
               ;; `ecb-source-path-functions' we must at least add the new source
               ;; path temporally to our paths. But the uses has also the choice to
               ;; save it for future sessions too.
               (if (not (ecb-path-matching-any-source-path-p filename))
                   (let* ((norm-filename (ecb-fix-filename filename))
                          (source-path (if (car ecb-add-path-for-not-matching-files)
                                           (if (= (aref norm-filename 0) ?/)
                                               ;; for Unix-style-path we add the
                                               ;; root-dir
                                               (substring norm-filename 0 1)
                                             ;; for win32-style-path we add the
                                             ;; drive; because `ecb-fix-filename'
                                             ;; also converts cygwin-path-style to
                                             ;; win32-path-style here also the
                                             ;; drive is added.
                                             (substring norm-filename 0 2))
                                         (file-name-directory norm-filename))))
                     (ecb-add-source-path source-path source-path
                                          (not (cdr ecb-add-path-for-not-matching-files)))))

               ;; now we can be sure that a matching source-path exists
               
               ;; Klaus: The explicit update of the directories buffer is not
               ;; necessary because the sync with the current source is done by
               ;; `ecb-select-source-file'!
               ;;           (ecb-update-directories-buffer)
               (ecb-select-source-file filename force)
               (ecb-update-methods-buffer--internal 'scroll-to-begin)
               (setq ecb-major-mode-selected-source major-mode)

               ;; Klaus Berndl <klaus.berndl@sdm.de>: is now be done at the
               ;; end of `ecb-rebuild-methods-buffer-with-tokencache' which is
               ;; called by `ecb-update-methods-buffer--internal'!

               ;; selected source has changed, therefore we must initialize
               ;; ecb-selected-token again.
               (ecb-token-sync 'force)
               )
              
              (;; synchronizing for dired-mode
               (eq major-mode 'dired-mode)
               (ecb-set-selected-directory
                (or (and (stringp dired-directory)
                         (file-exists-p dired-directory)
                         dired-directory)
                    (and (listp dired-directory)
                         (car dired-directory)))))
              (t nil))))

    ;; at the end we are running the hooks
    (run-hooks 'ecb-current-buffer-sync-hook)))


(defun ecb-window-sync-function ()
  (when (and ecb-window-sync
             (or (equal 'always ecb-window-sync)
                 (not (member major-mode ecb-window-sync))))
    (ecb-current-buffer-sync)))


(defun ecb-window-sync ()
  "Synchronizes all special ECB-buffers with current buffer.
Depending on the contents of current buffer this command performs different
synchronizing tasks but only if ECB is active and point stays in an
edit-window.

- If current buffer is a file-buffer then all special ECB-tree-buffers are
  synchronized with current buffer.

- If current buffer is a dired-buffer then the directory- and
  the sources-tree-buffer are synchronized if visible

In addition to this the hooks in `ecb-current-buffer-sync-hook' run."
  (interactive)
  (ecb-current-buffer-sync t))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: test!!
(defvar ecb-window-sync-old '(Info-mode dired-mode))
(defun ecb-toggle-window-sync (&optional arg)
  "Toggle auto synchronizing of the ECB-windows.
With prefix argument ARG, switch on if positive, otherwise switch off. If the
effect is that auto-synchronizing is switched off then the current value of
the option `ecb-window-sync' is saved so it can be used for the next switch on
by this command. See also the option `ecb-window-sync'."
  (interactive "P")
  (let* ((new-value (if (null arg)
                        (if ecb-window-sync
                            (progn
                              (setq ecb-window-sync-old
                                    ecb-window-sync)
                              nil)
                          ecb-window-sync-old)
                      (if (<= (prefix-numeric-value arg) 0)
                          (progn
                            (if ecb-window-sync
                                (setq ecb-window-sync-old
                                      ecb-window-sync))
                            nil)
                        (or ecb-window-sync
                            ecb-window-sync-old)))))
    (setq ecb-window-sync new-value)
    (message "Automatic synchronizing the ECB-windows is %s \(Value: %s\)."
               (if new-value "on" "off")
               new-value)))



(defun ecb-get-edit-window (other-edit-window)
  (save-selected-window
    (if (eq ecb-primary-mouse-jump-destination 'left-top)
	(select-window ecb-edit-window)
      (select-window ecb-last-edit-window-with-point))
    (ecb-with-adviced-functions
     (if other-edit-window
	 (let ((ecb-other-window-jump-behavior 'only-edit))
	   (other-window 1))))
    (selected-window)))

(defun ecb-find-file-and-display (filename other-edit-window)
  "Finds the file in the correct window. What the correct window is depends on
the setting in `ecb-primary-mouse-jump-destination' and the value of
OTHER-EDIT-WINDOW."
  (select-window (ecb-get-edit-window other-edit-window))
  (ecb-nav-save-current)
  (ecb-with-original-functions
   (find-file filename))
  (ecb-nav-add-item (ecb-nav-file-history-item-new)))

(defun ecb-tree-node-add-files
  (node path files type include-extension old-children &optional not-expandable)
  "For every file in FILES add a child-node to NODE."
  (dolist (file files)
    (let ((filename (ecb-fix-filename path file)))
      (tree-node-add-child
       node
       (ecb-new-child
        old-children
        (if include-extension
            file
          (file-name-sans-extension file))
        type filename
        (or not-expandable (= type 1))
        (if ecb-truncate-long-names 'end))))))

(defun ecb-update-directory-node (node)
  "Updates the directory node NODE and add all subnodes if any."
  (let ((old-children (tree-node-get-children node))
        (path (tree-node-get-data node)))
    (tree-node-set-children node nil)
    (if (file-accessible-directory-p path)
        (let ((files-and-dirs (ecb-get-files-and-subdirs path)))
          (ecb-tree-node-add-files node path (cdr files-and-dirs)
                                   0 t old-children)
          (if (ecb-show-sources-in-directories-buffer-p)
              (ecb-tree-node-add-files node path (car files-and-dirs) 1
                                       ecb-show-source-file-extension
                                       old-children t))
          (tree-node-set-expandable node (or (tree-node-get-children node)))))))

(defun ecb-get-source-paths-from-functions ()
  "Return a list of paths found by querying `ecb-source-path-functions'."
  (let ((func ecb-source-path-functions)
	(paths nil)
	(rpaths nil))
    (while func
      (setq paths (append paths (funcall (car ecb-source-path-functions)))
	    func (cdr func)))
    (while paths
      (setq rpaths (cons (expand-file-name (car paths)) rpaths)
	    paths (cdr paths)))
    rpaths))

(defun ecb-path-matching-any-source-path-p (path)
  (let ((source-paths (append (ecb-get-source-paths-from-functions)
                              ecb-source-path)))
    (catch 'exit
      (dolist (dir source-paths)
        (let ((norm-dir (ecb-fix-filename (if (listp dir) (car dir) dir) nil t)))
          (save-match-data
            (if (string-match (regexp-quote norm-dir)
                              (ecb-fix-filename (file-name-directory path)))
                (throw 'exit norm-dir)))
          nil)))))

(defun ecb-update-directories-buffer ()
  "Updates the ECB directories buffer."
  (interactive)
  (unless (or (not ecb-minor-mode)
              (not (equal (selected-frame) ecb-frame)))
    (save-selected-window
      (ecb-exec-in-directories-window
       ;;     (setq tree-buffer-type-faces
       ;;       (list (cons 1 ecb-source-in-directories-buffer-face)))
       (setq tree-buffer-indent ecb-tree-indent)
       (let* ((node (tree-buffer-get-root))
              (old-children (tree-node-get-children node))
              (paths (append (ecb-get-source-paths-from-functions)
			     ecb-source-path)))
         (tree-node-set-children node nil)
	 (dolist (dir paths)
	   (let* ((path (if (listp dir) (car dir) dir))
		  (norm-dir (ecb-fix-filename path nil t))
		  (name (if (listp dir) (cadr dir) norm-dir)))
	     (tree-node-add-child
	      node
	      (ecb-new-child old-children name 2 norm-dir nil
			     (if ecb-truncate-long-names 'beginning)))))
;; 	 (when (not paths)
;; 	   (tree-node-add-child node (tree-node-new "Welcome to ECB! Please select:"
;; 						    3 '(lambda()) t))
;;            (tree-node-add-child node (tree-node-new "" 3 '(lambda()) t))
;;            (tree-node-add-child node (tree-node-new "[F2] Customize ECB" 3
;;                                                     'ecb-customize t))
;;            (tree-node-add-child node (tree-node-new "[F3] ECB Help" 3
;;                                                     'ecb-show-help t))
;;            (tree-node-add-child
;;             node (tree-node-new
;;                   "[F4] Add Source Path" 3
;;                   '(lambda () (call-interactively 'ecb-add-source-path)) t)))
         (tree-buffer-update))))))

(defun ecb-new-child (old-children name type data &optional not-expandable shorten-name)
  "Return a node with type = TYPE, data = DATA and name = NAME. Tries to find
a node with matching TYPE and DATA in OLD-CHILDREN. If found no new node is
created but only the fields of this node will be updated. Otherwise a new node
is created."
  (catch 'exit
    (dolist (child old-children)
      (when (and (equal (tree-node-get-data child) data)
                 (= (tree-node-get-type child) type))
        (tree-node-set-name child name)
        (if not-expandable
            (tree-node-set-expandable child nil))
        (throw 'exit child)))
    (let ((node (tree-node-new name type data not-expandable nil
			       (if ecb-truncate-long-names 'end))))
      (tree-node-set-shorten-name node shorten-name)
      node)))

(defun ecb-add-source-path (&optional dir alias no-prompt-for-future-session)
  "Add a directory to the `ecb-source-path'."
  (interactive)
  ;; we must manually cut a filename because we must not add filenames to
  ;; `ecb-source-path'!
  (let* ((use-dialog-box nil)
         (my-dir (ecb-fix-filename
                  (or dir
                      (file-name-directory (read-file-name "Add source path: ")))
                  t))
         (my-alias (or alias
                       (read-string (format "Alias for \"%s\" (empty = no alias): "
                                            my-dir)))))
    (setq ecb-source-path (append ecb-source-path
                                  (list (if (> (length my-alias) 0)
                                            (list my-dir my-alias) my-dir))))
    (ecb-update-directories-buffer)
    (if (and (not no-prompt-for-future-session)
             (y-or-n-p "Add the new source-path also for future-sessions? "))
        (customize-save-variable 'ecb-source-path ecb-source-path)
      (customize-set-variable 'ecb-source-path ecb-source-path))))

(defun ecb-add-source-path-node (node)
  (call-interactively 'ecb-add-source-path))

(defun ecb-node-to-source-path (node)
  (ecb-add-source-path (tree-node-get-data node)))

(defun ecb-delete-s (child children sources)
  (when children
    (if (eq child (car children))
	(cdr sources)
      (cons (car sources) (ecb-delete-s child (cdr children) (cdr sources))))))

(defun ecb-delete-source-path (node)
  (let ((path (tree-node-get-data node)))
    (when (ecb-confirm (concat "Really delete source-path " path "?"))
      (setq ecb-source-path (ecb-delete-s
                             node (tree-node-get-children (tree-node-get-parent node))
                             ecb-source-path))
      (ecb-update-directories-buffer)
      (if (y-or-n-p "Delete source-path also for future-sessions? ")
          (customize-save-variable 'ecb-source-path ecb-source-path)
        (customize-set-variable 'ecb-source-path ecb-source-path)))))

(defun ecb-customize ()
  "Open a customize-buffer for all customize-groups of ECB."
  (interactive)
  (ecb-select-edit-window)
  (customize-group "ecb"))

;;====================================================
;; Mouse functions
;;====================================================

(defun ecb-tree-buffer-node-select-callback (node
					     mouse-button
					     shift-pressed
					     control-pressed
					     tree-buffer-name)
  "This is the callback-function ecb.el gives to every tree-buffer to call
when a node has been selected. This function does nothing if the click
combination is invalid \(see `ecb-interpret-mouse-click'."
  (let* ((ecb-button-list (ecb-interpret-mouse-click mouse-button
						     shift-pressed
						     control-pressed
						     tree-buffer-name))
	 (ecb-button (car ecb-button-list))
	 (shift-mode (cadr ecb-button-list)))
    ;; we need maybe later that something has clicked in a tree-buffer, e.g.
    ;; in `ecb-handle-major-mode-activation'.
    (setq ecb-item-in-tree-buffer-selected t)
    ;; first we dispatch to the right action
    (when ecb-button-list
      (cond ((string= tree-buffer-name ecb-directories-buffer-name)
	     (ecb-directory-clicked node ecb-button shift-mode))
	    ((string= tree-buffer-name ecb-sources-buffer-name)
	     (ecb-source-clicked node ecb-button shift-mode))
	    ((string= tree-buffer-name ecb-history-buffer-name)
	     (ecb-history-clicked node ecb-button shift-mode))
	    ((string= tree-buffer-name ecb-methods-buffer-name)
	     (ecb-method-clicked node ecb-button shift-mode))
	    (t nil)))

    ;; TODO: IMHO the mechanism how the physical keys are mapped and
    ;; interpreted to logical ecb-buttons and -actions should now slightly be
    ;; redesigned because now we evaluate below MOUSE-PRESSED outside
    ;; ecb-interpret-mouse-click and this is not very good. But for now it
    ;; works and it is the only location where such an outside-interpretation is
    ;; performed (Klaus).
    
    ;; now we go back to the tree-buffer but only if all of the following
    ;; conditions are true:
    ;; 1. mouse-button is 0, i.e. RET is pressed in the tree-buffer
    ;; 2. The tree-buffer-name is not contained in
    ;;    ecb-tree-RET-selects-edit-window--internal
    ;; 3. Either it is not the ecb-directories-buffer-name or
    ;;    at least `ecb-show-sources-in-directories-buffer-p' is true.
    (when (and (equal 0 mouse-button)
               (not (member tree-buffer-name
                            ecb-tree-RET-selects-edit-window--internal))
               (or (not (string= tree-buffer-name ecb-directories-buffer-name))
                   (ecb-show-sources-in-directories-buffer-p)))
      (ecb-goto-window tree-buffer-name)
      (tree-buffer-remove-highlight))))


(defun ecb-tree-buffer-node-expand-callback (node
					     mouse-button
					     shift-pressed
					     control-pressed
					     tree-buffer-name)
  "This is the callback-function ecb.el gives to every tree-buffer to call
when a node should be expanded. This function does nothing if the click
combination is invalid \(see `ecb-interpret-mouse-click')."
  (let* ((ecb-button-list (ecb-interpret-mouse-click mouse-button
						     shift-pressed
						     control-pressed
						     tree-buffer-name))
	 (ecb-button (car ecb-button-list))
	 (shift-mode (cadr ecb-button-list)))
    (when ecb-button-list
      (cond ((string= tree-buffer-name ecb-directories-buffer-name)
	     (ecb-update-directory-node node))
	    ((string= tree-buffer-name ecb-sources-buffer-name)
	     (ecb-source-clicked node ecb-button shift-mode))
	    ((string= tree-buffer-name ecb-history-buffer-name)
	     (ecb-history-clicked node ecb-button shift-mode))
	    ((string= tree-buffer-name ecb-methods-buffer-name)
	     nil)
	    (t nil)))))

(defun ecb-interpret-mouse-click (mouse-button
                                  shift-pressed
                                  control-pressed
                                  tree-buffer-name)
  "Converts the physically pressed MOUSE-BUTTON \(1 = mouse-1, 2 = mouse-2, 0 =
no mouse-button but RET or TAB) to ECB-mouse-buttons: either primary or
secondary mouse-button depending on the value of CONTROL-PRESSED and the
setting in `ecb-primary-secondary-mouse-buttons'. Returns a list '\(ECB-button
shift-mode) where ECB-button is either 1 \(= primary) or 2 \(= secondary) and
shift-mode is non nil if SHIFT-PRESSED is non nil. For an invalid and not
accepted click combination nil is returned.

Note: If MOUSE-BUTTON is 0 \(means no mouse-button but a key like RET or TAB
was hitted) then CONTROL-PRESSED is interpreted as ECB-button 2.

Currently the fourth argument TREE-BUFFER-NAME is not used here."
  (if (eq mouse-button 0)
      (list (if control-pressed 2 1) shift-pressed)
    (if (and (not (eq mouse-button 1)) (not (eq mouse-button 2)))
	nil
      (cond ((eq ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2)
	     (if control-pressed
		 nil
	       (list mouse-button shift-pressed)))
	    ((eq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)
	     (if (not (eq mouse-button 1))
		 nil
	       (list (if control-pressed 2 1) shift-pressed)))
	    ((eq ecb-primary-secondary-mouse-buttons 'mouse-2--C-mouse-2)
	     (if (not (eq mouse-button 2))
		 nil
	       (list (if control-pressed 2 1) shift-pressed)))
	    (t nil)))))

(defun ecb-remove-dir-from-caches (dir)
  (ecb-files-and-subdirs-cache-remove dir)
  (ecb-sources-cache-remove dir))

(defun ecb-directory-update-speedbar (dir)
  "Update the integrated speedbar if necessary."
  (and (ecb-speedbar-active-p)
       ;; depending on the value of `ecb-directory-update-speedbar' we have to
       ;; check if it is senseful to update the speedbar.
       (or (equal ecb-directories-update-speedbar t)
           (and (equal ecb-directories-update-speedbar 'auto)
                (not (or (get-buffer-window ecb-sources-buffer-name ecb-frame)
                         (member ecb-layout-name
                                 ecb-show-sources-in-directories-buffer))))
           (and (not (equal ecb-directories-update-speedbar 'auto))
                (functionp ecb-directories-update-speedbar)
                (funcall ecb-directories-update-speedbar dir)))
       (ecb-speedbar-update-contents)))
                    

(defun ecb-directory-clicked (node ecb-button shift-mode)
  (if (= 3 (tree-node-get-type node))
      (funcall (tree-node-get-data node))
    (ecb-update-directory-node node)
    (if shift-mode
        (ecb-mouse-over-directory-node node nil nil 'force))
    (if (or (= 0 (tree-node-get-type node)) (= 2 (tree-node-get-type node)))
        (progn
          (if (= 2 ecb-button)
              (tree-node-toggle-expanded node)
              
            ;; Removing the element from the sources-cache and the
            ;; files-and-subdirs-cache
            (if shift-mode
                (ecb-remove-dir-from-caches (tree-node-get-data node)))
            
            (ecb-set-selected-directory (tree-node-get-data node) shift-mode)
            ;; if we have running an integrated speedbar we must update the
            ;; speedbar 
            (ecb-directory-update-speedbar (tree-node-get-data node)))
          
          (ecb-exec-in-directories-window
           ;; Update the tree-buffer with optimized display of NODE
           (tree-buffer-update node)))
      (ecb-set-selected-source (tree-node-get-data node)
			       (and (ecb-edit-window-splitted) (eq ecb-button 2))
			       shift-mode))))

(defun ecb-source-clicked (node ecb-button shift-mode)
  (if shift-mode
      (ecb-mouse-over-source-node node nil nil 'force))
  (ecb-set-selected-source (tree-node-get-data node)
			   (and (ecb-edit-window-splitted) (eq ecb-button 2))
			   shift-mode))

(defun ecb-history-clicked (node ecb-button shift-mode)
  (if shift-mode
      (ecb-mouse-over-history-node node nil nil 'force))
  (ecb-set-selected-source (tree-node-get-data node)
			   (and (ecb-edit-window-splitted) (eq ecb-button 2))
			   shift-mode))


(defun ecb-string-make-singular (string)
  (if (equal (aref string (1- (length string))) ?s)
      (substring string 0 (1- (length string)))
    string))

(defun ecb-methods-node-get-semantic-type (node symbol->name-assoc-list)
  (cond ((= 1 (tree-node-get-type node))
         (let ((bucket-name
                (save-match-data
                  (if (string-match (concat (regexp-quote (nth 0 ecb-bucket-token-display))
                                            "\\(.+\\)"
                                            (regexp-quote (nth 1 ecb-bucket-token-display)))
                                    (tree-node-get-name node))
                      (match-string 1 (tree-node-get-name node))))))
           (if (stringp bucket-name)
               (or (car (delete nil (mapcar (function (lambda (elem)
                                                        (if (string= (cdr elem)
                                                                     bucket-name)
                                                            (car elem))))
                                            symbol->name-assoc-list)))
                   ;; This is a little hack for bucket-names not defined in
                   ;; symbol->name-assoc-list: First we strip a trailing 's'
                   ;; if there is any to be consistent with the singular names
                   ;; of the cars of symbol->name-assoc-list. Then we downcase
                   ;; the bucket-name and convert it to a symbol. This is done
                   ;; for example for the ECB created bucket-name "Parents"!
                   (intern (downcase (ecb-string-make-singular bucket-name)))))))
        ((= 0 (tree-node-get-type node))
         (ignore-errors (semantic-token-token (tree-node-get-data node))))
        (t nil)))

(defun ecb-expand-methods-nodes (&optional force-all)
  "Set the expand level of the nodes in the ECB-methods-buffer.
This command asks in the minibuffer for an indentation level LEVEL. With this
LEVEL you can precisely specify which level of nodes should be expanded. LEVEL
means the indentation-level of the nodes.

A LEVEL value X means that all nodes with an indentation-level <= X are
expanded and all other are collapsed. A negative LEVEL value means all visible
nodes are collapsed.

Nodes which are not indented have indentation-level 0!

Which node-types are expanded \(rsp. collapsed) by this command depends on
the options `ecb-methods-nodes-expand-spec' and
`ecb-methods-nodes-collapse-spec'! With optional argument FORCE-ALL all tokens
will be expanded/collapsed regardless of the values of these options.

Examples:
- LEVEL = 0 expands only nodes which have no indentation itself.
- LEVEL = 2 expands nodes which are either not indented or indented once or
  twice
- LEVEL ~ 10 should normally expand all nodes unless there are nodes which
  are indented deeper than 10.

Note 1: This command switches off auto. expanding of the method-buffer if
`ecb-expand-methods-switch-off-auto-expand' is not nil. But it can be switched
on again quickly with `ecb-toggle-auto-expand-token-tree' or \[C-c . a].

Note 2: All this is only valid for file-types parsed by semantic. For other
file types which are parsed by imenu or etags \(see
`ecb-process-non-semantic-files') FORCE-ALL is always true!"
  (interactive "P")
  (let* ((first-node (save-excursion
                       (goto-char (point-min))
                       (tree-buffer-get-node-at-point)))
         (level (ecb-read-number
                 "Expand indentation-level: "
                 (if (and first-node
                          (tree-node-is-expandable first-node)
                          (tree-node-is-expanded first-node))
                     -1
                   10))))
    ;; here we should switch off autom. expanding token-tree because otherwise
    ;; our expanding to a certain level takes no effect because if the current
    ;; token in the edit-buffer would be invisible afterwards (after the
    ;; expanding/collapsing) then immediately the token would be autom.
    ;; expanded to max level...
    (when ecb-expand-methods-switch-off-auto-expand
      (ecb-toggle-auto-expand-token-tree -1))
    (ecb-expand-methods-nodes-internal level force-all t)))

(defun ecb-expand-methods-nodes-internal (level &optional force-all resync-token)
  "Set the expand level of the nodes in the ECB-methods-buffer.

For description of LEVEL and FORCE-ALL see `ecb-expand-methods-nodes'.

If RESYNC-TOKEN is not nil then after expanding/collapsing the methods-buffer
is resynced to the current token of the edit-window.

Note: All this is only valid for file-types parsed by semantic. For other file
types which are parsed by imenu or etags \(see
`ecb-process-non-semantic-files') FORCE-ALL is always true!"
  (let ((symbol->name-assoc-list
         ;; if possible we get the local semantic-symbol->name-assoc-list of
         ;; the source-buffer.
         (or (save-excursion
               (ignore-errors
                 (set-buffer (get-file-buffer ecb-path-selected-source))
                 ;; for non-semantic buffers we set force-all always to t
                 (setq force-all (not (semantic-active-p)))
                 semantic-symbol->name-assoc-list))
             semantic-symbol->name-assoc-list)))
    (save-selected-window
      (ecb-exec-in-methods-window
       (let (;; normalizing the elements of `ecb-methods-nodes-expand-spec'
             ;; and `ecb-methods-nodes-collapse-spec'.
             (norm-expand-types (ecb-normalize-expand-spec
                                 ecb-methods-nodes-expand-spec))
             (norm-collapse-types (ecb-normalize-expand-spec
                                   ecb-methods-nodes-collapse-spec)))
         (tree-buffer-expand-nodes
          level
          (and (not force-all)
               (function (lambda (node current-level)
                           (or (equal norm-expand-types 'all)
                               (member (ecb-methods-node-get-semantic-type
                                        node symbol->name-assoc-list)
                                       norm-expand-types)))))
          (and (not force-all)
               (function (lambda (node current-level)
                           (or (equal norm-collapse-types 'all)
                               (member (ecb-methods-node-get-semantic-type
                                        node symbol->name-assoc-list)
                                       norm-collapse-types))))))
         (tree-buffer-scroll (point-min) (point-min)))))

    ;; we want resync the new method-buffer to the current token in the
    ;; edit-window.
    (if resync-token (ecb-token-sync 'force))))

(defun ecb-normalize-expand-spec (spec)
  (if (equal 'all spec)
      'all
    (mapcar (function (lambda (elem)
                        (intern
                         (downcase (ecb-string-make-singular
                                    (symbol-name elem))))))
            spec)))


(defun ecb-expand-directory-nodes (level)
  "Set the expand level of the nodes in the ECB-directories-buffer.
For argument LEVEL see `ecb-expand-methods-nodes'.

Be aware that for deep structured paths and a lot of source-paths this command
can last a long time - depending of machine- and disk-performance."
  (interactive "nLevel: ")
  (save-selected-window
    (ecb-exec-in-directories-window
     (tree-buffer-expand-nodes level)))
  (ecb-current-buffer-sync 'force))

(defun ecb-method-clicked (node ecb-button shift-mode &optional no-post-action
                                additional-post-action-list)
  (if shift-mode
      (ecb-mouse-over-method-node node nil nil 'force))
  (let ((data (tree-node-get-data node))
        (type (tree-node-get-type node))
        (filename ecb-path-selected-source)
        token found)
    ;; Klaus Berndl <klaus.berndl@sdm.de>: We must highlight the token
    (tree-buffer-highlight-node-data data)
    (cond
     ;; Type 0 = a token
     ((= type 0) (setq token data))
     ;; Type 1 = a title of a group
     ;; Just expand/collapse the node
     ((= type 1)
      (tree-node-toggle-expanded node)
      ;; Update the tree-buffer with optimized display of NODE
      (tree-buffer-update node))
     ;; Type 2 = a token name for a token not defined in current buffer; e.g.
     ;; parent or include tokens can be such tokens!
     ;; Try to find the token
     ((= type 2)
      (set-buffer (get-file-buffer ecb-path-selected-source))
      ;; Try to find source using JDE
      (setq found (ecb-jde-show-class-source data))
      ;; Try to find source using Semantic DB
      (when (and (not found) (featurep 'semanticdb) (semanticdb-minor-mode-p))
        (let ((parent (semanticdb-find-nonterminal-by-name data)))
          (when parent
            (setq token (cdar parent))
            (setq filename (semanticdb-full-filename (caar parent)))))))
     )
    (when (and token (not found))
      (ecb-semantic-assert-valid-token token)
      (if (eq 'include (semantic-token-token token))
          ;; Include token -> try to find source
          (progn
            (set-buffer (get-file-buffer ecb-path-selected-source))
            (let ((file (semantic-find-dependency token)))
              (when (and file (file-exists-p file))
                (ecb-find-file-and-display file
                                           (and (ecb-edit-window-splitted)
                                                (eq ecb-button 2))))))
        (ecb-jump-to-token filename token (ecb-get-edit-window
                                           (and (ecb-edit-window-splitted)
                                                (eq ecb-button 2)))
                           no-post-action
                           (if (and shift-mode
                                    (not (member 'ecb-token-visit-narrow-token
                                                 additional-post-action-list)))
                               (cons 'ecb-token-visit-narrow-token
                                     additional-post-action-list)
                             additional-post-action-list))))))


(defun ecb-token-visit-smart-token-start (token)
  "Go to the real token-name of TOKEN in a somehow smart way.
This is especially needed for languages like c++ where a often used style is
like:
    void
    ClassX::methodM\(arg1...)
    \{
      ...
    \}
Here we want to jump to the line \"ClassX::...\" and not to line \"void\".

Returns point."
  (goto-char (ecb-semantic-token-start token))
  (beginning-of-line)
  ;; We must bind the search to the max. of either the end-of-line-pos or the
  ;; token-end, because in some languages the token-name displayed in the
  ;; Methods-buffer and returned by the parsing engine can not be found in the
  ;; source-buffer. Perl is an example, because here imenu returns token-names
  ;; like <package>::<function> (e.g. bigfloat::norm) but in the source buffer
  ;; only "sub <function>" (e.g. "sub norm...") can be found. So to avoid
  ;; finding a wrong position in the source-buffer (e.g. if the token-name
  ;; returned by imenu is mentioned in a comment somewhere) we bind the
  ;; search.
  (search-forward (semantic-token-name token)
                  (max (ecb-line-end-pos)
                       (semantic-token-end token))
                  t)
  (beginning-of-line-text)
  (point))


(defun ecb-start-of-token-doc (token)
  "If TOKEN has an outside documentation located direct before TOKEN then
return the start of the documentation. Otherwise return nil"
  ;; there can be an error if token has no documentation - e.g.
  ;; in elisp
  (let ((comment (ignore-errors (semantic-find-documentation token 'flex))))
    (if (and comment
             (not (stringp comment)))
        ;; probably we have a semantic flex-object
        (semantic-flex-start comment))))


(defun ecb-token-visit-goto-doc-start (token)
  "Go to the beginning of the documentation of TOKEN if defined outside.
This is useful especially for languages like Java where the documentation
resides direct before the TOKEN in Javadoc format.
If the documentation is located within TOKEN then nothing is done.

If this function is set in `ecb-token-visit-post-actions' then it's strongly
recommended to add `ecb-token-visit-recenter' or
`ecb-token-visit-recenter-top' at the end too!

This action is not recommended for sources of type TeX, texinfo etc. So you
should not add this action to the 'default element of
`ecb-token-visit-post-actions'!

Returns current point."
  (let ((token-doc-start  (ecb-start-of-token-doc token)))
    (when token-doc-start
      (goto-char token-doc-start))
    (point)))


(defvar ecb-buffer-narrowed-by-ecb nil
  "If not nil then current buffer is narrowed to a token by ECB. Otherwise
the buffer is not narrowed or it is narrowed by ECB but one of the
interactive commands `narrow-to-*' or function/commands which use in turn one
of these `narrow-to-*'-functions.")
(make-variable-buffer-local 'ecb-buffer-narrowed-by-ecb)

(defadvice narrow-to-region (before ecb)
  "Set an internal ECB-state. This does not influence the behavior."
  (setq ecb-buffer-narrowed-by-ecb nil))

(defadvice narrow-to-defun (before ecb)
  "Set an internal ECB-state. This does not influence the behavior."
  (setq ecb-buffer-narrowed-by-ecb nil))

(defadvice narrow-to-page (before ecb)
  "Set an internal ECB-state. This does not influence the behavior."
  (setq ecb-buffer-narrowed-by-ecb nil))

(defadvice widen (before ecb)
  "Set an internal ECB-state. This does not influence the behavior."
  (setq ecb-buffer-narrowed-by-ecb nil))

(defun ecb-token-visit-narrow-token (token)
  "Narrow the source buffer to TOKEN.
If an outside located documentation belongs to TOKEN and if this documentation
is located direct before TOKEN \(e.g. Javadoc in Java) then this documentation
is included in the narrow.

Returns current point."
  (when (not (ecb-speedbar-sb-token-p token))
    (narrow-to-region (or (ecb-start-of-token-doc token)
                          (ecb-semantic-token-start token))
                      (ecb-semantic-token-end token))
    ;; This is the only location where this variable is set to not nil!
    ;; before every call to `narrow-to-*' or `widen' this variable is reset to
    ;; nil! 
    (setq ecb-buffer-narrowed-by-ecb t))
  (point))


(defun ecb-token-visit-recenter (token)
  "Recenter the source-buffer, so current line is in the middle of the window.
If this function is added to `ecb-token-visit-post-actions' then it's
recommended to add this function add the end of the action list for 'default
or a `major-mode' and not to add the function `ecb-token-visit-recenter-top'
too!

Returns current point."
  (set-window-start
   (selected-window)
   (ecb-line-beginning-pos (- (/ (window-height) 2))))
  (point))


(defun ecb-token-visit-recenter-top (token)
  "Recenter the source-buffer, so current line is in the middle of the window.
If this function is added to `ecb-token-visit-post-actions' then it's
recommended to add this function add the end of the action list for 'default
or a `major-mode' and not to add the function `ecb-token-visit-recenter' too!

Returns current point."
  (set-window-start (selected-window)
                    (ecb-line-beginning-pos)))


;; this mechanism is necessary because tree-buffer creates for mouse releasing
;; a new nop-command (otherwise the cursor jumps back to the tree-buffer).
(defvar ecb-unhighlight-hook-called nil)
(defun ecb-unhighlight-token-header ()
  (let ((key (tree-buffer-event-to-key last-input-event)))
    (when (not (or (and (equal key 'mouse-release)
                        (not ecb-unhighlight-hook-called))
                   (equal key 'mouse-movement)))
      (delete-overlay ecb-method-overlay)
      (remove-hook 'pre-command-hook 'ecb-unhighlight-token-header)))
  (setq ecb-unhighlight-hook-called t))

(defun ecb-token-visit-highlight-token-header (token)
  "Highlights line where `ecb-token-visit-smart-token-start' puts point for TOKEN.

Returns current point"
  (save-excursion
    (ecb-token-visit-smart-token-start token)
    (move-overlay ecb-method-overlay
                  (ecb-line-beginning-pos)
                  (ecb-line-end-pos)
                  (current-buffer)))
  (setq ecb-unhighlight-hook-called nil)
  (add-hook 'pre-command-hook 'ecb-unhighlight-token-header)
  (point))
  
(defun ecb-jump-to-token (filename token &optional window
                                   no-token-visit-post-actions
                                   additional-post-action-list)
  "Jump to token TOKEN in buffer FILENAME.
If NO-TOKEN-VISIT-POST-ACTIONS is not nil then the functions of
`ecb-token-visit-post-actions' are not performed. If
ADDITIONAL-POST-ACTION-LIST is a list of function-symbols then these functions
are performed after these ones of `ecb-token-visit-post-actions'. So if
NO-TOKEN-VISIT-POST-ACTIONS is not nil then only the functions of
ADDITIONAL-POST-ACTION-LIST are performed. If ADDITIONAL-POST-ACTION-LIST is
nil too then no post-actions are performed."
  (cond ((not (ecb-buffer-or-file-readable-p filename))
         (error "ECB: ecb-jump-to-token: Can not open filename %s."
                filename))
        (t
         (unless window
           (setq window (selected-window)))
         (select-window window)
         (ecb-semantic-assert-valid-token token)
         (ecb-nav-save-current)
         (find-file filename)
         ;; let us set the mark so the user can easily jump back.
         (if ecb-token-jump-sets-mark
             (push-mark nil t))
         (widen)
         (goto-char (ecb-semantic-token-start token))
         ;; process post action
         (unless no-token-visit-post-actions
           ;; first the default post actions
           (dolist (f (cdr (assoc 'default ecb-token-visit-post-actions)))
             (funcall f token))
           ;; now the mode specific actions
           (dolist (f (cdr (assoc major-mode ecb-token-visit-post-actions)))
             (funcall f token)))
         ;; now we perform the additional-post-action-list
         (dolist (f additional-post-action-list)
           (funcall f token))
         ;; Klaus Berndl <klaus.berndl@sdm.de>: Now we use a different
         ;; implementation of ecb-nav-token-history-item. Not longer storing
         ;; the whole token but the token-buffer and markers of token-start
         ;; and token-end. This prevents the navigation-tree from getting
         ;; unusable cause of invalid overlays after a full reparse!
         (let* ((tok-buf (or (ecb-semantic-token-buffer token)
                             (current-buffer)))
                (tok-name (semantic-token-name token))
                (tok-start (move-marker (make-marker)
                                        (semantic-token-start token) tok-buf))
                (tok-end (move-marker (make-marker)
                                        (semantic-token-end token) tok-buf)))
           (ecb-nav-add-item (ecb-nav-token-history-item-new
                              tok-name
                              tok-buf
                              tok-start
                              tok-end
                              ecb-buffer-narrowed-by-ecb))))))


(defun ecb-show-any-node-info-by-mouse-moving-p ()
  "Return not nil if for at least one tree-buffer showing node info only by
moving the mouse over a node is activated. See
`ecb-show-node-info-in-minibuffer'."
  (let ((when-list (mapcar (lambda (elem)
                             (car elem))
                           ecb-show-node-info-in-minibuffer)))
    (or (member 'if-too-long when-list)
        (member 'always when-list))))

;; access-functions for when and what value of
;; `ecb-show-node-info-in-minibuffer':
(defun ecb-show-node-info-index (tree-buffer-name)
  (cond ((string= tree-buffer-name ecb-directories-buffer-name)
         0)
        ((string= tree-buffer-name ecb-sources-buffer-name)
         1)
        ((string= tree-buffer-name ecb-history-buffer-name)
         2)
        ((string= tree-buffer-name ecb-methods-buffer-name)
         3)))

(defun ecb-show-node-info-when (tree-buffer-name)
  (car (nth (ecb-show-node-info-index tree-buffer-name)
            ecb-show-node-info-in-minibuffer)))

(defun ecb-show-node-info-what (tree-buffer-name)
  (cdr (nth (ecb-show-node-info-index tree-buffer-name)
            ecb-show-node-info-in-minibuffer)))

(defun ecb-get-file-info-text (file)
  "Return a file-info string for a file in the ECB sources buffer"
  (let ((attrs (file-attributes file)))
    (format "%s %8s %4d %10d %s %s"
	    (nth 8 attrs)
	    (user-login-name (nth 2 attrs))
	    (nth 3 attrs)
	    (nth 7 attrs)
	    (format-time-string "%Y/%m/%d %H:%M" (nth 5 attrs))
            (if (equal (ecb-show-node-info-what ecb-sources-buffer-name)
                       'file-info-full)
                file
              (file-name-nondirectory file)))
    ))

(defun ecb-show-minibuffer-info (node window tree-buffer-name)
  "Checks if in the minibuffer should be displayed any info about the current
node in the ECB-window WINDOW for the tree-buffer TREE-BUFFER-NAME only by
mouse-moving."
  (let ((when-elem (ecb-show-node-info-when tree-buffer-name)))
    (or (eq when-elem 'always)
        (and (eq when-elem 'if-too-long)
             window
             (>= (+ (length (tree-node-get-name node))
                    (tree-buffer-get-node-indent node))
                 (window-width window))))))

(defun ecb-mouse-over-directory-node (node &optional window no-message click-force)
  "Displays help text if mouse moves over a node in the directory buffer or if
CLICK-FORCE is not nil and always with regards to the settings in
`ecb-show-node-info-in-minibuffer'. NODE is the node for which help text
should be displayed, WINDOW is the related window, NO-MESSAGE defines if the
help-text should be printed here."
  (if (= (tree-node-get-type node) 1)
      (ecb-mouse-over-source-node node window no-message click-force)
    (if (not (= (tree-node-get-type node) 3))
        (let ((str (when (or click-force
                             (ecb-show-minibuffer-info node window
                                                       ecb-directories-buffer-name)
                             (and (not (equal (ecb-show-node-info-when ecb-directories-buffer-name)
                                              'never))
                                  (not (string= (tree-node-get-data node)
                                                (tree-node-get-name node)))
                                  (eq (tree-node-get-parent node)
                                      (tree-buffer-get-root))))
                     (if (equal (ecb-show-node-info-what ecb-directories-buffer-name)
                                'name)
                         (tree-node-get-name node)
                       (tree-node-get-data node)))))
          (prog1 str
            (unless no-message
              (tree-buffer-nolog-message str)))))))

(defun ecb-mouse-over-source-node (node &optional window no-message click-force)
  "Displays help text if mouse moves over a node in the sources buffer or if
CLICK-FORCE is not nil and always with regards to the settings in
`ecb-show-node-info-in-minibuffer'. NODE is the node for which help text
should be displayed, WINDOW is the related window, NO-MESSAGE defines if the
help-text should be printed here."
  (let ((str (ignore-errors ;; For buffers that hasn't been saved yet
               (when (or click-force
                         (ecb-show-minibuffer-info node window
                                                   ecb-sources-buffer-name))
                 (if (equal (ecb-show-node-info-what ecb-sources-buffer-name)
                            'name)
                     (tree-node-get-name node)
                   (ecb-get-file-info-text (tree-node-get-data node)))))))
    (prog1 str
      (unless no-message
        (tree-buffer-nolog-message str)))))

(defun ecb-mouse-over-history-node (node &optional window no-message click-force)
  "Displays help text if mouse moves over a node in the history buffer or if
CLICK-FORCE is not nil and always with regards to the settings in
`ecb-show-node-info-in-minibuffer'. NODE is the node for which help text
should be displayed, WINDOW is the related window, NO-MESSAGE defines if the
help-text should be printed here."
  (let ((str (ignore-errors ;; For buffers that hasn't been saved yet
               (when (or click-force
                         (ecb-show-minibuffer-info node window
                                                   ecb-history-buffer-name))
                 (if (equal (ecb-show-node-info-what ecb-history-buffer-name)
                            'name)
                     (tree-node-get-name node)
                   (tree-node-get-data node))))))
    (prog1 str
      (unless no-message
        (tree-buffer-nolog-message str)))))

(defun ecb-mouse-over-method-node (node &optional window no-message click-force)
  "Displays help text if mouse moves over a node in the method buffer or if
CLICK-FORCE is not nil and always with regards to the settings in
`ecb-show-node-info-in-minibuffer'. NODE is the node for which help text
should be displayed, WINDOW is the related window, NO-MESSAGE defines if the
help-text should be printed here."
  (let ((str (when (or click-force
                       (ecb-show-minibuffer-info node window
                                                 ecb-methods-buffer-name))
               (concat
                (tree-node-get-name node)
                (if (and (= 0 (tree-node-get-type node)) (tree-node-get-data
                                                          node)
                         (equal (ecb-show-node-info-what ecb-methods-buffer-name)
                                'name+type))
                    (concat ", "
                            (symbol-name (semantic-token-token (tree-node-get-data node))))
                  "")))))
    (prog1 str
      (unless no-message
        (tree-buffer-nolog-message str)))))

(defvar ecb-idle-timer-alist nil)
(defvar ecb-post-command-hooks nil)
(defun ecb-activate-ecb-sync-functions (idle-value func)
  "Adds function FUNC to `ecb-idle-timer-alist' and activates an idle-timer
with idle-time IDLE-VALUE if IDLE-VALUE not nil. If nil the FUNC is added to
`post-command-hook' and `ecb-post-command-hooks' and removed from the idle-list."
  (let* ((timer-elem (assoc func ecb-idle-timer-alist))
         (timer (cdr timer-elem)))
    (when timer-elem
      (cancel-timer timer)
      (setq ecb-idle-timer-alist (delq timer-elem ecb-idle-timer-alist)))
    (remove-hook 'post-command-hook func)
    (setq ecb-post-command-hooks (delq func ecb-post-command-hooks))
    (if idle-value
        (add-to-list 'ecb-idle-timer-alist
                     (cons func (run-with-idle-timer idle-value t func)))
      (add-hook 'post-command-hook func)
      (add-to-list 'ecb-post-command-hooks func))))

;;====================================================
;; ECB minor mode: Create buffers & menus & maps
;;====================================================

(defun ecb-menu-item (item)
  "Build an XEmacs compatible menu item from vector ITEM.
That is remove the unsupported :help stuff."
  (if ecb-running-xemacs
      (let ((n (length item))
            (i 0)
            slot l)
        (while (< i n)
          (setq slot (aref item i))
          (if (and (keywordp slot)
                   (eq slot :help))
              (setq i (1+ i))
            (setq l (cons slot l)))
          (setq i (1+ i)))
        (apply #'vector (nreverse l)))
    item))

(defvar ecb-menu-name "ECB")
(defvar ecb-menu-bar
  (list
   ecb-menu-name
   (ecb-menu-item
    [ "Select ECB frame"
      ecb-activate
      :active (not (equal (selected-frame) ecb-frame))
      :help "Select the ECB-frame."
      ])
   (ecb-menu-item
    [ "Synchronize ECB windows"
      (ecb-window-sync)
      :active (and (equal (selected-frame) ecb-frame)
                   (ecb-point-in-edit-window))
      :help "Synchronize the ECB windows with the current edit-window."
      ])
   (ecb-menu-item
    [ "Update directories buffer"
      ecb-update-directories-buffer
      :active (equal (selected-frame) ecb-frame)
      :help "Updates the directories buffer with current disk-state"
      ])
   (ecb-menu-item
    [ "Add buffers to history"
      ecb-add-all-buffers-to-history
      :active (and (equal (selected-frame) ecb-frame)
                   (ecb-window-live-p ecb-history-buffer-name))
      :help "Add all current file-buffers to history"
      ])
   "-"
   (ecb-menu-item
    [ "Rebuild methods buffer"
      ecb-rebuild-methods-buffer
      :active (equal (selected-frame) ecb-frame)
      :help "Rebuild the methods buffer completely"
      ])
   (ecb-menu-item
    [ "Expand methods buffer"
      ecb-expand-methods-nodes
      :active (equal (selected-frame) ecb-frame)
      :help "Expand all nodes of a certain indent-level"
      ])
   (ecb-menu-item
    [ "Toggle auto. expanding of the method buffer"
      ecb-toggle-auto-expand-token-tree
      :active (equal (selected-frame) ecb-frame)
      :help "Toggle auto. expanding of the method buffer"
      ])
   "-"
   (ecb-menu-item
    [ "Change layout"
      ecb-change-layout
      :active (equal (selected-frame) ecb-frame)
      :help "Change the layout."
      ])
   (ecb-menu-item
    [ "Redraw layout"
      ecb-redraw-layout
      :active (equal (selected-frame) ecb-frame)
      :help "Redraw the current layout."
      ])
   (ecb-menu-item
    [ "Toggle layout"
      ecb-toggle-layout
      :active (and (equal (selected-frame) ecb-frame)
                   (> (length ecb-toggle-layout-sequence) 1))
      :help "Toggle between several layouts"
      ])
   (ecb-menu-item
    [ "Toggle visibility of ECB windows"
      ecb-toggle-ecb-windows
      :active (equal (selected-frame) ecb-frame)
      :help "Toggle the visibility of all ECB windows."
      ])
   (list
    "Layout administration"
    (ecb-menu-item
     [ "Store current window-sizes"
       ecb-store-window-sizes
       :active (equal (selected-frame) ecb-frame)
       :help "Store current sizes of the ecb-windows in current layout."
       ])
    (ecb-menu-item
     [ "Restore sizes of the ecb-windows"
       ecb-restore-window-sizes
       :active (equal (selected-frame) ecb-frame)
       :help "Restore the sizes of the ecb-windows in current layout."
       ])
    (ecb-menu-item
     [ "Restore default-sizes of the ecb-windows"
       ecb-restore-default-window-sizes
       :active (equal (selected-frame) ecb-frame)
       :help "Restore the default-sizes of the ecb-windows in current layout."
       ])
    "-"
    (ecb-menu-item
     [ "Create new layout"
       ecb-create-new-layout
       :active (equal (selected-frame) ecb-frame)
       :help "Create a new ECB-layout."
       ])
    (ecb-menu-item
     [ "Delete new layout"
       ecb-delete-new-layout
       :active (equal (selected-frame) ecb-frame)
       :help "Delete an user-created ECB-layout."
       ])
    "-"
    (ecb-menu-item
     [ "Show help for a layout"
       ecb-show-layout-help
       :active t
       :help "Show the documentation for a layout."
       ]))
   "-"
   (ecb-menu-item
    [ "Toggle compile window"
      ecb-toggle-compile-window
      :active (equal (selected-frame) ecb-frame)
      :help "Toggle visibility of compile window."
      ])
   (ecb-menu-item
    [ "Toggle enlarged compile window"
      ecb-toggle-compile-window-height
      :active (and (equal (selected-frame) ecb-frame)
                   ecb-compile-window
                   (ecb-compile-window-live-p))
      :help "Toggle enlarged compile window."
      ])
   "-"
   (list
    "Navigate"
    (ecb-menu-item
     ["Previous \(back)"
      ecb-nav-goto-previous
      :active t
      :help "Go to the previous navigation point"
      ])
    (ecb-menu-item
     ["Next \(forward)"
      ecb-nav-goto-next
      :active t
      :help "Go to the next navigation point"
      ]))
   (list
    "Goto window"
    (ecb-menu-item
     ["Edit-window 1"
      ecb-goto-window-edit1
      :active t
      :help "Go to the first edit-window"
      ])
    (ecb-menu-item
     ["Edit-window 2"
      ecb-goto-window-edit2
      :active (ecb-edit-window-splitted)
      :help "Go to the second edit-window \(if splitted\)"
      ])
    (ecb-menu-item
     ["Directories"
      ecb-goto-window-directories
      :active (or (ecb-window-live-p ecb-directories-buffer-name)
                  (and (equal ecb-use-speedbar-instead-native-tree-buffer 'dir)
                       (ignore-errors (ecb-speedbar-active-p))))
      :help "Go to the directories window"
      ])
    (ecb-menu-item
     ["Sources"
      ecb-goto-window-sources
      :active (or (ecb-window-live-p ecb-sources-buffer-name)
                  (and (equal ecb-use-speedbar-instead-native-tree-buffer 'source)
                       (ignore-errors (ecb-speedbar-active-p))))
      :help "Go to the sources window"
      ])
    (ecb-menu-item
     ["Methods and Variables"
      ecb-goto-window-methods
      :active (or (ecb-window-live-p ecb-methods-buffer-name)
                  (and (equal ecb-use-speedbar-instead-native-tree-buffer 'method)
                       (ignore-errors (ecb-speedbar-active-p))))
      :help "Go to the methods/variables window"
      ])
    (ecb-menu-item
     ["History"
      ecb-goto-window-history
      :active (ecb-window-live-p ecb-history-buffer-name)
      :help "Go to the history window"
      ])
    (ecb-menu-item
     ["Speedbar"
      ecb-goto-window-speedbar
      :active (ignore-errors (ecb-speedbar-active-p))
      :help "Go to the integrated speedbar window"
      ])
    (ecb-menu-item
     ["Compilation"
      ecb-goto-window-compilation
      :active (and ecb-compile-window-height ecb-compile-window
                   (window-live-p ecb-compile-window))
      :help "Go to the history window"
      ])
    )
   (list
    "Display window maximized"
    (ecb-menu-item
     ["Edit-window 1"
      (progn
        (ecb-goto-window-edit1)
        (ecb-with-adviced-functions
         (delete-other-windows)))
      :active (ecb-edit-window-splitted)
      :help "Maximize the first edit-window"
      ])
    (ecb-menu-item
     ["Edit-window 2"
      (progn
        (ecb-goto-window-edit2)
        (ecb-with-adviced-functions
         (delete-other-windows)))
      :active (ecb-edit-window-splitted)
      :help "Maximize the second edit-window"
      ])
    (ecb-menu-item
     ["Directories"
      ecb-maximize-window-directories
      :active t
      :help "Maximize the directories window - even if currently not visible"
      ])
    (ecb-menu-item
     ["Sources"
      ecb-maximize-window-sources
      :active t
      :help "Maximize the sources window - even if currently not visible"
      ])
    (ecb-menu-item
     ["Methods and Variables"
      ecb-maximize-window-methods
      :active t
      :help "Maximize the methods/variables window - even if currently not visible"
      ])
    (ecb-menu-item
     ["History"
      ecb-maximize-window-history
      :active (ecb-window-live-p ecb-history-buffer-name)
      :help "Maximize the history window - even if currently not visible"
      ])
    (ecb-menu-item
     ["Speedbar"
      ecb-maximize-window-speedbar
      :active (ignore-errors (ecb-speedbar-active-p))
      :help "Maximize the integrated speedbar window if visible"
      ])
    )
   "-"
   (list
    "Preferences"
    (ecb-menu-item
     ["All..."
      (ecb-customize)
      :active t
      :help "Display all available option-groups..."
      ])
    "-"
    (ecb-menu-item
     ["General..."
      (customize-group "ecb-general")
      :active t
      :help "Customize general ECB options"
      ])
    (ecb-menu-item
     ["Directories..."
      (customize-group "ecb-directories")
      :active t
      :help "Customize ECB directories"
      ])
    (ecb-menu-item
     ["Sources..."
      (customize-group "ecb-sources")
      :active t
      :help "Customize ECB sources"
      ])
    (ecb-menu-item
     ["Methods..."
      (customize-group "ecb-methods")
      :active t
      :help "Customize ECB method display"
      ])
    (ecb-menu-item
     ["History..."
      (customize-group "ecb-history")
      :active t
      :help "Customize ECB history"
      ])
    (ecb-menu-item
     ["Layout..."
      (customize-group "ecb-layout")
      :active t
      :help "Customize ECB layout"
      ])
    (ecb-menu-item
     ["Face options..."
      (customize-group "ecb-face-options")
      :active t
      :help "Customize ECB faces"
      ])
    (ecb-menu-item
     ["Download options..."
      (customize-group "ecb-download")
      :active t
      :help "Customize options for downloading ECB"
      ])
    (ecb-menu-item
     ["Help options..."
      (customize-group "ecb-help")
      :active t
      :help "Customize options for the online help of ECB"
      ])
    (ecb-menu-item
     ["ECB/eshell options..."
      (customize-group "ecb-eshell")
      :active t
      :help "Customize options for the eshell integration of ECB"
      ])
    (ecb-menu-item
     ["Supporting non-semantic-sources..."
      (customize-group "ecb-non-semantic")
      :active t
      :help "Customize options for parsing non-semantic-sources"
      ])
    )
   (list
    "Upgrade and Download"
    (ecb-menu-item
     [ "Upgrade ECB-options to current ECB-version"
       ecb-upgrade-options
       :active (equal (selected-frame) ecb-frame)
       :help "Try to upgrade ECB-options to current ECB-version if necessary."
       ])
    "-"
    (ecb-menu-item
     [ "Download new ECB version"
       ecb-download-ecb
       :active (equal (selected-frame) ecb-frame)
       :help "Download a new ECB-version from the ECB-website."
       ])
    (ecb-menu-item
     [ "Download new semantic version"
       ecb-download-semantic
       :active (equal (selected-frame) ecb-frame)
       :help "Download a new semantic version from the semantic-website."
       ])
    )
   (list
    "Help"
    (ecb-menu-item
     [ "Show Online Help"
       ecb-show-help
       :active t
       :help "Show the online help of ECB."
       ])
    (ecb-menu-item
     [ "List of all commands"
       (let ((ecb-show-help-format 'info))
         (ecb-show-help)
         (Info-goto-node "Command Index"))
       :active t
       :help "Displays an index of all commands in the online-help."
       ])
    (ecb-menu-item
     [ "List of all options"
       (let ((ecb-show-help-format 'info))
         (ecb-show-help)
         (Info-goto-node "Option Index"))
       :active t
       :help "Displays an index of all user-options in the online-help."
       ])
    (ecb-menu-item
     [ "FAQ"
       (let ((ecb-show-help-format 'info))
         (ecb-show-help)
         (Info-goto-node "FAQ"))
       :active t
       :help "Show the FAQ of ECB."
       ])
    (ecb-menu-item
     [ "Submit problem report"
       ecb-submit-problem-report
       :active t
       :help "Submit a problem report to the ECB mailing list."
       ])
    (ecb-menu-item
     [ "ECB Debug mode"
       (setq ecb-debug-mode (not ecb-debug-mode))
       :active t
       :style toggle
       :selected ecb-debug-mode
       :help "Print debug-informations about parsing files in the message buffer."
       ])
    (ecb-menu-item
     [ "ECB Layout Debug mode"
       (setq ecb-layout-debug-mode (not ecb-layout-debug-mode))
       :active t
       :style toggle
       :selected ecb-layout-debug-mode
       :help "Print debug-informations about window-operations in the message buffer."
       ])
    "-"
    (ecb-menu-item
     ["Help preferences..."
      (customize-group "ecb-help")
      :active t
      :help "Customize options for the online help of ECB"
      ])
    "-"
    (concat "ECB " ecb-version)
    )
   "-"
   (ecb-menu-item
    [ "Deactivate ECB"
      ecb-deactivate
      :active t
      :help "Deactivate ECB."
      ])
   )
  "Menu for ECB minor mode.")

(defun ecb-add-to-minor-modes ()
  "Does all necessary to add ECB as a minor mode with current values of
`ecb-mode-map' and `ecb-minor-mode-text'"
  (if (fboundp 'add-minor-mode)
      ;; Emacs 21 & XEmacs
      ;; These Emacs-versions do all necessary itself
      (add-minor-mode 'ecb-minor-mode
                      'ecb-minor-mode-text ecb-mode-map)
    ;; Emacs 20.X
    (let (el)
      (if (setq el (assq 'ecb-minor-mode minor-mode-alist))
          ;; `minor-mode-alist' contains lists, not conses!!
          (setcar (cdr el) 'ecb-minor-mode-text)
        (setq minor-mode-alist
              (cons (list 'ecb-minor-mode 'ecb-minor-mode-text)
                    minor-mode-alist)))
      (if (setq el (assq 'ecb-minor-mode minor-mode-map-alist))
          (setcdr el ecb-mode-map)
        (setq minor-mode-map-alist
              (cons (cons 'ecb-minor-mode ecb-mode-map)
                    minor-mode-map-alist))))))

(defvar ecb-mode-map nil
  "Internal key-map for ECB minor mode.")

(defcustom ecb-key-map
  '("C-c ." . ((t "f" ecb-activate)
               (t "p" ecb-nav-goto-previous)
               (t "n" ecb-nav-goto-next)
               (t "lc" ecb-change-layout)
               (t "lr" ecb-redraw-layout)
               (t "lw" ecb-toggle-ecb-windows)
               (t "lt" ecb-toggle-layout)
               (t "s" ecb-window-sync)
               (t "r" ecb-rebuild-methods-buffer)
               (t "a" ecb-toggle-auto-expand-token-tree)
               (t "x" ecb-expand-methods-nodes)
               (t "o" ecb-show-help)
               (t "g1" ecb-goto-window-edit1)
               (t "g2" ecb-goto-window-edit2)
               (t "gc" ecb-goto-window-compilation)
               (t "gd" ecb-goto-window-directories)
               (t "gs" ecb-goto-window-sources)
               (t "gm" ecb-goto-window-methods)
               (t "gh" ecb-goto-window-history)
               (t "gb" ecb-goto-window-speedbar)
               (t "md" ecb-maximize-window-directories)
               (t "ms" ecb-maximize-window-sources)
               (t "mm" ecb-maximize-window-methods)
               (t "mh" ecb-maximize-window-history)
               (t "mb" ecb-maximize-window-speedbar)
               (t "e" ecb-eshell-goto-eshell)
               (t "\\" ecb-toggle-compile-window)
               (t "/" ecb-toggle-compile-window-height)
               (t "," ecb-cycle-maximized-ecb-buffers)
               (t "." ecb-cycle-through-compilation-buffers)))

  "*Specifies all key-bindings for the ECB minor-mode key-map.
The value is a cons-cell where the car is a common-prefix key for all the
key-bindings. The cdr is a list of key-bindings each of them a list again. A
key-binding has the following form:

  '\(<common-prefix-flag> <keysequence> <function>) where

<common-prefix-flag>: If t then the common-prefixkey defined as car of the
                      value \(see above) is used.
<keysequence>: If the common prefixkey is used then the final key-binding is the
               concatenation of the common-prefixkey \(see above) and this
               keysequence.
<function>: The function to bind to the key. This can also be a
            lambda-expression .

It is highly recommended to use one of the standard keys C-c or C-x as first key
of your common-prefixkey!

You MUST change this option via customize to take effect!

All keysequences must be inserted as a string and must follow the syntax needed
by `read-kbd-macro' or `kbd'. This means you can insert the key in the same
manner \"C-h k\" displays keysequences. Here is the summary of the syntax:

Text is divided into \"words \" separated by whitespace. Except for the words
described below, the characters of each word go directly as characters of the
keysequence. The whitespace that separates words is ignored. Whitespace in the
macro must be written explicitly, as in \"C-c SPC\".

  * The special words RET, SPC, TAB, DEL, LFD, ESC, and NUL represent special
   control characters. The words must be written in uppercase.

  * A word in angle brackets, e.g., <return>, <down>, <left> or <f1>, represents
    a function key. \(Note that in the standard configuration, the function
    key <return> and the control key RET are synonymous.). You can use angle
    brackets on the words RET, SPC, etc., but they are not required there.

  * Keys can be written by their ASCII code, using a backslash followed by up
    to six octal digits. This is the only way to represent keys with codes
    above \377.

  * One or more prefixes M- \(meta), C- \(control), S- \(shift), A- \(alt),
    H- \(hyper), and s- \(super) may precede a character or key notation. For
    function keys, the prefixes may go inside or outside of the brackets:
    C-<down> = <C-down>. The prefixes may be written in any order: M-C-x =
    C-M-x.
    Prefixes are not allowed on multi-key words, e.g., C-abc, except that the
    Meta prefix is allowed on a sequence of digits and optional minus sign:
    M--123 = M-- M-1 M-2 M-3.

  * The `^' notation for control characters also works:  ^M = C-m."
  :group 'ecb-general
  :type '(cons (choice :tag "Common prefix-key"
                       (const :tag "No common prefix-key" :value nil)
                       (string :tag "Prefix-key" :value "C-c ."))
               (repeat :tag "Key-bindings"
                       (list :tag "Key-definition"
                             (boolean :tag "o Use common prefix-key" :value t)
                             (string :tag "o Key")
                             (function :tag "o Function or lambda-expression"
                                       :value nil))))
  :set (function (lambda (symbol value)
                   (set symbol value)
                   ;; make a mode-map and save it
                   (setq ecb-mode-map
                         (let ((km (make-sparse-keymap))
                               (val-list (copy-list (cdr value)))
                               keq-string)
                           (dolist (elem val-list)
                             (setq key-string (concat (if (nth 0 elem) (car value))
                                                      " " (nth 1 elem)))
                             (define-key km (read-kbd-macro key-string) (nth 2 elem)))
                           (easy-menu-define ecb-minor-menu km
                             "ECB Minor Mode Menu" ecb-menu-bar)
                           km))
                   ;; add the minor-mode and and the minor-mode-map to the
                   ;; alists if not already contained. In this case just
                   ;; replace the values in the alists
                   (ecb-add-to-minor-modes))))

;;;###autoload
(defun ecb-activate ()
  "Activates ECB and creates the special buffers for the choosen layout.
For the layout see `ecb-layout-name'. This function raises always the
ECB-frame if called from another frame. This is the same as calling
`ecb-minor-mode' with a positive argument."
  (interactive)
  (ecb-minor-mode 1))

(defun ecb-activate-internal ()
  "Activates the ECB and creates all the buffers and draws the ECB-screen
with the actually chosen layout \(see `ecb-layout-name'). This function raises
always the ECB-frame if called from another frame."

  (if ecb-use-recursive-edit
      (if ecb-minor-mode
	  (progn
	    (message "ECB already activated. Drawing layout.")
            
	    (ecb-redraw-layout))
	(catch 'exit
	  (progn
	    (ecb-activate--impl)
	    (recursive-edit))
	  (ecb-deactivate-internal)))
    
    (ecb-activate--impl))
  ecb-minor-mode)


(defvar ecb-upgrade-check-done nil)

(defun ecb-activate--impl ()
  "See `ecb-activate'.  This is the implementation of ECB activation."

  (when (null ecb-frame)
    (setq ecb-frame (selected-frame)))
  
  (if ecb-minor-mode
      (progn
	(raise-frame ecb-frame)
	(select-frame ecb-frame)
	(ecb-redraw-layout)
	(ecb-update-directories-buffer))

    ;; we activate only if all before-hooks return non nil
    (when (run-hook-with-args-until-failure 'ecb-before-activate-hook)

      ;; checking the requirements
      (ecb-check-requirements)

      ;; initialize the navigate-library
      (ecb-nav-initialize)

      ;; maybe we must upgrade some not anymore compatible or even renamed
      ;; options
      (when (and ecb-auto-compatibility-check
                 (not ecb-upgrade-check-done))
        (ecb-check-not-compatible-options)
        (ecb-upgrade-not-compatible-options)
        (ecb-upgrade-renamed-options)
        (setq ecb-upgrade-check-done t))
      
      ;; first initialize the whole layout-engine
      (ecb-initialize-layout)

      ;; clear the token-tree-cache and the files-subdir-cache
      (ecb-clear-token-tree-cache)
      (ecb-clear-files-and-subdirs-cache)
      (ecb-sources-cache-clear)

      ;; initialize internal vars
      (ecb-initialize-internal-vars)
    
      ;; enable basic advices
      (ecb-enable-basic-advices)

      ;; enable window-function advices
      (ecb-activate-adviced-functions ecb-advice-window-functions)

      ;; set the ecb-frame
      (if ecb-new-ecb-frame
          (progn
            (run-hooks 'ecb-activate-before-new-frame-created-hook)
            (setq ecb-frame (make-frame))
            (put 'ecb-frame 'ecb-new-frame-created t))
        (setq ecb-frame (selected-frame))
        (put 'ecb-frame 'ecb-new-frame-created nil))
      (raise-frame ecb-frame)
      (select-frame ecb-frame)

      (ecb-enable-own-temp-buffer-show-function t)
      
      ;; now we can activate ECB
      (let ((curr-buffer-list (mapcar (lambda (buff)
                                        (buffer-name buff))
                                      (buffer-list))))
        ;; create all the ECB-buffers if they don´t already exist
        (unless (member ecb-directories-buffer-name curr-buffer-list)
          (tree-buffer-create
           ecb-directories-buffer-name
           ecb-frame
           'ecb-interpret-mouse-click
           'ecb-tree-buffer-node-select-callback
           'ecb-tree-buffer-node-expand-callback
           'ecb-mouse-over-directory-node
           'equal
           (list (cons 0 (funcall (or ecb-directories-menu-sorter
                                      'identity)
                                  (append ecb-directories-menu-user-extension
                                          ecb-directories-menu)))
                 (cons 1 (funcall (or ecb-sources-menu-sorter
                                      'identity)
                                  (append ecb-sources-menu-user-extension
                                          ecb-sources-menu)))
                 (cons 2 (funcall (or ecb-directories-menu-sorter
                                      'identity)
                                  (append ecb-directories-menu-user-extension
                                          ecb-source-path-menu))))
           (list (cons 0 ecb-directories-menu-title-creator)
                 (cons 1 ecb-directories-menu-title-creator)
                 (cons 2 ecb-directories-menu-title-creator))
           (nth 0 ecb-truncate-lines)
           t
           ecb-tree-indent
           ecb-tree-incremental-search
           ecb-tree-navigation-by-arrow
           ecb-tree-easy-hor-scroll
           (list (cons "[+]" (and ecb-tree-use-image-icons
                                  (ignore-errors (require 'sb-image))
                                  'speedbar-directory-plus))
                 (cons "[-]" (and ecb-tree-use-image-icons
                                  (ignore-errors (require 'sb-image))
                                  'speedbar-directory-minus)))
           (list (cons 1 ecb-source-in-directories-buffer-face))
           ecb-tree-expand-symbol-before
           ecb-directory-face
           ecb-directories-general-face
           ;; we add an after-create-hook to the tree-buffer
           (append
            (list (function (lambda ()
                              (local-set-key [f2] 'ecb-customize)
                              (local-set-key [f3] 'ecb-show-help)
                              (local-set-key [f4] 'ecb-add-source-path)
                              (local-set-key (kbd "C-t")
                                             'ecb-toggle-RET-selects-edit-window))))
            ecb-common-tree-buffer-after-create-hook
            ecb-directories-buffer-after-create-hook)
           ))
      
        (unless (member ecb-sources-buffer-name curr-buffer-list)
          (tree-buffer-create
           ecb-sources-buffer-name
           ecb-frame
           'ecb-interpret-mouse-click
           'ecb-tree-buffer-node-select-callback
           'ecb-tree-buffer-node-expand-callback
           'ecb-mouse-over-source-node
           'equal
           (list (cons 0 (funcall (or ecb-sources-menu-sorter
                                      'identity)
                                  (append ecb-sources-menu-user-extension
                                          ecb-sources-menu))))
           (list (cons 0 ecb-sources-menu-title-creator))
           (nth 1 ecb-truncate-lines)
           t
           ecb-tree-indent
           ecb-tree-incremental-search
           ecb-tree-navigation-by-arrow
           ecb-tree-easy-hor-scroll
           (list nil nil)
           nil
           nil
           ecb-source-face
           ecb-sources-general-face
           (append
            (list (function (lambda ()
                              (local-set-key (kbd "C-t")
                                             'ecb-toggle-RET-selects-edit-window))))
            ecb-common-tree-buffer-after-create-hook
            ecb-directories-buffer-after-create-hook)))
      
        (unless (member ecb-methods-buffer-name curr-buffer-list)
          (tree-buffer-create
           ecb-methods-buffer-name
           ecb-frame
           'ecb-interpret-mouse-click
           'ecb-tree-buffer-node-select-callback
           nil
           'ecb-mouse-over-method-node
           ;; Function which compares the node-data of a tree-buffer-node in the
           ;; method-buffer for equality. We must compare semantic-tokens but we
           ;; must not compare the tokens with eq or equal because they can be
           ;; re-grouped by semantic-adopt-external-members. the following
           ;; function is a save "equal"-condition for ECB because currently the
           ;; method buffer always displays only tokens from exactly the buffer
           ;; of the current edit-window.
           (if (fboundp 'semantic-equivalent-tokens-p)
               'semantic-equivalent-tokens-p
             (function
              (lambda (l r)
                (and (string= (semantic-token-name l) (semantic-token-name r))
                     (eq (semantic-token-token l) (semantic-token-token r))
                     (eq (ecb-semantic-token-start l) (ecb-semantic-token-start r))
                     (eq (ecb-semantic-token-end l) (ecb-semantic-token-end r))))))
           (list (cons 0 (funcall (or ecb-methods-menu-sorter
                                      'identity)
                                  (append ecb-methods-menu-user-extension
                                          ecb-methods-token-menu)))
                 (cons 1 (funcall (or ecb-methods-menu-sorter
                                      'identity)
                                  (append ecb-methods-menu-user-extension
                                          ecb-common-methods-menu)))
                 (cons 2 (funcall (or ecb-methods-menu-sorter
                                      'identity)
                                  (append ecb-methods-menu-user-extension
                                          ecb-common-methods-menu))))
           (list (cons 0 ecb-methods-menu-title-creator)
                 (cons 1 ecb-methods-menu-title-creator)
                 (cons 2 ecb-methods-menu-title-creator))
           (nth 2 ecb-truncate-lines)
           t
           ecb-tree-indent
           ecb-tree-incremental-search
           ecb-tree-navigation-by-arrow
           ecb-tree-easy-hor-scroll
           (list (cons "[+]" (and ecb-tree-use-image-icons
                                  (ignore-errors (require 'sb-image))
                                  'speedbar-box-plus))
                 (cons "[-]" (and ecb-tree-use-image-icons
                                  (ignore-errors (require 'sb-image))
                                  'speedbar-box-minus)))
           nil
           ecb-tree-expand-symbol-before
           ecb-method-face
           ecb-methods-general-face
           (append
            (list (function (lambda ()
                              (local-set-key (kbd "C-t")
                                             'ecb-toggle-RET-selects-edit-window))))
            ecb-common-tree-buffer-after-create-hook
            ecb-directories-buffer-after-create-hook))
          (setq ecb-methods-root-node (tree-buffer-get-root)))
      
        (unless (member ecb-history-buffer-name curr-buffer-list)
          (tree-buffer-create
           ecb-history-buffer-name
           ecb-frame
           'ecb-interpret-mouse-click
           'ecb-tree-buffer-node-select-callback
           'ecb-tree-buffer-node-expand-callback
           'ecb-mouse-over-history-node
           'equal
           (list (cons 0 (funcall (or ecb-history-menu-sorter
                                      'identity)
                                  (append ecb-history-menu-user-extension
                                          ecb-history-menu))))
           (list (cons 0 ecb-history-menu-title-creator))
           (nth 3 ecb-truncate-lines)
           t
           ecb-tree-indent
           ecb-tree-incremental-search
           ecb-tree-navigation-by-arrow
           ecb-tree-easy-hor-scroll
           (list nil nil)
           nil
           nil
           ecb-history-face
           ecb-history-general-face
           (append
            (list (function (lambda ()
                              (local-set-key (kbd "C-t")
                                             'ecb-toggle-RET-selects-edit-window))))
            ecb-common-tree-buffer-after-create-hook
            ecb-directories-buffer-after-create-hook))))
    
      ;; Now store all tree-buffer-names used by ECB
      ;; ECB must not use the variable `tree-buffers' but must always refer to
      ;; `ecb-tree-buffers'!!
      (setq ecb-tree-buffers (list ecb-directories-buffer-name
                                   ecb-sources-buffer-name
                                   ecb-methods-buffer-name
                                   ecb-history-buffer-name))

      ;; we need some hooks
      (add-hook 'semantic-after-partial-cache-change-hook
                'ecb-update-after-partial-reparse t)
      (add-hook 'semantic-after-toplevel-cache-change-hook
                'ecb-rebuild-methods-buffer-with-tokencache t)
      (ecb-activate-ecb-sync-functions ecb-highlight-token-with-point-delay
                                       'ecb-token-sync)
      (ecb-activate-ecb-sync-functions ecb-window-sync-delay
                                       'ecb-window-sync-function)
      (ecb-activate-ecb-sync-functions ecb-compilation-update-idle-time
                                       'ecb-compilation-buffer-list-changed-p)
      (ecb-activate-ecb-sync-functions nil 'ecb-layout-post-command-hook)
      (add-hook 'pre-command-hook 'ecb-layout-pre-command-hook)
      (add-hook 'after-save-hook 'ecb-update-methods-after-saving)
      (add-hook 'kill-buffer-hook 'ecb-kill-buffer-hook)

      ;; running the compilation-buffer update first time
      (ecb-compilation-buffer-list-init)
      
      ;; ediff-stuff; we operate here only with symbols to avoid bytecompiler
      ;; warnings
      (if (boundp 'ediff-quit-hook)
          (put 'ediff-quit-hook 'ecb-ediff-quit-hook-value
               ediff-quit-hook))
      (add-hook 'ediff-quit-hook 'ediff-cleanup-mess)
      (add-hook 'ediff-quit-hook 'ecb-ediff-quit-hook t)
      
      (setq ecb-minor-mode t)

      ;; menus
      (if ecb-running-xemacs
          (add-submenu nil ecb-minor-menu))

      (add-hook (if ecb-running-xemacs
                    'activate-menubar-hook
                  'menu-bar-update-hook)
                'ecb-compilation-update-menu)

      ;; run personal hooks before drawing the layout
      (run-hooks 'ecb-activate-before-layout-draw-hook)

      ;; now we draw the layout chosen in `ecb-layout'. This function
      ;; activates at its end also the adviced functions if necessary!
      ;; Here are the directories- and history-buffer updated.
      (let ((ecb-redraw-layout-quickly nil))
        (ecb-redraw-layout-full 'no-buffer-sync))
    
      (ecb-with-adviced-functions
       ;; activate the correct edit-window split
       (cond ((equal ecb-split-edit-window 'vertical)
              (split-window-vertically))
             ((equal ecb-split-edit-window 'horizontal)
              (split-window-horizontally))
             ((not ecb-split-edit-window)
              (delete-other-windows))))

      ;; now we synchronize all ECB-windows
      (ecb-current-buffer-sync 'force)
    
      ;; now update all the ECB-buffer-modelines
      (ecb-mode-line-format)

      (when (and ecb-display-default-dir-after-start
                 (null (buffer-file-name (window-buffer ecb-edit-window))))
        (ecb-set-selected-directory
         (ecb-fix-filename (save-excursion
                             (set-buffer (window-buffer ecb-edit-window))
                             default-directory))))
      
      ;; we run any personal hooks
      (run-hooks 'ecb-activate-hook)

      ;; enable mouse-tracking for the ecb-tree-buffers; we do this after running
      ;; the personal hooks because if a user put´s activation of
      ;; follow-mouse.el (`turn-on-follow-mouse') in the `ecb-activate-hook'
      ;; then our own ECB mouse-tracking must be activated later.
      ;; If `turn-on-follow-mouse' would be activated after our own follow-mouse
      ;; stuff, it would overwrite our mechanism and the show-node-name stuff
      ;; would not work!
      (if (ecb-show-any-node-info-by-mouse-moving-p)
          (tree-buffer-activate-follow-mouse))
    
      (message "The ECB is now activated.")

      ;; now we display all `ecb-not-compatible-options' and
      ;; `ecb-renamed-options'
      (when ecb-auto-compatibility-check
        (ecb-display-upgraded-options))

      ;; if we activate ECB first time then we display the node "First steps" of
      ;; the online-manual
      (when (null ecb-source-path)
        (let ((ecb-show-help-format 'info))
          (ecb-show-help)
          (Info-goto-node "First steps")))

      ;; display tip of the day if `ecb-tip-of-the-day' is not nil
      (ecb-show-tip-of-the-day)
      
      ;;now take a snapshot of the current window configuration
      (ecb-set-activated-window-configuration))))


(defun ecb-set-activated-window-configuration()
  "Set the `ecb-activated-window-configuration' after the ECB is activated."

  (save-window-excursion

   ;;set the edit window buffer to *scratch* so that we are not dependent on a
    ;;specific window being available
    
    (set-window-buffer ecb-edit-window (get-buffer-create "*scratch*"))
    
    (setq ecb-activated-window-configuration (current-window-configuration))))

(defun ecb-ediff-quit-hook ()
  "Added to the end of `ediff-quit-hook' during ECB is activated. It
does all necessary after finishing ediff."
  (when ecb-minor-mode
    (if (and (not (equal (selected-frame) ecb-frame))
             (y-or-n-p
              "Ediff finished. Do you want to delete the extra ediff-frame? "))
        (delete-frame (selected-frame) t))
    (select-frame ecb-frame)
    (ecb-redraw-layout)))

(defun ecb-deactivate ()
  "Deactivates the ECB and kills all ECB buffers and windows."
  (interactive)
  (ecb-minor-mode 0))

(defun ecb-deactivate-internal ()
  "Deactivates the ECB and kills all ECB buffers and windows."
  (unless (not ecb-minor-mode)

    (when (run-hook-with-args-until-failure 'ecb-before-deactivate-hook)
      
      ;; deactivating the adviced functions
      (ecb-activate-adviced-functions nil)
      (ecb-disable-basic-advices)

      (ecb-enable-own-temp-buffer-show-function nil)      

      ;; deactivate and reset the speedbar stuff
      (ignore-errors (ecb-speedbar-deactivate))

      ;; deactivating the eshell stuff; activation is done implicitly by
      ;; `ecb-eshell-goto-eshell'!
      (ecb-eshell-deactivate)
      
      (tree-buffer-deactivate-mouse-tracking)
      (tree-buffer-deactivate-follow-mouse)

      ;; remove the hooks
      (remove-hook 'semantic-after-partial-cache-change-hook
                   'ecb-update-after-partial-reparse)
      (remove-hook 'semantic-after-toplevel-cache-change-hook
                   'ecb-rebuild-methods-buffer-with-tokencache)
      (dolist (timer-elem ecb-idle-timer-alist)
        (cancel-timer (cdr timer-elem)))
      (setq ecb-idle-timer-alist nil)
      (dolist (hook ecb-post-command-hooks)
        (remove-hook 'post-command-hook hook))
      (setq ecb-post-command-hooks nil)
      (remove-hook 'pre-command-hook 'ecb-layout-pre-command-hook)
      (remove-hook 'after-save-hook 'ecb-update-methods-after-saving)
      (remove-hook 'kill-buffer-hook 'ecb-kill-buffer-hook)
      (if (get 'ediff-quit-hook 'ecb-ediff-quit-hook-value)
          (setq ediff-quit-hook (get 'ediff-quit-hook
                                     'ecb-ediff-quit-hook-value))
        (remove-hook 'ediff-quit-hook 'ecb-ediff-quit-hook))

      ;; menus
      (if ecb-running-xemacs
          (easy-menu-remove ecb-minor-menu))

      (remove-hook (if ecb-running-xemacs
                       'activate-menubar-hook
                     'menu-bar-update-hook)
                   'ecb-compilation-update-menu)

      ;; run any personal hooks
      (run-hooks 'ecb-deactivate-hook)
    
      ;; clear the ecb-frame
      (when (frame-live-p ecb-frame)
        (raise-frame ecb-frame)
        (select-frame ecb-frame)
        (ecb-select-edit-window)
        ;; first we delete all ECB-windows.
        (delete-other-windows)
        (if (get 'ecb-frame 'ecb-new-frame-created)
            (ignore-errors (delete-frame ecb-frame t))))
    
      (ecb-initialize-layout)

      ;; we can safely do the kills because killing non existing buffers
      ;; doesn´t matter.
      (tree-buffer-destroy ecb-directories-buffer-name)
      (tree-buffer-destroy ecb-sources-buffer-name)
      (tree-buffer-destroy ecb-methods-buffer-name)
      (tree-buffer-destroy ecb-history-buffer-name)

      (setq ecb-activated-window-configuration nil)

      ;; clear the caches
      (ecb-clear-token-tree-cache)
      (ecb-clear-files-and-subdirs-cache)
      (ecb-sources-cache-clear)
      
      (setq ecb-minor-mode nil)))
  (if (null ecb-minor-mode)
      (message "The ECB is now deactivated."))
  ecb-minor-mode)

;;;###autoload
(defun ecb-minor-mode (&optional arg)
  "Toggle ECB minor mode.
With prefix argument ARG, turn on if positive, otherwise off. Return non-nil
if the minor mode is enabled.

\\{ecb-mode-map}"
  (interactive "P")
  (let ((new-state (if (null arg)
                       (not ecb-minor-mode)
                     (> (prefix-numeric-value arg) 0))))
    (if new-state
        (ecb-activate-internal)
      (ecb-deactivate-internal)))
  (if ecb-running-xemacs
      (redraw-modeline t)
    (force-mode-line-update t))
  ecb-minor-mode)

(defun ecb-maximize-ecb-window-menu-wrapper (node)
  (let ((ecb-buffer (current-buffer)))
    (ecb-maximize-ecb-window)
    (ignore-errors (select-window (get-buffer-window ecb-buffer)))))


;; ----------- popup-menus for the tree-buffers ----------------------

(defun ecb-create-source (node)
  "Creates a new sourcefile in current directory."
  (let* ((use-dialog-box nil)
         (dir (ecb-fix-filename
               (funcall (if (file-directory-p (tree-node-get-data node))
                            'identity
                          'file-name-directory)
                        (tree-node-get-data node))))
         (filename (file-name-nondirectory
                    (read-file-name "Source name: " (concat dir "/")))))
    (ecb-select-edit-window)
    (if (string-match "\\.java$" filename)
        (ecb-jde-gen-class-buffer dir filename)
      (find-file (concat dir "/" filename)))
    (when (= (point-min) (point-max))
      (set-buffer-modified-p t)
      (let ((ecb-auto-update-methods-after-save nil))
        (save-buffer))
      (ecb-rebuild-methods-buffer-with-tokencache nil nil t))
    (ecb-remove-dir-from-caches dir)
    (ecb-set-selected-directory dir t)
    (ecb-current-buffer-sync)))

(defun ecb-grep-directory-internal (node find)
  (select-window (or ecb-last-edit-window-with-point ecb-edit-window))
  (let ((default-directory (concat (ecb-fix-filename
                                    (if (file-directory-p
                                         (tree-node-get-data node))
                                        (tree-node-get-data node)
                                      (file-name-directory
                                       (tree-node-get-data node))))
                                   ecb-directory-sep-string)))
    (call-interactively (if find
                            (or (and (fboundp ecb-grep-find-function)
                                     ecb-grep-find-function)
                                'grep-find)
                          (or (and (fboundp ecb-grep-function)
                                   ecb-grep-function)
                              'grep)))))

(defun ecb-grep-find-directory (node)
  (ecb-grep-directory-internal node t))

(defun ecb-grep-directory (node)
  (ecb-grep-directory-internal node nil))

(defun ecb-create-directory (parent-node)
  (make-directory (concat (tree-node-get-data parent-node) "/"
                          (read-from-minibuffer "Directory name: ")))
  (ecb-update-directory-node parent-node)
  (tree-buffer-update))

(defun ecb-delete-directory (node)
  "Deletes current directory."
  (let ((dir (tree-node-get-data node)))
    (when (ecb-confirm (concat "Really delete directory" dir "? "))
      (delete-directory (tree-node-get-data node))
      (ecb-update-directory-node (tree-node-get-parent node))
      (tree-buffer-update))))

(defun ecb-dired-directory-internal (node &optional other)
  (if (not (ecb-edit-window-splitted))
      (ecb-select-edit-window)
    (select-window (or (and ecb-last-edit-window-with-point
                            (window-live-p ecb-last-edit-window-with-point)
                            ecb-last-edit-window-with-point)
                       ecb-edit-window)))
  (let ((dir (ecb-fix-filename
              (funcall (if (file-directory-p (tree-node-get-data node))
                           'identity
                         'file-name-directory)
                       (tree-node-get-data node)))))
    (ecb-with-adviced-functions
     (funcall (if other
                  'dired-other-window
                'dired)
              dir))))

(defun ecb-dired-directory (node)
  (ecb-dired-directory-internal node))

(defun ecb-dired-directory-other-window (node)
  (ecb-dired-directory-internal node 'other))


(defvar ecb-common-directories-menu nil)
(setq ecb-common-directories-menu
      '(("Grep Directory" ecb-grep-directory t)
        ("Grep Directory recursive" ecb-grep-find-directory t)
        ("---")
        ("Open in Dired" ecb-dired-directory t)
        ("Open in Dired other window" ecb-dired-directory-other-window t)
        ("---")        
	("Create Sourcefile" ecb-create-source t)
	("Create Child Directory" ecb-create-directory t)
	("Delete Directory" ecb-delete-directory t)
        ("---")
	("Add Source Path" ecb-add-source-path-node t)))

(defvar ecb-directories-menu nil
  "Built-in menu for the directories-buffer for directories which are not a
source-path of `ecb-source-path'.")
(setq ecb-directories-menu
      (append
       ecb-common-directories-menu
       '(("Make This a Source Path" ecb-node-to-source-path t)
         ("---")
         ("Maximize window" ecb-maximize-ecb-window-menu-wrapper))))


(defvar ecb-directories-menu-title-creator
  (function (lambda (node)
              (tree-node-get-data node)))
  "The menu-title for the directories menu. Has to be either a string or a
function which is called with current node and has to return a string.")


(defvar ecb-source-path-menu nil
  "Built-in menu for the directories-buffer for directories which are elements of
`ecb-source-path'.")
(setq ecb-source-path-menu
      (append
       ecb-common-directories-menu
       '(("Delete Source Path" ecb-delete-source-path t)
         ("---")
         ("Maximize window" ecb-maximize-ecb-window-menu-wrapper))))


(defun ecb-delete-source (node)
  "Deletes current sourcefile."
  (let* ((file (tree-node-get-data node))
         (dir (ecb-fix-filename (file-name-directory file))))
    (when (ecb-confirm (concat "Really delete " (file-name-nondirectory file) "? "))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (ecb-delete-file file)
      (ecb-remove-dir-from-caches dir)
      (ecb-set-selected-directory dir t))))


(defvar ecb-sources-menu nil
  "Built-in menu for the sources-buffer.")
(setq ecb-sources-menu
      '(("Grep Directory" ecb-grep-directory t)
        ("Grep Directory recursive" ecb-grep-find-directory t)
        ("---")
        ("Open Dir in Dired" ecb-dired-directory t)
        ("Open Dir in Dired other window" ecb-dired-directory-other-window t)
        ("---")
	("Create Sourcefile" ecb-create-source t)
        ("Delete Sourcefile" ecb-delete-source t)
        ("---")
        ("Maximize window" ecb-maximize-ecb-window-menu-wrapper)))


(defvar ecb-sources-menu-title-creator
  (function (lambda (node)
              (file-name-nondirectory (tree-node-get-data node))))
  "The menu-title for the sources menu. See
`ecb-directories-menu-title-creator'.")

(defun ecb-methods-menu-jump-and-narrow (node)
  (ecb-method-clicked node 1 nil t '(ecb-token-visit-narrow-token
                                     ecb-token-visit-highlight-token-header)))

(defun ecb-methods-menu-widen (node)
  (ecb-select-edit-window)
  (widen)
  (setq ecb-buffer-narrowed-by-ecb nil))


;; Klaus Berndl <klaus.berndl@sdm.de>: This is for silencing the
;; byte-compiler. Normally there should be no warning when silentcomp-defun
;; is used for hs-minor-mode but....argghhh.
(if (not ecb-running-xemacs)
    (require 'hideshow))

(defun ecb-methods-menu-activate-hs ()
  "Activates `hs-minor-mode' in the buffer of `ecb-path-selected-source'. If
this fails then nil is returned otherwise t."
  (save-excursion
    (set-buffer (get-file-buffer ecb-path-selected-source))
    (if (or (not (boundp 'hs-minor-mode))
            (not hs-minor-mode))
        (if (fboundp 'hs-minor-mode)
            (progn
              (hs-minor-mode 1)
              t)
          nil)
      t)))

(defun ecb-methods-menu-show-block (node)
  (if (not (ecb-methods-menu-activate-hs))
      (ecb-error "hs-minor-mode can not be activated!")
    ;; point must be at beginning of token-name
    (ecb-method-clicked node 1 nil t '(ecb-token-visit-smart-token-start))
    (save-excursion
      (or (looking-at hs-block-start-regexp)
          (re-search-forward hs-block-start-regexp nil t))
      (hs-show-block))
    ;; Now we are at the beginning of the block or - with other word - on that
    ;; position `ecb-method-clicked' has set the point.
    (ecb-token-visit-highlight-token-header (tree-node-get-data node))))

(defun ecb-methods-menu-hide-block (node)
  (if (not (ecb-methods-menu-activate-hs))
      (ecb-error "hs-minor-mode can not be activated!")
    ;; point must be at beginning of token-name
    (ecb-method-clicked node 1 nil t '(ecb-token-visit-smart-token-start))
    (save-excursion
      (or (looking-at hs-block-start-regexp)
        (re-search-forward hs-block-start-regexp nil t))
      (hs-hide-block))
    (ecb-token-visit-highlight-token-header (tree-node-get-data node))))

(defun ecb-methods-menu-collapse-all (node)
  (ecb-expand-methods-nodes-internal -1 nil t))

(defun ecb-methods-menu-expand-0 (node)
  (ecb-expand-methods-nodes-internal 0 nil t))

(defun ecb-methods-menu-expand-1 (node)
  (ecb-expand-methods-nodes-internal 1 nil t))

(defun ecb-methods-menu-expand-2 (node)
  (ecb-expand-methods-nodes-internal 2 nil t))

(defun ecb-methods-menu-expand-all (node)
  (ecb-expand-methods-nodes-internal 100 nil t))

(defvar ecb-common-methods-menu nil
  "Built-in menu for the methods-buffer.")
(setq ecb-common-methods-menu
      '(("Undo narrowing of edit-window" ecb-methods-menu-widen t)
        ("---")
        ("Collapse all" ecb-methods-menu-collapse-all t)
        ("Expand level 0" ecb-methods-menu-expand-0 t)
        ("Expand level 1" ecb-methods-menu-expand-1 t)
        ("Expand level 2" ecb-methods-menu-expand-2 t)
        ("Expand all" ecb-methods-menu-expand-all t)
        ("---")
        ("Maximize window" ecb-maximize-ecb-window-menu-wrapper)))


(defvar ecb-methods-token-menu nil)
(setq ecb-methods-token-menu
      (append '(("Jump to token and hide block" ecb-methods-menu-hide-block t)
                ("Jump to token and show block" ecb-methods-menu-show-block t)
                ("---")
                ("Jump to token and narrow" ecb-methods-menu-jump-and-narrow t))
              ecb-common-methods-menu))

(defvar ecb-methods-menu-title-creator
  (function (lambda (node)
              (let ((data (tree-node-get-data node)))
                (if data
                    (cond ((semantic-token-p data)
                           (semantic-token-name data))
                          ((stringp data)
                           data)
                          (t (tree-node-get-name node)))
                  (tree-node-get-name node)))))
  "The menu-title for the methods menu. See
`ecb-directories-menu-title-creator'.")


;; three easy-entry functions for the history menu for convenience
;; Note: The node argument in the first two functions is not used.

(defun ecb-add-history-buffers-popup (node)
  (ecb-add-all-buffers-to-history))

(defun ecb-clear-history-only-not-existing (node)
  "Removes all history entries from the ECB history buffer where related
buffers does not exist anymore."
  (let ((ecb-clear-history-behavior 'not-existing-buffers))
    (ecb-clear-history)))

(defun ecb-clear-history-all (node)
  "Removes all history entries from the ECB history buffer."
  (let ((ecb-clear-history-behavior 'all))
    (ecb-clear-history)))

(defun ecb-clear-history-node (node)
  "Removes current entry from the ECB history buffer."
  (save-selected-window
    (ecb-exec-in-history-window
     (ecb-remove-from-current-tree-buffer node)
     (tree-buffer-update)
     (tree-buffer-highlight-node-data ecb-path-selected-source))))

(defun ecb-history-kill-buffer (node)
  "Kills the buffer for current entry."
  (ecb-clear-history-node node)
  (let ((data (tree-node-get-data node)))
    (when (get-file-buffer data)
      (kill-buffer (get-file-buffer data)))))


(defvar ecb-history-menu nil
  "Built-in menu for the history-buffer.")
(setq ecb-history-menu
      '(("Grep Directory" ecb-grep-directory t)
        ("Grep Directory recursive" ecb-grep-find-directory t)
        ("---")
        ("Open Dir in Dired" ecb-dired-directory t)
        ("Open Dir in Dired other window" ecb-dired-directory-other-window t)
        ("---")
        ("Delete Sourcefile" ecb-delete-source t)
        ("Kill Buffer" ecb-history-kill-buffer t)
        ("---")
        ("Add all filebuffer to history" ecb-add-history-buffers-popup t)
        ("---")
	("Remove Current Entry" ecb-clear-history-node t)
	("Remove All Entries" ecb-clear-history-all t)
	("Remove Non Existing Buffer Entries"
         ecb-clear-history-only-not-existing t)
        ("---")
        ("Maximize window" ecb-maximize-ecb-window-menu-wrapper)))


(defvar ecb-history-menu-title-creator
  (function (lambda (node)
              (tree-node-get-data node)))
  "The menu-title for the history menu. See
`ecb-directories-menu-title-creator'.")


;; ECB byte-compilation

(defun ecb-compile-file-if-necessary (file &optional force)
  "Compile the ECB-file FILE if necessary. This is done if FORCE is not nil or
FILE.el is newer than FILE.elc or if FILE.elc doesn't exist."
  (let ((elc-file (concat (file-name-sans-extension file) ".elc")))
    (if (or force
	    (not (file-exists-p elc-file))
	    (file-newer-than-file-p file elc-file))
        (byte-compile-file file))))

;;;###autoload
(defun ecb-byte-compile (&optional force-all)
  "Byte-compiles the ECB package.
This is done for all lisp-files of ECB if FORCE-ALL is not nil or for each
lisp-file FILE.el which is either newer than FILE.elc or if FILE.elc doesn't
exist."
  (interactive "P")
  (if (ecb-noninteractive)
      (if (ecb-check-requirements t)
          (ecb-error "Incorrect requirements; check the versions of semantic, eieio and speedbar!"))
    (ecb-check-requirements))
  (let ((load-path
	 (append (list (file-name-directory
			(or (locate-library "semantic")
			    (ecb-error "Semantic is not in the load-path!")))
                       (file-name-directory
			(or (locate-library "eieio")
			    (ecb-error "Eieio is not in the load-path!")))
                       (file-name-directory
			(or (locate-library "speedbar")
			    (ecb-error "Speedbar is not in the load-path!")))
		       (file-name-directory (locate-library "ecb")))
		 load-path))
	(files (directory-files (file-name-directory (locate-library "ecb"))
				t)))
    (save-excursion
      (dolist (file files)
	(if (and (string-match "\\(silentcomp\\|tree-buffer\\|ecb.*\\)\\.el$" file)
                 (not (string-match "ecb-autoloads" file)))
            (ecb-compile-file-if-necessary file force-all))))))

(defun ecb-auto-activate-hook()
  "If necessary, run `ecb-activate' when Emacs is started."
  (when ecb-auto-activate
    (ecb-activate)))

(defvar ecb-last-major-mode nil)


(defun ecb-handle-major-mode-activation ()
  "Added to `post-command-hook' after loading the ecb-library. Handles the
values of `ecb-major-modes-activate' and `ecb-major-modes-deactivate'.
Because this hook of `post-command-hook' does nothing if the major-mode has not
changed there should be no performance-problem!"
  ;; Klaus: I think we need this to prevent doing here (de)activation
  ;; immediately after the button-pressed event (which is a command) because
  ;; then a mysterious window-live-p error for the minibuffer-window occurs if
  ;; we click onto a file which deactivates ECB.
  ;; With this the (de)activation is first done after the button-released
  ;; event which is created by Emacs for every tree-buffer click and is bound
  ;; to a nop.
  ;; At least this is my current interpretation and it works :-)
  ;; TODO: detecting the real reason why this happens and fixing it.
  (if ecb-item-in-tree-buffer-selected
      (setq ecb-item-in-tree-buffer-selected nil)
    ;; do nothing if major-mode has not been changed.
    (when (not (equal ecb-last-major-mode major-mode))
      (let ((last-mode ecb-last-major-mode))
        (setq ecb-last-major-mode major-mode)
        (ignore-errors
          (cond ( ;; ecb-major-modes-activate is "All except deactivated:
                 (and (stringp ecb-major-modes-activate)
                      ;; ecb-major-modes-deactivate must be the major-mode-list
                      (listp ecb-major-modes-deactivate)
                      ecb-major-modes-deactivate
                      (not (listp (car ecb-major-modes-deactivate)))
                      ;; current major-mode must not be contained in
                      ;; ecb-major-modes-deactivate
                      (not (assoc major-mode ecb-major-modes-deactivate))
                      ;; current major-mode must not match the regexp of
                      ;; ecb-major-modes-activate.
                      (not (save-match-data
                             (string-match ecb-major-modes-activate
                                           (symbol-name major-mode))))
                      ;; the window must not be splitted or if splitted the
                      ;; last major-mode must be dired-mode
                      (or (equal (selected-window) (next-window))
                          (equal last-mode 'dired-mode)))
                 (if ecb-minor-mode
                     (and (ecb-point-in-edit-window) (ecb-show-ecb-windows))
                   (ecb-activate)))
                ;; ecb-major-modes-activate is a major-mode list:
                ((and (listp ecb-major-modes-activate)
                      ecb-major-modes-activate
                      (assoc major-mode ecb-major-modes-activate)
                      (or (equal (selected-window) (next-window))
                          (equal last-mode 'dired-mode)))
                 (if ecb-minor-mode
                     (and (ecb-point-in-edit-window) (ecb-show-ecb-windows))
                   (ecb-activate)
                   (let* ((layout (cdr (assoc major-mode
                                              ecb-major-modes-activate)))
                          (layout-to-set (if (equal layout 'default)
                                             (car (or (get 'ecb-layout-name 'saved-value)
                                                      (get 'ecb-layout-name 'standard-value)))
                                           layout)))
                     ;; if we must set a new layout we do this via customizing
                     ;; ecb-layout-name for current Emacs-session!
                     (if (not (string= layout-to-set ecb-layout-name))
                         (customize-set-variable 'ecb-layout-name layout-to-set)))))
                ;; ecb-major-modes-deactivate is "All except activated"
                ((and (listp ecb-major-modes-deactivate)
                      (member (car ecb-major-modes-deactivate)
                              '(hide-all-except-activated
                                deactivate-all-except-activated))
                      (stringp (cdr ecb-major-modes-deactivate))
                      ;; ecb-major-modes-activate must be a major-mode list
                      (listp ecb-major-modes-activate)
                      ecb-major-modes-activate
                      ;; current major-mode must not be contained in
                      ;; ecb-major-modes-activate.
                      (not (assoc major-mode ecb-major-modes-activate))
                      ecb-minor-mode
                      ;; point must be stay in the unsplitted edit-window of ECB
                      (ecb-point-in-edit-window)
                      (or (not (ecb-edit-window-splitted))
                          (equal last-mode 'dired-mode))
                      (not (save-match-data
                             (string-match (cdr ecb-major-modes-deactivate)
                                           (symbol-name major-mode)))))
                 (if (equal (car ecb-major-modes-deactivate)
                            'deactivate-all-except-activated)
                     (ecb-deactivate)
                   (ecb-hide-ecb-windows)))
                ;; ecb-major-modes-deactivate is a major-mode list
                ((and (listp ecb-major-modes-deactivate)
                      (listp (car ecb-major-modes-deactivate))
                      ecb-major-modes-deactivate
                      (assoc major-mode ecb-major-modes-deactivate)
                      ecb-minor-mode
                      (ecb-point-in-edit-window)
                      (or (not (ecb-edit-window-splitted))
                          (equal last-mode 'dired-mode)))
                 (if (equal (cdr (assoc major-mode ecb-major-modes-deactivate))
                            'hide)
                     (ecb-hide-ecb-windows)
                   (ecb-deactivate)))))))))
  
(add-hook 'post-command-hook 'ecb-handle-major-mode-activation)

(add-hook 'emacs-startup-hook 'ecb-auto-activate-hook)


(defun ecb-run-from-menubar ()
  "Activate ECB from the Tools-menu. See `ecb-activate'."
  (interactive)
  (ecb-activate))

(easy-menu-add-item nil
                    '("tools") 
                    (ecb-menu-item
                     [ "Start Code Browser (ECB)"
                       ecb-run-from-menubar
                       :active t
                       :help "Start the Emacs Code Browser."
                       ]))


;; Klaus Berndl <klaus.berndl@sdm.de>: Cause of the magic autostart stuff of
;; the advice-package we must disable at load-time all these advices!!
;; Otherwise would just loading ecb (not deactivating) activating each advice
;; AFTER the FIRST usage of our advices!!
(ecb-disable-basic-advices)
(ecb-activate-adviced-functions nil)
(ecb-speedbar-disable-advices)

(silentcomp-provide 'ecb)

;;; ecb.el ends here
