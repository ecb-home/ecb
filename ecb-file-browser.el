;;; ecb-file-browser.el --- the file-browser of Emacs

;; Copyright (C) 2000 - 2003 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
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

;; $Id$

;;; Commentary:

;; This file contains the code of the file-browser of ECB

(require 'ecb-util)
(require 'tree-buffer)
(require 'ecb-mode-line)
(require 'ecb-navigate)
(require 'ecb-face)
(require 'ecb-speedbar)
(require 'ecb-layout)

;; various loads
(require 'assoc)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(eval-when-compile
  (require 'silentcomp))

(silentcomp-defun ecb-speedbar-update-contents)

(defvar ecb-path-selected-directory nil
  "Path to currently selected directory.")

(defvar ecb-path-selected-source nil
  "Path to currently selected source.")

(defun ecb-file-browser-initialize ()
  (setq ecb-path-selected-directory nil
        ecb-path-selected-source nil))
  
;;====================================================
;; Customization
;;====================================================


(defgroup ecb-directories nil
  "Settings for the directories-buffer in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-sources nil
  "Settings for the sources-buffers in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-history nil
  "Settings for the history-buffer in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")

(defcustom ecb-source-path nil
  "*Paths where to find code sources.
Each path can have an optional alias that is used as it's display name. If no
alias is set, the path is used as display name."
  :group 'ecb-directories
  :group 'ecb-most-important
  :initialize 'custom-initialize-default
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
  :initialize 'custom-initialize-default
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (if (and ecb-minor-mode
			    (functionp 'ecb-set-selected-directory)
                            ecb-path-selected-directory)
		       (ecb-set-selected-directory ecb-path-selected-directory t))))
  :type '(radio (const :tag "Always" :value always)
                (const :tag "Never" :value never)
                (repeat :tag "With these layouts"
                        (string :tag "Layout name"))))


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

(defcustom ecb-cache-directory-contents '((".*" . 50))
  "*Cache contents of certain directories.
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

A directory will only be cached if and only if the directory-name matches
one regexp of this option and its content-number exceeds the related
threshold AND the directory-name does not match and regexp of
`ecb-cache-directory-contents-not'!

The cache entry for a certain directory will be refreshed and actualized only
by using the POWER-click \(see `ecb-primary-secondary-mouse-buttons') in the
directories-buffer of ECB.

Examples:

A value of \(\(\"/usr/home/john_smith/bigdir*\" . 1000)) means the contents of
every subdirectory of the home-directory of John Smith will be cached if the
directory contains more than 1000 entries and its name begins with \"bigdir\".

A value of \(\(\".*\" . 1000)) caches every directory which has more than 1000
entries.

A value of \(\(\".*\" . 0)) caches every directory regardless of the number of
entries.

Please note: If you want your home-dir being cached then you MUST NOT use
\"~\" because ECB tries always to match full path-names!"
  :group 'ecb-directories
  :group 'ecb-most-important
  :type `(repeat (cons (regexp :tag "Directory-regexp")
                       (integer :tag "Filenumber threshold" :value 1000))))


(defcustom ecb-cache-directory-contents-not nil
  "*Do not cache the contents of certain directories.
The value of this option is a list where the each element is a regular
expression a directory must match if it should not being cached.

If a directory-name matches at least one of the regexps of this option the
directory-contents will never being cached. See `ecb-cache-directory-contents'
to see when a directory will be cached.

This option can be useful when normally all directories with a certain amount
of content \(files and subdirs) should be cached but some special directories
not. This can be achieved by:
- setting `ecb-cache-directory-contents' to \(\".*\" . 500): Caches all
  directories with more then 500 entries
- setting `ecb-cache-directory-contents-not' to a value which matches these
  directories which should not being cached \(e.g. \(\"/usr/home/john_smith\")
  excludes the HOME-directory of John Smith from being cached).

Please note: If you want your home-dir exclude from being cached then you MUST
NOT use \"~\" because ECB tries always to match full path-names!"
  :group 'ecb-directories
  :type `(repeat (regexp :tag "Directory-regexp")))

(defcustom ecb-prescan-directories-for-emptyness t
  "*Prescan directories for emptyness.
ECB does this so directories are displayed as empty in the directories-buffer
even without user-interaction \(i.e. in previous ECB-versions the emptyness of
a directory has been first checked when the user has clicked onto a
directory). ECB optimizes this check as best as possible but if a directory
contains a lot of subdirectories which contain in turn a lot of entries, then
expanding such a directory or selecting it takes of course more time as
without this check - at least at the first time \(all following selects of a
directory uses the cached information if its subdirectories are empty or not).
Therefore this feature can be switched of via this option."
  :group 'ecb-directories
  :type 'boolean)

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


(defcustom ecb-excluded-directories-regexp "^\\(CVS\\|\\.[^xX]*\\)$"
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
  :group 'ecb-most-important
  :type '(repeat (cons :tag "Directory file-spec"
                       (regexp :tag "Directory regexp")
                       (choice :tag "Files to display"
                               :menu-tag "Files to display"
                               (const :tag "All files"
                                      :value ("" ""))
                               (const :tag "All, but no backups, objects, etc..."
                                      :value ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\)$\\)\\)" "^\\.\\(x?emacs\\|gnus\\)$"))
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

(defcustom ecb-directories-menu-user-extension
  '(("Version Control"
     (ecb-dir-popup-cvs-status "CVS Status" )
     (ecb-dir-popup-cvs-examine "CVS Examine")
     (ecb-dir-popup-cvs-update "CVS Update")))
  "*Static user extensions for the popup-menu of the directories buffer.
Value is a list of elements of the following type: Each element defines a new
menu-entry and is either:

a) Menu-command: A list containing two sub-elements, whereas the first is the
   function \(a function symbol) being called if the menu-entry is selected
   and the second is the name of the menu-entry.
b) Separator: A one-element-list and the element is the string \"---\": Then a
   non-selectable menu-separator is displayed.
c) Submenu: A list where the first element is the title of the submenu
   displayed in the main-menu and all other elements are either menu-commands
   \(see a) or separators \(see b) or another submenu \(see c). This allows
   deep nested menu-submenu-structures. Currently a level of 4 is allowed but
   in general there could be an infinite depth of nesting but it makes no
   sense - if possible at all - to define infinite nested defcustom-types. So
   there is a limit of 4 levels but tis is not a hard limit: Just increase the
   value of the `ecb-max-submenu-depth' *BEFORE* first loading ECB!

The function of a menu-command must follow the following guidelines: Such a
function must be defined with the macro `tree-buffer-defpopup-command! This
macro defines a new popup-command whereas the newly defined command gets one
argument NODE. See the docstring of `tree-buffer-defpopup-command' for further
details.

Example for the definition of such a popupmenu-command:

\(tree-buffer-defpopup-command ecb-my-special-dir-popup-function
  \"Prints the name of the directory of the node under point.\"
  \(let \(\(node-data=dir \(tree-node-get-data node)))
     \(message \"Dir under node: %s\" node-data=dir)))

Per default the static user-extensions are added at the beginning of the
built-in menu-entries of `ecb-directories-menu' but the whole menu can be
re-arranged with `ecb-directories-menu-sorter'.

These menu-extensions are static. A dynamic menu-extension can be achieved via
`ecb-directories-menu-user-extension-function'."
  :group 'ecb-directories
  :type (ecb-create-menu-user-ext-type 1 ecb-max-submenu-depth))

(defcustom ecb-directories-menu-user-extension-function nil
  "*Dynamic user extensions for the popup-menu of the directories buffer.
A function which has to return a list in the same format like the option
`ecb-directories-menu-user-extension'. This function is called when the user
opens the popup-menu for the directories buffer.

Per default the dynamic user-extensions are added in front of the static
extensions of `ecb-directories-menu-user-extension' but the whole menu can be
re-arranged with `ecb-directories-menu-sorter'."
  :group 'ecb-directories
  :type 'function)

(defcustom ecb-sources-menu-user-extension
  '(("Version control"
     (ecb-file-popup-ediff-revision "Ediff against revision")
     ("---")
     (ecb-file-popup-vc-next-action "Check In/Out")
     (ecb-file-popup-vc-log "Revision history")
     (ecb-file-popup-vc-annotate "Annotate")
     (ecb-file-popup-vc-diff "Diff against last version")))
  "*Static user extensions for the popup-menu of the sources buffer.
For further explanations see `ecb-directories-menu-user-extension'.

The node-argument of a menu-function contains as data the filename of the
source for which the popup-menu has been opened.

Per default the static user-extensions are added at the beginning of the
built-in menu-entries of `ecb-sources-menu' but the whole menu can be
re-arranged with `ecb-sources-menu-sorter'."
  :group 'ecb-sources
  :type (ecb-create-menu-user-ext-type 1 ecb-max-submenu-depth))

(defcustom ecb-sources-menu-user-extension-function nil
  "*Dynamic user extensions for the popup-menu of the sources buffer.
A function which has to return a list in the same format like the option
`ecb-sources-menu-user-extension'. This function is called when the user
opens the popup-menu for the sources buffer.

Per default the dynamic user-extensions are added in front of the static
extensions of `ecb-sources-menu-user-extension' but the whole menu can be
re-arranged with `ecb-sources-menu-sorter'."
  :group 'ecb-sources
  :type 'function)

(defcustom ecb-history-menu-user-extension
  '(("Version control"
     (ecb-file-popup-ediff-revision "Ediff against revision")
     ("---")
     (ecb-file-popup-vc-next-action "Check In/Out")
     (ecb-file-popup-vc-log "Revision history")
     (ecb-file-popup-vc-annotate "Annotate")
     (ecb-file-popup-vc-diff "Diff against last version")))
  "*Static user extensions for the popup-menu of the history buffer.
For further explanations see `ecb-directories-menu-user-extension'.

The node-argument of a menu-function contains as data the filename of the
source for which the popup-menu has been opened.

Per default the static user-extensions are added at the beginning of the
built-in menu-entries of `ecb-history-menu' but the whole menu can be
re-arranged with `ecb-history-menu-sorter'."
  :group 'ecb-history
  :type (ecb-create-menu-user-ext-type 1 ecb-max-submenu-depth))

(defcustom ecb-history-menu-user-extension-function nil
  "*Dynamic user extensions for the popup-menu of the history buffer.
A function which has to return a list in the same format like the option
`ecb-history-menu-user-extension'. This function is called when the user
opens the popup-menu for the history buffer.

Per default the dynamic user-extensions are added in front of the static
extensions of `ecb-history-menu-user-extension' but the whole menu can be
re-arranged with `ecb-history-menu-sorter'."
  :group 'ecb-history
  :type 'function)

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


(defcustom ecb-history-buffer-after-create-hook nil
  "*Local hook running after the creation of the history-buffer.
Every function of this hook is called once without arguments direct after
creating the history-buffer of ECB and it's local key-map. So for example a
function could be added which performs calls of `local-set-key' to define new
key-bindings only for the history-buffer of ECB."
  :group 'ecb-history
  :type 'hook)


;;====================================================
;; Internals
;;====================================================

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


(defmacro ecb-exec-in-history-window (&rest body)
  `(unwind-protect
       (when (ecb-window-select ecb-history-buffer-name)
	 ,@body)
     ))



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
  "Return not nil if DIR matches not any regexp of the option
`ecb-cache-directory-contents-not' but matches at least one regexp in
`ecb-cache-directory-contents' and NUMBER-OF-CONTENTS is greater then the
related threshold."
  (and (not (catch 'exit
              (dolist (elem ecb-cache-directory-contents-not)
                (let ((case-fold-search t))
                  (save-match-data
                    (if (string-match (car elem) dir)
                        (throw 'exit (car elem))))
                  nil))))
       (catch 'exit
         (dolist (elem ecb-cache-directory-contents)
           (let ((case-fold-search t))
             (save-match-data
               (if (and (string-match (car elem) dir)
                        (> number-of-contents (cdr elem)))
                   (throw 'exit (car elem))))
             nil)))))


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
  (let* ((fixed-path (ecb-fix-path dir))
         (cvsignore-content (ecb-file-content-as-string
                            (expand-file-name ".cvsignore" fixed-path)))
        (files nil))
    (when cvsignore-content
      (dolist (f (split-string cvsignore-content))
        (setq files (append (directory-files fixed-path nil
                                             (wildcard-to-regexp f) t)
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
      (let ((files (directory-files (ecb-fix-path dir) nil nil t))
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
        ;; divide real files and subdirs. For really large directories (~ >=
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
 \(<directory> . <cache-entry>) whereas <cache-entry> is a cons-cell too which
contains as car a 3-elem list \(tree-buffer-root <copy of tree-buffer-nodes>
buffer-string) for a full \(i.e. all files) cache and as cdr a 4-elem list
\(tree-buffer-root, tree-buffer-nodes, sources-buffer-string, <filter>) for a
filtered cache where <filter> is another cons-cell \(<filter-regexp> .
<filter-display>).")


(defun ecb-sources-cache-remove (dir)
  "Remove the cache-entry for DIR in `ecb-sources-cache'."
  (let ((cache-elem (assoc dir ecb-sources-cache)))
    (if cache-elem
        (setq ecb-sources-cache (delq cache-elem ecb-sources-cache)))))


(defun ecb-sources-cache-add-full (dir cache-elem-full)
  "Add the full sources-cache CACHE-ELEM-FULL for DIR to
`ecb-sources-cache'. If there is already a full cache-entry then replace it."
  (let ((elem (assoc dir ecb-sources-cache)))
    (if (not elem)
        (setq ecb-sources-cache
              (cons (cons dir (cons cache-elem-full nil))
                    ecb-sources-cache))
      (setcdr elem (cons cache-elem-full
                         (cdr (cdr elem)))))))

(defun ecb-sources-cache-add-filtered (dir cache-elem-filtered)
  "Add the filtered sources-cache CACHE-ELEM-FILTERED for DIR to
`ecb-sources-cache'. If there is already a filtered cache-entry then replace
it."
  (let ((elem (assoc dir ecb-sources-cache)))
    (if (not elem)
        (setq ecb-sources-cache
              (cons (cons dir (cons nil cache-elem-filtered))
                    ecb-sources-cache))
      (setcdr elem (cons (car (cdr elem))
                         cache-elem-filtered)))))

(defun ecb-sources-cache-get-full (dir)
  "Return the full value of a cached-directory DIR, means the 3-element-list
\(tree-buffer-root, tree-buffer-nodes, sources-buffer-string). If no
cache-entry for DIR is available then nil is returned."
  (car (cdr (assoc dir ecb-sources-cache))))

(defun ecb-sources-cache-get-filtered (dir)
  "Return the filtered value of a cached-directory DIR, means the
4-element-list \(tree-buffer-root, tree-buffer-nodes, sources-buffer-string,
filter-regexp). If no cache-entry for DIR is available then nil is returned."
  (cdr (cdr (assoc dir ecb-sources-cache))))

(defun ecb-sources-cache-clear ()
  "Clear the whole cache of `ecb-sources-cache'."
  (setq ecb-sources-cache nil))


(defun ecb-update-sources-buffer (dir-before-update)
  "Updates the sources-buffer with all sources contained in
`ecb-path-selected-directory' - the contents are either newly computed or come
from the `ecb-sources-cache'. DIR-BEFORE-UPDATE is the directory which was
selected before this update."

  ;; Here we add a cache-mechanism which caches for each path the node-tree
  ;; and the whole buffer-string of the sources-buffer. A cache-elem would be
  ;; removed from the cache if a directory is POWER-clicked in the directories
  ;; buffer because this is the only way to synchronize the sources-buffer
  ;; with the disk-contents of the clicked directory. This works because the
  ;; tree of the sources-buffer contains only not expandable nodes (see the
  ;; comment in `ecb-rebuild-methods-buffer-with-tagcache'). If we would
  ;; make the nodes in the Sources-buffer "expandable" this caching would not
  ;; work!
  
  (ecb-exec-in-sources-window
   ;; if we have a filtered cache we must display it - otherwise we use the
   ;; full cache if there is any
   (let ((cache-elem (or (ecb-sources-cache-get-filtered ecb-path-selected-directory)
                         (ecb-sources-cache-get-full ecb-path-selected-directory))))
     (if cache-elem
         (progn
           (tree-buffer-set-root (nth 0 cache-elem))
           (tree-buffer-update nil (cons (nth 2 cache-elem)
                                         (nth 1 cache-elem))))
       (let ((new-tree (tree-node-new-root))
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
           (setq new-cache-elem (list (tree-buffer-get-root)
                                      (ecb-copy-list tree-buffer-nodes)
                                      (buffer-substring (point-min)
                                                        (point-max))))
           (ecb-sources-cache-add-full ecb-path-selected-directory
                                       new-cache-elem))))
           
     (when (not (string= dir-before-update ecb-path-selected-directory))
       (tree-buffer-scroll (point-min) (point-min))))))

(defun ecb-sources-filter-by-ext (ext-str)
  "Filter the sources by extension EXT-STR."
  (if (= (length ext-str) 0)
      (ecb-apply-filter-to-sources-buffer
       "^[^.]+$" ;; matches only filenames with no extension
       "No ext.")
    (ecb-apply-filter-to-sources-buffer
     (format "\\.%s\\'" ext-str)
     (format "*.%s" ext-str))))

(tree-buffer-defpopup-command ecb-popup-sources-filter-by-ext
  "Filter the sources by extension by popup."
  (ecb-sources-filter-by-ext
   (read-string "Insert the filter-extension without leading dot: "
                (and node
                     (file-name-extension (tree-node-get-data node))))))

(defun ecb-sources-filter-by-regexp ()
  "Filter the sources by a regexp. Ask for the regexp."
  (let ((regexp-str (read-string "Insert the filter-regexp: ")))
    (if (> (length regexp-str) 0)
        (ecb-apply-filter-to-sources-buffer regexp-str))))
  
(tree-buffer-defpopup-command ecb-popup-sources-filter-by-regexp
  "Filter the sources by regexp by popup."
  (ecb-sources-filter-by-regexp))
  
(tree-buffer-defpopup-command ecb-popup-sources-filter-none
  "Remove any filter from the sources by popup."
  (ecb-apply-filter-to-sources-buffer nil))
  

(defun ecb-sources-filter ()
  "Apply a filter to the sources-buffer to reduce the number of entries.
So you get a better overlooking. There are three choices:
- Filter by extension: Just insert the extension you want the Sources-buffer
  being filtered. Insert the extension without leading dot!
- Filter by regexp: Insert the filter as regular expression.
- No filter: This means to display an entry for every file in the current
  selected directory \(all except these filter already filtered out by
  `ecb-source-file-regexps' and `ecb-sources-exclude-cvsignore').
Such a filter is only applied to the current selected directory, i.e. each
directory has its own filtered sources-buffer."
  (interactive)
  (let ((choice (ecb-query-string "Filter sources by:"
                                  '("extension" "regexp" "nothing"))))
    (cond ((string= choice "extension")
           (ecb-sources-filter-by-ext
            (read-string "Insert the filter-extension without leading dot: ")))
          ((string= choice "regexp")
           (ecb-sources-filter-by-regexp))
          (t (ecb-apply-filter-to-sources-buffer nil)))))

(defun ecb-sources-filter-modeline-prefix (buffer-name sel-dir sel-source)
  "Compute a mode-line prefix for the Sources-buffer so the current filter
applied to the sources is displayed. This function is only for using by
the option `ecb-mode-line-prefixes'."
  (let ((filtered-cache-elem (ecb-sources-cache-get-filtered sel-dir)))
    (if (null filtered-cache-elem)
        nil ;; no prefix if no filter
      (format "[Filter: %s]" (cdr (nth 3 filtered-cache-elem))))))

(defun ecb-apply-filter-to-sources-buffer (filter-regexp &optional filter-display)
  "Apply the regular expression FILTER-REGEXP to the files of
`ecb-path-selected-directory' and display only the filtered files in the
Sources-buffer. If FILTER-REGEXP is nil then any applied filter is removed and
all files are displayed."
  (save-selected-window
    (ecb-exec-in-sources-window
     (if (null filter-regexp)
         ;; no filtering
         (progn
           ;; remove the filtered cache by setting it to nil
           (ecb-sources-cache-add-filtered ecb-path-selected-directory nil)
           ;; update the sources buffer - because the filtered cache is nil
           ;; the full sources are displayed.
           (ecb-update-sources-buffer ecb-path-selected-directory)
           (tree-buffer-highlight-node-data ecb-path-selected-source))
       ;; apply the filter-regexp
       (let ((new-tree (tree-node-new-root))
             (old-children (tree-node-get-children (tree-buffer-get-root)))
             (all-files (car (ecb-get-files-and-subdirs ecb-path-selected-directory)))
             (filtered-files nil))
         (dolist (file all-files)
           (if (string-match filter-regexp file)
               (setq filtered-files
                     (cons file filtered-files))))
         (if (null filtered-files)
             (message "ECB has not applied this filter because it would filter out all files!")
           ;; building up the new files-tree
           (ecb-tree-node-add-files
            new-tree
            ecb-path-selected-directory
            (nreverse filtered-files)
            0 ecb-show-source-file-extension old-children t)

           ;; updating the buffer itself
           (tree-buffer-set-root new-tree)
           (tree-buffer-update)
           (tree-buffer-scroll (point-min) (point-min))
           (tree-buffer-highlight-node-data ecb-path-selected-source)

           ;; add the new filter to the cache, so the next call to
           ;; `ecb-update-sources-buffer' displays the filtered sources.
           (ecb-sources-cache-add-filtered ecb-path-selected-directory
                                           (list (tree-buffer-get-root)
                                                 (ecb-copy-list tree-buffer-nodes)
                                                 (buffer-string)
                                                 (cons filter-regexp
                                                       (or filter-display
                                                           filter-regexp)))))))))
  ;; now we update the mode-lines so the current filter (can be no filter) is
  ;; displayed in the mode-line. See `ecb-sources-filter-modeline-prefix'.
  (ecb-mode-line-format))

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
                 ;; This functions ensures a correct expandable-state of
                 ;; start-node
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
        ;; now we update the sources buffer for `ecb-path-selected-directory'
        (ecb-update-sources-buffer last-dir))))
  
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


(defvar ecb-history-filter nil
  "A cons-cell where car is the filter-function and the cdr is a string how
the current active filter should be displayed in the modeline of the
History-buffer. The filter-function gets the filename of an existing
file-buffer and has to return not nil if for this filename a history-entry
should be added.")

(defun ecb-reset-history-filter ()
  "Reset the `ecb-history-filter' so all file-buffers are displayed."
  (setq ecb-history-filter '(identity . nil)))

(defun ecb-history-filter-reset-p ()
  (null (cdr ecb-history-filter)))

(ecb-reset-history-filter)

(defun ecb-add-all-buffers-to-history ()
  "Add all current file-buffers to the history-buffer of ECB.
If `ecb-sort-history-items' is not nil then afterwards the history is sorted
alphabetically. Otherwise the most recently used buffers are on the top of the
history and the seldom used buffers at the bottom."
  (interactive)
  (ecb-reset-history-filter)
  (ecb-add-buffers-to-history))

(defun ecb-add-buffers-to-history ()
  "Add exactly these currently existing file-buffers to the history-buffer
which are not filtered out by current value of `ecb-history-filter'."
    ;; first we clear out the history buffer
  (save-excursion
    (set-buffer ecb-history-buffer-name)
    (tree-buffer-clear))
  (mapc (function (lambda (buf)
                    (when (buffer-file-name buf)
                      (ecb-add-item-to-history-buffer
                       (buffer-file-name buf)))))
        (reverse (buffer-list)))
  (ecb-sort-history-buffer)
  (ecb-update-history-window (buffer-file-name ecb-last-source-buffer))
  (when (and (save-excursion
               (set-buffer ecb-history-buffer-name)
               (tree-buffer-empty-p))
             (not (ecb-history-filter-reset-p)))
    (ecb-add-all-buffers-to-history)
    (message "ECB has not applied this filter because it would filter out all entries!"))
  ;; now the modeline has to display the current filter
  (ecb-mode-line-format))

  
(defun ecb-history-filter-modeline-prefix (buffer-name sel-dir sel-source)
  "Compute a mode-line prefix for the History-buffer so the current filter
applied to the history-entries is displayed. This function is only for using
by the option `ecb-mode-line-prefixes'."
  (and (cdr ecb-history-filter)
       (format "[Filter: %s]" (cdr ecb-history-filter))))


(defun ecb-add-item-to-history-buffer (filename)
  "Add a new item for FILENAME to the history buffer if the current filter of
`ecb-history-filter' does not filter out this file."
  (save-excursion
    (ecb-buffer-select ecb-history-buffer-name)
    (tree-node-remove-child-data (tree-buffer-get-root) filename)
    (when (funcall (car ecb-history-filter)
                   filename)
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
        filename t)))))


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

(defun ecb-set-selected-source (filename other-edit-window
					 no-edit-buffer-selection)
  "Updates all the ECB buffers and loads the file. The file is also
displayed unless NO-EDIT-BUFFER-SELECTION is set to non nil. In such case
the file is only loaded invisible in the background, all semantic-parsing
and ECB-Buffer-updating is done but the content of the main-edit window
is not changed. For the allowed values of OTHER-EDIT-WINDOW see
`ecb-combine-ecb-button/edit-win-nr'."
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
    (ecb-tag-sync 'force)))

(defun ecb-clear-history ()
  "Clears the ECB history-buffer."
  (interactive)
  (unless (or (not ecb-minor-mode)
              (not (equal (selected-frame) ecb-frame)))
    (save-selected-window
      (ecb-exec-in-history-window
       (tree-buffer-clear)
       (tree-buffer-update)
       (tree-buffer-highlight-node-data ecb-path-selected-source)))))


(defun ecb-tree-node-add-files
  (node path files type include-extension old-children
        &optional not-expandable)
  "For every file in FILES add a child-node to NODE."
  (dolist (file files)
    (let* ((filename (ecb-fix-filename path file))
           (file-1 (if include-extension
                       file
                     (file-name-sans-extension file)))
           (displayed-file file-1))
      ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Hier wenn dann den
      ;; empty-check einbauen wenn type = 0!
      (tree-node-add-child
       node
       (ecb-new-child
        old-children
        displayed-file
        type filename
        (or not-expandable
            (= type 1)
            (ecb-check-emptyness-of-dir filename))
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
          (tree-node-set-expandable node (or (tree-node-get-children node)))
          ;; if node is not expandable we set its expanded state to nil
          (tree-node-set-expanded node (if (not (tree-node-is-expandable node))
                                           nil
                                         (tree-node-is-expanded node)))))))


(defun ecb-get-source-paths-from-functions ()
  "Return a list of paths found by querying `ecb-source-path-functions'."
  (let ((func ecb-source-path-functions)
	(paths nil)
	(rpaths nil))
    (while func
      (setq paths (append paths (funcall (car ecb-source-path-functions)))
	    func (cdr func)))
    (while paths
      (setq rpaths (cons (ecb-fix-filename (car paths)) rpaths)
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
              ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Hier wenn dann den
              ;; empty-check einbauen!
	      (ecb-new-child old-children name 2 norm-dir
                             (ecb-check-emptyness-of-dir norm-dir)
			     (if ecb-truncate-long-names 'beginning)))))
         (tree-buffer-update))))))


;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: DAS TESTEN TESTEN TESTEN
(defvar ecb-directory-empty-cache nil
  "Cache for every directory if it is empty or not. This is an alist where an
element looks like:
   \(<directory> . \(\[nil|t] . <checked-with-show-sources>")


(defun ecb-directory-empty-cache-add (cache-elem)
  (if (not (ecb-directory-empty-cache-get (car cache-elem)))
      (setq ecb-directory-empty-cache
            (cons cache-elem ecb-directory-empty-cache))))


(defun ecb-directory-empty-cache-get (dir)
  (cdr (assoc dir ecb-directory-empty-cache)))


(defun ecb-directory-empty-cache-remove (dir)
  (let ((elem (assoc dir ecb-directory-empty-cache)))
    (if elem
        (setq ecb-directory-empty-cache
              (delete elem ecb-directory-empty-cache)))))


(defun ecb-clear-directory-empty-cache ()
  (setq ecb-directory-empty-cache nil))


(defun ecb-check-emptyness-of-dir (dir)
  (if (not ecb-prescan-directories-for-emptyness)
      nil
    (let ((cache-value (ecb-directory-empty-cache-get dir))
          (show-sources (ecb-show-sources-in-directories-buffer-p)))
      (if (and cache-value
               (equal (cdr cache-value) show-sources))
          (car cache-value)
        (ecb-directory-empty-cache-remove dir)
        (let ((entries (directory-files dir nil nil t))
              (just-files-means-empty (not show-sources))
              (full-file-name nil)
              (empty-p nil))
          (setq empty-p
                (catch 'found
                  (dolist (e entries)
                    (when (not (member e '("." ".." "CVS")))
                      (setq full-file-name (ecb-fix-filename dir e))
                      (if (file-directory-p full-file-name)
                          (throw 'found 'nil)
                        (if (not just-files-means-empty)
                            (throw 'found 'nil)))))
                  t))
          ;; now we add this value to the cache
          (ecb-directory-empty-cache-add (cons (ecb-fix-filename dir)
                                               (cons empty-p show-sources)))
          empty-p)))))


(defun ecb-new-child (old-children name type data &optional not-expandable shorten-name)
  "Return a node with type = TYPE, data = DATA and name = NAME. Tries to find
a node with matching TYPE and DATA in OLD-CHILDREN. If found no new node is
created but only the fields of this node will be updated. Otherwise a new node
is created."
  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Here we prescan for emptyness
  ;; of directories in case DATA is a directory. We do this with
  ;; `ecb-get-files-and-subdirs' because:
  ;; - only this function takes all ecb-options into account which influences
  ;;   the display of certain directories or files
  ;; - this function gets its informations from a fast cache access if
  ;;   possible
  ;; - stores autom. all scanned directories in its cache so for the next time
  ;;   the access is as fast as possible.
  ;; Nevertheless this mechanism increases the time ECB now needs when
  ;; clicking onto a directory or expanding a directory - the real loss of
  ;; time depends on the number of entries for the directory of DATA! So it
  ;; would be good if we good enhance this! The best would be a fast routine
  ;; which checks only if a directory contains subdirs and/or files and then
  ;; we give 'ecb-get-files-and-subdirs' a new argument which tells this
  ;; function if only the dir should be checked for emptyness or if a full
  ;; files- and subdirs-contents-access should be made. But i doubt if this is
  ;; possible.
  ;; When all this performs fast enough then we make an customizable option
  ;; for `ecb-prescan-directories-for-emptyness'
;;   (if (and ecb-prescan-directories-for-emptyness
;;            (file-directory-p data))
;;       (let ((files-and-subdirs (ecb-get-files-and-subdirs data)))
;;         (if (and (= (length (cdr files-and-subdirs)) 0)
;;                  (or (not (ecb-show-sources-in-directories-buffer-p))
;;                      (= (length (car files-and-subdirs)) 0)))
;;             (setq not-expandable t))))
;;   (if (and ecb-prescan-directories-for-emptyness
;;            (file-directory-p data)
;;            (ecb-check-emptyness-of-dir data))
;;       (setq not-expandable t))
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

(tree-buffer-defpopup-command ecb-add-source-path-node
  "Runs `ecb-add-source-path' from popup."
  (call-interactively 'ecb-add-source-path))


(tree-buffer-defpopup-command ecb-node-to-source-path
  "Add this node to the source-path."
  (ecb-add-source-path (tree-node-get-data node)))


(defun ecb-delete-s (child children sources)
  (when children
    (if (eq child (car children))
	(cdr sources)
      (cons (car sources) (ecb-delete-s child (cdr children) (cdr sources))))))


(tree-buffer-defpopup-command ecb-delete-source-path
  "Delete this source-path via popup."
  (let ((path (tree-node-get-data node)))
    (when (ecb-confirm (concat "Really delete source-path " path "?"))
      (setq ecb-source-path (ecb-delete-s
                             node (tree-node-get-children (tree-node-get-parent node))
                             ecb-source-path))
      (ecb-update-directories-buffer)
      (if (y-or-n-p "Delete source-path also for future-sessions? ")
          (customize-save-variable 'ecb-source-path ecb-source-path)
        (customize-set-variable 'ecb-source-path ecb-source-path)))))


(defun ecb-remove-dir-from-caches (dir)
  (ecb-files-and-subdirs-cache-remove dir)
  (ecb-directory-empty-cache-remove dir)
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


(defun ecb-directory-clicked (node ecb-button edit-window-nr shift-mode)
  "Handle clicking onto NODE in the directories-buffer. ECB-BUTTON can be 1, 2
or 3. If 3 then EDIT-WINDOW-NR contains the number of the edit-window the NODE
should be displayed. For 1 and 2 the value of EDIT-WINDOW-NR is ignored."
  (if (= 3 (tree-node-get-type node))
      (funcall (tree-node-get-data node))
    (ecb-update-directory-node node)
    (if shift-mode
        (ecb-mouse-over-directory-node node nil nil 'force))
    (if (or (= 0 (tree-node-get-type node)) (= 2 (tree-node-get-type node)))
        (progn
          (if (= 2 ecb-button)
              (when (tree-node-is-expandable node)
                (tree-node-toggle-expanded node)
                (ecb-exec-in-directories-window
                 ;; Update the tree-buffer with optimized display of NODE
                 (tree-buffer-update node)))
            
            ;; Removing the element from the sources-cache and the
            ;; files-and-subdirs-cache
            (if shift-mode
                (ecb-remove-dir-from-caches (tree-node-get-data node)))
            
            (ecb-set-selected-directory (tree-node-get-data node) shift-mode)
            ;; if we have running an integrated speedbar we must update the
            ;; speedbar 
            (ecb-directory-update-speedbar (tree-node-get-data node))))
          
;;           (ecb-exec-in-directories-window
;;            ;; Update the tree-buffer with optimized display of NODE
;;            (tree-buffer-update node)))
      (ecb-set-selected-source (tree-node-get-data node)
                               (ecb-combine-ecb-button/edit-win-nr ecb-button edit-window-nr)
			       shift-mode))))


(defun ecb-source-clicked (node ecb-button edit-window-nr shift-mode)
  "Handle clicking onto NODE in the sources-buffer. ECB-BUTTON can be 1, 2 or
3. If 3 then EDIT-WINDOW-NR contains the number of the edit-window the NODE
should be displayed. For 1 and 2 the value of EDIT-WINDOW-NR is ignored."
  (if shift-mode
      (ecb-mouse-over-source-node node nil nil 'force))
  (ecb-set-selected-source (tree-node-get-data node)
                           (ecb-combine-ecb-button/edit-win-nr ecb-button edit-window-nr)
			   shift-mode))


(defun ecb-history-clicked (node ecb-button edit-window-nr shift-mode)
  "Handle clicking onto NODE in the history-buffer. ECB-BUTTON can be 1, 2 or
3. If 3 then EDIT-WINDOW-NR contains the number of the edit-window the NODE
should be displayed. For 1 and 2 the value of EDIT-WINDOW-NR is ignored."
  (if shift-mode
      (ecb-mouse-over-history-node node nil nil 'force))
  (ecb-set-selected-source (tree-node-get-data node)
                           (ecb-combine-ecb-button/edit-win-nr ecb-button edit-window-nr)
                           shift-mode))

(defun ecb-expand-directory-nodes (level)
  "Set the expand level of the nodes in the ECB-directories-buffer.
For argument LEVEL see `ecb-expand-methods-nodes'.

Be aware that for deep structured paths and a lot of source-paths this command
can last a long time - depending of machine- and disk-performance."
  (interactive "nLevel: ")
  (save-selected-window
    (ecb-exec-in-directories-window
     (dolist (node (tree-node-get-children (tree-buffer-get-root)))
       (tree-buffer-expand-node node level))
     (tree-buffer-update)))
  (ecb-current-buffer-sync 'force))


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


;; needs methods
(tree-buffer-defpopup-command ecb-create-source
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
      (ecb-rebuild-methods-buffer-with-tagcache nil nil t))
    (ecb-remove-dir-from-caches dir)
    (ecb-set-selected-directory dir t)
    (ecb-current-buffer-sync)))


(defun ecb-grep-directory-internal (node find)
  (ecb-select-edit-window)
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


(tree-buffer-defpopup-command ecb-grep-find-directory
  "Runs grep-find for current directory."
  (ecb-grep-directory-internal node t))


(tree-buffer-defpopup-command ecb-grep-directory
  "Runs grep for current directory."
  (ecb-grep-directory-internal node nil))


(defun ecb-create-directory (parent-node)
  (make-directory (concat (tree-node-get-data parent-node) "/"
                          (read-from-minibuffer "Directory name: ")))
  (ecb-update-directory-node parent-node)
  (tree-buffer-update))


(tree-buffer-defpopup-command ecb-delete-directory
  "Delete current directory."
  (let ((dir (tree-node-get-data node)))
    (when (ecb-confirm (concat "Really delete directory" dir "? "))
      (delete-directory (tree-node-get-data node))
      (ecb-update-directory-node (tree-node-get-parent node))
      (tree-buffer-update))))


(defun ecb-dired-directory-internal (node &optional other)
  (ecb-select-edit-window)
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


(tree-buffer-defpopup-command ecb-dired-directory
  "Run dired for this directory."
  (ecb-dired-directory-internal node))


(tree-buffer-defpopup-command ecb-dired-directory-other-window
  "Run dired for this directory in the other window."
  (ecb-dired-directory-internal node 'other))


(defun ecb-dir-run-cvs-op (node op op-arg-list)
  (let ((dir (tree-node-get-data node)))
    (funcall op dir op-arg-list)))


(tree-buffer-defpopup-command ecb-dir-popup-cvs-status
  "Check status of directory \(and below) in pcl-cvs mode."
  (ecb-dir-run-cvs-op node 'cvs-status '("-v")))


(tree-buffer-defpopup-command ecb-dir-popup-cvs-examine
  "Examine directory \(and below) in pcl-cvs mode."
  (ecb-dir-run-cvs-op node 'cvs-examine '("-d" "-P")))


(tree-buffer-defpopup-command ecb-dir-popup-cvs-update
  "Update directory \(and below) in pcl-cvs mode."
  (ecb-dir-run-cvs-op node 'cvs-update '("-d" "-P")))


(defvar ecb-common-directories-menu nil)


(setq ecb-common-directories-menu
      '(("Grep"
         (ecb-grep-directory "Grep Directory")
         (ecb-grep-find-directory "Grep Directory recursive"))
        ;;("---")
        ("Dired"
         (ecb-dired-directory "Open in Dired")
         (ecb-dired-directory-other-window "Open in Dired other window"))
        ("---")
	(ecb-create-source "Create Sourcefile")
	(ecb-create-directory "Create Child Directory")
	(ecb-delete-directory "Delete Directory")
        ("---")
	(ecb-add-source-path-node "Add Source Path")))


(defvar ecb-directories-menu nil
  "Built-in menu for the directories-buffer for directories which are not a
source-path of `ecb-source-path'.")
(setq ecb-directories-menu
      (append
       ecb-common-directories-menu
       '((ecb-node-to-source-path "Make This a Source Path")
         ("---")
         (ecb-maximize-ecb-window-menu-wrapper "Maximize window"))))


(defvar ecb-directories-menu-title-creator
  (function (lambda (node)
              (let ((node-type (tree-node-get-type node))
                    (node-data (tree-node-get-name node)))
                    (cond ((= node-type 0) ;; directory
                           (format "%s  (Directory)" node-data))
                          ((= node-type 1) ;; source-file
                           (format "%s  (File)" node-data))
                          ((= node-type 2) ;; source-path
                           (format "%s  (Source-path)" node-data))))))
  "The menu-title for the directories menu. Has to be either a string or a
function which is called with current node and has to return a string.")


(tree-buffer-defpopup-command ecb-open-source-in-editwin1
  "Open current source-file the 1. edit-window."
  ;; We can use `ecb-source-clicked' for history-buffer too because shift-mode
  ;; is nil.
  (ecb-source-clicked node 3 1 nil))
(tree-buffer-defpopup-command ecb-open-source-in-editwin2
  "Open current source-file the 2. edit-window."
  (ecb-source-clicked node 3 2 nil))
(tree-buffer-defpopup-command ecb-open-source-in-editwin3
  "Open current source-file the 3. edit-window."
  (ecb-source-clicked node 3 3 nil))
(tree-buffer-defpopup-command ecb-open-source-in-editwin4
  "Open current source-file the 4. edit-window."
  (ecb-source-clicked node 3 4 nil))
(tree-buffer-defpopup-command ecb-open-source-in-editwin5
  "Open current source-file the 5. edit-window."
  (ecb-source-clicked node 3 5 nil))
(tree-buffer-defpopup-command ecb-open-source-in-editwin6
  "Open current source-file the 6. edit-window."
  (ecb-source-clicked node 3 6 nil))
(tree-buffer-defpopup-command ecb-open-source-in-editwin7
  "Open current source-file the 7. edit-window."
  (ecb-source-clicked node 3 7 nil))
(tree-buffer-defpopup-command ecb-open-source-in-editwin8
  "Open current source-file the 8. edit-window."
  (ecb-source-clicked node 3 8 nil))

(defun ecb-dir/source/hist-menu-editwin-entries ()
  "Generate popup-menu-entries for each edit-window if there are at least 2
edit-windows. Otherwise return nil."
  (let ((edit-win-list (ecb-canonical-edit-windows-list))
        (result nil))
    (when (> (length edit-win-list) 1)
      (dotimes (i (min 8 (length edit-win-list)))
        (setq result
              (append result
                      (list (list (intern (format "ecb-open-source-in-editwin%d" (1+ i)))
                                  (format "edit-window %d" (1+ i)))))))
      (append (list (list "---")) ;; we want a separator
              (list (append (list "Open source-file in ...")
                            result))))))


(defun ecb-directories-menu-creator (tree-buffer-name)
  "Creates the popup-menus for the directories-buffer."
  (setq ecb-layout-prevent-handle-ecb-window-selection t)
  (let ((dyn-user-extension
         (and (functionp ecb-directories-menu-user-extension-function)
              (funcall ecb-directories-menu-user-extension-function)))
        (dyn-builtin-extension (ecb-dir/source/hist-menu-editwin-entries)))
    (list (cons 0 (funcall (or ecb-directories-menu-sorter
                               'identity)
                           (append dyn-user-extension
                                   ecb-directories-menu-user-extension
                                   ecb-directories-menu)))
          (cons 1 (funcall (or ecb-sources-menu-sorter
                               'identity)
                           (append dyn-user-extension
                                   ecb-sources-menu-user-extension
                                   ecb-sources-menu
                                   dyn-builtin-extension)))
          (cons 2 (funcall (or ecb-directories-menu-sorter
                               'identity)
                           (append dyn-user-extension
                                   ecb-directories-menu-user-extension
                                   ecb-source-path-menu))))))


(defvar ecb-source-path-menu nil
  "Built-in menu for the directories-buffer for directories which are elements of
`ecb-source-path'.")
(setq ecb-source-path-menu
      (append
       ecb-common-directories-menu
       '((ecb-delete-source-path "Delete Source Path")
         ("---")
         (ecb-maximize-ecb-window-menu-wrapper "Maximize window"))))


(tree-buffer-defpopup-command ecb-delete-source
  "Deletes current sourcefile."
  (let* ((file (tree-node-get-data node))
         (dir (ecb-fix-filename (file-name-directory file))))
    (when (ecb-confirm (concat "Really delete " (file-name-nondirectory file) "? "))
      (when (get-file-buffer file)
        (kill-buffer (get-file-buffer file)))
      (ecb-delete-file file)
      (ecb-remove-dir-from-caches dir)
      (ecb-set-selected-directory dir t))))


(tree-buffer-defpopup-command ecb-file-popup-ediff-revision
  "Diff file against repository with ediff."
  (let ((file (tree-node-get-data node)))
    (ediff-revision file)))


(tree-buffer-defpopup-command ecb-file-popup-vc-next-action
  "Checkin/out file."
  (let ((file (tree-node-get-data node)))
    (find-file file)
    (vc-next-action nil)))


(tree-buffer-defpopup-command ecb-file-popup-vc-log
  "Print revision history of file."
  (let ((file (tree-node-get-data node)))
    (find-file file)
    (vc-print-log)))


(tree-buffer-defpopup-command ecb-file-popup-vc-annotate
  "Annotate file"
  (let ((file (tree-node-get-data node)))
    (find-file file)
    (vc-annotate nil)))


(tree-buffer-defpopup-command ecb-file-popup-vc-diff
  "Diff file against last version in repository."
  (let ((file (tree-node-get-data node)))
    (find-file file)
    (vc-diff nil)))


(defvar ecb-sources-menu nil
  "Built-in menu for the sources-buffer.")


(setq ecb-sources-menu
      '(("Grep"
         (ecb-grep-directory "Grep Directory")
         (ecb-grep-find-directory "Grep Directory recursive"))
        ("Dired"
         (ecb-dired-directory "Open Dir in Dired")
         (ecb-dired-directory-other-window "Open Dir in Dired other window"))
        ("Filter"
         (ecb-popup-sources-filter-by-ext "Filter by extension")
         (ecb-popup-sources-filter-by-regexp "Filter by a regexp")
         (ecb-popup-sources-filter-none "No filter"))
        ("---")        
	(ecb-create-source "Create Sourcefile")
        (ecb-delete-source "Delete Sourcefile")
        ("---")
        (ecb-maximize-ecb-window-menu-wrapper "Maximize window")))


(defvar ecb-sources-menu-title-creator
  (function (lambda (node)
              (file-name-nondirectory (tree-node-get-data node))))
  "The menu-title for the sources menu. See
`ecb-directories-menu-title-creator'.")

(defun ecb-sources-menu-creator (tree-buffer-name)
  "Creates the popup-menus for the sources-buffer."
  (setq ecb-layout-prevent-handle-ecb-window-selection t)
  (let ((dyn-user-extension
         (and (functionp ecb-sources-menu-user-extension-function)
              (funcall ecb-sources-menu-user-extension-function)))
        (dyn-builtin-extension (ecb-dir/source/hist-menu-editwin-entries)))
    (list (cons 0 (funcall (or ecb-sources-menu-sorter
                               'identity)
                           (append dyn-user-extension
                                   ecb-sources-menu-user-extension
                                   ecb-sources-menu
                                   dyn-builtin-extension))))))

;; history popups

(tree-buffer-defpopup-command ecb-history-kill-buffer
  "Kills the buffer for current entry."
  (let ((data (tree-node-get-data node)))
    (when (get-file-buffer data)
      (kill-buffer (get-file-buffer data)))))

(defun ecb-history-filter-by-ext (ext-str)
  "Filter history entries by extension."
  (if (= (length ext-str) 0)
      (setq ecb-history-filter
            (cons `(lambda (filename)
                     (save-match-data
                       (string-match "^[^.]+$" filename)))
                  "No ext."))
    (setq ecb-history-filter
          (cons `(lambda (filename)
                   (save-match-data
                     (string-match ,(format "\\.%s\\'" ext-str)
                                   filename)))
                (format "*.%s" ext-str))))
  (ecb-add-buffers-to-history))

(tree-buffer-defpopup-command ecb-popup-history-filter-by-ext
  "Filter history entries by extension by popup."
  (let ((ext-str (read-string "Insert the filter-extension without leading dot: "
                              (and node
                                   (file-name-extension (tree-node-get-data node))))))
    (ecb-history-filter-by-ext ext-str)))

(defun ecb-history-filter-by-regexp ()
  "Filter history entries by regexp."
  (let ((regexp-str (read-string "Insert the filter-regexp: ")))
    (if (> (length regexp-str) 0)
        (setq ecb-history-filter
              (cons `(lambda (filename)
                       (save-match-data
                         (string-match ,regexp-str filename)))
                    regexp-str))))
  (ecb-add-buffers-to-history))

(tree-buffer-defpopup-command ecb-popup-history-filter-by-regexp
  "Filter history entries by regexp by popup."
  (ecb-history-filter-by-regexp))
  
(tree-buffer-defpopup-command ecb-popup-history-filter-all-existing
  "No history filter, i.e. add all existing file-buffers to the history."
  (ecb-add-all-buffers-to-history))
  

(defun ecb-history-filter ()
  "Apply a filter to the history-buffer to reduce the number of entries.
So you get a better overlooking. There are three choices:
- Filter by extension: Just insert the extension you want the History-buffer
  being filtered. Insert the extension without leading dot!
- Filter by regexp: Insert the filter as regular expression.
- No filter: This means to display an entry for all currently living
  file-buffers."
  (interactive)
  (let ((choice (ecb-query-string "Filter history by:"
                                  '("extension" "regexp" "no filter"))))
    (cond ((string= choice "extension")
           (ecb-history-filter-by-ext
            (read-string "Insert the filter-extension without leading dot: ")))
          ((string= choice "regexp")
           (ecb-history-filter-by-regexp))
          (t (ecb-add-all-buffers-to-history)))))

(defvar ecb-history-menu nil
  "Built-in menu for the history-buffer.")


(setq ecb-history-menu
      '(("Grep"
         (ecb-grep-directory "Grep Directory")
         (ecb-grep-find-directory "Grep Directory recursive"))
        ;;("---")
        ("Dired"
         (ecb-dired-directory "Open Dir in Dired")
         (ecb-dired-directory-other-window "Open Dir in Dired other window"))
	("Filter"
         (ecb-popup-history-filter-by-ext "Filter by extension")
         (ecb-popup-history-filter-by-regexp "Filter by regexp")
         (ecb-popup-history-filter-all-existing "No filter"))
        ("---")
        (ecb-popup-history-filter-all-existing "Exactly all living file-buffers")
        (ecb-history-kill-buffer "Kill Buffer")
        (ecb-delete-source "Delete Sourcefile")
        ("---")
        (ecb-maximize-ecb-window-menu-wrapper "Maximize window")))


(defvar ecb-history-menu-title-creator
  (function (lambda (node)
              (tree-node-get-name node)))
  "The menu-title for the history menu. See
`ecb-directories-menu-title-creator'.")

(defun ecb-history-menu-creator (tree-buffer-name)
  "Creates the popup-menus for the history-buffer."
  (setq ecb-layout-prevent-handle-ecb-window-selection t)
  (let ((dyn-user-extension
         (and (functionp ecb-history-menu-user-extension-function)
              (funcall ecb-history-menu-user-extension-function)))
        (dyn-builtin-extension (ecb-dir/source/hist-menu-editwin-entries)))
    (list (cons 0 (funcall (or ecb-history-menu-sorter
                               'identity)
                           (append dyn-user-extension
                                   ecb-history-menu-user-extension
                                   ecb-history-menu
                                   dyn-builtin-extension))))))


(defun ecb-create-directories-tree-buffer ()
  "Create the tree-buffer for directories"
  (tree-buffer-create
   ecb-directories-buffer-name
   ecb-frame
   'ecb-interpret-mouse-click
   'ecb-tree-buffer-node-select-callback
   'ecb-tree-buffer-node-expand-callback
   'ecb-tree-buffer-node-collapsed-callback
   'ecb-mouse-over-directory-node
   'equal
   (list 0)
   (list 1)
   'ecb-directories-menu-creator
   (list (cons 0 ecb-directories-menu-title-creator)
         (cons 1 ecb-directories-menu-title-creator)
         (cons 2 ecb-directories-menu-title-creator))
   (nth 0 ecb-truncate-lines)
   t
   ecb-tree-indent
   ecb-tree-incremental-search
   nil
   ecb-tree-navigation-by-arrow
   ecb-tree-easy-hor-scroll
   (nth 0 ecb-tree-image-icons-directories)
   (nth 1 ecb-tree-image-icons-directories)
   ecb-tree-buffer-style
   ecb-tree-guide-line-face
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
                                     'ecb-toggle-RET-selects-edit-window)
                      (if (not ecb-running-xemacs)
                          (define-key tree-buffer-key-map
                            [mode-line mouse-2]
                            'ecb-toggle-maximize-ecb-window-with-mouse)))))
    ecb-common-tree-buffer-after-create-hook
    ecb-directories-buffer-after-create-hook)
   ))

(defun ecb-create-sources-tree-buffer ()
  "Create the tree-buffer for sources"
  (tree-buffer-create
   ecb-sources-buffer-name
   ecb-frame
   'ecb-interpret-mouse-click
   'ecb-tree-buffer-node-select-callback
   'ecb-tree-buffer-node-expand-callback
   'ecb-tree-buffer-node-collapsed-callback
   'ecb-mouse-over-source-node
   'equal
   nil
   nil                     ;(list 0) ;; set this list if you want leaf-symbols
   'ecb-sources-menu-creator
   (list (cons 0 ecb-sources-menu-title-creator))
   (nth 1 ecb-truncate-lines)
   t
   ecb-tree-indent
   ecb-tree-incremental-search
   nil
   ecb-tree-navigation-by-arrow
   ecb-tree-easy-hor-scroll
   (nth 0 ecb-tree-image-icons-directories)
   (nth 2 ecb-tree-image-icons-directories)
   ecb-tree-buffer-style
   ecb-tree-guide-line-face
   nil
   ecb-tree-expand-symbol-before
   ecb-source-face
   ecb-sources-general-face
   (append
    (list (function (lambda ()
                      (local-set-key (kbd "C-t")
                                     'ecb-toggle-RET-selects-edit-window)
                      (if (not ecb-running-xemacs)
                          (define-key tree-buffer-key-map
                            [mode-line mouse-2]
                            'ecb-toggle-maximize-ecb-window-with-mouse)))))
    ecb-common-tree-buffer-after-create-hook
    ecb-directories-buffer-after-create-hook)))

(defun ecb-create-history-tree-buffer ()
  "Create the tree-buffer for history"
  (tree-buffer-create
   ecb-history-buffer-name
   ecb-frame
   'ecb-interpret-mouse-click
   'ecb-tree-buffer-node-select-callback
   'ecb-tree-buffer-node-expand-callback
   'ecb-tree-buffer-node-collapsed-callback
   'ecb-mouse-over-history-node
   'equal
   nil
   nil
   'ecb-history-menu-creator
   (list (cons 0 ecb-history-menu-title-creator))
   (nth 3 ecb-truncate-lines)
   t
   ecb-tree-indent
   ecb-tree-incremental-search
   nil
   ecb-tree-navigation-by-arrow
   ecb-tree-easy-hor-scroll
   (nth 0 ecb-tree-image-icons-directories)
   (nth 4 ecb-tree-image-icons-directories)
   ecb-tree-buffer-style
   ecb-tree-guide-line-face
   nil
   ecb-tree-expand-symbol-before
   ecb-history-face
   ecb-history-general-face
   (append
    (list (function (lambda ()
                      (local-set-key (kbd "C-t")
                                     'ecb-toggle-RET-selects-edit-window)
                      (if (not ecb-running-xemacs)
                          (define-key tree-buffer-key-map
                            [mode-line mouse-2]
                            'ecb-toggle-maximize-ecb-window-with-mouse)))))
    ecb-common-tree-buffer-after-create-hook
    ecb-directories-buffer-after-create-hook)))

(silentcomp-provide 'ecb-file-browser)

;;; ecb-file-browser.el ends here
