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

;; $Id$

;;; Commentary:
;;
;; ECB stands for "Emacs Code Browser".  While Emacs already has good
;; *editing* support for many modes, its *browsing* support is somewhat
;; lacking. That's where ECB comes in: it displays a number of informational
;; windows that allow for easy source code navigation and overview.
;;
;; The informational windows can contain:
;;
;; - A directory tree,
;; - a list of source files in the current directory,
;; - a list of functions/classes/methods/... in the current file, (ECB uses
;;   the Semantic Bovinator, or Imenu, or etags, for getting this list so all
;;   languages supported by any of these tools are automatically supported by
;;   ECB too)
;; - a history of recently visited files, 
;; - the Speedbar and
;; - output from compilation (the "*compilation*" window) and other modes like
;;   help, grep etc. or whatever a user defines to be displayed in this
;;   window.
;;
;; As an added bonus, ECB makes sure to keep these informational windows visible,
;; even when you use C-x 1 and similar commands.
;;
;; It goes without saying that you can configure the layout, ie which
;; informational windows should be displayed where. ECB comes with a number of
;; ready-made window layouts to choose from.
;;
;; Here is an ascii-screenshot of what ECB offers you:
;;
;;   ------------------------------------------------------------------
;;   |              |                                                 |
;;   | Directories  |                                                 |
;;   |              |                                                 |
;;   |--------------|                                                 |
;;   |              |                                                 |
;;   | Sources      |                                                 |
;;   |              |                                                 |
;;   |--------------|                   Edit-area                     |
;;   |              |    (can be splitted in several edit-windows)    |
;;   | Methods/Vars |                                                 |
;;   |              |                                                 |
;;   |--------------|                                                 |
;;   |              |                                                 |
;;   | History      |                                                 |
;;   |              |                                                 |
;;   ------------------------------------------------------------------
;;   |                                                                |
;;   |                 Compilation-window (optional)                  |
;;   |                                                                |
;;   ------------------------------------------------------------------
;;

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
;; - Semantic, author-version between >= 1.4
;;   (http://cedet.sourceforge.net/semantic.shtml).
;; - Eieio, author-version >= 0.17
;;   (http://cedet.sourceforge.net/eieio.shtml).
;; - speedbar, author version >= 0.14beta1
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

(eval-when-compile
  (require 'silentcomp))


;; We need this libraries already here if we miss some requirements and we
;; want to offer the user to download them.
(require 'ecb-upgrade)
(require 'ecb-util)


;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: When the cedet 2.X library is
;; stable then we should handle here cedet instead of these three single
;; libraries. 

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

;; rest of ecb loads
(require 'tree-buffer)
(require 'ecb-file-browser)
(require 'ecb-method-browser)
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
(require 'ecb-winman-support)
(require 'ecb-compatibility)

;; various loads
(require 'assoc)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))


;; XEmacs
(silentcomp-defun redraw-modeline)
(silentcomp-defvar modeline-map)
;; Emacs
(silentcomp-defun force-mode-line-update)

(silentcomp-defvar dired-directory)
(silentcomp-defvar current-menubar)
(silentcomp-defun find-menu-item)
(silentcomp-defun add-submenu)
(silentcomp-defun delete-menu-item)
(silentcomp-defun ediff-cleanup-mess)
(silentcomp-defvar ediff-quit-hook)
(silentcomp-defun Info-goto-node)

(silentcomp-defun ecb-speedbar-active-p)
(silentcomp-defun ecb-speedbar-deactivate)
(silentcomp-defvar ecb-speedbar-buffer-name)


;;====================================================
;; Variables
;;====================================================
(defvar ecb-tree-buffers nil
  "The names of the tree-buffers of ECB.")

(defvar ecb-major-mode-selected-source nil
  "Major-mode of currently selected source.")

(defvar ecb-item-in-tree-buffer-selected nil
  "Only true if any item in any tree-buffer has been selected in recent
command.")

(defun ecb-initialize-internal-vars ()
  (setq ecb-tree-buffers nil
        ecb-major-mode-selected-source nil
        ecb-item-in-tree-buffer-selected nil)
  (ecb-file-browser-initialize)
  (ecb-method-browser-initialize))

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

(defgroup ecb-tree-buffer nil
  "General settings related to the tree-buffers of ECB."
  :group 'ecb
  :prefix "ecb-")

(defgroup ecb-most-important nil
  "The most important settings of ECB you should know."
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
  :group 'ecb-most-important
  :type 'boolean)

(defcustom ecb-activation-selects-ecb-frame-if-already-active 'ask
  "*Trying to activate an already activated ECB selects the ECB-frame.
If t then the ECB-frame is selected, if nil then it is not. If 'ask then ECB
asks if the ECB-frame should be selected if the current-frame is not the
`ecb-frame'."
  :group 'ecb-general
  :type '(radio (const :tag "Select the ECB-frame" :value t)
                (const :tag "Ask if the ECB-frame should be selected" :value ask)
                (const :tag "Do not select the ECB-frame" :value nil)))

(defcustom ecb-major-modes-show-or-hide (cons nil nil)
  "*List of major-modes which show or hide the ecb-windows.
The value is a cons-cell where the car contains all major-mode-symbols which
should show the special ecb-windows and the cdr contains all
major-mode-symbols which should hide the special ecb-windows. If the symbol of
a major-mode is neither contained in the car-\"show-list\" nor in the
cdr-\"hide-list\" then the visibility-state of the ecb-windows does not
change."
  :group 'ecb-general
  :group 'ecb-most-important
  :type '(cons (repeat :tag "Modes for activation"
                       (symbol :tag "Major-mode"))
               (repeat :tag "Modes for deactivation"
                       (symbol :tag "Major-mode"))))


(defcustom ecb-clear-caches-before-activate nil
  "*Clear all ECB internal caches before startup.
If t then ECB clears all its internal caches before starting up. Caches are
used for files- and subdirs \(see `ecb-cache-directory-contents' and
`ecb-cache-directory-contents-not') for semantic-tags and for the
history-filter.

This caches are completely clean at load-time of the ECB-library!

Default is nil, because is makes sense not to clear these caches at start-time
because ECB is often deacticated temporally especially in combination with
window-managers like escreen.el. In these situations the internal state of ECB
should be preserved for next activation."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-bucket-node-display '("" "" ecb-bucket-node-face)
  "*How ECB displays bucket-nodes in a ECB tree-buffer.
Bucket-nodes have only one job: Nodes with similar properties will be dropped
into one bucket for such a common property and all these nodes will be added
as children to the bucket-node. Besides being expandable and collapsable a
bucket-node has no senseful action assigned. Examples for bucket-nodes are
\"[+] Variables\", \"[+] Dependencies\" etc. in the Methods-buffer or buckets
which combine filenames with same extension under a bucket-node with name this
extension.

This option defines how bucket-node should be displayed. The name of the
bucket-node is computed by ECB but you can define a prefix, a suffix and a
special face for the bucket-node

The default are empty prefix/suffix-strings and 'ecb-bucket-node-face'. But
an alternative can be for example '\(\"[\" \"]\" nil) which means no special
face and a display like \"[+] [<bucket-name>]\"."
  :group 'ecb-general
  :set (function (lambda (symbol value)
		   (set symbol value)
		   (ecb-clear-tag-tree-cache)))
  :type '(list (string :tag "Bucket-prefix" :value "[")
               (string :tag "Bucket-suffix" :value "]")
               (choice :tag "Bucket-face" :menu-tag "Bucket-face"
                       (const :tag "No special face" :value nil)
                       (face :tag "Face" :value ecb-bucket-node-face)))
  :initialize 'custom-initialize-default)

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
                   (ecb-redraw-layout-full))))

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
  :group 'ecb-general
  :type 'function)

(defcustom ecb-grep-find-function (if (fboundp 'igrep-find)
                                      'igrep-find 'grep-find)
  "*Function used for performing a recursive grep.
For more Details see option `ecb-grep-function' and replace \"grep\" with
\"recursive grep\" or \"grep-find\"."
  :group 'ecb-general
  :type 'function)

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
  :group 'ecb-tree-buffer
  :set (function (lambda (sym val)
                   (set sym val)
                   (setq ecb-tree-RET-selects-edit-window--internal
                         (ecb-copy-list val))))
  :type `(set (const :tag ,ecb-directories-buffer-name
                     :value ,ecb-directories-buffer-name)
              (const :tag ,ecb-sources-buffer-name
                     :value ,ecb-sources-buffer-name)
              (const :tag ,ecb-methods-buffer-name
                     :value ,ecb-methods-buffer-name)
              (const :tag ,ecb-history-buffer-name
                     :value ,ecb-history-buffer-name)))

(defcustom ecb-tree-indent 4
  "*Indent size for tree buffer.
If you change this during ECB is activated you must deactivate and activate
ECB again to take effect."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type 'integer)

(defcustom ecb-tree-expand-symbol-before t
  "*Show the expand symbol before the items in a tree.
When the expand-symbol is located before the items then the tree looks like:

\[-] ECB
    \[+] code-save
    \[-] ecb-images
        \[-] directories

When located after then the tree looks like:

ECB \[-]
  code-save \[+]
  ecb-images \[-]
    directories \[-]

The after-example above use a value of 2 for `ecb-tree-indent' whereas the
before-example uses a value of 4.

It is recommended to display the expand-symbol before because otherwise it
could be that with a deep nested item-structure with and/or with long
item-names \(e.g. a deep directory-structure with some long
subdirectory-names) the expand-symbol is not visible in the tree-buffer and
the tree-buffer has to be horizontal scrolled to expand an item."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type 'boolean)


(defcustom ecb-tree-buffer-style (if ecb-images-can-be-used
                                     'image
                                   'ascii-guides)
  "*The style of the tree-buffers.
There are three different styles available:

Image-style \(value 'image):
Very nice and modern - just try it. For this style the options
`ecb-tree-indent' and `ecb-tree-expand-symbol-before' have no effect!
Note: GNU Emacs <= 21.3.X for Windows does not support image-display so ECB
uses always 'ascii-guides even when here 'image is set!

Ascii-style with guide-lines \(value 'ascii-guides):
\[-] ECB
 |  \[+] code-save
 `- \[-] ecb-images
     |  \[-] directories
     |   |  \[-] height-15
     |   |   |  * close.xpm
     |   |   |  * empty.xpm
     |   |   |  * leaf.xpm
     |   |   `- * open.xpm
     |   |  \[+] height-17
     |   |  \[+] height-19
     |   `- \[+] height-21
     |  \[x] history
     |  \[x] methods
     `- \[x] sources

Ascii-style without guide-lines \(value 'ascii-no-guides) - this is the style
used by ECB <= 1.96:
\[-] ECB
    \[+] code-save
    \[-] ecb-images
        \[-] directories
            \[-] height-15
                * close.xpm
                * empty.xpm
                * leaf.xpm
                * open.xpm
            \[+] height-17
            \[+] height-19
            \[+] height-21
        \[x] history
        \[x] methods
        \[x] sources

With both ascii-styles the tree-layout can be affected with the options
`ecb-tree-indent' and `ecb-tree-expand-symbol-before'."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type '(radio (const :tag "Images-style" :value image)
                (const :tag "Ascii-style with guide-lines" :value ascii-guides)
                (const :tag "Ascii-style w/o guide-lines" :value ascii-no-guides)))

(defcustom ecb-tree-image-icons-directories
  (let ((base (concat (if ecb-regular-xemacs-package-p
                          (format "%s/" (locate-data-directory "ecb"))
                        ecb-ecb-dir)
                      "ecb-images/")))
        (append (mapcar (function (lambda (i)
                                    (if i
                                        (concat base i))))
                        '("default/height-17"
                          "directories/height-17"
                          nil
                          "methods/height-15_to_21"
                          nil))))
  "*Directories where the images for the tree-buffer can be found.
This is a five-element list where:
1. element: Default directory where the default images for the tree-buffer can
   be found. It should contain an image for every name of
   `tree-buffer-tree-image-names'. The name of an image-file must be:
   \"ecb-<NAME of TREE-BUFFER-TREE-IMAGE-NAMES>.<ALLOWED EXTENSIONS>\".
2. element: Directory for special images for the Directories-buffer.
3. element: Directory for special images for the Sources-buffer.
4. element: Directory for special images for the Methods-buffer.
5. element: Directory for special images for the History-buffer.

The directories of the elements 2 - 5 are additional image-directories which
are searched first for images needed for the respective tree-buffer. If the
image can not be found in this directory then the default-directory \(1.
element) is searched. If the image can't even be found there the related
ascii-symbol is used - which is defined in `tree-buffer-tree-image-names'.

All but the first element \(the default directory) can be nil.

ECB comes with images defined in four different heights - so for the most
senseful font-heights of a tree-buffer a fitting image-size should be
available. The images reside either in the subdirectory \"ecb-images\" of the
ECB-installation or - if ECB is installed as regular XEmacs-package - in the
ECB-etc data-directory \(the directory returned by \(locate-data-directory
\"ecb\")."
  :group 'ecb-tree-buffer
  :type '(list (directory :tag "Full default image-path")
               (choice :tag "Directories" :menu-tag "Directories"
                       (const :tag "No special path" :value nil)
                       (directory :tag "Full image-path for directories"))
               (choice :tag "Sources" :menu-tag "Sources"
                       (const :tag "No special path" :value nil)
                       (directory :tag "Full image-path for sources"))
               (choice :tag "Methods" :menu-tag "Methods"
                       (const :tag "No special path" :value nil)
                       (directory :tag "Full image-path for methods"))
               (choice :tag "History" :menu-tag "History"
                       (const :tag "No special path" :value nil)
                       (directory :tag "Full image-path for history"))))

(defcustom ecb-truncate-lines '(t t t t)
  "*Truncate lines in ECB buffers.
If you change this during ECB is activated you must deactivate and activate
ECB again to take effect."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
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
  :group 'ecb-tree-buffer
  :type '(radio :value 5
                (const :tag "No hor. mouse scrolling" :value nil)
                (integer :tag "Scroll step")))

(defcustom ecb-truncate-long-names t
  "*Truncate long names that don't fit in the width of the ECB windows.
If you change this during ECB is activated you must deactivate and activate
ECB again to take effect."
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
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
                       (ecb-activate-ecb-autocontrol-functions
                        value 'ecb-window-sync-function))))
  :initialize 'custom-initialize-default)

(defcustom ecb-tree-incremental-search 'prefix
  "*Enable incremental search in the ECB-tree-buffers.
For a detailed explanation see the online help section \"Working with the
keyboard in the ECB buffers\". If you change this during ECB is activated you
must deactivate and activate ECB again to take effect."
  :group 'ecb-tree-buffer
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
  :group 'ecb-tree-buffer
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
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
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
  option `ecb-mouse-click-destination'.
- ECB Methods: Jumping to the method in that edit-window specified by the
  option `ecb-mouse-click-destination'.

A click with the primary mouse-button while the SHIFT-key is pressed called
the POWER-click and does the following \(depending on the ECB-buffer where the
POWER-click occurs):
+ Directory-buffer: Refreshing the directory-contents-cache \(see
  `ecb-cache-directory-contents').
+ Sources- and History-buffer: Only displaying the source-contents in the
  method-buffer but not displaying the source-file in the edit-window.
+ Methods-buffer: Narrowing to the clicked method/variable/ect... \(see
  `ecb-tag-visit-post-actions'). This works only for sources supported by
  semantic!

In addition always the whole node-name is displayed in the minibuffer after a
POWER-click \(for this see also `ecb-show-node-info-in-minibuffer').

The secondary mouse-button is for opening \(jumping to) the file in another
edit-window \(see the documentation `ecb-mouse-click-destination').

The following combinations are possible:
- primary: mouse-2, secondary: C-mouse-2 \(means mouse-2 while CTRL-key is
  pressed). This is the default setting.
- primary: mouse-1, secondary: C-mouse-1
- primary: mouse-1, secondary: mouse-2

Note: If the tree-buffers are used with the keyboard instead with the mouse
then [RET] is interpreted as primary mouse-button and [C-RET] as secondary
mouse-button!

If you change this during ECB is activated you must deactivate and activate
ECB again to take effect!"
  :group 'ecb-tree-buffer
  :group 'ecb-most-important
  :type '(radio (const :tag "Primary: mouse-2, secondary: Ctrl-mouse-2"
                       :value mouse-2--C-mouse-2)
                (const :tag "Primary: mouse-1, secondary: Ctrl-mouse-1"
                       :value mouse-1--C-mouse-1)
                (const :tag "Primary: mouse-1, secondary: mouse-2"
                       :value mouse-1--mouse-2)))

;; Thanks to David Hay for the suggestion <David.Hay@requisite.com>
(defcustom ecb-mouse-click-destination 'last-point
  "*Destination of a mouse-button click.
Defines in which edit-window \(if splitted) ECB does the \"right\" action
\(opening a source, jumping to a method/variable etc.) after clicking with a
mouse-button \(see `ecb-primary-secondary-mouse-buttons') onto a node. There
are two possible choices:
- left-top: Does the \"right\" action always in the left/topmost edit-window.
- last-point: Does the \"right\" action always in that edit-window which had
  the point before.
This is if the user has clicked either with the primary mouse-button or
has activated a popup-menu in the tree-buffer.

A click with the secondary mouse-button \(see again
`ecb-primary-secondary-mouse-buttons') does the \"right\" action always in
another edit-window related to the setting in this option: If there are two
edit-windows then the \"other\" edit-window is used and for more than 2
edit-windows the \"next\" edit-window is used \(whereas the next edit-window
of the last edit-window is the first edit-window).

If the edit-window is not splitted this setting has no effect.

Note: If the tree-buffers are used with the keyboard instead with the mouse
then this option takes effect too because [RET] is interpreted as primary
mouse-button and [C-RET] as secondary mouse-button!"
  :group 'ecb-general
  :group 'ecb-most-important
  :type '(radio (const :tag "Left/topmost edit-window"
                       :value left-top)
                (const :tag "Last edit-window with point"
                       :value last-point)))

(defcustom ecb-minor-mode-text " ECB"
  "*String to display in the mode line when ECB minor mode is active.
\(When the string is not empty, make sure that it has a leading space.)

Because for ECB it is quite obvious if it is active or not when the
ECB-windows are visible this text is only display in the modeline if the
ECB-windows are hidden."
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
This is done for some critical situations concerning semantic-tags and their
overlays \(or extends for XEmacs). Normally you should not need this switched
on! But if you get errors like \"destroyed extend\" for XEmacs or
\"wrong-argument-type\" concerning overlays for GNU Emacs then you should
switch on this option and submitting a bug-report to the ecb-mailing-list
\(`ecb-submit-problem-report') after getting the error again!"
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-run-ediff-in-ecb-frame t
  "*Run ediff-sessions in the same frame as ECB is running.
If not nil then ECB ensures that ediff runs in the same frame as ECB and ECB
restores exactly the \"before-ediff\"-window-layout after quiting ediff. If
nil then ediff decides in which frame it will run - depending on the current
window-layout \(e.g. if the ecb-windows are currently hidden) this can be the
ecb-frame but this can also be a newly created frame or any other frame."
  :group 'ecb-general
  :type 'boolean)


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
  :group 'ecb-tree-buffer
  :type 'hook)
  
;;====================================================
;; Internals
;;====================================================


(defun ecb-window-select (name)
  (let ((window (get-buffer-window name ecb-frame)))
    (if window
	(select-window window)
      nil)))

(defun ecb-goto-ecb-window (name)
  (when (and ecb-minor-mode
             (not ecb-windows-hidden)
             (or (equal name ecb-speedbar-buffer-name)
                 (member name ecb-tree-buffers-of-current-layout)))
    (raise-frame ecb-frame)
    (select-frame ecb-frame)
    (or (ecb-window-select name)
        ;; the window is not visible because another one is maximized;
        ;; therefore we first redraw the layout
        (progn
          (ecb-redraw-layout-full nil nil nil nil)
          ;; now we can go to the window
          (ecb-window-select name)))))

(defun ecb-goto-window-directories ()
  "Make the ECB-directories window the current window.
If `ecb-use-speedbar-instead-native-tree-buffer' is 'dir then goto to the
speedbar-window."
  (interactive)
  (or (ecb-goto-ecb-window ecb-directories-buffer-name)
      (and (equal ecb-use-speedbar-instead-native-tree-buffer 'dir)
           (ecb-goto-window-speedbar))))

(defun ecb-goto-window-sources ()
  "Make the ECB-sources window the current window.
If `ecb-use-speedbar-instead-native-tree-buffer' is 'source then goto to the
speedbar-window."
  (interactive)
  (or (ecb-goto-ecb-window ecb-sources-buffer-name)
      (and (equal ecb-use-speedbar-instead-native-tree-buffer 'source)
           (ecb-goto-window-speedbar))))

(defun ecb-goto-window-methods ()
  "Make the ECB-methods window the current window.
If `ecb-use-speedbar-instead-native-tree-buffer' is 'method then goto to the
speedbar-window."
  (interactive)
  (or (ecb-goto-ecb-window ecb-methods-buffer-name)
      (and (equal ecb-use-speedbar-instead-native-tree-buffer 'method)
           (ecb-goto-window-speedbar))))

(defun ecb-goto-window-history ()
  "Make the ECB-history window the current window."
  (interactive)
  (ecb-goto-ecb-window ecb-history-buffer-name))

(defun ecb-goto-window-speedbar ()
  "Make the ECB-speedbar window the current window.
This command does nothing if no integrated speedbar is visible in the
ECB-frame."
  (interactive)
  (ecb-goto-ecb-window ecb-speedbar-buffer-name))

(defun ecb-goto-window-edit-last ()
  "Make the last selected edit-window window the current window. This is the
same as if `ecb-mouse-click-destination' is set to 'last-point."
  (interactive)
  (when ecb-minor-mode
    (raise-frame ecb-frame)
    (select-frame ecb-frame)
    (let ((ecb-mouse-click-destination 'last-point))
      (ecb-select-edit-window))))

(defun ecb-goto-window-edit1 ()
  "Make the \(first) edit-window window the current window."
  (interactive)
  (when ecb-minor-mode
    (raise-frame ecb-frame)
    (select-frame ecb-frame)
    (ecb-select-edit-window 1)))

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
             (equal 'visible (ecb-compile-window-state)))
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
  (ecb-display-one-ecb-buffer ecb-speedbar-buffer-name))

(defun ecb-toggle-RET-selects-edit-window ()
  "Toggles if RET in a tree-buffer should finally select the edit-window.
See also the option `ecb-tree-RET-selects-edit-window'."
  (interactive)
  (let ((tree-buffer (ecb-point-in-ecb-tree-buffer)))
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

(defun ecb-kill-buffer-hook ()
  "Function added to the `kill-buffer-hook' during ECB activation.
It does several tasks:
- Depending on the value in `ecb-kill-buffer-clears-history' the corresponding
  entry in the history-buffer is removed.
- Clearing the method buffer if a file-buffer has been killed.
- The entry of the removed file-buffer is removed from `ecb-tag-tree-cache'."
  (let* ((curr-buf (current-buffer))
         (buffer-file (ecb-fix-filename (buffer-file-name curr-buf))))
    ;; 1. clearing the history if necessary
    (when ecb-kill-buffer-clears-history
      (let ((node (if buffer-file
                      (save-selected-window
                        (ecb-exec-in-history-window (tree-buffer-find-node-data buffer-file))))))
        (when (and node
                   (or (equal ecb-kill-buffer-clears-history 'auto)
                       (and (equal ecb-kill-buffer-clears-history 'ask)
                            (y-or-n-p "Remove history entry for this buffer? "))))
          (save-selected-window
            (ecb-exec-in-history-window
             (tree-buffer-remove-node node)))
          (ecb-update-history-window))))

    ;; 2. clearing the method buffer if a file-buffer is killed
    (if buffer-file
        (ecb-rebuild-methods-buffer-with-tagcache nil nil t))

    ;; 3. removing the file-buffer from `ecb-tag-tree-cache'. Must be done
    ;;    after 2. because otherwise a new element in the cache would be
    ;;    created again by `ecb-rebuild-methods-buffer-with-tagcache'.
    (if buffer-file
        (ecb-clear-tag-tree-cache buffer-file))

    ;; 4. Preventing from killing the special-ecb-buffers by accident
    (when (member curr-buf (ecb-get-current-visible-ecb-buffers))
      (ecb-error "Killing an special ECB-buffer is not possible!"))))

(defun ecb-current-buffer-sync (&optional force)
  "Synchronizes all special ECB-buffers with current buffer.

Depending on the contents of current buffer this function performs several
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
               ;;   with next-error etc. ) doesnÂ´t work correct. CanÂ´t debug down
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
               ;; end of `ecb-rebuild-methods-buffer-with-tagcache' which is
               ;; called by `ecb-update-methods-buffer--internal'!

               ;; selected source has changed, therefore we must initialize
               ;; ecb-selected-tag again.
               (ecb-tag-sync 'force)
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

(defun ecb-combine-ecb-button/edit-win-nr (ecb-button edit-window-nr)
  "Depending on ECB-BUTTON and EDIT-WINDOW-NR return one value:
- nil id ECB-BUTTON is 1.
- t if ECB-BUTTON is 2 and the edit-area of ECB is splitted.
- EDIT-WINDOW-NR if ECB-BUTTON is 3."
  (cond ((eq ecb-button 1) nil)
        ((eq ecb-button 2) (ecb-edit-window-splitted))
        ((eq ecb-button 3) edit-window-nr)))

(defun ecb-get-edit-window (other-edit-window)
  "Get the correct edit-window. Which one is the correct one depends on the
value of OTHER-EDIT-WINDOW \(which is a value returned by
`ecb-combine-ecb-button/edit-win-nr') and `ecb-mouse-click-destination'.
- OTHER-EDIT-WINDOW is nil: Get the edit-window according to the option
  `ecb-mouse-click-destination'.
- OTHER-EDIT-WINDOW is t: Get the next edit-window in the cyclic list of
  current edit-windows starting either from the left-top-most one or from the
  last edit-window with point (depends on
  `ecb-mouse-click-destination').
- OTHER-EDIT-WINDOW is an integer: Get exactly the edit-window with that
  number > 0."
  (let ((edit-win-list (ecb-canonical-edit-windows-list)))
    (cond ((null other-edit-window)
           (if (eq ecb-mouse-click-destination 'left-top)
               (car edit-win-list)
             ecb-last-edit-window-with-point))
          ((integerp other-edit-window)
           (ecb-get-edit-window-by-number other-edit-window edit-win-list))
          (t
           (ecb-next-listelem edit-win-list
                              (if (eq ecb-mouse-click-destination 'left-top)
                                  (car edit-win-list)
                                ecb-last-edit-window-with-point))))))

(defun ecb-customize ()
  "Open a customize-buffer for all customize-groups of ECB."
  (interactive)
  (ecb-select-edit-window)
  (customize-group "ecb"))

(defun ecb-customize-most-important ()
  "Open a customize-buffer for the most important options of ECB."
  (interactive)
  (ecb-select-edit-window)
  (customize-group "ecb-most-important"))

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
    ;; in `ecb-handle-major-mode-visibilty'.
    (setq ecb-item-in-tree-buffer-selected t)
    (if (/= mouse-button 0)
        (setq ecb-layout-prevent-handle-ecb-window-selection t))
    ;; first we dispatch to the right action
    (when ecb-button-list
      (cond ((string= tree-buffer-name ecb-directories-buffer-name)
	     (ecb-directory-clicked node ecb-button nil shift-mode))
	    ((string= tree-buffer-name ecb-sources-buffer-name)
	     (ecb-source-clicked node ecb-button nil shift-mode))
	    ((string= tree-buffer-name ecb-history-buffer-name)
	     (ecb-history-clicked node ecb-button nil shift-mode))
	    ((string= tree-buffer-name ecb-methods-buffer-name)
	     (ecb-method-clicked node ecb-button nil shift-mode))
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
      (ecb-goto-ecb-window tree-buffer-name)
      (tree-buffer-remove-highlight))))


(defun ecb-tree-buffer-node-collapsed-callback (node
                                                mouse-button
                                                shift-pressed
                                                control-pressed
                                                tree-buffer-name)
  "This is the callback-function ecb.el gives to every tree-buffer to call
when a node has been collapsed."
  (if (/= mouse-button 0)
      (setq ecb-layout-prevent-handle-ecb-window-selection t)))

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
    (if (/= mouse-button 0)
        (setq ecb-layout-prevent-handle-ecb-window-selection t))
    (when ecb-button-list
      (cond ((string= tree-buffer-name ecb-directories-buffer-name)
	     (ecb-update-directory-node node))
	    ((string= tree-buffer-name ecb-sources-buffer-name)
	     (ecb-source-clicked node ecb-button nil shift-mode))
	    ((string= tree-buffer-name ecb-history-buffer-name)
	     (ecb-history-clicked node ecb-button nil shift-mode))
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



(defvar ecb-idle-timer-alist nil)
(defvar ecb-post-command-hooks nil)
(defvar ecb-pre-command-hooks nil)
(defun ecb-activate-ecb-autocontrol-functions (idle-value func)
  "Adds function FUNC to `ecb-idle-timer-alist' and activates an idle-timer
with idle-time IDLE-VALUE if IDLE-VALUE not nil. If nil or 'post the FUNC is
added to `post-command-hook' and `ecb-post-command-hooks' and removed from the
idle-list. If 'pre the FUNC is added to `pre-command-hook' and
`ecb-pre-command-hooks' and removed from the idle-list."
  (let* ((timer-elem (assoc func ecb-idle-timer-alist))
         (timer (cdr timer-elem)))
    (when timer-elem
      (ecb-cancel-timer timer)
      (setq ecb-idle-timer-alist (delq timer-elem ecb-idle-timer-alist)))
    (remove-hook 'post-command-hook func)
    (remove-hook 'pre-command-hook func)
    (setq ecb-post-command-hooks (delq func ecb-post-command-hooks))
    (setq ecb-pre-command-hooks (delq func ecb-pre-command-hooks))
    (cond ((or (null idle-value) (equal idle-value 'post))
           (add-hook 'post-command-hook func)
           (add-to-list 'ecb-post-command-hooks func))
          ((equal idle-value 'pre)
           (add-hook 'pre-command-hook func)
           (add-to-list 'ecb-pre-command-hooks func))
          (t
           (add-to-list 'ecb-idle-timer-alist
                        (cons func
                              (ecb-run-with-idle-timer idle-value t func)))))))

(defun ecb-monitor-autocontrol-functions ()
  "Checks if all necessary ECB-hooks are contained in `post-command-hook' rsp.
`pre-command-hook'. If one of them has been removed by Emacs \(Emacs resets
these hooks to nil if any of the contained functions fails!) then this
function readds them to these hooks."
  ;; post-command-hook
  (dolist (hook (cons 'ecb-handle-major-mode-visibilty
                      ecb-post-command-hooks))
    (when (not (member hook post-command-hook))
      (add-hook 'post-command-hook hook)))
  ;; pre-command-hook
  (dolist (hook ecb-pre-command-hooks)
    (when (not (member hook pre-command-hook))
      (add-hook 'pre-command-hook hook))))

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
      ecb-select-ecb-frame
      :active (and ecb-minor-mode
                   (not (equal (selected-frame) ecb-frame)))
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
    [ "Add all buffers to history"
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
      ecb-toggle-auto-expand-tag-tree
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
     ["Last selected edit-window"
      ecb-goto-window-edit-last
      :active t
      :help "Go to the last selected edit-window"
      ])
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
      :active (member ecb-directories-buffer-name
                      ecb-tree-buffers-of-current-layout)
      :help "Go to the directories window"
      ])
    (ecb-menu-item
     ["Sources"
      ecb-goto-window-sources
      :active (member ecb-sources-buffer-name
                      ecb-tree-buffers-of-current-layout)
      :help "Go to the sources window"
      ])
    (ecb-menu-item
     ["Methods and Variables"
      ecb-goto-window-methods
      :active (member ecb-methods-buffer-name
                      ecb-tree-buffers-of-current-layout)
      :help "Go to the methods/variables window"
      ])
    (ecb-menu-item
     ["History"
      ecb-goto-window-history
      :active (member ecb-history-buffer-name
                      ecb-tree-buffers-of-current-layout)
      :help "Go to the history window"
      ])
    (ecb-menu-item
     ["Speedbar"
      ecb-goto-window-speedbar
      :active (and ecb-use-speedbar-instead-native-tree-buffer
                   (member ecb-speedbar-buffer-name
                           ecb-tree-buffers-of-current-layout))
      :help "Go to the integrated speedbar window"
      ])
    (ecb-menu-item
     ["Compilation"
      ecb-goto-window-compilation
      :active (equal 'visible (ecb-compile-window-state))
      :help "Go to the history window"
      ])
    )
   (list
    "Display window maximized"
    (ecb-menu-item
     ["Directories"
      ecb-maximize-window-directories
      :active (member ecb-directories-buffer-name
                      ecb-tree-buffers-of-current-layout)
      :help "Maximize the directories window - even if currently not visible"
      ])
    (ecb-menu-item
     ["Sources"
      ecb-maximize-window-sources
      :active (member ecb-sources-buffer-name
                      ecb-tree-buffers-of-current-layout)
      :help "Maximize the sources window - even if currently not visible"
      ])
    (ecb-menu-item
     ["Methods and Variables"
      ecb-maximize-window-methods
      :active (member ecb-methods-buffer-name
                      ecb-tree-buffers-of-current-layout)
      :help "Maximize the methods/variables window - even if currently not visible"
      ])
    (ecb-menu-item
     ["History"
      ecb-maximize-window-history
      :active (member ecb-history-buffer-name
                      ecb-tree-buffers-of-current-layout)
      :help "Maximize the history window - even if currently not visible"
      ])
    (ecb-menu-item
     ["Speedbar"
      ecb-maximize-window-speedbar
      :active (and ecb-use-speedbar-instead-native-tree-buffer
                   (member ecb-speedbar-buffer-name
                           ecb-tree-buffers-of-current-layout))
      :help "Maximize the integrated speedbar window - even if not visible"
      ])
    )
   "-"
   (list
    "Preferences"
    (ecb-menu-item
     ["Most important..."
      (customize-group "ecb-most-important")
      :active t
      :help "Customize the most important options"
      ])
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
     ["Tree-buffer style and handling..."
      (customize-group "ecb-tree-buffer")
      :active t
      :help "Customize the tree-buffers of ECB"
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
    (ecb-menu-item
     ["Supporting window-managers..."
      (customize-group "ecb-winman-support")
      :active t
      :help "Customize options for the window-manager-support"
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
     [ "ECB NEWS"
       (ecb-display-news-for-upgrade t)
       :active t
       :help "Displays the NEWS-file of ECB."
       ])
    (ecb-menu-item
     [ "List of most important options"
       (let ((ecb-show-help-format 'info))
         (ecb-show-help)
         (Info-goto-node "Most important options"))
       :active t
       :help "Displays a a list of options which you should know."
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
     [ "List of all commands"
       (let ((ecb-show-help-format 'info))
         (ecb-show-help)
         (Info-goto-node "Command Index"))
       :active t
       :help "Displays an index of all commands in the online-help."
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
     [ "Conflicts with other packages"
       (let ((ecb-show-help-format 'info))
         (ecb-show-help)
         (Info-goto-node "Conflicts and bugs"))
       :active t
       :help "What to do for conflicts with other packages."
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
`ecb-mode-map' and `ecb-minor-mode-text'."
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
  '("C-c ." . ((t "fh" ecb-history-filter)
               (t "fs" ecb-sources-filter)
               (t "fm" ecb-methods-filter)
               (t "fr" ecb-methods-filter-regexp)
               (t "ft" ecb-methods-filter-tagclass)
               (t "fc" ecb-methods-filter-current-type)
               (t "fp" ecb-methods-filter-protection)
               (t "fn" ecb-methods-filter-nofilter)
               (t "fl" ecb-methods-filter-delete-last)
               (t "ff" ecb-methods-filter-function)
               (t "p" ecb-nav-goto-previous)
               (t "n" ecb-nav-goto-next)
               (t "lc" ecb-change-layout)
               (t "lr" ecb-redraw-layout)
               (t "lw" ecb-toggle-ecb-windows)
               (t "lt" ecb-toggle-layout)
               (t "s" ecb-window-sync)
               (t "r" ecb-rebuild-methods-buffer)
               (t "a" ecb-toggle-auto-expand-tag-tree)
               (t "x" ecb-expand-methods-nodes)
               (t "h" ecb-show-help)
               (t "gl" ecb-goto-window-edit-last)
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
               (t "e" eshell)
               (t "o" ecb-toggle-scroll-other-window-scrolls-compile)
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
  :group 'ecb-most-important
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
                               (val-list (ecb-copy-list (cdr value)))
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

(defun ecb-clean-up-after-activation-failure (msg err)
  "Complete cleanup of all ECB-setups and report an error with message MSG."
  (let ((ecb-minor-mode t))
    (ecb-deactivate-internal t)
    (if ecb-running-xemacs
        (redraw-modeline t)
      (force-mode-line-update t))
    (error "ECB %s: %s (error-type: %S, error-data: %S)" ecb-version msg
           (car err) (cdr err))))
  

(defvar ecb-last-window-config-before-deactivation nil
  "Contains the last `ecb-current-window-configuration' directly before
ECB has been deactivated. Do not set this variable!")

(defvar ecb-max-specpdl-size-old nil)
(defvar ecb-max-lisp-eval-depth-old nil)

(defun ecb-activate--impl ()
  "See `ecb-activate'.  This is the implementation of ECB activation."
  (when (or (null ecb-frame) (not (frame-live-p ecb-frame)))
    (setq ecb-frame (selected-frame)))
  
  (if ecb-minor-mode
      (when (and (not (equal (selected-frame) ecb-frame))
                 (or (equal ecb-activation-selects-ecb-frame-if-already-active t)
                     (and (equal ecb-activation-selects-ecb-frame-if-already-active 'ask)
                          (y-or-n-p "ECB is already active in another frame. Select it? "))))
        (ecb-select-ecb-frame)
        (ecb-update-directories-buffer))

    ;; we activate only if all before-hooks return non nil
    (when (run-hook-with-args-until-failure 'ecb-before-activate-hook)

      ;; max-specpdl-size and max-lisp-eval-depth
      (when (< max-specpdl-size 3000)
        (setq ecb-max-specpdl-size-old max-specpdl-size)
        (setq max-specpdl-size 3000))
      (when (< max-lisp-eval-depth 1000)
        (setq ecb-max-lisp-eval-depth-old max-lisp-eval-depth)
        (setq max-lisp-eval-depth 1000))
      
      (condition-case err-obj
          (progn
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

            ;; clear the tag-tree-cache, the files-subdir-cache, the
            ;; sources-cache and the history-filter.
            (when ecb-clear-caches-before-activate
              (ecb-clear-tag-tree-cache)
              (ecb-clear-files-and-subdirs-cache)
              (ecb-clear-directory-empty-cache)
              (ecb-sources-cache-clear)
              (ecb-reset-history-filter))

            ;; initialize internal vars
            (ecb-initialize-internal-vars)
    
            ;; enable basic advices
            (ecb-enable-advices ecb-basic-adviced-functions)

            ;; enable permanent advices - these advices will never being
            ;; deactivated after first activation of ECB unless
            ;; `ecb-split-edit-window-after-start' is not 'before-activation
            ;; (see `ecb-deactivate-internal')
            (ecb-enable-advices ecb-permanent-adviced-functions)

            ;; enable advices for not supported window-managers
            (ecb-enable-advices ecb-winman-not-supported-function-advices)

            ;; enable advices for the compatibility with other packages
            (ecb-enable-advices ecb-compatibility-advices)
            
            ;; set the ecb-frame
            (let ((old-ecb-frame ecb-frame))
              (if ecb-new-ecb-frame
                  (progn
                    (run-hooks 'ecb-activate-before-new-frame-created-hook)
                    (setq ecb-frame (make-frame))
                    (put 'ecb-frame 'ecb-new-frame-created t))
                (setq ecb-frame (selected-frame))
                (put 'ecb-frame 'ecb-new-frame-created nil))
              ;; If ECB is acivated in a frame unequal to that frame which was
              ;; the ecb-frame at last deactivation then we initialize the
              ;; `ecb-edit-area-creators'.
              (if (not (equal ecb-frame old-ecb-frame))
                  (ecb-edit-area-creators-init)))
            (raise-frame ecb-frame)
            (select-frame ecb-frame)

            (ecb-enable-own-temp-buffer-show-function t)
      
            ;; now we can activate ECB
            (let ((curr-buffer-list (mapcar (lambda (buff)
                                              (buffer-name buff))
                                            (buffer-list))))
              ;; create all the ECB-buffers if they don´t already exist
              (unless (member ecb-directories-buffer-name curr-buffer-list)
                (ecb-create-directories-tree-buffer))
      
              (unless (member ecb-sources-buffer-name curr-buffer-list)
                (ecb-create-sources-tree-buffer))
      
              (unless (member ecb-methods-buffer-name curr-buffer-list)
                (ecb-create-methods-tree-buffer))
      
              (unless (member ecb-history-buffer-name curr-buffer-list)
                (ecb-create-history-tree-buffer)))
    
            ;; Now store all tree-buffer-names used by ECB ECB must not use
            ;; the variable `tree-buffers' but must always refer to
            ;; `ecb-tree-buffers'!!
            (setq ecb-tree-buffers (list ecb-directories-buffer-name
                                         ecb-sources-buffer-name
                                         ecb-methods-buffer-name
                                         ecb-history-buffer-name))

            ;; activate the eshell-integration - does not load eshell but prepares
            ;; ECB to run eshell right - if loaded and activated
            (ecb-eshell-activate-integration)
      
            ;; we need some hooks
            (add-hook (ecb--semantic-after-partial-cache-change-hook)
                      'ecb-update-after-partial-reparse t)
            (add-hook (ecb--semantic-after-toplevel-cache-change-hook)
                      'ecb-rebuild-methods-buffer-with-tagcache t)
            (ecb-activate-ecb-autocontrol-functions ecb-highlight-tag-with-point-delay
                                                    'ecb-tag-sync)
            (ecb-activate-ecb-autocontrol-functions ecb-window-sync-delay
                                                    'ecb-window-sync-function)
            (ecb-activate-ecb-autocontrol-functions ecb-compilation-update-idle-time
                                                    'ecb-compilation-buffer-list-changed-p)
            (ecb-activate-ecb-autocontrol-functions 'post
                                                    'ecb-layout-post-command-hook)
            (ecb-activate-ecb-autocontrol-functions 'pre
                                                    'ecb-layout-pre-command-hook)
            (add-hook 'after-save-hook 'ecb-update-methods-after-saving)
            (add-hook 'kill-buffer-hook 'ecb-kill-buffer-hook)

            (add-hook 'find-file-hooks 'ecb-find-file-hook)

            ;; after adding all idle-timers and post- and pre-command-hooks we
            ;; activate the monitoring
            (ecb-activate-ecb-autocontrol-functions 1 'ecb-monitor-autocontrol-functions)

            ;; running the compilation-buffer update first time
            (ecb-compilation-buffer-list-init)
      
            ;; ediff-stuff; we operate here only with symbols to avoid bytecompiler
            ;; warnings
            (if (boundp 'ediff-quit-hook)
                (put 'ediff-quit-hook 'ecb-ediff-quit-hook-value
                     ediff-quit-hook))
            (add-hook 'ediff-quit-hook 'ediff-cleanup-mess)
            (add-hook 'ediff-quit-hook 'ecb-ediff-quit-hook t)
            ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: suspending ediff and
            ;; especially reactivating does currently not really work good...
            ;; (add-hook 'ediff-suspend-hook 'ecb-ediff-quit-hook t)
            (add-hook 'ediff-before-setup-hook
                      'ecb-ediff-before-setup-hook)
            
            ;; menus - dealing with the menu for XEmacs is really a pain...
            (when ecb-running-xemacs
              (let ((dummy-buf-name " *dummytogetglobalmap*"))
                (save-excursion
                  (set-buffer (get-buffer-create dummy-buf-name))
                  (add-submenu nil ecb-minor-menu)
                  (kill-buffer dummy-buf-name)))
              (save-excursion
                (dolist (buf (buffer-list))
                  (set-buffer buf)
                  (if (null (car (find-menu-item current-menubar
                                                 (list ecb-menu-name))))
                      (add-submenu nil ecb-minor-menu)))))

            (add-hook (if ecb-running-xemacs
                          'activate-menubar-hook
                        'menu-bar-update-hook)
                      'ecb-compilation-update-menu)

            ;; modeline for xemacs
            (if ecb-running-xemacs
                (ecb-activate-xemacs-modeline-menu 1))
            )
        (error
         (ecb-clean-up-after-activation-failure
          "Errors during the basic setup of ECB." err-obj)))

      (condition-case err-obj
          ;; run personal hooks before drawing the layout
          (run-hooks 'ecb-activate-before-layout-draw-hook)
        (error
         (ecb-clean-up-after-activation-failure
          "Errors during the hooks of ecb-activate-before-layout-draw-hook."
          err-obj)))
         
      (setq ecb-minor-mode t)

      ;; now we draw the screen-layout of ECB.
      (condition-case err-obj
          ;; now we draw the layout chosen in `ecb-layout'. This function
          ;; activates at its end also the adviced functions if necessary!
          ;; Here the directories- and history-buffer will be updated.
          (let ((ecb-redraw-layout-quickly nil)
                (use-last-win-conf (and ecb-last-window-config-before-deactivation
                                        (equal ecb-split-edit-window-after-start
                                               'before-deactivation)
                                        (not (ecb-window-configuration-invalidp
                                              ecb-last-window-config-before-deactivation)))))
            (if use-last-win-conf                     
                (setq ecb-edit-area-creators
                      (nth 4 ecb-last-window-config-before-deactivation)))
            (ecb-redraw-layout-full 'no-buffer-sync
                                    nil
                                    (if use-last-win-conf
                                        (nth 6 ecb-last-window-config-before-deactivation))
                                    (if use-last-win-conf
                                        (nth 5 ecb-last-window-config-before-deactivation)
                                      nil))
            ;; if there was no compile-window before deactivation then we have
            ;; to hide the compile-window after activation
            (if (and use-last-win-conf
                     (null (nth 2 ecb-last-window-config-before-deactivation)))
                (ecb-toggle-compile-window -1))

            (when (member ecb-split-edit-window-after-start
                          '(vertical horizontal nil))
              (ecb-with-adviced-functions
                (delete-other-windows)
                (cond ((equal ecb-split-edit-window-after-start 'horizontal)
                       (split-window-horizontally))
                      ((equal ecb-split-edit-window-after-start 'vertical)
                       (split-window-vertically)))))
            
            ;; now we synchronize all ECB-windows
            (ecb-window-sync)
            
            ;; now update all the ECB-buffer-modelines
            (ecb-mode-line-format))
        (error
         (ecb-clean-up-after-activation-failure
          "Errors during the layout setup of ECB." err-obj)))

      (condition-case err-obj
          (let ((edit-window (car (ecb-canonical-edit-windows-list))))
            (when (and ecb-display-default-dir-after-start
                       (null (buffer-file-name
                              (window-buffer edit-window))))
              (ecb-set-selected-directory
               (ecb-fix-filename (save-excursion
                                   (set-buffer (window-buffer edit-window))
                                   default-directory)))))
        (error
         (ecb-clean-up-after-activation-failure
          "Errors during setting the default directory." err-obj)))

      (condition-case err-obj
          ;; we run any personal hooks
          (run-hooks 'ecb-activate-hook)
        (error
         (ecb-clean-up-after-activation-failure
          "Errors during the hooks of ecb-activate-hook." err-obj)))

      (condition-case err-obj
          ;; enable mouse-tracking for the ecb-tree-buffers; we do this after
          ;; running the personal hooks because if a user put´s activation of
          ;; follow-mouse.el (`turn-on-follow-mouse') in the
          ;; `ecb-activate-hook' then our own ECB mouse-tracking must be
          ;; activated later. If `turn-on-follow-mouse' would be activated
          ;; after our own follow-mouse stuff, it would overwrite our
          ;; mechanism and the show-node-name stuff would not work!
          (if (ecb-show-any-node-info-by-mouse-moving-p)
              (tree-buffer-activate-follow-mouse))
        (error
         (ecb-clean-up-after-activation-failure
          "Errors during the mouse-tracking activation." err-obj)))

      (setq ecb-minor-mode t)
      (message "The ECB is now activated.")

      (condition-case err-obj
          ;; now we display all `ecb-not-compatible-options' and
          ;; `ecb-renamed-options'
          (if ecb-auto-compatibility-check
              (if (not (ecb-display-upgraded-options))
                  (ecb-display-news-for-upgrade))
            (ecb-display-news-for-upgrade))
        (error
         (ecb-clean-up-after-activation-failure
          "Error during the compatibility-check of ECB." err-obj)))

      ;; if we activate ECB first time then we display the node "First steps" of
      ;; the online-manual
      (ignore-errors
        (when (null ecb-source-path)
          (let ((ecb-show-help-format 'info))
            (ecb-show-help)
            (Info-goto-node "First steps"))))

      ;; display tip of the day if `ecb-tip-of-the-day' is not nil
      (ignore-errors
        (ecb-show-tip-of-the-day))

      (condition-case err-obj
          ;;now take a snapshot of the current window configuration
          (setq ecb-activated-window-configuration
                (ecb-current-window-configuration))
        (error
         (ecb-clean-up-after-activation-failure
          "Errors during the snapshot of the windows-configuration." err-obj)))
      )))


;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Should we add this function to
;; `ediff-suspend-hook' too?! We should add something but this functions is
;; not perfectly....in general suspending ediff need some work here...
(defun ecb-ediff-quit-hook ()
  "Added to the end of `ediff-quit-hook' during ECB is activated. It
does all necessary after finishing ediff."
  (when ecb-minor-mode
    (if (and (not (equal (selected-frame) ecb-frame))
             (y-or-n-p
              "Ediff finished. Do you want to delete the extra ediff-frame? "))
        (delete-frame (selected-frame) t))
    (select-frame ecb-frame)
    (when ecb-before-ediff-window-config
      (ecb-set-window-configuration ecb-before-ediff-window-config)
      (setq ecb-before-ediff-window-config nil))))

(defvar ecb-before-ediff-window-config nil)

;; We must not add this function to `ediff-before-setup-windows-hook' because
;; this hook is called very often - see docu. The hook
;; `ediff-before-setup-hook' is called only once - so it can be used to store
;; window-configs!
(defun ecb-ediff-before-setup-hook ()
  (if (and ecb-minor-mode
           (equal (selected-frame) ecb-frame))
      (progn
        (setq ecb-before-ediff-window-config (ecb-current-window-configuration))
        (if ecb-run-ediff-in-ecb-frame
            (progn
              (ecb-toggle-ecb-windows -1)
              (ecb-toggle-compile-window -1))
          (if (not ecb-windows-hidden)
              (ecb-with-adviced-functions
               (delete-other-windows (car (ecb-canonical-edit-windows-list)))))))
    (setq ecb-before-ediff-window-config nil)))

(defun ecb-deactivate ()
  "Deactivates the ECB and kills all ECB buffers and windows."
  (interactive)
  (ecb-minor-mode 0))

(defun ecb-deactivate-internal (&optional run-no-hooks)
  "Deactivates the ECB and kills all ECB buffers and windows."
  (unless (not ecb-minor-mode)

    (when (or run-no-hooks
              (run-hook-with-args-until-failure 'ecb-before-deactivate-hook))

      (setq ecb-last-window-config-before-deactivation
            (ecb-current-window-configuration))
      
      ;; deactivating the adviced functions
      (ecb-activate-adviced-functions nil)
      (ecb-disable-advices ecb-basic-adviced-functions)
      (ecb-disable-advices ecb-speedbar-adviced-functions)
      (ecb-disable-advices ecb-eshell-adviced-functions)
      (ecb-disable-advices ecb-winman-not-supported-function-advices)
      (ecb-disable-advices ecb-compatibility-advices)
      (ecb-enable-ecb-advice 'walk-windows 'around -1)
      (ecb-enable-ecb-advice 'one-window-p 'around -1)
      ;; we disable the permanent advices later

      (ecb-enable-own-temp-buffer-show-function nil)      

      ;; deactivate and reset the speedbar stuff
      (ignore-errors (ecb-speedbar-deactivate))

      ;; deactivates the eshell-integration; this disables also the
      ;; eshell-advices! 
      (ecb-eshell-deactivate-integration)

      (tree-buffer-activate-mouse-tracking)
      (tree-buffer-deactivate-mouse-tracking)
      (tree-buffer-activate-follow-mouse)
      (tree-buffer-deactivate-follow-mouse)

      ;; remove the hooks
      (remove-hook (ecb--semantic-after-partial-cache-change-hook)
                   'ecb-update-after-partial-reparse)
      (remove-hook (ecb--semantic-after-toplevel-cache-change-hook)
                   'ecb-rebuild-methods-buffer-with-tagcache)
      (dolist (timer-elem ecb-idle-timer-alist)
        (ecb-cancel-timer (cdr timer-elem)))
      (setq ecb-idle-timer-alist nil)
      (dolist (hook ecb-post-command-hooks)
        (remove-hook 'post-command-hook hook))
      (setq ecb-post-command-hooks nil)
      (dolist (hook ecb-pre-command-hooks)
        (remove-hook 'pre-command-hook hook))
      (setq ecb-pre-command-hooks nil)
      (remove-hook 'after-save-hook 'ecb-update-methods-after-saving)
      (remove-hook 'kill-buffer-hook 'ecb-kill-buffer-hook)

      (add-hook 'find-file-hooks 'ecb-find-file-hook)
      
      (if (get 'ediff-quit-hook 'ecb-ediff-quit-hook-value)
          (setq ediff-quit-hook (get 'ediff-quit-hook
                                     'ecb-ediff-quit-hook-value))
        (remove-hook 'ediff-quit-hook 'ecb-ediff-quit-hook))
      (remove-hook 'ediff-before-setup-hook
                   'ecb-ediff-before-setup-hook)

      ;; menus - dealing with the menu for XEmacs is really a pain...
      (ignore-errors
        (when ecb-running-xemacs
          (save-excursion
            (dolist (buf (buffer-list))
              (set-buffer buf)
              (if (car (find-menu-item current-menubar
                                       (list ecb-menu-name)))
                  (delete-menu-item (list ecb-menu-name)))))))
      
      (remove-hook (if ecb-running-xemacs
                       'activate-menubar-hook
                     'menu-bar-update-hook)
                   'ecb-compilation-update-menu)

      ;; modeline for xemacs
      (if ecb-running-xemacs
          (ecb-activate-xemacs-modeline-menu -1))

      ;; run any personal hooks
      (unless run-no-hooks
        (run-hooks 'ecb-deactivate-hook))
    
      ;; clear the ecb-frame. Here we try to preserve the split-state after
      ;; deleting the ECB-screen-layout.
      (when (frame-live-p ecb-frame)
        (raise-frame ecb-frame)
        (select-frame ecb-frame)
        (condition-case oops
            (let* ((config (ecb-window-configuration-data))
                   (window-before-redraw (nth 0 config))
                   (pos-before-redraw (nth 1 config))
                   (edit-win-data-before-redraw (nth 2 config))
                   (edit-win-list-after-redraw nil))
              ;; first we make all windows of the ECB-frame not dedicated and
              ;; then we delete all ECB-windows
              (ecb-select-edit-window)
              (ecb-make-windows-not-dedicated ecb-frame)

              ;; deletion of all windows. (All other advices are already
              ;; disabled!) 
              (ecb-with-original-permanent-functions
               (delete-other-windows))
              
              ;; some paranoia....
              (set-window-dedicated-p (selected-window) nil)

              ;; now we restore the edit-windows as before the deactivation
              ;; (All other advices are already disabled!)
              (if (= (length edit-win-data-before-redraw)
                     (ecb-edit-area-creators-number-of-edit-windows))
                  (ecb-with-original-permanent-functions
                   (ecb-restore-edit-area))
                (ecb-edit-area-creators-init))
              
              (setq edit-win-list-after-redraw (ecb-canonical-edit-windows-list))

              ;; a safety-check if we have now at least as many windows as
              ;; edit-windows before deactivation. If yes we restore all
              ;; window-data as before deactivation.
              (when (= (length edit-win-list-after-redraw)
                       (length edit-win-data-before-redraw))
                (dotimes (i (length edit-win-data-before-redraw))
                  (let ((win (nth i edit-win-list-after-redraw))
                        (data (nth i edit-win-data-before-redraw)))
                    (set-window-buffer win (nth 0 data))
                    (set-window-start win (nth 1 data))
                    (set-window-point win (nth 2 data))
                    (if (> (length edit-win-list-after-redraw) 1)
                        (ecb-set-window-size win (nth 3 data)))
                    )))

              ;; at the end we always stay in that window as before the
              ;; deactivation.
              (when (integerp window-before-redraw)
                (ecb-select-edit-window window-before-redraw))       
              ;; if we were in an edit-window before deactivation let us go to
              ;; the old place
              (when pos-before-redraw
                (goto-char pos-before-redraw)))
          (error
           ;; in case of an error we make all windows not dedicated and delete
           ;; at least all other windows
           (ecb-warning "ecb-deactivate-internal (error-type: %S, error-data: %S)"
                        (car oops) (cdr oops))
           (ignore-errors (ecb-make-windows-not-dedicated ecb-frame))
           (ignore-errors (delete-other-windows))))
        
        (if (get 'ecb-frame 'ecb-new-frame-created)
            (ignore-errors (delete-frame ecb-frame t))))
        
      (ecb-initialize-layout)

      ;; we do NOT disable the permanent-advices of
      ;; `ecb-permanent-adviced-functions' unless the user don't want
      ;; preserving the split-state after reactivating ECB.
      (when (not (equal ecb-split-edit-window-after-start 'before-activation))
        (ecb-disable-advices ecb-permanent-adviced-functions)
        (ecb-edit-area-creators-init))

      ;; we can safely do the kills because killing non existing buffers
      ;; doesn´t matter.
      (tree-buffer-destroy ecb-directories-buffer-name)
      (tree-buffer-destroy ecb-sources-buffer-name)
      (tree-buffer-destroy ecb-methods-buffer-name)
      (tree-buffer-destroy ecb-history-buffer-name)

      (setq ecb-activated-window-configuration nil)

      (setq ecb-minor-mode nil)

      ;; max-specpdl-size and max-lisp-eval-depth
      (when ecb-max-specpdl-size-old
        (setq max-specpdl-size ecb-max-specpdl-size-old))
      (when ecb-max-lisp-eval-depth-old
        (setq max-lisp-eval-depth ecb-max-lisp-eval-depth-old))))
  
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


(tree-buffer-defpopup-command ecb-maximize-ecb-window-menu-wrapper
  "Expand the current ECB-window from popup-menu."
  (ecb-display-one-ecb-buffer (buffer-name (current-buffer))))


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

(defun ecb-handle-major-mode-visibilty ()
  "Added to `post-command-hook' after loading the ecb-library. Handles the
value `ecb-major-modes-show-or-hide'. Because this hook of `post-command-hook'
does nothing if the major-mode has not changed there should be no
performance-problem!"
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
    ;; do nothing if major-mode has not been changed or if a minibuffer is
    ;; active or if now one of the ecb-buffers is active
    (when (and (not (> (minibuffer-depth) 0))
               (not (equal ecb-last-major-mode major-mode))
               (not (member (buffer-name (current-buffer)) ecb-tree-buffers)))
      (let ((last-mode ecb-last-major-mode))
        (setq ecb-last-major-mode major-mode)
        (ignore-errors
          (cond ((member major-mode (car ecb-major-modes-show-or-hide))
                 (let ((edit-win-list (ecb-canonical-edit-windows-list)))
                   ;; the window must not be splitted or if splitted the last
                   ;; major-mode must be dired-mode
                   (when (or (not (ecb-edit-window-splitted edit-win-list))
                             (equal last-mode 'dired-mode))
                     (and (ecb-point-in-edit-window edit-win-list)
                          ecb-windows-hidden
                          (ecb-show-ecb-windows)))))
                ((member major-mode (cdr ecb-major-modes-show-or-hide))
                 (let ((edit-win-list (ecb-canonical-edit-windows-list)))
                   ;; the window must not be splitted or if splitted the last
                   ;; major-mode must be dired-mode
                   (when (or (not (ecb-edit-window-splitted edit-win-list))
                             (equal last-mode 'dired-mode))
                     (and (ecb-point-in-edit-window edit-win-list)
                          (not ecb-windows-hidden)
                          (ecb-hide-ecb-windows)))))))))))
  
(add-hook 'post-command-hook 'ecb-handle-major-mode-visibilty)

(add-hook 'emacs-startup-hook 'ecb-auto-activate-hook)

(defun ecb-run-from-menubar ()
  "Activate ECB from the Tools-menu. See `ecb-activate'."
  (interactive)
  (ecb-activate))

(progn
  (require 'easymenu)
  (easy-menu-add-item nil
                      '("tools") 
                      (ecb-menu-item
                       [ "Start Code Browser (ECB)"
                         ecb-run-from-menubar
                         :active t
                         :help "Start the Emacs Code Browser."
                         ])))


;; Klaus Berndl <klaus.berndl@sdm.de>: Cause of the magic autostart stuff of
;; the advice-package we must disable at load-time all these advices!!
;; Otherwise would just loading ecb (not activating!) activate each advice
;; AFTER the FIRST usage of our advices!!
(ecb-disable-advices ecb-basic-adviced-functions)
(ecb-disable-advices ecb-speedbar-adviced-functions)
(ecb-disable-advices ecb-eshell-adviced-functions)
(ecb-disable-advices ecb-permanent-adviced-functions)
(ecb-activate-adviced-functions nil)
(ecb-enable-ecb-advice 'walk-windows 'around -1)
(ecb-enable-ecb-advice 'one-window-p 'around -1)

;; clearing all caches at load-time
(ecb-clear-tag-tree-cache)
(ecb-clear-files-and-subdirs-cache)
(ecb-clear-directory-empty-cache)
(ecb-sources-cache-clear)
(ecb-reset-history-filter)


(silentcomp-provide 'ecb)

;;; ecb.el ends here
