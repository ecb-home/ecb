;;; ecb.el --- a code browser

;; Copyright (C) 2000, 2001 Jesper Nordenberg

;; Author: Jesper Nordenberg <mayhem@home.se>
;; Maintainer: Jesper Nordenberg <mayhem@home.se>
;; Keywords: java, class, browser
;; Created: Jul 2000

(defvar ecb-version "1.40beta2"
  "Current ECB version.")

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
;; The Emacs code browser (ECB) creates four buffers: *ECB Directories*, *ECB
;; Sources*, *ECB Methods* and *ECB History*. These buffers can be used to
;; navigate through source code with the mouse.
;;
;; To use the Emacs code browser add the ECB files to your load path and add the
;; following line to your .emacs file:
;;
;; (require 'ecb)
;;
;; Optional: You can byte-compile ECB after the ECB-package is loaded with
;; `ecb-byte-compile'.
;;
;; ECB requires version 1.3.3 of Eric's semantic bovinator
;; (http://www.ultranet.com/~zappo/semantic.shtml).
;; If you are working with Java, ECB works best when the JDE package
;; (http://sunsite.auc.dk/jde) is installed.
;; 
;; ECB is activated by calling:
;;
;; M-x ecb-activate
;;
;; ECB can also be (de)activated/toggled by M-x ecb-minor-mode.
;;
;; After activating ECB you should call `ecb-show-help' to get a detailed
;; description of what ECB offers to you and how to use ECB.
;;
;; The latest version of the ECB is available at
;; http://home.swipnet.se/mayhem/ecb.html

;; $Id: ecb.el,v 1.119 2001/07/01 18:09:22 berndl Exp $

;;; Code:

;; TODO: After semantic 1.4 is stable (means no beta stadium) we throw away
;; the compatibility with semantic 1.3.X!
(eval-and-compile
  (if (locate-library "semantic-load")
      (progn
        (message "ECB uses semantic 1.4")
        (require 'semantic)
        (setq semantic-load-turn-everything-on nil)
        (require 'semantic-load)
        (defun semantic-toplevel-get-cache ()
          semantic-toplevel-bovine-cache))
    ;; semantic 1.3.X
    (message "ECB uses semantic 1.3.X")
    (defconst semantic-version "1.3.X")
    (require 'semantic)
    (require 'semantic-el)
    (require 'semantic-c)
    (require 'semantic-make)
    (defun semantic-active-p ()
      (or semantic-toplevel-bovine-table
          semantic-toplevel-bovinate-override))
    (defun semantic-toplevel-get-cache ()
      (car semantic-toplevel-bovine-cache))
    )
  )

(require 'tree-buffer)
(require 'ecb-layout)
(require 'ecb-mode-line)
(require 'ecb-util)
(require 'ecb-help)

(require 'easymenu)
(require 'assoc);; Semantic fix

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

;;====================================================
;; Variables
;;====================================================
(defvar ecb-selected-method-start 0
  "The currently selected method.")
(defvar ecb-path-selected-directory nil
  "Path to currently selected directory.")
(defvar ecb-path-selected-source nil
  "Path to currently selected source.")
(defvar ecb-selected-token nil
  "The currently selected Semantic token.")
(make-variable-buffer-local 'ecb-selected-token)
(defvar ecb-methods-root-node nil
  "Path to currently selected source.")

(defvar ecb-minor-mode nil
  "Do not set this variable directly. Use `ecb-activate' and
`ecb-deactivate'!")

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

(defcustom ecb-use-recursive-edit nil
  "*Tell ECB to use a recursive edit so that it can easily be deactivated
by \(keyboard-escape-quit)."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-source-path nil
  "*Path where to find code sources."
  :group 'ecb-directories
  :set (function (lambda(symbol value)
		   (set symbol value)
		   (if (and ecb-minor-mode
			    (functionp 'ecb-update-directories-buffer))
		       (ecb-update-directories-buffer))))
  :type '(repeat (directory :tag "Path")))

(defvar ecb-source-path-functions nil
  "List of functions to call for finding sources. Each time the function
`ecb-update-directories-buffer' is called, the functions in this variable will
be evaluated. Such a function must return either nil or a list of strings
where each string is a path.")

(defcustom ecb-show-sources-in-directories-buffer nil
  "*Show source files in directories buffer."
  :group 'ecb-directories
  :type 'boolean)

(defcustom ecb-directories-buffer-name "*ECB Directories*"
  "*Name of the ECB directory buffer. Because it is not a normal buffer for
editing you should enclose the name with stars, e.g. \"*ECB Directories*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object of
the ECB-directory-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-directories
  :type 'string)

(defface ecb-sources-face
  '((((class color) (background light)) (:foreground "medium blue"))
    (((class color) (background dark))  (:foreground "LightBlue1"))
    (t (:background "gray")))
  "*Define a face for displaying sources in the directories buffer."
  :group 'faces
  :group 'ecb-directories)

(defcustom ecb-source-in-directories-buffer-face
  'ecb-sources-face
  "*Face for source files in the directories buffer."
  :group 'ecb-directories
  :type 'face)

(defcustom ecb-excluded-directories-regexp "^\\(CVS\\|\\..*\\)$"
  "*Specifies directories that should not be included in the directories
list. The value of this variable should be a regular expression."
  :group 'ecb-directories
  :type 'regexp)

(defcustom ecb-auto-expand-directory-tree t
  "*Automatically expand the directory tree to the current source file."
  :group 'ecb-directories
  :type 'boolean)

(defcustom ecb-sources-buffer-name "*ECB Sources*"
  "*Name of the ECB sources buffer. Because it is not a normal buffer for
editing you should enclose the name with stars, e.g. \"*ECB Sources*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object of
the ECB-sources-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-sources
  :type 'string)

(defcustom ecb-source-file-regexps
  '("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\)$\\)\\)"
    "^\\.\\(emacs\\|gnus\\)$")
  "*Specifies which files are shown as source files. Consists of one exclude
regexp and one include regexp. A file is displayed in the source-buffer of ECB
iff: The file does not match the exclude regexp OR the file matches the
include regexp. There are three predefined and useful combinations of an
exclude and include regexp:
- All files
- All, but no backup, object, lib or ini-files \(except .emacs and .gnus). This
  means all files except those starting with \".\", \"#\" or ending with
  \"~\", \".elc\", \".obj\", \".o\", \".lib\", \".dll\", \".a\", \".so\".
  (but including .emacs and .gnus)
- Common source file types (.c, .java etc.)
In addition to these predefined values a custom exclude and include
combination can be defined."
  :group 'ecb-sources
  :type '(radio (const :tag "All files"
		       :value ("" ""))
		(const :tag "All, but no backup, object, lib or ini-files \(except .emacs and .gnus)"
		       :value ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|dll\\|a\\|so\\|cache\\)$\\)\\)" "^\\.\\(emacs\\|gnus\\)$"))
		(const :tag "Common source file types (.c, .java etc.)"
		       :value ("" "\\(\\(M\\|m\\)akefile\\|.*\\.\\(java\\|el\\|c\\|cc\\|h\\|hh\\|txt\\|html\\|texi\\|info\\|bnf\\)\\)$"))
		(list :tag "Custom (tips: \"$^\" matches no files, \"\" mathes all files)"
		      (regexp :tag "Exclude regexp"
			      :value "")
		      (regexp :tag "Include regexp"
			      :value ""))))

(defcustom ecb-show-source-file-extension t
  "*Show the file extension of source files."
  :group 'ecb-sources
  :type 'boolean)

(defcustom ecb-sources-sort-method 'name
  "*Defines how the source files are sorted."
  :group 'ecb-sources
  :type '(radio (const :tag "By name"
                       :value name)
                (const :tag "By extension"
                       :value extension)
                (const :tag "No sorting"
                       :value nil)))
                
(defcustom ecb-history-buffer-name "*ECB History*"
  "*Name of the ECB history buffer. Because it is not a normal buffer for
editing you should enclose the name with stars, e.g. \"*ECB History*\".

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
  "*Defines which entries of the history buffer should be deleted if
`ecb-clear-history' is called. Three options are available:
- not-existing-buffers: All entries which represent a buffername not existing
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
                
(defcustom ecb-methods-buffer-name "*ECB Methods*"
  "*Name of the ECB methods buffer. Because it is not a normal buffer for
editing you should enclose the name with stars, e.g. \"*ECB Methods*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object of
the ECB-methods-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-methods
  :type 'string)

(defcustom ecb-auto-update-methods-after-save t
  "*Automatically updating the ECB method buffer after saving
the current source-buffer."
  :group 'ecb-methods
  :type 'boolean)

(defcustom ecb-show-method-arguments 'only-type
  "*Show method argument types and/or names. You have the following
choices:
- only-type: Show only the type of the argument
- type-and-name: Show both type and name
- nil: Do not show arguments.
In an untyped language like emacs-lisp show always only the argumentnames
instead."
  :group 'ecb-methods
  :type '(radio (const :tag "Show only type"
                       :value only-type)
                (const :tag "Show type and name"
                       :value type-and-name)
                (const :tag "Do not show arguments"
                       :value nil)))

(defcustom ecb-show-method-return-type 'after
  "*Show method return type. You can specify where the return type
is displayed:
- after: <method-name> \(<arguments>) : <return type> \(= UML notation)
- before: <return type> <method-name> \(<arguments>)"
  :group 'ecb-methods
  :type '(radio (const :tag "Display after method \(UML\)"
                       :value after)
                (const :tag "Display before method"
                       :value before)
                (const :tag "Do not show return type"
                       :value nil)))

(defcustom ecb-font-lock-methods t
  "*Adds font-locking \(means highlighting) to the ECB-method buffer." 
  :group 'ecb-methods
  :type 'boolean)

(defcustom ecb-show-classes 'before
  "*How to show classes in the methods buffer."
  :group 'ecb-methods
  :type '(radio (const :tag "Display before methods"
                       :value before)
                (const :tag "Display after methods"
                       :value after)
                (const :tag "Do not show classes"
                       :value nil)))

(defcustom ecb-font-lock-method-faces '(font-lock-function-name-face
                                        font-lock-type-face
                                        font-lock-variable-name-face
                                        font-lock-type-face
                                        bold
                                        font-lock-variable-name-face
                                        font-lock-type-face)
  "*Specify how to highlight the parts of a method in the method buffer.
The value must be a list of exactly seven elements each of them either nil
\(not highlighting this part) or a face for this part. The sequence within the
list must be \(methodename argumenttype argumentname returntype classtype
variablename variabletype).

This option takes only effect if `ecb-font-lock-methods' is on."
  :group 'ecb-methods
  :type '(list (radio :tag "Method name"
                      (const :tag "Do not highlight" :value nil)
                      (face))
               (radio :tag "Argument type"
                      (const :tag "Do not highlight" :value nil)
                      (face))
               (radio :tag "Argument name"
                      (const :tag "Do not highlight" :value nil)
                      (face))
               (radio :tag "Return type"
                      (const :tag "Do not highlight" :value nil)
                      (face))
               (radio :tag "Class type"
                      (const :tag "Do not highlight" :value nil)
                      (face))
               (radio :tag "Variable name"
                      (const :tag "Do not highlight" :value nil)
                      (face))
               (radio :tag "Variable type"
                      (const :tag "Do not highlight" :value nil)
                      (face))))

(defcustom ecb-sort-methods t
  "*Sort the methods in the methods buffer." 
  :group 'ecb-methods
  :type 'boolean)

(defcustom ecb-method-jump-sets-mark t
  "*Jumping to a method from the ECB-method buffer now sets the mark
so the user can easily jump back."
  :group 'ecb-methods
  :type 'boolean)

(defcustom ecb-sort-variables t
  "*Sort the variables in the methods buffer." 
  :group 'ecb-methods
  :type 'boolean)

(defcustom ecb-show-variables 'collapsed
  "*How to show variables in the methods buffer."
  :group 'ecb-methods
  :type '(radio (const :tag "Show variables expanded"
                       :value expanded)
                (const :tag "Show variables collapsed"
                       :value collapsed)
                (const :tag "Do not show variables"
                       :value nil)))

(defcustom ecb-show-parents 'expanded
  "*How to show parents (extends and implements) of a class in the
methods buffer \(see also `ecb-exclude-parents-regexp')."
  :group 'ecb-methods
  :type '(radio (const :tag "Show parents expanded"
                       :value expanded)
                (const :tag "Show parents collapsed"
                       :value collapsed)
                (const :tag "Do not show parents"
                       :value nil)))

(defcustom ecb-exclude-parents-regexp nil
  "*Regexp which parent classes should not be shown in the methods buffer
\(see also `ecb-show-parents'). If nil then all parents will be shown if
`ecb-show-parents' is not nil."
  :group 'ecb-methods
  :type '(radio (const :tag "Do not exclude any parents"
                       :value nil)
                (regexp :tag "Parents-regexp to exclude")))

(defcustom ecb-highlight-token-with-point 'highlight-scroll
  "*How to highlight the method or variable under the cursor.
- highlight-scroll: Always scroll the method buffer, so the current method of the
  edit-window is highlighted in the method-window.
- highlight: Only highlight the current method of the edit window in the
  method window if the method is visible in the method-window.
- nil: No highlighting is done.
See also `ecb-highlight-token-with-point-delay'."
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
Therefore the default value is a delay of 0.25 seconds."
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

(defcustom ecb-highlight-method-header-after-jump 'secondary-selection
  "*If not nil then highlight the method-header in the source-buffer
after jumping to this method by clicking in the ECB-method-buffer onto this
method. If not nil then it must be a face."
  :group 'ecb-methods
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (or running-xemacs (facep value))
                       (overlay-put ecb-method-overlay 'face value))))
  :type '(radio (const :tag "No highlighting of method-header" :value nil)
                (face :tag "Face for the highligthing"
                      :value secondary-selection)))

(defcustom ecb-tree-indent 2
  "*Indent size for tree buffer. If you change this during ECB is activated
you must deactivate and activate ECB again to take effect."
  :group 'ecb-general
  :type 'integer)

(defcustom ecb-tree-expand-symbol-before nil
  "*Show the expand symbol before the items in a tree."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-truncate-lines t
  "*Truncate lines in ECB buffers. If you change this during ECB is activated
you must deactivate and activate ECB again to take effect."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-window-sync t
  "*Synchronize the ECB-windows with current edit window."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-window-sync-delay 0.25
  "*Time Emacs must be idle before the ECB-windows are synchronized with
current edit window. If nil then there is no delay, means synchronization
takes place immediately. A small value of about 0.25 seconds saves CPU
resources and you get even though almost the same effect as if you set no
delay."
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
  "*Enable incremental search in the ECB-tree-buffers. For a detailed
explanation see the online help section \"Working with the keyboard in the ECB
buffers\". If you change this during ECB is activated you must deactivate and
activate ECB again to take effect."
  :group 'ecb-general
  :type '(radio (const :tag "Match only prefix"
                       :value prefix)
                (const :tag "Match every substring"
                       :value substring)
                (const :tag "No incremental search"
                       :value nil)))

(defcustom ecb-show-node-name-in-minibuffer 'if-too-long
  "*Show the name of the ECB-buffer item under mouse in minibuffer.
If set to 'always of 'if-too-long then this works always only by moving the
mouse over a node regardless if the ECB-window is the active window or not."
  :group 'ecb-general
  :set (function (lambda (symbol value)
                   (set symbol value)
                   (if (and (boundp 'ecb-minor-mode)
                            ecb-minor-mode)
                       (if (or (equal value 'always)
                               (equal value 'if-too-long))
                           (tree-buffer-activate-follow-mouse)
                         (tree-buffer-deactivate-follow-mouse)
                         (tree-buffer-deactivate-mouse-tracking)))))
  :type '(radio (const :tag "Always"
                       :value always)
                (const :tag "If longer than window-width"
                       :value if-too-long)
                (const :tag "After SHIFT-primary-mouse-button-click"
                       :value shift-click)
                (const :tag "Never"
                       :value nil)))

(defcustom ecb-show-file-info-in-minibuffer t
  "*Show file information about the file under mouse in minibuffer."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-show-complete-file-name-in-minibuffer nil
  "*Show the complete file name including directories for the file under mouse
in minibuffer."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-primary-secondary-mouse-buttons 'mouse-2--C-mouse-2
  "*Primary- and secondary mouse button for using the ECB-buffers.
A click with the primary button causes the main effect in each ECB-buffer:
- ECB Directories: Expanding/collapsing nodes and displaying files in the ECB
  Sources buffer.
- ECB sources/history: Opening the file in that edit-window specified by the
  option `ecb-primary-mouse-jump-destination'.
- ECB Methods: Jumping to the method in that edit-window specified by the
  option `ecb-primary-mouse-jump-destination'.
A click with the primary mouse-button while the SHIFT-key is pressed only
displays the complete clicked node in the minibuffer. This is useful if the
node is longer as the window-width of the ECB-window and `ecb-truncate-lines'
is not nil.

The secondary mouse-button is for opening \(jumping to) the file in the other
window \(see the documentation `ecb-primary-mouse-jump-destination').

The following combinations are possible:
- primary: mouse-2, secondary: C-mouse-2 \(means mouse-2 while CTRL-key is
  pressed). This is the default setting.
- primary: mouse-1, secondary: C-mouse-1
- primary: mouse-1, secondary: mouse-2

If you change this during ECB is activated you must deactivate and activate
ECB again to take effect"
  :group 'ecb-general
  :type '(radio (const :tag "Primary: mouse-2, secondary: Ctrl-mouse-2"
                       :value mouse-2--C-mouse-2)
                (const :tag "Primary: mouse-1, secondary: Ctrl-mouse-1"
                       :value mouse-1--C-mouse-1)
                (const :tag "Primary: mouse-1, secondary: mouse-2"
                       :value mouse-1--mouse-2)))
  
  

;; Thanks to David Hay for the suggestion <David.Hay@requisite.com>
(defcustom ecb-primary-mouse-jump-destination 'left-top
  "*Jump-destination of a primary mouse-button click \(see
`ecb-primary-secondary-mouse-buttons') in an ECB-window, if you click onto a
source or method or variable. Defines in which edit-window \(if splitted) ECB
does the \"right\" action \(opening the source, jumping to a method/variable).
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

  
(defcustom ecb-activate-before-layout-draw-hook nil
  "*Normal hook run at the end of activating the ecb-package by running
`ecb-activate'. This hooks are run after all the internal setup process
but directly before(!) drawing the layout specified in `ecb-layout' \(means
before dividing the frame into several windows).
A senseful using of this hook can be maximizing the Emacs-frame for example,
because this should be done before the layout is drawn because ECB computes
the size of the ECB-windows with the current frame size!
If you need a hook-option for the real end of the activating process (i.e.
after the layout-drawing) look at `ecb-activate-hook'."
  :group 'ecb-general
  :type 'hook)

(defcustom ecb-activate-hook nil
  "*Normal hook run at the end of activating the ecb-package by running
`ecb-activate'. This hooks are run at the real end of the activating
process, means after the layout has been drawn!. If you need hooks which are
run direct before the layout-drawing look at
`ecb-activate-before-layout-draw-hook'."
  :group 'ecb-general
  :type 'hook)

(defcustom ecb-deactivate-hook nil
  "*Normal hook run at the end of deactivating \(but before the ecb-layout is
cleared!) ECB by running `ecb-deactivate'."
  :group 'ecb-general
  :type 'hook)

;;====================================================
;; Methods
;;====================================================

(defconst ecb-language-modes-args-separated-with-space
  '(emacs-lisp-mode scheme-mode lisp-mode))

(defconst ecb-methodname 0)
(defconst ecb-argumenttype 1)
(defconst ecb-argumentname 2)
(defconst ecb-returntype 3)
(defconst ecb-classtype 4)
(defconst ecb-variablename 5)
(defconst ecb-variabletype 6)

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
  (interactive)
  (ecb-goto-window ecb-directories-buffer-name))

(defun ecb-goto-window-sources ()
  (interactive)
  (ecb-goto-window ecb-sources-buffer-name))

(defun ecb-goto-window-methods ()
  (interactive)
  (ecb-goto-window ecb-methods-buffer-name))

(defun ecb-goto-window-history ()
  (interactive)
  (ecb-goto-window ecb-history-buffer-name))

(defun ecb-goto-window-edit1 ()
  (interactive)
  (when ecb-minor-mode
    (raise-frame ecb-frame)
    (select-frame ecb-frame)    
    (ecb-select-edit-window nil)))

(defun ecb-goto-window-edit2 ()
  (interactive)
  (when ecb-minor-mode
    (raise-frame ecb-frame)
    (select-frame ecb-frame)    
    (ecb-select-edit-window t)))

(defun ecb-goto-window-compilation ()
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

(defun ecb-highlight-text (orig-text type)
  "If `ecb-font-lock-methods' is not nil then dependend to TYPE the face
specified in `ecb-font-lock-method-faces' is added to TEXT. If either
`font-lock-mode' in the source-buffer is nil or the face is not defined then
nothing is done. Returns TEXT."
  (let ((text (copy-sequence orig-text)))
    (if (stringp text)
        (if (and ecb-font-lock-methods font-lock-mode)
            (let* ((face-option-val (nth type ecb-font-lock-method-faces))
                   (face (or (and face-option-val
				  (if running-xemacs
                                      ;; facep seems not to work correct with
                                      ;; XEmacs and boundp returns nil for
                                      ;; faces like bold, italic, underline.
				      (or (boundp face-option-val)
                                          (equal face-option-val 'bold)
                                          (equal face-option-val 'italic)
                                          (equal face-option-val 'underline))
				    (facep face-option-val))
                                  face-option-val)
                             'default)))
              (put-text-property 0 (length text) 'face face text)
              ;; some special heuristic for better handling of the lisp-dialects
              (when (and (memq major-mode
                               ecb-language-modes-args-separated-with-space)
                         (eq type ecb-argumentname)
                         (not (eq face 'default))
                         ;; lets look if some special keywords like &optional or :key
                         ;; are in the text.
                         (or (string-match "^\\(&[^& \t]+\\)" text)
                             (string-match "^\\(:[^: \t]+\\)" text)))
                (put-text-property (match-beginning 1) (match-end 1)
                                   'face (if (boundp 'font-lock-type-face)
                                             'font-lock-type-face 'default) text)))))
    text))

(defun ecb-get-method-sig (method-token)
  "Returns the complete method-signature as a string and does also the
highlighting of the methods if `ecb-font-lock-methods' is not nil."
  ;; all strings i this method must be build with concat and not with format
  ;; because format does not preserve text-properties!
  (let* ((method-type (semantic-token-type method-token))
         (return-type (ecb-highlight-text
                       (if (and ecb-show-method-return-type
                                (> (length method-type) 0))
                           (if (listp method-type)
                               (car method-type) method-type)
                         "")
                       ecb-returntype))
         (method-and-args
          (concat
           (ecb-highlight-text (semantic-token-name method-token)
                               ecb-methodname)
           " ("
           (if ecb-show-method-arguments
               (mapconcat
                (lambda(method-arg-token)
                  (let ((method-arg-type
                         (ignore-errors
                           (semantic-token-type method-arg-token))))
                    (if method-arg-type
                        (concat (ecb-highlight-text (if (listp method-arg-type)
                                                        (car method-arg-type)
                                                      method-arg-type)
                                                    ecb-argumenttype)
                                (ecb-highlight-text
                                 (if (eq ecb-show-method-arguments 'type-and-name)
                                     (concat " "
                                             (if (listp method-arg-token)
                                                 (car method-arg-token)
                                               method-arg-token)))
                                 ecb-argumentname))
                      ;; there is no type so we probably have an untyped language
                      ;; like emacs-lisp etc. In such a case we display the
                      ;; argument-name.
                      (ecb-highlight-text (if (listp method-arg-token)
                                              (car method-arg-token)
                                            method-arg-token)
                                          ecb-argumentname))))
                (delete nil (semantic-token-function-args method-token))
                ;; dependent of the language we separate the args either with a
                ;; space or with a comma. With this trick there is no need to
                ;; recognice in lisp-like languages such keywords like &optional to
                ;; set the commas correct.
                (if (memq major-mode ecb-language-modes-args-separated-with-space)
                    " "
                  ", ")))
           ")")))
    ;; now lets build the complete signature
    (cond ((eq ecb-show-method-return-type 'before)
           (concat return-type
                   (if (> (length return-type) 0) " " "")
                   method-and-args))
          ((eq ecb-show-method-return-type 'after)
           (concat method-and-args
                   (if (> (length return-type) 0) " : " "")
                   return-type))
          (t method-and-args))))
  
(defun ecb-get-variable-text (var-token)
  (let ((type (semantic-token-type var-token)))
    (concat (ecb-highlight-text (semantic-token-name var-token) ecb-variablename)
            (if type
                (concat " : " (ecb-highlight-text 
			       (cond ((semantic-token-p type)
				      (semantic-prototype-nonterminal type))
				     (t type))
			       ecb-variabletype))
              ""))))

(defun ecb-add-classes (node token &optional flatten)
  (let ((children (semantic-find-nonterminal-by-token 'type token)))
    (dolist (type children)
      (let ((n (if (and flatten (= 1 (length children)))
                   node
                 (tree-node-new (ecb-highlight-text (semantic-token-name type)
                                                    ecb-classtype)
                                0 type)))
	    (parents (delq nil
                           (mapcar (function
				    (lambda (p)
				      (if (or (not p)
					      (not ecb-exclude-parents-regexp)
					      (not (string-match
						    ecb-exclude-parents-regexp p)))
					  p)))
                                   (semantic-token-type-parent type)))))
	(when (and parents ecb-show-parents)
	  (let ((pn (tree-node-new "[Parents]" 1 nil)))
	    (dolist (p (if (listp parents) parents (list parents)))
	      (tree-node-add-child
	       pn
	       (tree-node-new (ecb-highlight-text p ecb-classtype) 2 p t)))
	    (when (eq ecb-show-parents 'expanded)
	      (tree-node-set-expanded pn t))
	    (tree-node-add-child n pn)))
        (unless (and flatten (= 1 (length children)))
          (tree-node-add-child node n))
        (ecb-add-tokens n (semantic-token-type-parts type))))))
  
(defun ecb-add-methods (node token methods)
  (if ecb-sort-methods
      (setq methods (sort methods (lambda(a b)
                                    (string< (semantic-token-name a)
                                             (semantic-token-name b))))))
  (dolist (method methods)
    (tree-node-add-child node (tree-node-new
                               (ecb-get-method-sig method) 0
                               method t))))

(defun ecb-add-variables (node token variables)
  (when (and ecb-show-variables variables)
    (let ((var-node node))
      (when (eq ecb-show-variables 'collapsed)
        (setq var-node (tree-node-new "[Variables]" 1 nil))
        (tree-node-add-child node var-node))
      (if ecb-sort-variables
          (setq variables (sort variables (lambda(a b)
                                            (string< (semantic-token-name a)
                                                     (semantic-token-name b))))))
      (dolist (var variables)
        (tree-node-add-child var-node (tree-node-new
                                       (ecb-get-variable-text var)
                                       0 var t))))))
  
(defun ecb-add-tokens (node token &optional flatten)
  ;; Semantic 1.4beta2 fix for EIEIO class parts
  ;; They are really strings, but here converted to tokens
  (if (stringp (car token))
      (setq token (mapcar (lambda (s) (list s 'variable nil)) token)))
  (let ((methods (semantic-find-nonterminal-by-token 'function token))
        (variables (semantic-find-nonterminal-by-token 'variable token)))
    (setq flatten (and flatten
                       (not methods)
                       (not (and variables ecb-show-variables))))
    (tree-node-set-expanded node t)
    (when (eq ecb-show-classes 'before)
      (ecb-add-classes node token flatten))
    (ecb-add-variables node token variables)
    (ecb-add-methods node token methods)
    (when (eq ecb-show-classes 'after)
      (ecb-add-classes node token flatten))))

(defun ecb-expand-tree (path node)
  (catch 'exit
    (dolist (child (tree-node-get-children node))
      (let ((name (tree-node-get-name child)))
        (when (and (>= (length path) (length name))
                   (string= (substring path 0 (length name)) name)
                   (or (= (length path) (length name))
                       (eq (elt path (length name)) ecb-directory-sep-char)))
          (let ((was-expanded (tree-node-is-expanded child)))
            (tree-node-set-expanded child t)
            (ecb-update-directory-node child)
            (throw 'exit
                   (or (when (> (length path) (length name))
                         (ecb-expand-tree (substring path (1+ (length name)))
                                          child))
                       (not was-expanded)))))))))

(defun ecb-get-source-files (dir files)
  (let (source-files)
    (dolist (file files)
      (let ((long-file-name (concat dir ecb-directory-sep-string file)))
	(if (and (not (file-directory-p long-file-name))
		 (or (string-match (cadr ecb-source-file-regexps) file)
		     (not (string-match (car ecb-source-file-regexps) file))))
	    (setq source-files (append source-files (list file))))))
    source-files))

(defun ecb-set-selected-directory (path)
  (let ((last-dir ecb-path-selected-directory))
    (save-selected-window
      (setq ecb-path-selected-directory (ecb-fix-filename path))
  
      (when (or (not ecb-show-sources-in-directories-buffer)
		ecb-auto-expand-directory-tree)
	(ecb-exec-in-directories-window
	 (when ecb-auto-expand-directory-tree
	   ;; Expand tree to show selected directory
	   (if (ecb-expand-tree ecb-path-selected-directory (tree-buffer-get-root))
	       (tree-buffer-update)))
	 (when (not ecb-show-sources-in-directories-buffer)
	   (tree-buffer-highlight-node-data ecb-path-selected-directory))))

      (ecb-exec-in-sources-window
       (let ((old-children (tree-node-get-children (tree-buffer-get-root))))
	 (tree-node-set-children (tree-buffer-get-root) nil)
	 (ecb-tree-node-add-files
	  (tree-buffer-get-root)
	  ecb-path-selected-directory
	  (ecb-get-source-files
	   ecb-path-selected-directory
	   (directory-files ecb-path-selected-directory nil nil t))
	  0
	  ecb-show-source-file-extension
	  old-children ecb-sources-sort-method t))
       (tree-buffer-update)
       (when (not (string= last-dir ecb-path-selected-directory))
	 (tree-buffer-scroll (point-min) (point-min))))))
  ;; set the default-directory of each tree-buffer to current selected
  ;; directory so we can open files via find-file from each tree-buffer.
  (save-excursion
    (dolist (buf tree-buffers)
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
  
(defun ecb-select-source-file (filename)
  "Updates the directories, sources and history buffers to match the filename
given."
  (save-selected-window
    (ecb-set-selected-directory (file-name-directory filename))
    (setq ecb-path-selected-source filename)
  
    ;; Update directory buffer
    (when ecb-show-sources-in-directories-buffer
      (ecb-exec-in-directories-window
       (tree-buffer-highlight-node-data ecb-path-selected-source)))
    
    ;; Update source buffer
    (ecb-exec-in-sources-window
     (tree-buffer-highlight-node-data ecb-path-selected-source))

    ;; Update history buffer
    (ecb-exec-in-history-window
     (tree-node-remove-child-data (tree-buffer-get-root) ecb-path-selected-source)
     (tree-node-set-children
      (tree-buffer-get-root)
      (let ((history-items
	     (cons
	      (tree-node-new (ecb-get-source-name ecb-path-selected-source) 0
			     ecb-path-selected-source t
			     (tree-buffer-get-root))
	      (tree-node-get-children (tree-buffer-get-root)))))
	(if ecb-sort-history-items
	    (sort history-items
		  (function (lambda (l r) (string< (tree-node-get-name l)
						   (tree-node-get-name r)))))
	  history-items)))
     (tree-buffer-update)
     (tree-buffer-highlight-node-data ecb-path-selected-source))))


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
    (ecb-update-methods-buffer--internal)))

;; This variable is only set and evaluated by the functions
;; `ecb-update-methods-buffer--internal' and
;; `ecb-rebuild-methods-buffer-after-parsing'!
(defvar ecb-method-buffer-needs-rebuild t)
(defun ecb-update-methods-buffer--internal (&optional scroll-to-top)
  "Updates the methods buffer with the current buffer. The only thing what
must be done is to start the toplevel parsing of semantic, because the rest is
done by `ecb-rebuild-methods-buffer-after-parsing' because this function is in
the `semantic-after-toplevel-bovinate-hook'.
If optional argument SCROLL-TO-TOP is non nil then the method-buffer is
displayed with window-start and point at beginning of buffer."
  (when (and (equal (selected-frame) ecb-frame)
             (get-buffer-window ecb-methods-buffer-name))
    ;; Set here `ecb-method-buffer-needs-rebuild' to t so we can see below if
    ;; `ecb-rebuild-methods-buffer-after-parsing' was called auto. after
    ;; `semantic-bovinate-toplevel'.
    (setq ecb-method-buffer-needs-rebuild t)

    (semantic-bovinate-toplevel t)
    
    ;; If the `semantic-bovinate-toplevel' has done no full reparsing but only
    ;; used it´s still valid `semantic-toplevel-bovine-cache' or only has done
    ;; a partial reparsing of dirty tokens the hooks in
    ;; `semantic-after-toplevel-bovinate-hook' are not evaluated and therefore
    ;; `ecb-rebuild-methods-buffer-after-parsing' was not called. Therefore we
    ;; call it here manually. `ecb-rebuild-methods-buffer-after-parsing' is
    ;; the only function which sets `ecb-method-buffer-needs-rebuild' to nil
    ;; to signalize that a "manually" rebuild of the method buffer is not
    ;; necessary.
    (if ecb-method-buffer-needs-rebuild
	(ecb-rebuild-methods-buffer-after-parsing))
    (when scroll-to-top
      (save-selected-window
	(ecb-exec-in-methods-window
	 (tree-buffer-scroll (point-min) (point-min)))))))
  
(defun ecb-rebuild-methods-buffer-after-parsing ()
  "Rebuilds the ECB-method buffer after toplevel-parsing by semantic. This
function is added to the hook `semantic-after-toplevel-bovinate-hook'."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (get-buffer-window ecb-methods-buffer-name)
             ;; In semantic 1.4 the functions of the hook
             ;; `semantic-after-toplevel-bovinate-hook' are also called after
             ;; clearing the cache to set the cache to nil if a buffer is
             ;; parsed which has no tokens like plain text-buffers. Here we do
             ;; not want rebuilding the method-buffer if the cache is nil but
             ;; the current buffer is set up for semantic-parsing, because the
             ;; real rebuild should be done after the cache is filled again.
             (or (semantic-toplevel-get-cache)
                 (not (semantic-active-p))))
    ;; This is a fix for semantic 1.4beta2
    ;; otherwise it parses the mini-buffer
    (unless (string-match "^ *\\*" (buffer-name))
      (tree-node-set-children ecb-methods-root-node nil)
      (ecb-add-tokens ecb-methods-root-node
		      ;; this works because at call-time of the hooks in
		      ;; `semantic-after-toplevel-bovinate-hook' the cache is
		      ;; always either still valid or rebuild.
                      (semantic-toplevel-get-cache) t)
      ;; also the whole buffer informations should be preserved!
      (save-excursion
	(ecb-buffer-select ecb-methods-buffer-name)
        (setq tree-buffer-indent ecb-tree-indent)
	(tree-buffer-update))
      (ecb-mode-line-format)
      ;; signalize that the rebuild has already be done
      (setq ecb-method-buffer-needs-rebuild nil))))

(defun ecb-rebuild-methods-buffer ()
  "Updates the methods buffer with the current buffer after deleting the
complete previous parser-information, means no semantic-cache is used! Point
must stay in an edit-window otherwise nothing is done.
This method is merely needed if semantic parses not the whole buffer because
it reaches a not parsable code.
Examples when a call to this function is necessary:
+ If an elisp-file is parsed which contains in the middle a defun X where the
  closing ) is missing then semantic parses only until this defun X is reached
  and you will get an incomplete ECB-method buffer. In such a case you must
  complete the defun X and then call this function to completely reparse the
  elisp-file and rebuild the ECB method buffer!
+ If you change only the name of a method or a variable and you want the new
  name be shown immediately in the ECB-method buffer then you must call this
  function."
  (interactive)
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (ecb-point-in-edit-window)
             (y-or-n-p "Do you want completely rebuilding the method buffer? "))
    ;; to force a really complete rebuild we must completely clear the
    ;; semantic cache
    (semantic-clear-toplevel-cache)
    (ecb-update-methods-buffer--internal)))

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
	(set-buffer (find-file-noselect ecb-path-selected-source))
	(ecb-update-methods-buffer--internal 'scroll-to-begin))
    ;; open the selected source in the edit-window and do all the update and
    ;; parsing stuff with this buffer
    (ecb-find-file-and-display ecb-path-selected-source
			       other-edit-window)
    (ecb-update-methods-buffer--internal 'scroll-to-begin)))

(defun ecb-remove-from-current-tree-buffer (node)
  (when node
    (tree-node-remove-child (tree-buffer-get-root) node)))

(defun ecb-clear-history (&optional clearall)
  "Clears the ECB history-buffer. If CLEARALL is nil then the behavior is
defined in the option `ecb-clear-history-behavior' otherwise the value of
CLEARALL overrides the value of this option:
< 0: Means not-existing-buffers
> 0: Means existing-buffers
= 0: Means all
For further explanation see `ecb-clear-history-behavior'."
  (interactive "P")
  (unless (or (not ecb-minor-mode)
              (not (equal (selected-frame) ecb-frame)))
    (save-selected-window
      (ecb-exec-in-history-window
       (let ((buffer-file-name-list (mapcar (lambda (buff)
                                              (buffer-file-name buff))
                                            (buffer-list)))
             (tree-childs (tree-node-get-children (tree-buffer-get-root)))
             (clear-behavior (or (if (and clearall (integerp clearall))
                                     (cond ((= clearall 0) 'all)
                                           ((< clearall 0) 'not-existing-buffers)
                                           (t 'existing-buffers)))
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

(defun ecb-token-sync ()
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    (when ecb-highlight-token-with-point
      (let* ((tok (semantic-current-nonterminal)))
        (when (not (equal ecb-selected-token tok))
          (setq ecb-selected-token tok)
          (save-selected-window
            (ecb-exec-in-methods-window
             (tree-buffer-highlight-node-data
              tok (equal ecb-highlight-token-with-point 'highlight)))))))))

(defun ecb-current-buffer-sync (&optional force opt-buffer)
  "Synchronizes the ECB buffers with the current buffer. Unless FORCE is non
nil then do this only if current-buffer differs from the source displayed in
the ECB tree-buffers."
  (interactive "P")
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    (ignore-errors
      (let ((filename (buffer-file-name (if opt-buffer opt-buffer (current-buffer)))))
        (when (and filename
                   (or force
                       (not (string= filename ecb-path-selected-source))))
          ;; KB: seems this little sleep is necessary because otherwise jumping to
          ;; certain markers in new opened files (e.g. with next-error etc. )
          ;; doesn´t work correct. Can´t debug down this mysterious thing!
          ;; Regardless of the size of the file to load, this 0.1 fraction of a
          ;; sec is enough!
          (sit-for 0.1)
          (ecb-update-directories-buffer)
          (ecb-select-source-file filename)
          ;; selected source has changed, therfore we must initialize
          ;; ecb-selected-token again.
          (setq ecb-selected-token nil)
          (ecb-update-methods-buffer--internal 'scroll-to-begin)
          (ecb-token-sync))))))

(defun ecb-window-sync-function ()
  (when (and ecb-window-sync ecb-minor-mode (equal (selected-frame) ecb-frame))
    (ecb-current-buffer-sync)))

(defun ecb-find-file-and-display (filename other-edit-window)
  "Finds the file in the correct window. What the correct window is depends on
the setting in `ecb-primary-mouse-jump-destination' and the value of
OTHER-EDIT-WINDOW."
  (if (eq ecb-primary-mouse-jump-destination 'left-top)
      (select-window ecb-edit-window)
    (select-window ecb-last-edit-window-with-point))
  (ecb-with-adviced-functions
   (if other-edit-window
       (let ((ecb-other-window-jump-behavior 'only-edit))
	 (other-window 1))))
  (ecb-with-original-functions
   (find-file ecb-path-selected-source)
   (pop-to-buffer (buffer-name))))

(defun ecb-tree-node-add-files
  (node path files type include-extension old-children sort-method
	&optional not-expandable)
  (dolist (file (if sort-method
		    (let ((sorted-files (sort files 
					      (function
					       (lambda(a b) (string< a b))))))
		      (if (eq sort-method 'extension)
			  (sort sorted-files 
				(function (lambda(a b)
					    (string< (file-name-extension a t)
						     (file-name-extension b t)))))
			sorted-files))))
    (let ((filename (concat path ecb-directory-sep-string file)))
      (tree-node-add-child
       node
       (ecb-new-child
        old-children
        (if include-extension
            file
          (file-name-sans-extension file))
        type filename (or not-expandable (= type 1)))))))
  
(defun ecb-update-directory-node (node)
  "Updates the directory node NODE and add all subnodes if any."
  (let ((old-children (tree-node-get-children node))
        (path (tree-node-get-data node)))
    (tree-node-set-children node nil)
    (if (file-accessible-directory-p path)
        (let* ((files (directory-files path nil nil t))
	       dirs
	       (normal-files (ecb-get-source-files path files)))
          (dolist (file files)
            (let ((filename (concat path ecb-directory-sep-string file)))
              (if (file-accessible-directory-p filename)
                  (if (not (string-match ecb-excluded-directories-regexp file))
                      (setq dirs (append dirs (list file)))))))
          (ecb-tree-node-add-files node path dirs 0 t old-children 'name)
          (if ecb-show-sources-in-directories-buffer
              (ecb-tree-node-add-files node path normal-files 1
                                       ecb-show-source-file-extension
                                       old-children ecb-sources-sort-method))
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
              (paths (append (ecb-get-source-paths-from-functions) ecb-source-path)))
         (tree-node-set-children node nil)
	 (dolist (dir paths)
	   (let ((norm-dir (ecb-fix-filename dir t)))
	     (tree-node-add-child node (ecb-new-child old-children
						      norm-dir 2 norm-dir))))
	 (when (not paths)
	   (tree-node-add-child node (tree-node-new "Welcome to ECB! Please select:"
						    3 '(lambda()) t))
           (tree-node-add-child node (tree-node-new "" 3 '(lambda()) t))
           (tree-node-add-child
            node (tree-node-new
                  "[F1] Add Source Path" 3
                  '(lambda () (call-interactively 'ecb-add-source-path)) t))
           (tree-node-add-child node (tree-node-new "[F2] Customize ECB" 3
                                                    'ecb-customize t))
           (tree-node-add-child node (tree-node-new "[F3] ECB Help" 3
                                                    'ecb-show-help t)))
	 (tree-buffer-update))))))
  
(defun ecb-new-child (old-children name type data &optional not-expandable)
  (catch 'exit
    (dolist (child old-children)
      (when (and (equal (tree-node-get-data child) data)
                 (= (tree-node-get-type child) type))
        (tree-node-set-name child name)
        (if not-expandable
            (tree-node-set-expandable child nil))
        (throw 'exit child)))
    (tree-node-new name type data not-expandable)))

(defun ecb-add-source-path (&optional dir)
  (interactive "DAdd source path: ")
  (setq ecb-source-path (append ecb-source-path (list dir)))
  (ecb-update-directories-buffer)
  (customize-save-variable 'ecb-source-path ecb-source-path))

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
  (setq ecb-source-path (ecb-delete-s
			 node (tree-node-get-children (tree-node-get-parent node))
			 ecb-source-path))
  (ecb-update-directories-buffer)
  (customize-save-variable 'ecb-source-path ecb-source-path))

(defun ecb-customize ()
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
    ;; in the following we only operate with ecb-button and shift-mode and
    ;; never with mouse-button, shift-pressed and control-pressed!!
    (when ecb-button-list
      (cond ((string= tree-buffer-name ecb-directories-buffer-name)
	     (ecb-directory-clicked node ecb-button shift-mode))
	    ((string= tree-buffer-name ecb-sources-buffer-name)
	     (ecb-source-clicked node ecb-button shift-mode))
	    ((string= tree-buffer-name ecb-history-buffer-name)
	     (ecb-source-clicked node ecb-button shift-mode))
	    ((string= tree-buffer-name ecb-methods-buffer-name)
	     (ecb-method-clicked node ecb-button shift-mode))
	    (t nil)))))

(defun ecb-tree-buffer-node-expand-callback (node
					     mouse-button
					     shift-pressed
					     control-pressed
					     tree-buffer-name)
  "This is the callback-function ecb.el gives to every tree-buffer to call
when a node should be expanded. This function does nothing if the click
combination is invalid \(see `ecb-interpret-mouse-click'."
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
	     (ecb-source-clicked node ecb-button shift-mode))
	    ((string= tree-buffer-name ecb-methods-buffer-name)
	     nil)
	    (t nil)))))

(defun ecb-interpret-mouse-click (mouse-button
                                  shift-pressed
                                  control-pressed
                                  tree-buffer-name)
  "Converts the physically pressed MOUSE-BUTTON \(1 = mouse-1, 2 = mouse-2, 0 =
no mouse-button but a key like RET or TAB) to ECB-mouse-buttons: either primary
or secondary mouse-button depending on the value of CONTROL-PRESSED and the
setting in `ecb-primary-secondary-mouse-buttons'. Returns a list '\(ECB-button
shift-mode) where ECB-button is either 1 \(= primary) or 2 \(= secondary) and
shift-mode is non nil if SHIFT-PRESSED is non nil. For an invalid and not
accepted click combination nil is returned.

Note: If MOUSE-BUTTON is 0 \(means no mouse-button but a key like RET or TAB
was hitted) then only nil is accepted for SHIFT-PRESSED and CONTROL-PRESSED.

Currently the fourth argument TREE-BUFFER-NAME is not used here."
  (if (and (eq mouse-button 0) (not shift-pressed) (not control-pressed))
      (list 1 nil)
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

(defun ecb-directory-clicked (node ecb-button shift-mode)
  (if (= 3 (tree-node-get-type node))
      (funcall (tree-node-get-data node))
    (ecb-update-directory-node node)
    (if (or (= 0 (tree-node-get-type node)) (= 2 (tree-node-get-type node)))
	(if shift-mode
	    (ecb-mouse-over-directory-node node)
	  (progn
	    (if (= 2 ecb-button)
		(tree-node-toggle-expanded node)
	      (ecb-set-selected-directory (tree-node-get-data node)))
	    (ecb-exec-in-directories-window
	     ;; Update the tree-buffer with optimized display of NODE
	     (tree-buffer-update node))))
      (ecb-set-selected-source (tree-node-get-data node)
			       (and (ecb-edit-window-splitted) (eq ecb-button 2))
			       shift-mode))))

(defun ecb-source-clicked (node ecb-button shift-mode)
  (if shift-mode
      (ecb-mouse-over-source-node node))
  (ecb-set-selected-source (tree-node-get-data node)
			   (and (ecb-edit-window-splitted) (eq ecb-button 2))
			   shift-mode))

;; this mechanism is necessary because tree-buffer creates for mouse releasing
;; a new nop-command (otherwise the cursor jumps back to the tree-buffer).
(defvar ecb-unhighlight-hook-called nil)
(defun ecb-unhighlight-method-header ()
  (let ((key (tree-buffer-event-to-key last-input-event)))
    (when (not (or (and (equal key 'mouse-release)
                        (not ecb-unhighlight-hook-called))
                   (equal key 'mouse-movement)))
      (if ecb-highlight-method-header-after-jump
          (delete-overlay ecb-method-overlay))
      (remove-hook 'pre-command-hook 'ecb-unhighlight-method-header)))
  (setq ecb-unhighlight-hook-called t))

(defun ecb-method-clicked (node ecb-button shift-mode)
  (if shift-mode
      (ecb-mouse-over-method-node node)
    (when (= 1 (tree-node-get-type node))
      (tree-node-toggle-expanded node)
      ;; Update the tree-buffer with optimized display of NODE
      (tree-buffer-update node))
    (if (= 2 (tree-node-get-type node))
	(progn
	  (jde-show-class-source (tree-node-get-data node)))
      (when (tree-node-get-data node)
	(ecb-find-file-and-display ecb-path-selected-source
				   (and (ecb-edit-window-splitted)
                                        (eq ecb-button 2)))
	;; let us set the mark so the user can easily jump back.
	(if ecb-method-jump-sets-mark
	    (push-mark))
	;; Semantic 1.4beta2 fix for EIEIO class parts
	(ignore-errors
	  (goto-char (semantic-token-start (tree-node-get-data node))))
        (when ecb-highlight-method-header-after-jump
          (save-excursion
            (move-overlay ecb-method-overlay
                          (tree-buffer-line-beginning-pos)
                          (tree-buffer-line-end-pos)
                          (current-buffer)))
          (setq ecb-unhighlight-hook-called nil)
          (add-hook 'pre-command-hook 'ecb-unhighlight-method-header))))))

(defun ecb-get-file-info-text (file)
  (let ((attrs (file-attributes file)))
    (format "%s %8s %4d %10d %s %s"
	    (nth 8 attrs)
	    (user-login-name (nth 2 attrs))
	    (nth 3 attrs)
	    (nth 7 attrs)
	    (format-time-string "%Y/%m/%d %H:%M" (nth 5 attrs))
	    (if ecb-show-complete-file-name-in-minibuffer file
	      (file-name-nondirectory file))
	    )))

(defun ecb-show-minibuffer-info (node window)
  "Checks if in the minibuffer should displayed any info about the current
node in the ECB-window WINDOW."
  (or (eq ecb-show-node-name-in-minibuffer 'always)
      (eq ecb-show-node-name-in-minibuffer 'shift-click)
      (and (eq ecb-show-node-name-in-minibuffer 'if-too-long)
	   (>= (+ (length (tree-node-get-name node))
		  (tree-buffer-get-node-indent node))
	       (window-width window)))))

;; argument buffer currently not used in the following three functions!
(defun ecb-mouse-over-directory-node (node &optional buffer window)
  (if (= (tree-node-get-type node) 1)
      (ecb-mouse-over-source-node node)
    (if (not (= (tree-node-get-type node) 3))
	(tree-buffer-nolog-message
         (when (ecb-show-minibuffer-info node window)
           (tree-node-get-data node))))))

(defun ecb-mouse-over-source-node (node &optional buffer window)
  ;; For buffers that hasnt been saved yet
  (ignore-errors
    (tree-buffer-nolog-message
     (when (ecb-show-minibuffer-info node window)
       (if ecb-show-file-info-in-minibuffer
           (ecb-get-file-info-text (tree-node-get-data node))
         (if ecb-show-complete-file-name-in-minibuffer
             (tree-node-get-data node)
           (tree-node-get-name node)))))))

(defun ecb-mouse-over-method-node (node &optional buffer window)
  (tree-buffer-nolog-message (when (ecb-show-minibuffer-info node window)
                               (tree-node-get-name node))))


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

(defvar ecb-prefix-key [(control ?c) ?.]
  "The common prefix key in ECB minor mode.")

(defvar ecb-prefix-map
  (let ((km (make-sparse-keymap)))
    (define-key km "f" 'ecb-activate)
    (define-key km "l" 'ecb-redraw-layout)
    (define-key km "t" 'ecb-toggle-ecb-windows)
    (define-key km "r" 'ecb-rebuild-methods-buffer)
    (define-key km "o" 'ecb-show-help)
    (define-key km "1" 'ecb-goto-window-edit1)
    (define-key km "2" 'ecb-goto-window-edit2)
    (define-key km "c" 'ecb-goto-window-compilation)
    (define-key km "d" 'ecb-goto-window-directories)
    (define-key km "s" 'ecb-goto-window-sources)
    (define-key km "m" 'ecb-goto-window-methods)
    (define-key km "h" 'ecb-goto-window-history)
    km)
  "Default key bindings in ECB minor mode.")

(defun ecb-menu-item (item)
  "Build an XEmacs compatible menu item from vector ITEM.
That is remove the unsupported :help stuff."
  (if running-xemacs
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
    [ "Deactivate ECB"
      ecb-deactivate
      :active t
      :help "Deactivate ECB."
      ])
   "-"
   (ecb-menu-item
    [ "Select ECB frame"
      ecb-activate
      :active (not (equal (selected-frame) ecb-frame))
      :help "Select the ECB-frame."
      ])
   (ecb-menu-item
    [ "Redraw layout"
      ecb-redraw-layout
      :active (equal (selected-frame) ecb-frame)
      :help "Redraw the layout."
      ])
   (ecb-menu-item
    [ "Toggle visibility of ECB windows"
      ecb-toggle-ecb-windows
      :active (equal (selected-frame) ecb-frame)
      :help "Toggle the visibility of all ECB windows."
      ])
   (ecb-menu-item
    [ "Rebuild method buffer"
      ecb-rebuild-methods-buffer
      :active (equal (selected-frame) ecb-frame)
      :help "Rebuild the method buffer completely"
      ])
   "-"
   (list
    "Goto window"
    (ecb-menu-item
     ["Edit-window 1"
      ecb-goto-window-edit1
      :active t
      :help "Go to the first edit-window"
      ])
    (ecb-menu-item
     ["Edit-window 2 \(if splitted\)"
      ecb-goto-window-edit2
      :active t
      :help "Go to the second edit-window \(if splitted\)"
      ])
    (ecb-menu-item
     ["Directories"
      ecb-goto-window-directories
      :active t
      :help "Go to the directories window"
      ])
    (ecb-menu-item
     ["Sources"
      ecb-goto-window-sources
      :active t
      :help "Go to the sources window"
      ])
    (ecb-menu-item
     ["Methods and Variables"
      ecb-goto-window-methods
      :active t
      :help "Go to the methods/variables window"
      ])
    (ecb-menu-item
     ["History"
      ecb-goto-window-history
      :active t
      :help "Go to the history window"
      ])
    (ecb-menu-item
     ["Compilation"
      ecb-goto-window-compilation
      :active (and ecb-compile-window-height ecb-compile-window)
      :help "Go to the history window"
      ])
    )
   "-"
   (list
    "Preferences"
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
    )
   "-"
   (list
    "Help"
    (ecb-menu-item
     [ "Show Online Help"
       ecb-show-help
       :active (equal (selected-frame) ecb-frame)
       :help "Show the online help of ECB."
       ])
    (ecb-menu-item
     [ "Submit problem report"
       ecb-submit-problem-report
       :active (equal (selected-frame) ecb-frame)
       :help "Submit a problem report to the ECB mailing list."
       ])
    "-"
    (concat "ECB " ecb-version)
    )
   )
  "Menu for ECB minor mode.")

(defvar ecb-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km ecb-prefix-key ecb-prefix-map)
    (easy-menu-define ecb-minor-menu km "ECB Minor Mode Menu" ecb-menu-bar)
    km)
  "Keymap for ECB minor mode.")

(defun ecb-activate ()
  "Activates the ECB and creates all the buffers and draws the ECB-screen
with the actually choosen layout \(see `ecb-layout-nr'). This function raises
always the ECB-frame if called from another frame."
  (interactive)
  (ecb-minor-mode 1))

(defun ecb-activate-internal ()
  "Activates the ECB and creates all the buffers and draws the ECB-screen
with the actually choosen layout \(see `ecb-layout-nr'). This function raises
always the ECB-frame if called from another frame."
  (if ecb-use-recursive-edit
      (if ecb-minor-mode
	  (progn
	    (message "ECB already activated.  Drawing layout.")
            
	    (ecb-redraw-layout))
	(catch 'exit
	  (progn
	    (ecb-activate--impl)
	    (recursive-edit))
	  (ecb-deactivate-internal)))
    
    (ecb-activate--impl))
  ecb-minor-mode)

(defun ecb-activate--impl ()
  "See `ecb-activate'.  This is the implementation of ECB activation."
  
  (if ecb-minor-mode
      (progn
	(raise-frame ecb-frame)
	(select-frame ecb-frame)
	(ecb-redraw-layout)
	(ecb-update-directories-buffer))

    (setq ecb-old-compilation-window-height compilation-window-height)
    
    ;; first initialize the whole layout-engine
    (ecb-initialize-layout)

    ;; enable basic advices
    (ecb-enable-basic-advices)
    
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
	 (list (cons 0 ecb-directories-menu) (cons 1 ecb-sources-menu)
	       (cons 2 ecb-source-path-menu))
	 ecb-truncate-lines
	 t
	 ecb-tree-indent
	 ecb-tree-incremental-search
	 (list (cons 1 ecb-source-in-directories-buffer-face))
	 ecb-tree-expand-symbol-before
	 ;; we add an after-create-hook to the tree-buffer
	 (function (lambda ()
		     (local-set-key [f1] 'ecb-add-source-path)
		     (local-set-key [f2] 'ecb-customize)
		     (local-set-key [f3] 'ecb-show-help)))
	 ))
      
      (unless (member ecb-sources-buffer-name curr-buffer-list)
	(tree-buffer-create
	 ecb-sources-buffer-name
	 ecb-frame
	 'ecb-interpret-mouse-click
	 'ecb-tree-buffer-node-select-callback
	 'ecb-tree-buffer-node-expand-callback
         'ecb-mouse-over-source-node
	 (list (cons 0 ecb-sources-menu))
	 ecb-truncate-lines
	 t
	 ecb-tree-indent
	 ecb-tree-incremental-search))
      
      (unless (member ecb-methods-buffer-name curr-buffer-list)
	(tree-buffer-create
	 ecb-methods-buffer-name
	 ecb-frame
	 'ecb-interpret-mouse-click
	 'ecb-tree-buffer-node-select-callback
	 nil
         'ecb-mouse-over-method-node
	 nil
	 ecb-truncate-lines
	 t
	 ecb-tree-indent
	 ecb-tree-incremental-search
	 nil
	 ecb-tree-expand-symbol-before)
	(setq ecb-methods-root-node (tree-buffer-get-root)))
      
      (unless (member ecb-history-buffer-name curr-buffer-list)
	(tree-buffer-create
	 ecb-history-buffer-name
	 ecb-frame
	 'ecb-interpret-mouse-click
	 'ecb-tree-buffer-node-select-callback
	 'ecb-tree-buffer-node-expand-callback
         'ecb-mouse-over-source-node
	 (list (cons 0 ecb-history-menu))
	 ecb-truncate-lines
	 t
	 ecb-tree-indent
	 ecb-tree-incremental-search)))
    
    ;; we need some hooks
    (add-hook 'semantic-after-toplevel-bovinate-hook
	      'ecb-rebuild-methods-buffer-after-parsing)
    (ecb-activate-ecb-sync-functions ecb-highlight-token-with-point-delay
                                     'ecb-token-sync)
    (ecb-activate-ecb-sync-functions ecb-window-sync-delay
                                     'ecb-window-sync-function)
    (add-hook 'pre-command-hook 'ecb-pre-command-hook-function)
    (add-hook 'after-save-hook 'ecb-update-methods-after-saving)

    ;; ediff-stuff; we operate here only with symbols to avoid bytecompiler
    ;; warnings
    (if (boundp 'ediff-quit-hook)
	(put 'ediff-quit-hook 'ecb-ediff-quit-hook-value
	     (symbol-value 'ediff-quit-hook)))
    (add-hook 'ediff-quit-hook 'ediff-cleanup-mess)
    (add-hook 'ediff-quit-hook 'ecb-ediff-quit-hook t)

    ;; menus
    (if running-xemacs
        (add-submenu nil ecb-minor-menu)
      (easy-menu-add ecb-minor-menu))

    (setq ecb-minor-mode t)

    ;; we must update the directories buffer first time
    (ecb-update-directories-buffer)

    ;; run personal hooks before drawing the layout
    (run-hooks 'ecb-activate-before-layout-draw-hook)

    ;; now we draw the layout choosen in `ecb-layout'. This function
    ;; acivates at its end also the adviced functions if necessary!
    (ecb-redraw-layout)
    
    (ecb-with-adviced-functions
     (cond ((equal ecb-split-edit-window 'vertical)
            (split-window-vertically))
           ((equal ecb-split-edit-window 'horizontal)
            (split-window-horizontally))
           ((not ecb-split-edit-window)
            (delete-other-windows))))

    (ecb-update-directories-buffer)
    ;; now update all the ECB-buffer-modelines
    (ecb-mode-line-format)

    ;; we run any personal hooks
    (run-hooks 'ecb-activate-hook)

    ;; enable mouse-tracking for the tree-buffers; we do this after running
    ;; the personal hooks because if a user put´s activation of
    ;; follow-mouse.el (`turn-on-follow-mouse') in the `ecb-activate-hook'
    ;; then our own ECb mouse-tracking must be activated later.
    ;; If `turn-on-follow-mouse' would be activated after our own follow-mouse
    ;; stuff, it would overwrite our mechanism and the show-node-name stuff
    ;; would not work!
    (if (or (equal ecb-show-node-name-in-minibuffer 'always)
            (equal ecb-show-node-name-in-minibuffer 'if-too-long))
        (tree-buffer-activate-follow-mouse))
    
    (message "The ECB is now activated.")))

(defun ecb-deactivate ()
  "Deactivates the ECB and kills all ECB buffers and windows."
  (interactive)
  (ecb-minor-mode 0))

(defun ecb-deactivate-internal ()
  "Deactivates the ECB and kills all ECB buffers and windows."
  (unless (not ecb-minor-mode)
    
    ;; deactivating the adviced functions
    (ecb-activate-adviced-functions nil)
    (ecb-disable-basic-advices)

    (tree-buffer-deactivate-mouse-tracking)
    (tree-buffer-deactivate-follow-mouse)

    (setq compilation-window-height ecb-old-compilation-window-height)
    
    ;; we can safely do the kills because killing non existing buffers
    ;; doesn´t matter.
    (tree-buffer-destroy ecb-directories-buffer-name)
    (tree-buffer-destroy ecb-sources-buffer-name)
    (tree-buffer-destroy ecb-methods-buffer-name)
    (tree-buffer-destroy ecb-history-buffer-name)
    ;; remove the hooks
    (remove-hook 'semantic-after-toplevel-bovinate-hook
		 'ecb-rebuild-methods-buffer-after-parsing)
    (dolist (timer-elem ecb-idle-timer-alist)
      (cancel-timer (cdr timer-elem)))
    (setq ecb-idle-timer-alist nil)
    (dolist (hook ecb-post-command-hooks)
      (remove-hook 'post-command-hook hook))
    (setq ecb-post-command-hooks nil)
    (remove-hook 'pre-command-hook 'ecb-pre-command-hook-function)
    (remove-hook 'after-save-hook 'ecb-update-methods-after-saving)
    ;; ediff-stuff; we operate here only with symbols to avoid bytecompiler
    ;; warnings
    (if (get 'ediff-quit-hook 'ecb-ediff-quit-hook-value)
	(set 'ediff-quit-hook (get 'ediff-quit-hook
				   'ecb-ediff-quit-hook-value))
      (remove-hook 'ediff-quit-hook 'ecb-ediff-quit-hook))

    ;; menus
    (easy-menu-remove ecb-minor-menu)

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

    (setq ecb-minor-mode nil))
  (message "The ECB is now deactivated.")
  ecb-minor-mode)

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
  (force-mode-line-update t)
  ecb-minor-mode)

;; Adding ECB to the minor modes
(if (fboundp 'add-minor-mode)
    ;; Emacs 21 & XEmacs
    (add-minor-mode 'ecb-minor-mode
                    'ecb-minor-mode-text ecb-mode-map)
  ;; Emacs 20.X
  (or (assq 'ecb-minor-mode minor-mode-alist)
      (setq minor-mode-alist
            (cons (list 'ecb-minor-mode 'ecb-minor-mode-text)
                  minor-mode-alist)))
    
  (or (assq 'ecb-minor-mode minor-mode-map-alist)
      (setq minor-mode-map-alist
            (cons (cons 'ecb-minor-mode ecb-mode-map)
                  minor-mode-map-alist))))


(defvar ecb-common-directories-menu nil)
(setq ecb-common-directories-menu
      '(("Create File" ecb-create-file)
	("Create Source" ecb-create-directory-source)
	("Delete Directory" ecb-delete-directory)
	("Create Child Directory" ecb-create-directory)
	("Add Source Path" ecb-add-source-path-node)))

(defvar ecb-directories-menu nil)
(setq ecb-directories-menu
      (append
       ecb-common-directories-menu
       '(("Make This a Source Path" ecb-node-to-source-path))))

(defvar ecb-source-path-menu nil)
(setq ecb-source-path-menu
      (append
       ecb-common-directories-menu
       '(("Delete Source Path" ecb-delete-source-path))))

(defvar ecb-sources-menu nil)
(setq ecb-sources-menu
      '(("Delete File" ecb-delete-source-2)
	("Create File" ecb-create-file-2)
	("Create Source" ecb-create-source-2)))

;; three easy-entry functions for the history menu for conveniance
;; Note: The node argument in the first two functions is not used.
(defun ecb-clear-history-only-not-existing (node)
  "Removes all history entries from the ECB history buffer where related
buffers does not exist anymore."
  (ecb-clear-history -1))

(defun ecb-clear-history-all (node)
  "Removes all history entries from the ECB history buffer."
  (ecb-clear-history 0))

(defun ecb-clear-history-node (node)
  "Removes current entry from the ECB history buffer."
  (save-selected-window
    (ecb-exec-in-history-window
     (let ((buffer-file-name-list (mapcar (lambda (buff)
					    (buffer-file-name buff))
					  (buffer-list))))
       (when (or (not (member (tree-node-get-data node) buffer-file-name-list))
		 (not (equal ecb-clear-history-behavior 'not-existing-buffers)))
	 (ecb-remove-from-current-tree-buffer node)
	 (tree-buffer-update)
	 (tree-buffer-highlight-node-data ecb-path-selected-source))))))

(defvar ecb-history-menu nil)
(setq ecb-history-menu
      '(("Delete File" ecb-delete-source-2)
	("Remove Current Entry" ecb-clear-history-node)
	("Remove All Entries" ecb-clear-history-all)
	("Remove Non Existing Buffer Entries" ecb-clear-history-only-not-existing)))


;; ECB byte-compilation

(defun ecb-compile-file-if-necessary (file &optional force)
  "Compile the ECB-file FILE if necessary. This is done if FORCE is not nil or
FILE.el is newer than FILE.elc or if FILE.elc doesn't exist."
  (let* ((root (file-name-sans-extension file))
	 (elc-file (concat root ".elc")))
    (if (or force
	    (not (file-exists-p elc-file))
	    (file-newer-than-file-p file  elc-file))
	(progn
	  (message (format "Byte-compiling %s..." 
			   (file-name-nondirectory file)))
	  (byte-compile-file file)))))


(defun ecb-byte-compile (&optional force-all)
  "Bytecompiles the ECB package. This is done for all lisp-files of ECB if
FORCE-ALL is not nil or for each lisp-file FILE.el which is either newer than
FILE.elc or if FILE.elc doesn't exist."
  (interactive "P")
  (let ((load-path
	 (append (list (file-name-directory
			(or (locate-library "semantic")
			    (error "Semantic is not in the load-path!")))
		       (file-name-directory (locate-library "ecb")))
		 load-path))
	(files (directory-files (file-name-directory (locate-library "ecb"))
				t)))
    (save-excursion
      (dolist (file files)
	(if (string-match "\\(tree-buffer\\|ecb.*\\)\\.el$" file)
	    (ecb-compile-file-if-necessary file force-all))))))

(provide 'ecb)

;;; ecb.el ends here
