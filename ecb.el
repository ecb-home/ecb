;;; ecb.el --- a code browser

;; Copyright (C) 2000 Jesper Nordenberg

;; Author: Jesper Nordenberg <mayhem@home.se>
;; Maintainer: Jesper Nordenberg <mayhem@home.se>
;; Keywords: java, class, browser
;; Created: Jul 2000
;; Version: 1.10

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; The Emacs code browser (ECB) creates four buffers: *ECB Directories*,
;; *ECB Sources*, *ECB Methods* and *ECB History*. These buffers can be
;; used to navigate through source code with the mouse.
;;
;; To use the Emacs code browser add the ECB files to your load path
;; and add the following line to your .emacs file:
;;
;; (require 'ecb)
;;
;; ECB requires version 1.2.1 or higher of Eric's semantic bovinator
;; (http://www.ultranet.com/~zappo/semantic.shtml).
;; If you are working with Java, ECB works best when the JDE package
;; (http://sunsite.auc.dk/jde) is installed.
;; 
;; ECB is activated by calling:
;;
;; (ecb-activate)
;;
;; After activating ECB you should call `ecb-show-help' to get a detailed
;; description of what ECB offers to you and how to use ECB.
;;
;;
;; TODO:
;; - Fix XEmacs incompatibilities (I need help on this one!)
;; - More layouts
;; - More functions on the pop-up menus. Suggestions are welcome!
;; - Convert the code to EIEIO
;; - Lots more...
;;
;; The latest version of the ECB is available at
;; http://home.swipnet.se/mayhem/ecb.html

;;; Code:

(require 'semantic)
(require 'semantic-el)
(require 'semantic-c)
(require 'tree-buffer)
(require 'ecb-layout)
(require 'ecb-util)
;; (require 'wid-browse)

(require 'assoc) ;; Semantic fix

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
(defvar ecb-methods-root-node nil
  "Path to currently selected source.")

(defvar ecb-activated nil
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

(defcustom ecb-source-path nil
  "*Path where to find code sources."
  :group 'ecb-directories
  :set '(lambda(symbol value)
	  (set symbol (mapcar (lambda (path)
                                (ecb-strip-slash path))
                              value))
	  (if (and ecb-activated
                   (functionp 'ecb-update-directories-buffer))
	      (ecb-update-directories-buffer)))
  :type '(repeat (directory :tag "Path")))

(defcustom ecb-show-sources-in-directories-buffer nil
  "*Show source files in directories buffer."
  :group 'ecb-directories
  :type 'boolean)

(defcustom ecb-directories-buffer-name "*ECB Directories*"
  "*Name of the ECB-directory-buffer which is displayed in the modeline.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB Directories*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object
of the ECB-directory-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating
and then activating ECB again!"
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

(defcustom ecb-excluded-directories-regexp ".*CVS.*"
  "*Specifies directories that should not be included in the directories
list. The value of this variable should be a regular expression."
  :group 'ecb-directories
  :type 'regexp)

(defcustom ecb-auto-expand-directory-tree t
  "*Automatically expand the directory tree to the current source file."
  :group 'ecb-directories
  :type 'boolean)

(defcustom ecb-sources-buffer-name "*ECB Sources*"
  "*Name of the ECB-sources-buffer which is displayed in the modeline.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB Sources*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object
of the ECB-sources-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating
and then activating ECB again!"
  :group 'ecb-sources
  :type 'string)

(defcustom ecb-source-file-regexp "\\(\\(M\\|m\\)akefile\\|^\\.\\(emacs\\|gnus\\)\\|.*\\.\\(java\\|el\\|c\\|cc\\|h\\|hh\\|txt\\|html\\|mk\\|xml\\|dtd\\|texi\\|info\\|bnf\\|cpp\\|hpp\\)\\)$"
  "*Files matching this regular expression will be shown in the source
buffer."
  :group 'ecb-sources
  :type 'regexp)

(defcustom ecb-show-source-file-extension t
  "*Show the file extension of source files."
  :group 'ecb-sources
  :type 'boolean)

(defcustom ecb-history-buffer-name "*ECB History*"
  "*Name of the ECB-history-buffer which is displayed in the modeline.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB History*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object
of the ECB-history-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating
and then activating ECB again!"
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
  "*Name of the ECB-methods-buffer which is displayed in the modeline.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB Methods*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object
of the ECB-methods-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating
and then activating ECB again!"
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

(defcustom ecb-tree-indent 2
  "*Indent size for trees."
  :group 'ecb-general
  :type 'integer)

(defcustom ecb-tree-expand-symbol-before nil
  "*Show the expand symbol before the items in a tree."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-truncate-lines t
  "*Truncate lines in ECB buffers."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-window-sync t
  "*Synchronize ECB with edit window."
  :group 'ecb-general
  :type 'boolean)

(defcustom ecb-show-node-name-in-minibuffer 'always
  "*Show the name of the item under mouse in minibuffer."
  :group 'ecb-general
  :type '(radio (const :tag "Always"
                       :value always)
                (const :tag "If longer than window-width"
                       :value if-too-long)
                (const :tag "Never"
                       :value nil)))

;; Thanks to David Hay for the suggestion <David.Hay@requisite.com>
(defcustom ecb-left-mouse-jump-destination 'left-top
  "*Jump-destination of a left-mouse-button click in an ECB-window,
if you click onto a source or method or variable. Defines in which edit-window
\(if splitted) ECB does the \"right\" action \(opening the source, jumping to
a method/variable). There are two possible choices:
- left-top: Does the \"right\" action alyways in the left/topmost edit-window.
- last-point: Does the \"right\" action alyways in that edit-window which had
  the point before.

If the edit-window is not splitted this setting doesn´t matter.

Note: A click with the middle-mouse-button does the \"right\" action always in
the \"other\" window related to the setting in this option."
  :group 'ecb-general
  :type '(radio (const :tag "Left/topmost edit-window"
                       :value left-top)
                (const :tag "Last edit-window with point"
                       :value last-point)))
  

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
  "*Normal hook run at the end of deactivating the ecb-package by running
`ecb-deactivate'."
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

(defun ecb-highlight-text(orig-text type)
  "If `ecb-font-lock-methods' is not nil then dependend to TYPE the face
specified in `ecb-font-lock-method-faces' is added to TEXT, otherwise TEXT
will get the face 'default. Returns TEXT."
  (let ((text (copy-sequence orig-text)))
    (if (stringp text)
	(if ecb-font-lock-methods
	    (let ((face (or (nth type ecb-font-lock-method-faces) 'default)))
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
				   'face 'font-lock-type-face text)))
	  (put-text-property 0 (length text) 'face 'default text)))
    text))

(defun ecb-get-method-sig(method-token)
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
  
(defun ecb-get-variable-text(var-token)
  (let ((type (semantic-token-type var)))
    (concat (ecb-highlight-text (semantic-token-name var-token) ecb-variablename)
	    (if type
		(concat " : " (ecb-highlight-text type ecb-variabletype))
	      ""))))

(defun ecb-add-classes(node token &optional flatten)
  (let ((children (semantic-find-nonterminal-by-token 'type token)))
    (dolist (type children)
      (let ((n (if (and flatten (= 1 (length children)))
		   node
		 (tree-node-new (ecb-highlight-text (semantic-token-name type)
                                                    ecb-classtype)
                                0
                                type))))
	(unless (and flatten (= 1 (length children)))
	    (tree-node-add-child node n))
	(ecb-add-tokens n (semantic-token-type-parts type))))))
  
(defun ecb-add-methods(node token methods)
  (if ecb-sort-methods
      (setq methods (sort methods (lambda(a b)
				    (string< (semantic-token-name a)
					     (semantic-token-name b))))))
  (dolist (method methods)
    (tree-node-add-child node (tree-node-new
			       (ecb-get-method-sig method) 0
			       method t))))
  
(defun ecb-add-variables(node token variables)
  (when (and ecb-show-variables variables)
    (let ((var-node node))
      (when (eq ecb-show-variables 'collapsed)
	(setq var-node (tree-node-new "[Variables]" 1
				      nil))
	(tree-node-add-child node var-node))
      (if ecb-sort-variables
	  (setq variables (sort variables (lambda(a b)
					    (string< (semantic-token-name a)
						     (semantic-token-name b))))))
      (dolist (var variables)
	(tree-node-add-child var-node (tree-node-new
				       (ecb-get-variable-text var)
				       0 var t))))))
  
(defun ecb-add-tokens(node token &optional flatten)
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

(defun ecb-expand-tree(path node)
  (catch 'exit
    (dolist (child (tree-node-get-children node))
      (let ((name (tree-node-get-name child)))
	(when (and (>= (length path) (length name))
		   (string= (substring path 0 (length name)) name)
		   (or (= (length path) (length name))
		       (eq (elt path (length name)) ?/)))
	  (let ((was-expanded (tree-node-is-expanded child)))
	    (tree-node-set-expanded child t)
	    (ecb-update-directory-node child)
	    (throw 'exit
		   (or (when (> (length path) (length name))
			 (ecb-expand-tree (substring path (1+ (length name)))
					  child))
		       (not was-expanded)))))))))

(defun ecb-set-selected-directory(path)
  (setq path (ecb-strip-slash path))
  (setq ecb-path-selected-directory path)
  
  (when (or (not ecb-show-sources-in-directories-buffer)
	    ecb-auto-expand-directory-tree)
    (save-selected-window
      (when (get-buffer-window ecb-directories-buffer-name)
	(pop-to-buffer ecb-directories-buffer-name)
	(when ecb-auto-expand-directory-tree
	  ;; Expand tree to show selected directory
	  (if (ecb-expand-tree path (tree-buffer-get-root))
	      (tree-buffer-update)))
	(when (not ecb-show-sources-in-directories-buffer)
	  (tree-buffer-highlight-node-data ecb-path-selected-directory)))))

  (ecb-buffer-select ecb-sources-buffer-name)
  (let ((old-children (tree-node-get-children (tree-buffer-get-root))))
    (tree-node-set-children (tree-buffer-get-root) nil)
    (ecb-tree-node-add-files
     (tree-buffer-get-root)
     path
     (directory-files ecb-path-selected-directory nil ecb-source-file-regexp)
     0
     ecb-show-source-file-extension
     old-children t))
  (tree-buffer-update))
			       
(defun ecb-get-source-name(filename)
  "Returns the source name of a file."
  (let ((f (file-name-nondirectory filename)))
    (if ecb-show-source-file-extension
	f
      (file-name-sans-extension f))))
  
(defun ecb-select-source-file(filename)
  "Updates the directories, sources and history buffers to match the filename
given."
  (save-current-buffer
    (ecb-set-selected-directory (file-name-directory filename))
    (setq ecb-path-selected-source filename)
    (let ((node (tree-node-find-child-data (tree-buffer-get-root)
					   ecb-path-selected-source)))
      (save-selected-window
	(when ecb-show-sources-in-directories-buffer
	  (if (get-buffer-window ecb-directories-buffer-name)
	      (pop-to-buffer ecb-directories-buffer-name))
	  (tree-buffer-highlight-node-data ecb-path-selected-source))
	(if (get-buffer-window ecb-sources-buffer-name)
	    (pop-to-buffer ecb-sources-buffer-name))
	(tree-buffer-highlight-node-data ecb-path-selected-source))

      (ecb-buffer-select ecb-history-buffer-name)
      (let ((child (tree-node-find-child-data
		    (tree-buffer-get-root) ecb-path-selected-source)))
	(when child
	    (tree-node-remove-child
	     (tree-buffer-get-root) child))
	(tree-node-set-children
	 (tree-buffer-get-root)
	 (let ((history-items
		(cons
		 (tree-node-new (tree-node-get-name node) 0
				ecb-path-selected-source t
				(tree-buffer-get-root))
		 (tree-node-get-children (tree-buffer-get-root)))))
	   (if ecb-sort-history-items
	       (sort history-items
		     (function (lambda (l r) (string< (tree-node-get-name l)
						      (tree-node-get-name r)))))
	     history-items)))
	(setq tree-buffer-highlighted-node-data ecb-path-selected-source)
	(tree-buffer-update)))))

(defun ecb-update-methods-after-saving ()
  "Updates the methods-buffer after saving if this option is turned on and if
current-buffer is saved."
  (if (and ecb-auto-update-methods-after-save
           ecb-last-edit-window-with-point
           ;; this prevents updating the method buffer after saving a not
           ;; current buffer (e.g. with `save-some-buffers'), because this
           ;; would result in displaying a method-buffer not belonging to the
           ;; current source-buffer.
           (eq (current-buffer)
               (window-buffer ecb-last-edit-window-with-point)))
      (ecb-update-methods-buffer)))

(defun ecb-update-methods-buffer()
  "Updates the methods buffer with the current buffer."
  (tree-node-set-children ecb-methods-root-node nil)
  ;;  (print (semantic-bovinate-toplevel t))
  (ecb-add-tokens ecb-methods-root-node
		  (condition-case nil
		      ;; semantic <= 1.2.1
		      (semantic-bovinate-toplevel 0 nil t)
		    (wrong-number-of-arguments
		     ;; semantic >= 1.3.1
		     (semantic-bovinate-toplevel t)))
		  t)
  (save-selected-window
    ;; also the whole buffer informations should be preserved!
    (save-excursion
      (ecb-buffer-select ecb-methods-buffer-name)
      (setq tree-buffer-indent ecb-tree-indent)
      (tree-buffer-update))))
;;     (set-window-point (selected-window) 1)))

(defun ecb-set-selected-source(filename &optional window-skips
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
      (save-selected-window
        (save-excursion
          (set-buffer (find-file-noselect ecb-path-selected-source))
          (ecb-update-methods-buffer)))
    ;; open the selected source in the edit-window and do all the update and
    ;; parsing stuff with this buffer
    (ecb-find-file-and-display ecb-path-selected-source
                                   window-skips)
    (ecb-update-methods-buffer)))

(defun ecb-select-method(method-start)
  (setq ecb-selected-method-start method-start)
  (when (get-buffer-window ecb-methods-buffer-name)
    (save-excursion
      (ecb-buffer-select ecb-methods-buffer-name)
      (tree-buffer-highlight-node-data ecb-selected-method-start))))


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
  (ecb-buffer-select ecb-history-buffer-name)
  (let ((buffer-file-name-list (mapcar (lambda (buff)
                                         (buffer-file-name buff))
                                       (buffer-list)))
        (tree-childs (tree-node-get-children (tree-buffer-get-root)))
        (clear-behavior (or (if (and clearall (integerp clearall))
                                (cond ((= clearall 0) 'all)
                                      ((< clearall 0) 'not-existing-buffers)
                                      (t 'existing-buffers)))
                            ecb-clear-history-behavior))
        child-data child)
    (while tree-childs
      (setq child-data (tree-node-get-data (car tree-childs)))
      (if (or (eq clear-behavior 'all)
              (and (eq clear-behavior 'not-existing-buffers)
                   (not (member child-data buffer-file-name-list)))
              (and (eq clear-behavior 'existing-buffers)
                   (member child-data buffer-file-name-list)))
          (ecb-remove-from-current-tree-buffer (car tree-childs)))
      (setq tree-childs (cdr tree-childs))))
  (tree-buffer-update))

(defun ecb-current-buffer-sync(&optional opt-buffer)
  "Synchronizes the ECB buffers with the current buffer."
  (interactive)
  ;;(message (prin1-to-string this-command))
  (let ((filename (buffer-file-name (if opt-buffer opt-buffer (current-buffer)))))
    (when (and filename (not (string= filename ecb-path-selected-source)))
      ;; KB: seems this little sleep is necessary because otherwise jumping to
      ;; certain markers in new opened files (e.g. with next-error etc. )
      ;; doesn´t work correct. Can´t debug down this mysterious thing!
      ;; Regardless of the size of the file to load, this 0.1 fraction of a
      ;; sec is enough!
      (sit-for 0.1)
      (ecb-select-source-file filename)
      (ecb-update-methods-buffer))))

(defun ecb-find-file-and-display(filename &optional window-skips)
  "Finds the file in the correct window. What the correct window is depends on
the setting in `ecb-left-mouse-jump-destination'."
  (if (eq ecb-left-mouse-jump-destination 'left-top)
      (select-window ecb-edit-window)
    (select-window ecb-last-edit-window-with-point))
  (ecb-with-adviced-functions
   (if window-skips
       (let ((ecb-other-window-jump-behavior 'only-edit))
         (other-window window-skips))))
  (ecb-with-original-functions
   (find-file ecb-path-selected-source)
   (pop-to-buffer (buffer-name))))

(defun ecb-switch-to-edit-buffer()
  (select-window ecb-edit-window))
  
(defun ecb-get-directories(path)
  (let ((files (directory-files path nil "^[^.].*"))
	dirs)
    (dolist (file files dirs)
      (if (and (file-accessible-directory-p (concat path "/" file))
	       (not (string-match ecb-excluded-directories-regexp file)))
	  (setq dirs (list-append dirs (list file)))))))

(defun ecb-tree-node-add-files
  (node path files type include-extension old-children &optional not-expandable)
  (dolist (file files)
    (let ((filename (concat path "/" file))
	  child)
      (tree-node-add-child
       node
       (ecb-new-child
	old-children
	(if include-extension
	    file
	  (file-name-sans-extension file))
	type filename (or not-expandable (= type 1)))))))
  
(defun ecb-update-directory-node(node)
  (let ((old-children (tree-node-get-children node))
	(path (tree-node-get-data node)))
    (tree-node-set-children node nil)
    (if (file-accessible-directory-p path)
	(let ((files (directory-files path nil "^[^.].*"))
	      dirs normal-files)
	  (dolist (file files)
	    (let ((filename (concat path "/" file)))
	      (if (file-accessible-directory-p filename)
		  (if (not (string-match ecb-excluded-directories-regexp file))
		      (setq dirs (list-append dirs (list file))))
		(if (string-match ecb-source-file-regexp file)
		    (setq normal-files (list-append normal-files (list file)))))))
	  (ecb-tree-node-add-files node path dirs 0 t old-children)
	  (if ecb-show-sources-in-directories-buffer
	      (ecb-tree-node-add-files node path normal-files 1
					   ecb-show-source-file-extension
					   old-children))
	  (tree-node-set-expandable node (or (tree-node-get-children node)))))))

(defun ecb-update-directories-buffer()
  "Updates the ECB directories buffer."
  (interactive)
  (save-current-buffer
    (ecb-buffer-select ecb-directories-buffer-name)
;;     (setq tree-buffer-type-faces
;; 	  (list (cons 1 ecb-source-in-directories-buffer-face)))
    (setq tree-buffer-indent ecb-tree-indent)
    (let* ((node (tree-buffer-get-root))
	   (old-children (tree-node-get-children node)))
      (tree-node-set-children node nil)
      (if ecb-source-path
	  (progn
	    (dolist (dir ecb-source-path)
	      (tree-node-add-child node (ecb-new-child old-children dir 0 dir)))
	    (tree-buffer-update))
	(progn
	  (erase-buffer)
	  (insert "No source paths set.\nPress F2 to customize\nTo get help, call\necb-show-help."))))))

(defun ecb-new-child(old-children name type data &optional not-expandable)
  (catch 'exit
    (dolist (child old-children)
      (when (and (equal (tree-node-get-data child) data)
		 (= (tree-node-get-type child) type))
	(tree-node-set-name child name)
	(if not-expandable
	    (tree-node-set-expandable child nil))
	(throw 'exit child)))
    (tree-node-new name type data not-expandable)))

(defun ecb-buffer-select(name)
  (set-buffer (get-buffer name)))

;;====================================================
;; Mouse functions
;;====================================================

(defun ecb-directory-clicked(node mouse-button shift-pressed)
  (ecb-update-directory-node node)
  (if (= 0 (tree-node-get-type node))
      (if shift-pressed
          (ecb-mouse-over-node node)
        (progn
          (when (= 1 mouse-button)
            (tree-node-toggle-expanded node))
          (ecb-set-selected-directory (tree-node-get-data node))
          (ecb-buffer-select ecb-directories-buffer-name)
          (tree-buffer-update)))
    (ecb-set-selected-source (tree-node-get-data node)
                             (if ecb-split-edit-window
                                 mouse-button 0)
                             shift-pressed)))

(defun ecb-source-clicked(node mouse-button shift-pressed)
  (if shift-pressed
      (ecb-mouse-over-node node))
  (ecb-set-selected-source (tree-node-get-data node)
                           (if ecb-split-edit-window
                               mouse-button 0)
                           shift-pressed))

(defun ecb-method-clicked(node mouse-button shift-pressed)
  (if shift-pressed
      (ecb-mouse-over-node node)
    (when (= 1 (tree-node-get-type node))
      (tree-node-toggle-expanded node)
      (tree-buffer-update))
    (when (tree-node-get-data node)
      (ecb-find-file-and-display ecb-path-selected-source
                                 (if ecb-split-edit-window
                                     mouse-button 0))
      (goto-char (semantic-token-start (tree-node-get-data node))))))

(defun ecb-mouse-over-node(node)
  (cond ((eq ecb-show-node-name-in-minibuffer 'always)
         (message "%s" (tree-node-get-name node)))
        ((eq ecb-show-node-name-in-minibuffer 'if-too-long)
         (if (>= (+ (length (tree-node-get-name node))
                    (tree-buffer-get-node-indent node))
                 (window-width))
             (message "%s" (tree-node-get-name node))
           ;; we must delete here the old message so no wrong info is
           ;; displayed.
           (message nil)))))

(defun ecb-mouse-over-method-node(node)
  (if ecb-show-node-name-in-minibuffer
      (when (tree-node-get-data node)
	(let ((doc (semantic-token-docstring (tree-node-get-data node))))
	  (when (stringp doc)
	    (message doc))))))

;; ECB help stuff, stolen something from recentf.el

(defconst ecb-help-message
"
                              ===================
                              General description
                              ===================      

ECB offers a few ECB-windows for browsing your sources comfortable with the
mouse. There are currently three different types of ECB-windows:

1. ECB Directories:

- Select directories and, if enabled, source files, in the \"*ECB Directories*\"
  buffer by clicking the left mouse button on the directory name or by hitting
  ENTER/RETURN when the cursor is placed on the item line.

- Directory names with a \"[+]\" symbol after \(or before) them can be
  expanded/collapsed by left-clicking on the symbol, pressing the TAB key when
  the cursor is placed on the package line or clicking the middle mouse button
  on the item.

- Right clicking on an item will open a popup menu where different operations
  on the item under the mouse cursor can be performed.

- Pressing F1 in the packages buffer will update it. Pressing F2 will open the
  ECB customization group in the edit window ECB Sources:

2. ECB Sources:

- Source files can be select by clicking the left mouse button or hitting
  ENTER/RETURN on the source row in the \"*ECB Sources*\" or \"*ECB History*\"
  windows.

  IMPORTANT: If you hold down the SHIFT-key while clicking with the left mouse
  button on a source row in the \"*ECB Sources*\" or \"*ECB History*\" windows
  then the source will not be displayed in the edit-window but it will be
  scanned in the background and all it´s methods and variables are listed in
  the \"ECB Methods\" window. So you can get an overlook over the source
  without changing the buffer in the edit-window.

- Clicking on the source file with the middle mouse button will open the class
  file in the other edit window.

- Right clicking on a source file will open a popup menu where different
  operation on the item under the mouse cursor can be performed.

3. ECB Methods:

- The \"*ECB Methods*\" buffer contains the methods \(and variables, if you
  want) in the selected source file. When a method/variable is selected with
  the left mouse button or ENTER/RETURN the edit buffer will jump to the
  method/variable.

- Clicking on a method/variable with the middle mouse button will jump to the
  method in the other edit window.

In addition to these ECB-windows you have always one or two edit-windows in
the ECB-frame and \(if you want) at the bottom a compilation-window, where all
the output of Emacs-compilation \(compile, grep etc.) is shown.


                          ===========================
                          Activation and deactivation
                          ===========================

Call M-x `ecb-activate' and M-x `ecb-deactivate' to activate or deactivate
ECB.


                                 ============
                                 Usage of ECB
                                 ============                    

Working with the mouse in the ECB-buffers:
------------------------------------------

Normally you get best usage if you use ECB with a mouse.

- Left-button: Opens the source/jumps to method/variable in the edit-window.
               If the edit-window is splitted in two edit-windows then you can
               choose in which of the edit-windows ECB jumps if you click with
               the left button: See `ecb-left-mouse-jump-destination'!

- Middle-button: Like left-button but do this in the \"other\" edit-window if
                 the edit-window is splitted, otherwise exactly like
                 left-button.

If you hold down shift-key while you click with left- or middle-button the
item under mouse-point is displayed in the echo-area. This is useful if you
have longer items than the window-width of an ECB-window and truncated lines
so you can read the whole item.
IMPORTANT: Doing this in the \"*ECB Sources*\" or \"*ECB History*\" windows
does not only show the node in the echo area but it also opens the clicked
source only in the background and shows all its methods/variables in \"ECB
Methods\"; the buffer of the edit-window is not changed!

- Right-button: Opens a special context popup-menu for the clicked item where
                you can choose several senseful actions.


Working with the edit-window of ECB:
------------------------------------

ECB offers you all what you need to work with the edit-window as if the
edit-window would be the only window of the ECB-frame.

ECB offers you to advice the following functions so they work best with ECB
- `other-window'
- `delete-window'
- `delete-other-windows'
- `split-window-horizontally'
- `split-window-vertically'
- `find-file-other-window'
- `switch-to-buffer-other-window'

The behavior of the adviced functions is:
- All these adviced functions behaves exactly like their corresponding
  original functons but they always act as if the edit-window\(s) of ECB would
  be the only window\(s) of the ECB-frame. So the edit-window\(s) of ECB seems
  to be a normal Emacs-frame to the user.
- If called in a not edit-window of ECB all these function jumps first to the
  \(first) edit-window, so you can never destroy the ECB-window layout
  unintentionally.

**Attention**:
If you want to work within the edit-window with splitting and unsplitting the
edit-window\(s) it is highly recommended to use the adviced-functions of ECB
instead of the original Emacs-functions \(see above). For example the adviced
`other-window' can only work correct if you split the edit window with the
adviced `split-window-vertically' \(or horizontally) and NOT with the original
`split-window-vertically'!

Per default ECB advices all the functions mentioned above but with the option
`ecb-advice-window-functions' you can customizes which functions should be
adviced by ECB.


Working with or without a compile window:
-----------------------------------------

With the option `ecb-compile-window-height' you can define if the ECB layout
should contain per default a compilation-window at the bottom \(and if yes the
height of it). If yes ECB displays all output of compilation-mode \(compile,
grep etc.) in this special window. If not ECB splits the edit-window \(or uses
the \"other\" edit-window if already splitted) vertically and displays the
compilation-output there.
Same for displaying help-buffers or similar stuff.

With the option `ecb-compile-window-temporally-enlarge' you can allow Emacs to
enlarge temporally the ECB-compile-window after finishing compilation-output.

Know Bug: The setting in `ecb-compile-window-height' works correct for all
compilation-output of Emacs (compile, grep etc.) but for some other output
like help-buffers etc. Emacs enlarges the height of the compile-window for
it´s output. Currently ECB can´t restore auto. the height of the
compile-window for such outputs. But you can always restore the correct layout
by calling `ecb-redraw-layout'!.


Redrawing the ECB-layout:
-------------------------

If you have unintenionally destroyed the ECB-layout, you can always restore the
layout with calling `ecb-redraw-layout'.


Available interactive ECB commands:
-----------------------------------

- `ecb-activate'
- `ecb-deactivate'
- `ecb-update-directories-buffer' (normally not needed)
- `ecb-current-buffer-sync' (normally not needed)
- `ecb-redraw-layout'
- `ecb-clear-history'
- `ecb-show-help'


                             ====================
                             Customization of ECB
                             ====================

All customization of ECB is divided into the following customize groups:
- ecb-general: General customization of ECB
- ecb-directories: Customization of the ECB-directories buffer.
- ecb-sources: Customization of the ECB-sources buffer.
- ecb-methods: Customization of the ECB-methods buffer.
- ecb-history: Customization of the ECB-history buffer.
- ecb-layout: Customization of the layout of ECB.

You can highly customize all the ECB behavior/layout so just go to this groups
and you will see all well documented ECB-options.

But you must always customize the option `ecb-source-path'!
Maybe you should also check all the options in the customize group
'ecb-layout' before you begin to work with ECB.

Available hooks:
- `ecb-activate-before-layout-draw-hook'
- `ecb-activate-hook'
- `ecb-deactivate-hook'
Look at the documentation of these hooks to get description.")

(defconst ecb-help-buffer-name "*ECB help*")

(defvar ecb-buffer-before-help nil)

(defun ecb-cancel-dialog (&rest ignore)
  "Cancel the ECB dialog."
  (interactive)
  (kill-buffer (current-buffer))
  (if ecb-buffer-before-help
      (switch-to-buffer ecb-buffer-before-help t))
  (setq ecb-buffer-before-help nil)
  (message "ECB dialog canceled."))

(defvar ecb-dialog-mode-map nil
  "`ecb-dialog-mode' keymap.")

(if ecb-dialog-mode-map
    ()
  (setq ecb-dialog-mode-map (make-sparse-keymap))
  (define-key ecb-dialog-mode-map "q" 'ecb-cancel-dialog)
  (define-key ecb-dialog-mode-map (kbd "C-x k") 'ecb-cancel-dialog)
;;   (define-key ecb-dialog-mode-map [down-mouse-1] 'widget-button-click)
  (set-keymap-parent ecb-dialog-mode-map help-mode-map))

(defun ecb-dialog-mode ()
  "Major mode used to display the ECB-help

These are the special commands of `ecb-dialog-mode' mode:
    q -- cancel this dialog."
  (interactive)
  (setq major-mode 'ecb-dialog-mode)
  (setq mode-name "ecb-dialog")
  (use-local-map ecb-dialog-mode-map))

(defun ecb-show-help ()
  "Shows the online help of ECB."
  (interactive)
  (if (not (ecb-point-in-edit-window))
      (ecb-other-window))
  (if (get-buffer ecb-help-buffer-name)
      (switch-to-buffer ecb-help-buffer-name t)
    (if (not ecb-buffer-before-help)
        (setq ecb-buffer-before-help (current-buffer)))
    (with-current-buffer (get-buffer-create ecb-help-buffer-name)
      (switch-to-buffer (current-buffer) t)
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (let ((all (overlay-lists)))
        ;; Delete all the overlays.
        (mapcar 'delete-overlay (car all))
        (mapcar 'delete-overlay (cdr all)))
      ;; Insert the dialog header
      (insert "Type \"q\" to quit.\n")
      (insert ecb-help-message)
      (insert "\n\n")
      (make-variable-buffer-local 'buffer-read-only)
      (setq buffer-read-only t)
;;     (widget-insert "Click on Cancel or type \"q\" to quit.\n")
;;     (widget-insert "\n")
;;     (widget-insert ecb-help-message)
;;     (widget-insert "\n\n")
;;     ;; Insert the Cancel button
;;       (widget-create 'push-button
;;                      :notify 'ecb-cancel-dialog
;;                      "Cancel")
      (ecb-dialog-mode)
      (goto-char (point-min))
      (help-make-xrefs)
;;       (widget-setup)
      (goto-char (point-min)))))

;;====================================================
;; Create buffers & menus
;;====================================================

(defun ecb-activate ()
  "Activates the ECB and creates all the buffers and draws the ECB-screen
with the actually choosen layout \(see `ecb-layout-nr')."
  (interactive)
  (if ecb-activated
      (ecb-redraw-layout)
    (let ((curr-buffer-list (mapcar (lambda (buff)
                                      (buffer-name buff))
                                    (buffer-list))))
      ;; create all the ECB-buffers if they don´t already exist
      (unless (member ecb-directories-buffer-name curr-buffer-list)
        (tree-buffer-create
         ecb-directories-buffer-name
         'ecb-directory-clicked
         'ecb-update-directory-node
         'ecb-mouse-over-node
         (list (cons 0 ecb-directories-menu) (cons 1 ecb-sources-menu))
         ecb-truncate-lines
         (list (cons 1 ecb-source-in-directories-buffer-face))
         ecb-tree-expand-symbol-before)
        ;; if we want some keys only defined in a certain tree-buffer we
        ;; must do this directly after calling the tree-buffer-create
        ;; function because this function makes the tree-buffer-key-map
        ;; variable buffer-local for its tree-buffer and creates the sparse
        ;; keymap.
        (define-key tree-buffer-key-map [f1] 'ecb-update-directories-buffer)
        (define-key tree-buffer-key-map [f2]
          '(lambda()
             (interactive)
             (ecb-switch-to-edit-buffer)
             (customize-group 'ecb))))        
      
      (unless (member ecb-sources-buffer-name curr-buffer-list)
        (tree-buffer-create
         ecb-sources-buffer-name
         'ecb-source-clicked
         'ecb-source-clicked
         'ecb-mouse-over-node
         (list (cons 0 ecb-sources-menu))
         ecb-truncate-lines))
      
      (unless (member ecb-methods-buffer-name curr-buffer-list)
        (tree-buffer-create
         ecb-methods-buffer-name
         'ecb-method-clicked
         nil
         'ecb-mouse-over-node
         nil
         ecb-truncate-lines
         (list (cons 0 t))
         ecb-tree-expand-symbol-before)
        (setq ecb-methods-root-node (tree-buffer-get-root)))
      
      (unless (member ecb-history-buffer-name curr-buffer-list)
        (tree-buffer-create
         ecb-history-buffer-name
         'ecb-source-clicked
         'ecb-source-clicked
         'ecb-mouse-over-node
         (list (cons 0 ecb-history-menu))
         ecb-truncate-lines)))
    
    ;; we need some hooks
    (remove-hook 'post-command-hook 'ecb-hook)
    (add-hook 'post-command-hook 'ecb-hook)
    (add-hook 'pre-command-hook 'ecb-pre-command-hook-function)
    (add-hook 'after-save-hook 'ecb-update-methods-after-saving)
    ;; we add a function to this hook at the end because this function should
    ;; be called at the end of all hook-functions of this hook!
    (add-hook 'compilation-finish-functions
              'ecb-layout-return-from-compilation t)
    (add-hook 'compilation-mode-hook
              'ecb-layout-go-to-compile-window)
    (add-hook 'compilation-mode-hook
              'ecb-set-edit-window-split-hook-function)
    (add-hook 'help-mode-hook
              'ecb-set-edit-window-split-hook-function)
    (setq ecb-activated t)
    ;; we must update the directories buffer first time
    (ecb-update-directories-buffer)
    
    ;; run personal hooks before drawing the layout
    (run-hooks 'ecb-activate-before-layout-draw-hook)
    ;; now we draw the layout choosen in `ecb-layout'. This function
    ;; acivates at its end also the adviced functions if necessary!
    (ecb-redraw-layout)
    ;; at the real end we run any personal hooks
    (run-hooks 'ecb-activate-hook)
    (message "The ECB is now activated.")))

(defun ecb-deactivate ()
  "Deactivates the ECB and kills all ECB buffers and windows."
  (interactive)
  (unless (not ecb-activated)
    ;; deactivating the adviced functions
    (ecb-activate-adviced-functions nil)

    ;; restore the old compilation-window-height
    (setq compilation-window-height ecb-old-compilation-window-height)

    (if ecb-edit-window
	(ecb-switch-to-edit-buffer))   
    ;; first we delete all ECB-windows.
    (delete-other-windows)
    ;; we can safely do the kills because killing non existing buffers
    ;; doesn´t matter.
    (kill-buffer ecb-directories-buffer-name)
    (kill-buffer ecb-sources-buffer-name)
    (kill-buffer ecb-methods-buffer-name)
    (kill-buffer ecb-history-buffer-name)
    ;; remove the hooks
    (remove-hook 'post-command-hook 'ecb-hook)
    (remove-hook 'pre-command-hook 'ecb-pre-command-hook-function)
    (remove-hook 'after-save-hook 'ecb-update-methods-after-saving)
    (remove-hook 'compilation-finish-functions
                 'ecb-layout-return-from-compilation)
    (remove-hook 'compilation-mode-hook
                 'ecb-layout-go-to-compile-window)
    (remove-hook 'compilation-mode-hook
                 'ecb-set-edit-window-split-hook-function)
    (remove-hook 'help-mode-hook
                 'ecb-set-edit-window-split-hook-function)
    (setq ecb-activated nil)
    ;; run any personal hooks
    (run-hooks 'ecb-deactivate-hook))
  (message "The ECB is now deactivated."))

(defvar ecb-directories-menu nil)
(setq ecb-directories-menu (make-sparse-keymap "Directory Menu"))
(define-key ecb-directories-menu [ecb-create-file] '("Create File" . t))
(define-key ecb-directories-menu [ecb-create-directory-source]
  '("Create Source" . t))
(define-key ecb-directories-menu [ecb-delete-directory]
  '("Delete Directory" . t))
(define-key ecb-directories-menu [ecb-create-directory]
  '("Create Child Directory" . t))

(defvar ecb-sources-menu nil)
(setq ecb-sources-menu (make-sparse-keymap "Source Menu"))
(define-key ecb-sources-menu [ecb-delete-source-2] '("Delete Source" . t))
(define-key ecb-sources-menu [ecb-create-file-2] '("Create File" . t))
(define-key ecb-sources-menu [ecb-create-source-2] '("Create Source" . t))

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
  (ecb-buffer-select ecb-history-buffer-name)
  (ecb-remove-from-current-tree-buffer node)
  (tree-buffer-update))

(defvar ecb-history-menu nil)
(setq ecb-history-menu (make-sparse-keymap "History Menu"))
(define-key ecb-history-menu [ecb-delete-source-2] '("Delete Source" . t))
(define-key ecb-history-menu [ecb-clear-history-node]
  '("Remove current entry" . t))
(define-key ecb-history-menu [ecb-clear-history-all]
  '("Remove all entries" . t))
(define-key ecb-history-menu [ecb-clear-history-only-not-existing]
  '("Remove not existing buffer-entries" . t))

(defun ecb-hook()
  (if (and ecb-window-sync (eq (selected-frame) ecb-frame))
      (condition-case nil
	  (ecb-current-buffer-sync)
	(error nil))))

(provide 'ecb)

;;; ecb.el ends here
