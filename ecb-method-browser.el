;;; ecb-method-browser.el --- the method-browser of Emacs

;; Copyright (C) 2000 - 2003 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
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

;; $Id: ecb-method-browser.el,v 1.1 2003/10/18 18:08:19 berndl Exp $

;;; Commentary:

;; This file contains the code for the method-browser of ECB

(require 'tree-buffer)
(require 'ecb-util)
(require 'ecb-layout)
(require 'ecb-mode-line)
(require 'ecb-navigate)
(require 'ecb-face)
(require 'ecb-speedbar)

(require 'semantic-load)

;; various loads
(require 'assoc)

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(eval-when-compile
  (require 'silentcomp))

(silentcomp-defun semanticdb-minor-mode-p)
(silentcomp-defun semanticdb-find-nonterminal-by-name)
(silentcomp-defun semanticdb-full-filename)
(silentcomp-defun hs-minor-mode)
(silentcomp-defun hs-show-block)
(silentcomp-defun hs-hide-block)
(silentcomp-defvar hs-minor-mode)
(silentcomp-defvar hs-block-start-regexp)
(silentcomp-defvar imenu--index-alist)

(silentcomp-defun ecb-get-tags-for-non-semantic-files)
(silentcomp-defun ecb-create-non-semantic-tree)

(defvar ecb-selected-token nil
  "The currently selected Semantic token.")
(make-variable-buffer-local 'ecb-selected-token)

(defvar ecb-methods-root-node nil
  "Path to currently selected source.")

(defun ecb-method-browser-initialize ()
  (setq ecb-selected-token nil)
  (setq ecb-methods-root-node nil))

;;====================================================
;; Customization
;;====================================================

(defgroup ecb-methods nil
  "Settings for the methods-buffer in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")


(defgroup ecb-non-semantic nil
  "Settings for parsing and displaying non-semantic files."
  :group 'ecb
  :prefix "ecb-")


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
                         :value (ignore "")
                         (const :tag "Separator" :value ("---"))
                         (list :tag "Menu-command"
                               (function :tag "Function" :value ignore)
                               (string :tag "Entry-name"))
                         (cons :tag "Submenu"
                               (string :tag "Submenu-title")
                               (repeat (choice :tag "Submenu-entry" :menu-tag "Submenu-entry"
                                               :value (ignore "")
                                               (const :tag "Separator" :value ("---"))
                                               (list :tag "Submenu-command"
                                                     (function :tag "Function"
                                                               :value ignore)
                                                     (string :tag "Entry-name"))))))))


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


(defcustom ecb-methods-buffer-after-create-hook nil
  "*Local hook running after the creation of the methods-buffer.
Every function of this hook is called once without arguments direct after
creating the methods-buffer of ECB and it's local key-map. So for example a
function could be added which performs calls of `local-set-key' to define new
key-bindings only for the methods-buffer of ECB."
  :group 'ecb-methods
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


(defmacro ecb-exec-in-methods-window (&rest body)
  `(unwind-protect
       (when (ecb-window-select ecb-methods-buffer-name)
	 ,@body)
     ))


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
  (let ((display (ecb-find-assoc token-type ecb-show-tokens)))
    (if display
	display
      (setq display (ecb-find-assoc t ecb-show-tokens))
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


(defun ecb-semantic-active-for-file (filename)
  "Return not nil if FILENAME is already displayed in a buffer and if semantic
is active for this buffer."
  (and (get-file-buffer filename)
       (save-excursion
         (set-buffer (get-file-buffer filename))
         (semantic-active-p))))


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


(defvar ecb-method-buffer-needs-rebuild t
  "This variable is only set and evaluated by the functions
`ecb-update-methods-buffer--internal' and
`ecb-rebuild-methods-buffer-with-tokencache'!")


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
  ;; The most important function for (re)building the Method-buffer
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
      (unless (or (and no-update-semantic cached-tree) ;; for semantic-sources
                  (and (not (semantic-active-p)) ;; for non-semantic-sources
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


(defun ecb-find-file-and-display (filename other-edit-window)
  "Finds the file in the correct window. What the correct window is depends on
the setting in `ecb-primary-mouse-jump-destination' and the value of
OTHER-EDIT-WINDOW."
  (select-window (ecb-get-edit-window other-edit-window))
  (ecb-nav-save-current)
  (ecb-with-original-functions
   (find-file filename))
  (ecb-nav-add-item (ecb-nav-file-history-item-new)))


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
       (let ( ;; normalizing the elements of `ecb-methods-nodes-expand-spec'
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


(defvar ecb-unhighlight-hook-called nil
  "This mechanism is necessary because tree-buffer creates for mouse releasing a
new nop-command \(otherwise the cursor jumps back to the tree-buffer).")


(defun ecb-unhighlight-token-header ()
  (let ((key (tree-buffer-event-to-key last-input-event)))
    (when (not (or (and (equal key 'mouse-release)
                        (not ecb-unhighlight-hook-called))
                   (equal key 'mouse-movement)))
      (delete-overlay ecb-method-overlay)
      (remove-hook 'pre-command-hook 'ecb-unhighlight-token-header)))
  (setq ecb-unhighlight-hook-called t))


(defun ecb-token-visit-highlight-token-header (token)
  "Highlights line where `ecb-token-visit-smart-token-start' puts point for
TOKEN. Returns current point"
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

;;; popup-menu stuff for the methods-buffer

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

(defun ecb-methods-menu-jump-and-narrow (node)
  (ecb-method-clicked node 1 nil t '(ecb-token-visit-narrow-token
                                     ecb-token-visit-highlight-token-header)))


(defun ecb-methods-menu-widen (node)
  (ecb-select-edit-window)
  (widen)
  (setq ecb-buffer-narrowed-by-ecb nil))


(if (not ecb-running-xemacs)
    ;; Klaus Berndl <klaus.berndl@sdm.de>: This is for silencing the
    ;; byte-compiler. Normally there should be no warning when
    ;; silentcomp-defun is used for hs-minor-mode but....argghhh.
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
      '( ;;("---")
        ("Expand/Collapse"
         (ecb-methods-menu-collapse-all "Collapse all")
         (ecb-methods-menu-expand-0 "Expand level 0")
         (ecb-methods-menu-expand-1 "Expand level 1")
         (ecb-methods-menu-expand-2 "Expand level 2")
         (ecb-methods-menu-expand-all "Expand all"))
        ("---")
        (ecb-maximize-ecb-window-menu-wrapper "Maximize window")))


(defvar ecb-methods-token-menu nil)
(setq ecb-methods-token-menu
      (append '(("Hide/Show"
                 (ecb-methods-menu-hide-block "Jump to token and hide block")
                 (ecb-methods-menu-show-block "Jump to token and show block"))
                ("Narrow/Widen"
                 (ecb-methods-menu-jump-and-narrow "Jump to token and narrow")
                 (ecb-methods-menu-widen "Undo narrowing of edit-window")))
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


(silentcomp-provide 'ecb-method-browser)

;;; ecb-method-browser.el end here
