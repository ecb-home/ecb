;;; ecb-analyse.el --- ECB analysis display interactor

;;; Copyright (C) 2004 Klaus Berndl,

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: browser, code, programming, analyse
;; Created: 2004

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

;; $Id: ecb-analyse.el,v 1.1 2004/12/20 17:15:16 berndl Exp $


;;; Commentary:
;;
;; Displays the analysing informations of semantic-analyse in a special
;; tree-buffer.
;;

;;; Code:

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: we must set this error-save when
;; run with semantic 1.X?
(require 'semantic-analyze)
(require 'ecb-common-browser)
(require 'ecb-method-browser)

(eval-when-compile
  (require 'silentcomp))

(defgroup ecb-analyse nil
  "Settings for the analyse-buffer in the Emacs code browser."
  :group 'ecb
  :prefix "ecb-")


(defcustom ecb-analyse-buffer-name " *ECB Analyse*"
  "*Name of the ECB analyse buffer.
Because it is not a normal buffer for editing you should enclose the name with
stars, e.g. \"*ECB Analyse*\".

If it is necessary for you you can get emacs-lisp access to the buffer-object of
the ECB-analyse-buffer by this name, e.g. by a call of `set-buffer'.

Changes for this option at runtime will take affect only after deactivating and
then activating ECB again!"
  :group 'ecb-analyse
  :type 'string)

(defcustom ecb-analyse-buffer-after-create-hook nil
  "*Local hook running after the creation of the analyse-buffer.
Every function of this hook is called once without arguments direct after
creating the analyse-buffer of ECB and it's local key-map. So for example a
function could be added which performs calls of `local-set-key' to define new
key-bindings only for the analyse-buffer of ECB."
  :group 'ecb-analyse
  :type 'hook)

(defcustom ecb-analyse-show-node-info '(if-too-long . name)
  "*When to display which node-info in the history-buffer.
Define which node info should displayed after moving the mouse over a node
\(or after a shift click onto the node) in the history-buffer.

You can define \"when\" a node-info should be displayed:
See `ecb-directories-show-node-info' for the possible choices.

You can define what info should be displayed:
- name: The full name of the node
- full-info: All infos available to a node.

Do NOT set this option directly via setq but use always customize!"
  :group 'ecb-analyse
  :type '(cons (choice :tag "When"
                       (const :tag "Always" :value always)
                       (const :tag "If too long" :value if-too-long)
                       (const :tag "After shift click" :value shift-click)
                       (const :tag "Never" :value never))
               (choice :tag "What"
                       (const :tag "Node-name" :value name)
                       (const :tag "Full info" :value full-info))))


(defcustom ecb-analyse-collapsed-buckets nil
  "*Buckets collapsed when displaying the current semantic analysis.
The semantic analyse-modul offers several categories of analysis which are
called buckets here. These are for example:

Context: The current context, which is the current function/method, variable,
class etc. \(what exactly depends on the programming language) point is in.
This means not the current function/method/variable/class-name point stand on
but the current surrounding context. Example: If point stays somewhere within
a defun-definition in emacs-lisp or within a java-method then this defun rsp.
method is the context.

Arguments: The arguments of the context if the context is a function/method.

Local Variables: All accessible and bound local variables visible at current
point.

Prefix: The currently parsed prefix, which is mostly the current identifier
point stands on.

Assignee: See the semantic manual

Function: Current function-name point stands on.

Argument #: When point is located within a function-call then this is the
number of the argument point stands on.

Completions: All possible completions for current prefix \(see above). This is
probably the most helpful bucket.

If one of these categories/buckets are not needed per default then add the
bucket-name \(s.a.) to this option and ECB will per default collapse this
bucket. So most needed buckets are better visible in the analyse-buffer."
  :group 'ecb-analyse
  :type '(repeat (choice :tag "Bucket" :menu-tag "Bucket"
                         (const :tag "Context" :value "Context")
                         (const :tag "Arguments" :value "Arguments")
                         (const :tag "Local Variables" :value "Local Variables")
                         (const :tag "Prefix" :value "Prefix")
                         (const :tag "Assignee" :value "Assignee")
                         (const :tag "Function" :value "Function")
                         (const :tag "Argument #" :value "Argument #")
                         (const :tag "Completions" :value "Completions")
                         (string :tag "Other bucketname"))))

(defcustom ecb-analyse-fontified-buckets '("Context")
  "*Buckets whose elements should be fontified as in the methods-buffer.
If the name of a category/bucket is contained in this option then all elements
of this bucket will be displayed as in the methods-buffer - at least if an
element is a semantic-tag. This means if `ecb-font-lock-tags' is not nil these
elements will be fontified and also displayed with an appropriate icon if
possible. The default value does this only for the Context-bucket because for
most of the other buckets this makes not really much sense.

For available buckets see `ecb-analyse-collapsed-buckets'.

For the faces used to display a bucket-node itself or bucket-elements not
fontified see the options `ecb-analyse-bucket-node-face' rsp.
`ecb-analyse-bucket-element-face'."
  :group 'ecb-analyse
  :type '(repeat (choice :tag "Bucket" :menu-tag "Bucket"
                         (const :tag "Context" :value "Context")
                         (const :tag "Arguments" :value "Arguments")
                         (const :tag "Local Variables" :value "Local Variables")
                         (const :tag "Prefix" :value "Prefix")
                         (const :tag "Assignee" :value "Assignee")
                         (const :tag "Function" :value "Function")
                         (const :tag "Argument #" :value "Argument #")
                         (const :tag "Completions" :value "Completions")
                         (string :tag "Other bucketname"))))

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>:  add such items to the
;; anslyse-popup-menu!
(defvar ecb----semantic-ia-sb-easymenu-definition
  '( "---"
;     [ "Expand" speedbar-expand-line nil ]
;     [ "Contract" speedbar-contract-line nil ]
     [ "Tag Information" semantic-ia-sb-show-tag-info t ]
     [ "Jump to Tag" speedbar-edit-line t ]
     [ "Complete" speedbar-edit-line t ]
     )
  "Extra menu items Analysis mode.")

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: first we start simple by creating
;; an anaylse-tree-buffer which can be included in a layout and all should
;; work. Later we could design a basic-mechanism which:
;; - creates a "mode"-tree-buffer for stuff like analyse, class browser etc.
;; - a mechansims which copy the anaylse-tree-buffer (or in future the
;;   class-browser-tree-buffer etc.) to that "mode"-tree-buffer so you can
;;   display in that basic tree-buffer different "modes". Probably this would
;;   be the best: We have 4 basic tree-buffers (directories, sources, methods
;;   and history and one additional "mode"-tree-buffer which can be added to a
;;   layout. Then there is a command which display different tree-buffers uin
;;   the mode-tree-buffer, e.g. the analyse tree-buffer or a
;;   class-browser-tree-buffer.

(defconst ecb-analyse-nodedata-tag-with-pos 0)
(defconst ecb-analyse-nodedata-tag-without-pos 1)
(defconst ecb-analyse-nodedata-no-tag 2)

(defconst ecb-analyse-nodetype-bucket 0)
(defconst ecb-analyse-nodetype-context 1)
(defconst ecb-analyse-nodetype-arguments 2)
(defconst ecb-analyse-nodetype-completions 3)
(defconst ecb-analyse-nodetype-localvars 4)
(defconst ecb-analyse-nodetype-prefix 5)
(defconst ecb-analyse-nodetype-assignee 6)
(defconst ecb-analyse-nodetype-function 7)
(defconst ecb-analyse-nodetype-function-arg 8)

(defun ecb-analyse-buffer-sync ()
  "Synchronize the analyse buffer with the current buffer and point.
This means in fact display the current analysis for current point."
  (interactive)
  (ecb-do-if-buffer-visible-in-ecb-frame 'ecb-analyse-buffer-name
    ;; (current-buffer) is here the current buffer of the edit-area!
    (let ((analysis nil)
          (completions nil)
          (fnargs nil)
          (cnt nil)
          )
      ;; Try and get some sort of analysis
      (ignore-errors
        (save-excursion
          (setq analysis (semantic-analyze-current-context (point)))
          (setq cnt (semantic-find-tag-by-overlay))
          (when analysis
            (setq completions (semantic-analyze-possible-completions analysis))
            (setq fnargs (semantic-get-local-arguments (point)))
            )))
      (save-selected-window
        (ecb-exec-in-analyse-window
         ;; we must remove the old nodes
         (tree-buffer-set-root (tree-node-new-root))
         (when analysis
           ;; Now insert information about the context
           (when cnt
             (ecb-analyse-add-nodes "Context" "Context"
                                    cnt ecb-analyse-nodetype-context))
           (when fnargs
             (ecb-analyse-add-nodes "Arguments" "Arguments" fnargs
                                    ecb-analyse-nodetype-arguments))
           ;; Let different classes draw more nodes.
           (ecb-analyse-more-nodes analysis)
           (when completions
             (ecb-analyse-add-nodes "Completions" "Completions" completions
                                    ecb-analyse-nodetype-completions)))
         (tree-buffer-update))))))

(defmethod ecb-analyse-more-nodes ((context semantic-analyze-context))
  "Show a set of ecb-nodes specific to CONTEXT."
  (let ((localvars (oref context localvariables)))
    (when localvars
      (ecb-analyse-add-nodes "Local Variables" "Local Variables" localvars
                             ecb-analyse-nodetype-localvars)))
  (let ((prefix (oref context prefix)))
    (when prefix
      (ecb-analyse-add-nodes "Prefix" "Prefix" prefix ecb-analyse-nodetype-prefix))))

(defmethod ecb-analyse-more-nodes ((context semantic-analyze-context-assignment))
  "Show a set of ecb-nodes specific to CONTEXT."
  (call-next-method)
  (let ((assignee (oref context assignee)))
    (when assignee
      (ecb-analyse-add-nodes "Assignee" "Assignee" assignee
                             ecb-analyse-nodetype-assignee))))

(defmethod ecb-analyse-more-nodes ((context semantic-analyze-context-functionarg))
  "Show a set of ecb-nodes specific to CONTEXT."
  (call-next-method)
  (let ((func (oref context function)))
    (when func
      (ecb-analyse-add-nodes "Function" "Function" func ecb-analyse-nodetype-function)
      ;; An index for the argument the prefix is in:
      (let ((arg (oref context argument)))
	(when arg
	  (ecb-analyse-add-nodes "Argument #"
                                 (format "Argument # %d" (oref context index))
                                 arg
                                 ecb-analyse-nodetype-function-arg))))))

(defmacro ecb-exec-in-analyse-window (&rest body)
  "Evaluates BODY in the analyse-window of ECB. If that window is not
visible then return the symbol 'window-not-visible. Otherwise the return
value of BODY is returned."
  `(unwind-protect
       (if (not (ecb-window-select ecb-analyse-buffer-name))
           'window-not-visible
	 ,@body)
     ))

;; Each category of nodes gets its own nodetype, so we can offer different
;; popup-menus for different categories (e.g. completions have other senseful
;; popup-menu-entries than the rest. The date of a node will always be a cons
;; where the car is the analyse-elem and the cdr is a const if it is a
;; semantic-tag (positionless or with position) or not.

(defun ecb-analyse-add-nodes (bucket bucket-name list nodetype)
  "Create ecb-nodes from LIST. BUCKET is one of the categories/buckets
mentioned in `ecb-analyse-collapsed-buckets'. BUCKET-NAME is the name a bucket
should be displayed with. LIST is a list of tags for this bucket. NODETYPE is
an integer which will be added as type to the nodes created for the elements
of LIST. If optional arg FONTIFY-TAGS is not nil then the tags contained in
LIST will be displayed as in the methods-buffer of ECB \(if
`ecb-font-lock-tags' is not nil)."
  (when (> (length list) 0)
    (save-excursion
      (set-buffer ecb-analyse-buffer-name)
      (let* ((bucket-name-formatted (ecb-merge-face-into-text bucket-name
                                                              ecb-analyse-bucket-node-face))
             (bucket-node (tree-node-new bucket-name-formatted
                                         ecb-analyse-nodetype-bucket
                                         (list 'ecb-bucket-node
                                               ecb-analyse-nodetype-bucket)
                                         nil
                                         (tree-buffer-get-root))))
        ;; We expand per default all categories (= Buckets)
        ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Maybe customize this with
        ;; an option?
        (tree-node-set-expanded bucket-node (not (member bucket
                                                         ecb-analyse-collapsed-buckets)))
        (dolist (elem list)
          (let* ((fontify-tags (member bucket ecb-analyse-fontified-buckets))
                 (string-1 (cond ((stringp elem)
                                  elem)
                                 ((ecb--semantic-tag-p elem)
                                  (if fontify-tags
                                      (ecb-displayed-tag-name elem)
                                    (ecb--semantic-format-tag-uml-concise-prototype elem)))
                                 (t "foo")))
                 (string (concat string-1)))
            (unless fontify-tags
              (ecb-merge-face-into-text string ecb-analyse-bucket-element-face))
            (if (ecb--semantic-tag-p elem)
                ;; In case of a semantic-tag the speedbar-code does some nifty
                ;; tag-info-display when clicking onto the exp-button
                (tree-node-new string nodetype
                               (list elem
                                     (if (ecb--semantic-tag-with-position-p elem)
                                         ecb-analyse-nodedata-tag-with-pos
                                       ecb-analyse-nodedata-tag-without-pos)
                                     nodetype)
                               t bucket-node nil)
              (tree-node-new string nodetype
                             (list elem ecb-analyse-nodedata-no-tag nodetype)
                             t bucket-node nil))))))))

(defun ecb-compare-analyse-buffer-node-data (left right)
  "Return not nil when LEFT and RIGHT are identical node-datas."
  (and (equal (nth 2 left) (nth 2 right))
       (ecb-compare-methods-buffer-node-data (car left) (car right))))

(defun ecb-analyse-node-clicked (node ecb-button edit-window-nr
                                      shift-mode meta-mode)
  "Handle clicking onto NODE in the analyse-buffer. ECB-BUTTON can be 1, 2 or
3. If 3 then EDIT-WINDOW-NR contains the number of the edit-window the NODE
should be displayed. For 1 and 2 the value of EDIT-WINDOW-NR is ignored."
  (if shift-mode
      (ecb-mouse-over-analyse-node node nil nil 'force))
  (let* ((data (tree-node-get-data node))
         (tag (nth 0 data))
         (type (tree-node-get-type node)))
    (cond
     ((= type ecb-analyse-nodetype-bucket)
      (tree-node-toggle-expanded node)
      ;; Update the tree-buffer with optimized display of NODE
      (tree-buffer-update node))
     ((= type ecb-analyse-nodetype-completions)
      ;; We must highlight the tag
      (tree-buffer-highlight-node-data data)
      (ecb-find-file-and-display ecb-path-selected-source nil)
      (let* ((a (semantic-analyze-current-context (point)))
             (bounds (oref a bounds))
             (movepoint nil))
        (save-excursion
          (if (and (<= (point) (cdr bounds)) (>= (point) (car bounds)))
              (setq movepoint t))
          (goto-char (car bounds))
          (delete-region (car bounds) (cdr bounds))
          (insert (semantic-tag-name tag))
          (if movepoint (setq movepoint (point))))
        (if movepoint
            (goto-char movepoint))))
     (t
      ;; We must highlight the tag
      (tree-buffer-highlight-node-data data)
      ;; if we have a positioned tag we jump to it
      (when (and tag (= (nth 1 data) ecb-analyse-nodedata-tag-with-pos))
        (ecb-semantic-assert-valid-tag tag)
        (ecb-jump-to-tag (buffer-file-name (ecb--semantic-tag-buffer tag))
                         tag
                         (ecb-get-edit-window
                          (ecb-combine-ecb-button/edit-win-nr
                           ecb-button edit-window-nr))
                         t nil)
        (when meta-mode
          (ecb-run-with-idle-timer 0.001 nil 'ecb-hide-ecb-windows)))))))

(defun ecb-set-analyse-buffer ()
  (add-hook 'ecb-current-buffer-sync-hook-internal 'ecb-analyse-buffer-sync)
  (ecb-with-dedicated-window ecb-analyse-buffer-name 'ecb-set-analyse-buffer
    (switch-to-buffer ecb-analyse-buffer-name)))

(defun ecb-maximize-window-analyse ()
  "Maximize the ECB-analyse-window.
I.e. delete all other ECB-windows, so only one ECB-window and the
edit-window\(s) are visible \(and maybe a compile-window). Works also if the
ECB-analyse-window is not visible in current layout."
  (interactive)
  (ecb-display-one-ecb-buffer ecb-analyse-buffer-name))

(defun ecb-goto-window-analyse ()
  "Make the ECB-analyse window the current window."
  (interactive)
  (ecb-goto-ecb-window ecb-analyse-buffer-name))


;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: entweder die beiden folgenden
;; Funktionen anpassen oder erstmal weglassen
(defun ecb----semantic-ia-sb-show-tag-info ()
  "Display information about the tag on the current line.
Same as clicking on the <i> button.
See `semantic-ia-sb-tag-info' for more."
  (interactive)
  (let ((tok nil))
    (save-excursion
      (end-of-line)
      (forward-char -1)
      (setq tok (get-text-property (point) 'speedbar-token)))
    (semantic-ia-sb-tag-info nil tok 0)))

(defun ecb----semantic-ia-sb-tag-info (text tag indent)
  "Display as much information as we can about tag.
Show the information in a shrunk split-buffer and expand
out as many details as possible.
TEXT, TAG, and INDENT are speedbar function arguments."
  (when (semantic-tag-p tag)
    (unwind-protect
	(let ((ob nil))
	  (speedbar-select-attached-frame)
	  (setq ob (current-buffer))
	  (with-output-to-temp-buffer "*Tag Information*"
	    ;; Output something about this tag:
	    (save-excursion
	      (set-buffer "*Tag Information*")
	      (goto-char (point-max))
	      (insert
	       (semantic-format-tag-prototype tag nil t)
	       "\n")
	      (let ((typetok
		     (condition-case nil
			 (save-excursion
			   (set-buffer ob)
			   (semantic-analyze-tag-type tag))
		       (error nil))))
		(if typetok
		    (insert (semantic-format-tag-prototype
			     typetok nil t))
		  ;; No type found by the analyzer
		  ;; The below used to try and select the buffer from the last
		  ;; analysis, but since we are already in the correct buffer, I
		  ;; don't think that is needed.
		  (let ((type (semantic-tag-type tag)))
		    (cond ((semantic-tag-p type)
			   (setq type (semantic-tag-name type)))
			  ((listp type)
			   (setq type (car type))))
		    (if (semantic-lex-keyword-p type)
			(setq typetok
			      (semantic-lex-keyword-get type 'summary))))
		  (if typetok
		      (insert typetok))
		  ))
	      ))
	  ;; Make it small
	  (shrink-window-if-larger-than-buffer
	   (get-buffer-window "*Tag Information*")))
      (select-frame speedbar-frame))))

(defun ecb-mouse-over-analyse-node (node &optional window no-message click-force)
  "Displays help text if mouse moves over a node in the analyse buffer or if
CLICK-FORCE is not nil and always with regards to the settings in
`ecb-analyse-show-node-info'. NODE is the node for which help text should be
displayed, WINDOW is the related window, NO-MESSAGE defines if the help-text
should be printed here."
  (let ((str (when (or click-force
                       (ecb-show-minibuffer-info node window
                                                 (car ecb-analyse-show-node-info)))
               (tree-node-get-name node))))
    ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Implement the full-info part.
    (prog1 str
      (unless no-message
        (tree-buffer-nolog-message str)))))

(defun ecb-analyse-node-mouse-highlighted-p (node)
  "Return not nil when NODE has a positioned tag as data or belongs to the
completions."
  (or (equal ecb-analyse-nodedata-tag-with-pos
             (nth 1 (tree-node-get-data node)))
      (= (tree-node-get-type node) ecb-analyse-nodetype-completions)))

(defun ecb-analyse-menu-creator (tree-buffer-name)
  ;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: Implement this
  nil)

(defun ecb-create-analyse-tree-buffer ()
  "Create the tree-buffer for analyse-display."
  (ecb-tree-buffers-add ecb-analyse-buffer-name 'ecb-analyse-buffer-name)
  (tree-buffer-create
   ecb-analyse-buffer-name
   ecb-frame
   ecb-tree-mouse-action-trigger
   'ecb-interpret-mouse-click
   'ecb-tree-buffer-node-select-callback
   'ecb-tree-buffer-node-expand-callback
   'ecb-tree-buffer-node-collapsed-callback
   'ecb-mouse-over-analyse-node
   'ecb-analyse-node-mouse-highlighted-p
   'ecb-compare-analyse-buffer-node-data
   nil
   nil
   'ecb-analyse-menu-creator
   nil ;; TODO: menu-titles coming later
;;    (list (cons ecb-methods-nodetype-tag ecb-methods-menu-title-creator)
;;          (cons ecb-methods-nodetype-bucket ecb-methods-menu-title-creator)
;;          (cons ecb-methods-nodetype-externtag ecb-methods-menu-title-creator))
   (ecb-member-of-symbol/value-list ecb-analyse-buffer-name
                                    ecb-tree-truncate-lines)
   t
   ecb-tree-indent
   nil ;; ecb-tree-incremental-search
   nil ;; ecb-methods-incr-searchpattern-node-prefix
   ecb-tree-navigation-by-arrow
   ecb-tree-easy-hor-scroll
   (car ecb-tree-image-icons-directories)
   (ecb-member-of-symbol/value-list ecb-analyse-buffer-name
                                    (cdr ecb-tree-image-icons-directories)
                                    'car 'cdr)
   ecb-tree-buffer-style
   ecb-tree-guide-line-face
   nil
   ecb-tree-expand-symbol-before
   ecb-analyse-face
   ecb-analyse-general-face
   (append
    (list (function (lambda ()
                      (local-set-key (kbd "C-t")
                                     'ecb-toggle-RET-selects-edit-window)
                      (if (not ecb-running-xemacs)
                          (define-key tree-buffer-key-map
                            [mode-line mouse-2]
                            'ecb-toggle-maximize-ecb-window-with-mouse)))))
    ecb-common-tree-buffer-after-create-hook
    ecb-analyse-buffer-after-create-hook)))

(silentcomp-provide 'ecb-analyse)

;;; ecb-anaylse.el ends here
