;;; tree-buffer.el --- functions for tree buffers

;; Copyright (C) 2000, 2001 Jesper Nordenberg

;; Author: Jesper Nordenberg <mayhem@home.se>
;; Maintainer: Jesper Nordenberg <mayhem@home.se>
;; Keywords: java, class, browser

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
;; Functions for tree buffers.
;;
;; This file is part of the ECB package which can be found at:
;; http://home.swipnet.se/mayhem/ecb.html

;; $Id: tree-buffer.el,v 1.39 2001/05/06 07:02:23 berndl Exp $

;;; Code:

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(defconst running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(if running-xemacs
    ;; XEmacs
    (progn
      (defalias 'tree-buffer-line-beginning-pos 'point-at-bol)
      (defalias 'tree-buffer-window-display-height 'window-displayed-height)
      (defalias 'tree-buffer-event-to-key 'event-key)
      (require 'overlay)
      (defface secondary-selection
        '((((class color) (background light)) (:foreground "blue" :background "LightGray"))
          (((class color) (background dark))  (:foreground "blue" :background "LightGray"))
          (t ()))
        "Face for highlights."))
  ;; GNU Emacs
  (defalias 'tree-buffer-line-beginning-pos 'line-beginning-position)
  (defun tree-buffer-window-display-height (&optional window)
    (1- (window-height window)))
  (defalias 'tree-buffer-event-to-key 'event-basic-type))


(defvar tree-buffer-root nil)
(defvar tree-buffer-nodes nil)
(defvar tree-buffer-key-map nil)
(defvar tree-buffer-indent nil)
(defvar tree-buffer-highlighted-node-data nil)
(defvar tree-buffer-menus nil)
(defvar tree-buffer-type-facer nil)
(defvar tree-buffer-expand-symbol-before nil)
(defvar tree-buffer-is-click-valid-fn nil)
(defvar tree-node-selected-fn nil)
(defvar tree-node-expanded-fn nil)
(defvar tree-node-mouse-over-fn nil)
(defvar tree-buffer-highlight-overlay nil)
(defvar tree-buffer-incr-searchpattern nil)
(defvar tree-buffer-incr-search nil)

(defun list-append(list item)
  (if list
      (progn
        (setcdr (last list) item)
        list)
    item))

(defun filter(list fn)
  (let ((res nil))
    (while list
      (if (funcall fn (car list))
          (setq res (cons (car list) res)))
      (setq list (cdr list)))
    (nreverse res)))

(defun tree-buffer-get-node-name-start-column(node)
  "Returns the buffer column where the name of the node starts."
  (+ (tree-buffer-get-node-indent node)
     (if (and tree-buffer-expand-symbol-before
              (tree-node-is-expandable node))
         4 0)))

(defun tree-buffer-get-node-name-start-point(node)
  "Returns the buffer point where the name of the node starts."
  (let ((linenr (tree-buffer-find-node node)))
    (when linenr
      (goto-line linenr)
      (beginning-of-line)
      (+ (point) (tree-buffer-get-node-name-start-column node)))))

(defun tree-buffer-get-node-name-end-point(node)
  "Returns the buffer point where the name of the node ends."
  (+ (tree-buffer-get-node-name-start-point node)
     (length (tree-node-get-name node))))

(defun tree-buffer-at-expand-symbol(node p)
  (if tree-buffer-expand-symbol-before
      (< p (1- (tree-buffer-get-node-name-start-point node)))
    (> p (tree-buffer-get-node-name-end-point node))))

(defun tree-buffer-select(mouse-button shift-pressed control-pressed)
  "If the callback-function in `tree-buffer-is-click-valid-fn' returns nil
then nothing is done. Otherwise: If the node is expandable and the node is not
expanded then the callback-function in `tree-node-expanded-fn' is called with
the node, the clicked MOUSE-BUTTON \(1 for mouse-1, 2 for mouse-2, 0 for no
mouse-button but a key like RET or TAB), SHIFT-PRESSED and CONTROL-PRESSED
informations and the name of the tree-buffer as arguments. If the node is not
expandable then the callback-function in `tree-node-selected-fn' is called
with the same arguments as `tree-node-expanded-fn'."
  (when (and tree-buffer-is-click-valid-fn
             (funcall tree-buffer-is-click-valid-fn mouse-button
                      shift-pressed control-pressed (buffer-name)))
    (let ((p (point))
          (node (tree-buffer-get-node-at-point)))
      (when node
        (if (and (tree-node-is-expandable node)
                 (tree-buffer-at-expand-symbol node p)
                 ;; if the expand-symbol is displayed before and mouse-button
                 ;; = 0, means RET is pressed, we do not toggle-expand but work
                 ;; as if point would not be at expand-symbol. This is for
                 ;; conveniance.
                 (not (and (= mouse-button 0)
                           tree-buffer-expand-symbol-before)))
            (progn
              (when (and (not (tree-node-is-expanded node))
                         tree-node-expanded-fn)
                (funcall tree-node-expanded-fn node mouse-button
                         shift-pressed control-pressed (buffer-name)))
              (when (tree-node-is-expandable node)
                (tree-node-toggle-expanded node))
              ;; Update the tree-buffer with optimized display of NODE
              (tree-buffer-update node))
          (when tree-node-selected-fn
            (funcall tree-node-selected-fn node mouse-button
                     shift-pressed control-pressed (buffer-name))))))))

(defun tree-buffer-get-node-at-point()
  (let ((linenr (+ (count-lines 1 (point)) (if (= (current-column) 0) 0 -1))))
    (nth linenr tree-buffer-nodes)))

(defun tree-buffer-get-node-indent(node)
  (* tree-buffer-indent (1- (tree-node-get-depth node))))

(defun tree-buffer-find-node-data(node-data)
  (catch 'exit
    (dolist (node tree-buffer-nodes)
      (when (equal (tree-node-get-data node) node-data)
        (throw 'exit node)))))

(defun tree-buffer-find-node(node)
  (catch 'exit
    (let ((linenr 1))
      (dolist (node2 tree-buffer-nodes)
        (when (eq node node2)
          (throw 'exit linenr))
        (setq linenr (1+ linenr))))))

(defun tree-buffer-get-node-facer(node)
  (let ((facer (cdr (assoc (tree-node-get-type node) tree-buffer-type-facer))))
    (if facer
        facer
      nil)))

(defun tree-buffer-node-set-face(node face)
  "Sets the face for a node name."
  (put-text-property (tree-buffer-get-node-name-start-point node)
                     (tree-buffer-get-node-name-end-point node) 'face face))

(defun tree-buffer-recenter (node window)
  "If NODE is not visible then first recenter the window WINDOW so NODE is
best visible, means NODE is displayed in the middle of the window if possible.
If NODE is expanded then recenter the WINDOW so as much as possible subnodes
of NODE will be visible. If NODE is not expandable then WINDOW is always
displayed without empty-lines at the end, means WINDOW is always best filled."
  (let* ((node-point (save-excursion
                      (goto-line (tree-buffer-find-node node))
                      (tree-buffer-line-beginning-pos)))
         (point-lines-before (count-lines (point-min) node-point))
         (point-lines-after (1- (count-lines node-point (point-max)))))
    ;; first make point best visible, means display node in the middle of the
    ;; window if possible (if there are enough lines before/after the node).
    (if (not (pos-visible-in-window-p node-point window))
        (if (< node-point (window-start window))
            (set-window-start
             window
             (save-excursion
               (goto-char node-point)
               (forward-line
                (* -1 (min point-lines-before
                           (/ (tree-buffer-window-display-height window) 2))))
               (tree-buffer-line-beginning-pos)))
          (set-window-start window
                            (save-excursion
                              (goto-char (window-start window))
                              (forward-line
                               (- (+ 1
                                     (count-lines (window-start window) node-point)
                                     (min point-lines-after
                                          (/ (tree-buffer-window-display-height window) 2)))
                                  (tree-buffer-window-display-height window)))
                              (tree-buffer-line-beginning-pos)))))
    ;; now optimize the window display for displaying as much possible
    ;; subnodes of node.
    (if (tree-node-is-expanded node)
        (let ((exp-node-children-count (tree-node-count-subnodes-to-display node))
              (point-window-line (count-lines (window-start window) node-point)))
          ;; if the current node is not already displayed in the first line of the
          ;; window (= condition 1) and if not all of it큦 children are visible in
          ;; the window then we can do some optimization.
          (if (and (save-excursion
                     (goto-char node-point)
                     (forward-line -1)
                     (pos-visible-in-window-p (point) window))
                   (not (save-excursion
                          (goto-char node-point)
                          (forward-line exp-node-children-count)
                          (pos-visible-in-window-p (point) window))))
              ;; optimize the display of NODE and it큦 children so as much as
              ;; possible are visible.
              (set-window-start window
                                (save-excursion
                                  (goto-char (window-start window))
                                  (forward-line
                                   (min point-window-line
                                        (- (+ 1 point-window-line
                                              exp-node-children-count)
                                           (tree-buffer-window-display-height window))))
                                  (tree-buffer-line-beginning-pos)))))
      ;; maybe there are empty lines in the window after the last non-empty
      ;; line. If they are we scroll until the whole window is filled with
      ;; non-empty lines.
      (if (not (tree-node-is-expandable node))
          (let ((w-height (tree-buffer-window-display-height window))
                (full-lines-in-window (count-lines (window-start window)
                                                   (window-end window t))))
            (if (< full-lines-in-window
                   w-height)
                (set-window-start window
                                  (save-excursion
                                    (goto-char (window-start window))
                                    (forward-line (- full-lines-in-window w-height))
                                    (tree-buffer-line-beginning-pos)))))))))

;; Klaus: Now we use overlays to highlight current node in a tree-buffer. This
;; makes it easier to do some facing with the nodes itself and above all this
;; the faces of the node are always visible even if the node is highlighted
;; (useful e.g. if you show the sources in the ECB directory buffer, and if
;; you do some syntax highlighting in the method-buffer).
(defun tree-buffer-remove-highlight()
  (when tree-buffer-highlighted-node-data
    (let ((node (tree-buffer-find-node-data tree-buffer-highlighted-node-data)))
      (when node
        (delete-overlay tree-buffer-highlight-overlay))))
  (setq tree-buffer-highlighted-node-data nil))

(defun tree-buffer-highlight-node-data(node-data &optional dont-make-visible)
  (if node-data
      (let ((node (tree-buffer-find-node-data node-data))
            (w (get-buffer-window (current-buffer))))
        (when node
;;        (tree-buffer-remove-highlight)
          (setq tree-buffer-highlighted-node-data node-data)
          (save-excursion
            (move-overlay tree-buffer-highlight-overlay
                          (tree-buffer-get-node-name-start-point node)
                          (tree-buffer-get-node-name-end-point node)))
          (when (not dont-make-visible)
            ;; make node visible if not and optimize the windows display for
            ;; the node.
            (tree-buffer-recenter node w))))
    (tree-buffer-remove-highlight)))

(defun tree-buffer-insert-text(text &optional facer)
  "Insert TEXT at point and faces it with FACER. FACER can be a face then the
text gets this face or it can be a function-symbol which is called to face the
inserted TEXT. Such a function gets two arguments: Point where TEXT has been
inserted and the TEXT itself"
  (let ((p (point)))
    (insert text)
    (put-text-property p (+ p (length text)) 'mouse-face 'highlight)
    (if facer
	(if (functionp facer)
	    (funcall facer p text)
	  (put-text-property p (+ p (length text)) 'face facer)))))
    
(defun tree-buffer-add-node(node depth)
  (insert (make-string (* depth tree-buffer-indent) ? ))
  (when (and tree-buffer-expand-symbol-before
             (tree-node-is-expandable node))
    (tree-buffer-insert-text (if (tree-node-is-expanded node) "[-]" "[+]"))
    (insert " "))
  (tree-buffer-insert-text (tree-node-get-name node) (tree-buffer-get-node-facer node))
  (when (and (not tree-buffer-expand-symbol-before)
             (tree-node-is-expandable node))
    (insert " ")
    (tree-buffer-insert-text (if (tree-node-is-expanded node) "[-]" "[+]")))
  (insert "\n")
  (setq tree-buffer-nodes (list-append tree-buffer-nodes (list node)))
  (if (tree-node-is-expanded node)
      (dolist (node (tree-node-get-children node))
        (tree-buffer-add-node node (1+ depth)))))

(defun tree-node-count-subnodes-to-display(node)
  "Returns the number of ALL subnodes of NODE which will currently be displayed
if NODE is expanded, means the number of all the children of NODE \(if NODE is
expanded) plus recursive the number of the children of each expanded child.
Example:
\[-] NODE
    \[+] child 1
    \[-] child 2
        \[+] child 2.1
        \[-] child 2.2
            \[+] child 2.2.1
            \[+] child 2.2.2
        \[+] child 2.3
    \[-] child 3
        \[+] child 3.1
    \[+] child 4
The result for NODE here is 10"
  (let ((result 0))
    (when (and (tree-node-is-expandable node)
               (tree-node-is-expanded node))
      (setq result (+ result (length (tree-node-get-children node))))
      (dolist (child (tree-node-get-children node))
        (setq result (+ result (tree-node-count-subnodes-to-display child)))))
    result))

(defun tree-buffer-update(&optional node)
  "Updates the current tree-buffer. The buffer will be completely rebuild with
it큦 current nodes. window-start and point will be preserved.
If NODE is not nil and a valid and expanded node with at least one child then
the display of this node is optimized so the node itself and as much as
possible of it큦 children \(and also recursive the children of a child if it큦
aleady expanded, see `tree-node-count-subnodes-to-display') are visible in
current tree-buffer."
  (let* ((w (get-buffer-window (current-buffer)))
         (ws (window-start w))
         (p (point))
         (buffer-read-only nil)
         (next-line-add-newlines nil))
    (setq tree-buffer-nodes nil)
    (erase-buffer)
    (dolist (node (tree-node-get-children tree-buffer-root))
      (tree-buffer-add-node node 0))
    (tree-buffer-highlight-node-data tree-buffer-highlighted-node-data)
    (goto-char p)
    (set-window-start w ws)
    ;; let큦 optimize the display of the expanded node NODE and it큦 children.
    (when node
      (tree-buffer-recenter node w))))

(defun tree-buffer-scroll(point window-start)
  "Scrolls current tree-buffer. The window will start at WINDOW-START and
point will stay on POINT."
  (goto-char point)
  (set-window-start (get-buffer-window (current-buffer)) window-start))

(defun tree-buffer-set-root(root)
  (setq tree-buffer-root root)
  (tree-node-set-expanded tree-buffer-root t))

(defun tree-buffer-get-root()
  tree-buffer-root)

(defun tree-buffer-show-menu(event)
  (interactive "e")
  (mouse-set-point event)
  (when tree-buffer-menus
    (let* ((node (tree-buffer-get-node-at-point))
           (menu (cdr (assoc (tree-node-get-type node) tree-buffer-menus)))
           (map (make-sparse-keymap (tree-node-get-data node))))
      (when menu
        (set-keymap-parent map menu)
        (let ((fn (x-popup-menu event map)))
          (if fn
              (eval (list (car fn) 'node))))))))

;; idea is stolen from ido.el, written by Kim F. Storm <stormware@get2net.dk>
(defun tree-buffer-find-common-substring (lis subs &optional only-prefix)
  "Return common substring beginning with SUBS in each element of LIS. If
ONLY-PREFIX is not nil then only common prefix is returned."
  (let ((change-word-sub (concat (if only-prefix "^" "")
                                 (regexp-quote subs)))
        res alist)
    (setq res (mapcar (function (lambda (word)
                                  (let ((case-fold-search t)
                                        (m (string-match change-word-sub word)))
                                    (if m
                                        (substring word m)
                                      ;; else no match
                                      nil))))
                      lis))
    (setq res (delq nil res));; remove any nil elements (shouldn't happen)
    (setq alist (mapcar (function (lambda (r)
                                    (cons r 1)))
                        res));; could use an  OBARRAY

    ;; try-completion returns t if there is an exact match.
    (let ((completion-ignore-case t))
      (try-completion subs alist))))

(defconst tree-buffer-incr-searchpattern-prefix "^[ \t]*\\(\\[[+-]\\]\\)? ?")
(defun tree-buffer-incremental-node-search ()
  "Incremental search for a node in current tree-buffer. Each displayable
key \(e.g. all keys normally bound to `self-insert-command') is appended to
the current seach-pattern. The tree-buffer tries to jump to the current
search-pattern. If no match is found then nothing is done. Some special keys:
- \[backspace] and \[delete]: Delete the last character from the search-pattern.
- \[home]: Delete the complete search-pattern
- \[end]: Expand either to a complete node if current search-pattern is
         already unique or expands to the greates common prefix of the nodes
The current search-pattern is shown in the echo area.
After selecting a node with RET the search-pattern is cleared out.

Do NOT call this function directly. It works only if called from the binding
mentioned above!"
  (interactive)
  (let ((last-comm (tree-buffer-event-to-key last-command-event)))
    (cond  ((or (equal last-comm 'delete)
                (equal last-comm 'backspace))
          ;; reduce by one from the end
          (setq tree-buffer-incr-searchpattern
                (substring tree-buffer-incr-searchpattern
                           0
                           (max 0 (1- (length tree-buffer-incr-searchpattern))))))
         ;; delete the complete search-pattern
         ((equal last-comm 'home)
          (setq tree-buffer-incr-searchpattern ""))
         ;; expand to the max. common prefix
         ((equal last-comm 'end)
          (let* ((node-name-list (mapcar 'tree-node-get-name
                                         (tree-node-get-children tree-buffer-root)))
                 (common-prefix (tree-buffer-find-common-substring
                                 node-name-list tree-buffer-incr-searchpattern
                                 (if (equal tree-buffer-incr-search 'prefix) t))))
            (if (stringp common-prefix)
                (setq tree-buffer-incr-searchpattern common-prefix))))
         (t
          ;; add the last command to the end
          (setq tree-buffer-incr-searchpattern
                (concat tree-buffer-incr-searchpattern
                        (char-to-string last-comm)))))
    (message "%s node search: [%s]%s"
             (buffer-name (current-buffer))
             tree-buffer-incr-searchpattern
             (if (let ((case-fold-search t))
                   (save-excursion
                     (goto-char (point-min))
                     (re-search-forward
                      (concat tree-buffer-incr-searchpattern-prefix
                              (if (equal tree-buffer-incr-search 'substring)
                                  "[^()]*"
                                "")
                              (regexp-quote tree-buffer-incr-searchpattern)) nil t)))
                 ;; we have found a matching ==> jump to it
                 (progn
                   (goto-char (match-end 0))
                   "")
               " - no match"))))
  
(defun tree-buffer-create(name is-click-valid-fn node-selected-fn
                               node-expanded-fn node-mouse-over-fn
                               menus tr-lines read-only tree-indent
                               incr-search
                               &optional type-facer expand-symbol-before)
  "Creates a new tree buffer with
NAME: Name of the buffer
IS-CLICK-VALID-FN: `tree-buffer-create' rebinds down-mouse-1 and down-mouse-2
                   and also in combination with shift and control to
                   `tree-buffer-select'. IS-CLICK-VALID-FN is called first if
                   a node or an expand-symbol is clicked. This function is
                   called with four arguments:
                   - mouse-button: The clicked mouse-button \(1 = mouse-1, 2 =
                     mouse 2)
                   - shift-pressed: non nil if the SHIFT-key was pressed
                     during mouse-click.
                   - control-pressed: non nil if the CONTROL-key was pressed
                     during mouse-click.
                   - tree-buffer-name: The buffer-name of the tree-buffer
                     where the node has been clicked.
                   The function must return not nil iff exactly this click is
                   accepted. If the function returns nil then really nothing is
                   done by the tree-buffer after this click!
NODE-SELECTED-FN: Function to call if a node has been selected
                  This function is called with the following paramters:
                  - node: The selected node
                  - mouse-button
                  - shift-pressed
                  - control-pressed
                  - tree-buffer-name
                  For the last four arguments see the description above.
NODE-EXPANDED-FN: Function to call if a node is expandable, point stays onto
                  the expand-symbol and node is not already expanded. This
                  function is called with the same arguments as
                  NODE-SELECTED-FN and should add all children nodes to this
                  node \(if possible).
NODE-MOUSE-OVER-FN: Function to call when the mouse is moved over a node.
MENUS: Nil or a list of one or two conses, each cons for a node-type \(0 or 1)
       Example: \(\(0 . menu-for-type-0) \(1 . menu-for-type-1)). The cdr of a
       cons must be a menu.
TR-LINES: Should lines in this tree buffer be truncated \(not nil)
READ-ONLY: Should the treebuffer be read-only \(not nil)
TREE-INDENT: spaces subnodes should be indented.
INCR-SEARCH: Should the incremental search be anabled in the tree-buffer.
             Three choices: 'prefix, 'substring, nil. See
             `tree-buffer-incremental-node-search'.
TYPE-FACER: Nil or a list of one or two conses, each cons for a node-type \(0
            or 1). The cdr of a cons can be:
            - a symbol of a face
            - a symbol of a function which gets to arguments \(see
              `tree-buffer-insert-text'). This function can do anything, but
              normally it should face a tree-buffer node.
            - the symbol t. Then the tree-buffer assumes that the node-text is
              already faces and therefore it does not face the node, means it
              does nothing then inserting the node-text, if the tree-buffer is
              updated.
EXPAND-SYMBOL-BEFORE: If not nil then the expand-symbol \(is displayed before
                      the node-text."
  (let ((nop (function (lambda() (interactive)))))
    (set-buffer (get-buffer-create name))

    (make-local-variable 'truncate-lines)
    (make-local-variable 'truncate-partial-width-windows)
    (make-local-variable 'tree-buffer-key-map)
    (make-local-variable 'tree-buffer-root)
    (make-local-variable 'tree-buffer-nodes)
    (make-local-variable 'tree-buffer-indent)
    (make-local-variable 'tree-buffer-is-click-valid-fn)
    (make-local-variable 'tree-node-selected-fn)
    (make-local-variable 'tree-node-expanded-fn)
    (make-local-variable 'tree-node-update-fn)
    (make-local-variable 'tree-node-mouse-over-fn)
    (make-local-variable 'tree-buffer-highlighted-node-data)
    (make-local-variable 'tree-buffer-menus)
    (make-local-variable 'tree-buffer-type-facer)
    (make-local-variable 'tree-buffer-expand-symbol-before)
    (make-local-variable 'tree-buffer-highlight-overlay)
    (make-local-variable 'tree-buffer-incr-searchpattern)
    (make-local-variable 'tree-buffer-incr-search)
  
    (setq truncate-lines tr-lines)
    (setq truncate-partial-width-windows tr-lines)
    (setq buffer-read-only read-only)
    (setq tree-buffer-key-map (make-sparse-keymap))
    (setq tree-buffer-is-click-valid-fn is-click-valid-fn)
    (setq tree-node-selected-fn node-selected-fn)
    (setq tree-node-expanded-fn node-expanded-fn)
    (setq tree-node-mouse-over-fn node-mouse-over-fn)
    (setq tree-buffer-indent tree-indent)
    (setq tree-buffer-highlighted-node-data nil)
    (setq tree-buffer-menus menus)
    (setq tree-buffer-root (tree-node-new "root" 0 "root"))
    (setq tree-buffer-type-facer type-facer)
    (setq tree-buffer-expand-symbol-before expand-symbol-before)
    (setq tree-buffer-highlight-overlay (make-overlay 1 1))
    (overlay-put tree-buffer-highlight-overlay 'face 'secondary-selection)
    (setq tree-buffer-incr-searchpattern "")
    (setq tree-buffer-incr-search incr-search)

    (when incr-search
      ;; settings for the incremental search.
      ;; for all keys which are bound to `self-insert-command' in `global-map'
      ;; we change this binding to `tree-buffer-klaus'.
      (substitute-key-definition 'self-insert-command
                                 'tree-buffer-incremental-node-search
                                 tree-buffer-key-map
                                 global-map)
      (define-key tree-buffer-key-map [delete]
        'tree-buffer-incremental-node-search)
      (define-key tree-buffer-key-map [backspace]
        'tree-buffer-incremental-node-search)
      (define-key tree-buffer-key-map [home]
        'tree-buffer-incremental-node-search)
      (define-key tree-buffer-key-map [end]
        'tree-buffer-incremental-node-search))
    
    (define-key tree-buffer-key-map "\C-m"
      (function (lambda()
		  (interactive)
                  ;; reinitialize the select pattern after selecting a node
                  (setq tree-buffer-incr-searchpattern "")
		  (tree-buffer-select 0 nil nil))))
    
    (define-key tree-buffer-key-map [tab]
      (function
       (lambda()
	 (interactive)
	 (let ((node (tree-buffer-get-node-at-point)))
	   (when (tree-node-is-expandable node)
	     (when (not (tree-node-is-expanded node))
	       (funcall tree-node-expanded-fn node 0 nil nil (buffer-name)))
	     (when (tree-node-is-expandable node)
	       (tree-node-toggle-expanded node))
	     ;; Update the tree-buffer with optimized display of NODE           
	     (tree-buffer-update node))))))

    ;; mouse-1
    (define-key tree-buffer-key-map
      (if running-xemacs '(button1) [down-mouse-1])
      (function (lambda(e)
		  (interactive "e")
		  (mouse-set-point e)
		  (tree-buffer-select 1 nil nil))))
  
    (define-key tree-buffer-key-map
      (if running-xemacs '(shift button1) [S-down-mouse-1])
      (function (lambda(e)
		  (interactive "e")
		  (mouse-set-point e)
		  (tree-buffer-select 1 t nil))))

    (define-key tree-buffer-key-map
      (if running-xemacs '(control button1) [C-down-mouse-1])
      (function (lambda(e)
		  (interactive "e")
		  (mouse-set-point e)
		  (tree-buffer-select 1 nil t))))

    (define-key tree-buffer-key-map [drag-mouse-1] nop)
    (define-key tree-buffer-key-map [mouse-1] nop)
    (define-key tree-buffer-key-map [double-mouse-1] nop)
    (define-key tree-buffer-key-map [triple-mouse-1] nop)

    ;; mouse-2
    (define-key tree-buffer-key-map
      (if running-xemacs '(button2) [down-mouse-2])
      (function (lambda(e)
		  (interactive "e")
		  (mouse-set-point e)
		  (tree-buffer-select 2 nil nil))))

    (define-key tree-buffer-key-map
      (if running-xemacs '(shift button2) [S-down-mouse-2])
      (function (lambda(e)
		  (interactive "e")
		  (mouse-set-point e)
		  (tree-buffer-select 2 t nil))))

    (define-key tree-buffer-key-map
      (if running-xemacs '(control button2) [C-down-mouse-2])
      (function (lambda(e)
		  (interactive "e")
		  (mouse-set-point e)
		  (tree-buffer-select 2 nil t))))

    (define-key tree-buffer-key-map [mouse-2] nop)
    (define-key tree-buffer-key-map [double-mouse-2] nop)
    (define-key tree-buffer-key-map [triple-mouse-2] nop)

    ;; mouse-3
    (define-key tree-buffer-key-map
      (if running-xemacs '(button3) [down-mouse-3])
      'tree-buffer-show-menu)
    (define-key tree-buffer-key-map [mouse-3] nop)
    (define-key tree-buffer-key-map [double-mouse-3] nop)
    (define-key tree-buffer-key-map [triple-mouse-3] nop)

    (define-key tree-buffer-key-map [mouse-movement]
      'tree-buffer-mouse-move)

    ;; mouse-movement
    (define-key tree-buffer-key-map [mouse-movement]
      (function (lambda(e)
		  (interactive "e")
		  (save-excursion
		    (mouse-set-point e);; (cadadr e)
		    (let ((node (tree-buffer-get-node-at-point)))
		      (when (and tree-node-mouse-over-fn node)
			(funcall tree-node-mouse-over-fn node)))))))

    (use-local-map tree-buffer-key-map)))

;;; Tree node

(defun tree-node-add-child(node child)
  (tree-node-set-children node (list-append (tree-node-get-children node) (list child)))
  (tree-node-set-parent child node))

(defun tree-node-remove-child(node child)
  "Removes the child from the node."
  (tree-node-set-parent child nil)
  (tree-node-set-children node
                          (delq child (tree-node-get-children node))))

(defun tree-node-find-child-data(node child-data)
  "Finds the first child with the given child-data."
  (catch 'exit
    (dolist (child (tree-node-get-children node))
      (when (equal (tree-node-get-data child) child-data)
        (throw 'exit child)))))

(defun tree-node-remove-child-data(node child-data)
  "Removes the first child with the given child-data. Returns the removed
child."
  (catch 'exit
    (let ((last-cell nil)
	  (cell (tree-node-get-children node)))
      (while cell
	(when (equal (tree-node-get-data (car cell)) child-data)
	  (if last-cell
	      (setcdr last-cell (cdr cell))
	    (tree-node-set-children node (cdr cell)))
	  (setcdr cell nil)
	  (tree-node-set-parent (car cell) nil)
	  (throw 'exit cell))
	(setq last-cell cell)
	(setq cell (cdr cell))))))

(defun tree-node-find-child-name(node child-name)
  (catch 'exit
    (dolist (child (tree-node-get-children node))
      (when (equal (tree-node-get-name child) child-name)
        (throw 'exit child)))))

;;; Tree node

(defconst tree-node-name 0)
(defconst tree-node-type 1)
(defconst tree-node-data 2)
(defconst tree-node-expanded 3)
(defconst tree-node-parent 4)
(defconst tree-node-children 5)
(defconst tree-node-expandable 6)

(defun tree-node-new(name type data &optional not-expandable parent)
  (let ((a (make-vector 7 nil)))
    (tree-node-set-name a name)
    (tree-node-set-type a type)
    (tree-node-set-data a data)
    (tree-node-set-expanded a nil)
    (tree-node-set-parent a parent)
    (tree-node-set-children a nil)
    (tree-node-set-expandable a (not not-expandable))
    a))

(defun tree-node-get-name(node)
  (aref node tree-node-name))

(defun tree-node-set-name(node name)
  (aset node tree-node-name name))

(defun tree-node-get-type(node)
  (aref node tree-node-type))

(defun tree-node-set-type(node type)
  (aset node tree-node-type type))

(defun tree-node-get-data(node)
  (aref node tree-node-data))

(defun tree-node-set-data(node data)
  (aset node tree-node-data data))

(defun tree-node-is-expanded(node)
  (aref node tree-node-expanded))

(defun tree-node-set-expanded(node expanded)
  (aset node tree-node-expanded expanded))

(defun tree-node-is-expandable(node)
  (aref node tree-node-expandable))

(defun tree-node-set-expandable(node expandable)
  (aset node tree-node-expandable expandable))

(defun tree-node-get-parent(node)
  (aref node tree-node-parent))

(defun tree-node-set-parent(node parent)
  (aset node tree-node-parent parent))

(defun tree-node-get-children(node)
  (aref node tree-node-children))

(defun tree-node-set-children(node children)
  (aset node tree-node-children children))

(defun tree-node-toggle-expanded(node)
  (tree-node-set-expanded node (not (tree-node-is-expanded node))))

(defun tree-node-get-depth(node)
  (let ((parent (tree-node-get-parent node)))
    (if parent
        (1+ (tree-node-get-depth parent))
      '0)))

(provide 'tree-buffer)

;;; tree-buffer.el ends here
