;;; tree-buffer.el --- functions for tree buffers

;; Copyright (C) 2000 by Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;; Maintainer: Jesper Nordenberg <mayhem@home.se>
;; Keywords: java, class, browser

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
;; Functions for tree buffers.
;;
;; This file is part of the ECB package which can be found at:
;; http://home.swipnet.se/mayhem/ecb.html

;;; Code:

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(defvar tree-buffer-root nil)
(defvar tree-buffer-nodes nil)
(defvar tree-buffer-key-map nil)
(defvar tree-buffer-indent nil)
(defvar tree-buffer-highlighted-node-data nil)
(defvar tree-buffer-menus nil)
(defvar tree-buffer-type-facer nil)
(defvar tree-buffer-expand-symbol-before nil)
(defvar tree-node-selected-fn nil)
(defvar tree-node-expanded-fn nil)
(defvar tree-buffer-highlight-overlay nil)

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
  (+ (tree-buffer-get-node-name-start-point node) (length (tree-node-get-name node))))

(defun tree-buffer-at-expand-symbol(node p)
  (if tree-buffer-expand-symbol-before
      (< p (1- (tree-buffer-get-node-name-start-point node)))
    (> p (tree-buffer-get-node-name-end-point node))))

(defun tree-buffer-select(mouse-button &optional shift-pressed)
  (let ((p (point))
	(node (tree-buffer-get-node-at-point)))
    (when node
      (if (and (tree-node-is-expandable node)
	       (tree-buffer-at-expand-symbol node (point)))
	  (progn
	    (when (not (tree-node-is-expanded node))
	      (funcall tree-node-expanded-fn node))
	    (when (tree-node-is-expandable node)
	      (tree-node-toggle-expanded node))
	    (tree-buffer-update))
	(funcall tree-node-selected-fn node mouse-button shift-pressed)))))

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
      'default)))

(defun tree-buffer-node-set-face(node face)
  "Sets the face for a node name."
  (put-text-property (tree-buffer-get-node-name-start-point node)
		     (tree-buffer-get-node-name-end-point node) 'face face))

;; (defun tree-buffer-remove-highlight()
;;   (when tree-buffer-highlighted-node-data
;;     (let ((node (tree-buffer-find-node-data tree-buffer-highlighted-node-data)))
;;       (when node
;; 	(tree-buffer-node-set-face node (tree-buffer-get-node-face node)))))
;;   (setq tree-buffer-highlighted-node-data nil))

;; (defun tree-buffer-highlight-node-data(node-data)
;;   (tree-buffer-remove-highlight)
;;   (setq tree-buffer-highlighted-node-data node-data)
;;   (when tree-buffer-highlighted-node-data
;;     (let ((node (tree-buffer-find-node-data tree-buffer-highlighted-node-data)))
;;       (when node
;; 	(tree-buffer-node-set-face node 'region)))))

;; Klaus: Now we use overlays to highlight current node in a tree-buffer. This
;; makes it easier to do same facing with the nodes itself and above all this
;; the facees of the node are always visible even if the node is highlighted
;; (useful e.g. if you show the sources in the ECB directory buffer, and if
;; you do some syntax highlighting in the method-buffer).
(defun tree-buffer-remove-highlight()
  (when tree-buffer-highlighted-node-data
    (let ((node (tree-buffer-find-node-data tree-buffer-highlighted-node-data)))
      (when node
        (delete-overlay tree-buffer-highlight-overlay))))
  (setq tree-buffer-highlighted-node-data nil))

(defun tree-buffer-highlight-node-data(node-data)
  (tree-buffer-remove-highlight)
  (setq tree-buffer-highlighted-node-data node-data)
  (when tree-buffer-highlighted-node-data
    (let ((node (tree-buffer-find-node-data tree-buffer-highlighted-node-data)))
      (when node
        (move-overlay tree-buffer-highlight-overlay
                      (tree-buffer-get-node-name-start-point node)
                      (tree-buffer-get-node-name-end-point node))))))
  
(defun tree-buffer-insert-text(text &optional facer)
  "Insert TEXT at point and faces it with FACER. FACER can be a face then the
text gets this face or it can be a function-symbol which is called to face the
inserted TEXT. Such a function gets two arguments: Point where TEXT has been
inserted and the TEXT itself"
  (let ((p (point)))
    (insert text)
    (put-text-property p (+ p (length text)) 'mouse-face 'highlight)
    (if facer
        (cond ((facep facer)
               (put-text-property p (+ p (length text)) 'face facer))
              ((functionp facer)
               (funcall facer p text))
              (t ;; do nothing, maybe the inserted text is already faced
               )))))
               
    
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

(defun tree-buffer-update()
  (let ((ws (window-start))
	(p (point)))
    (setq tree-buffer-nodes nil)
    (erase-buffer)
    (dolist (node (tree-node-get-children tree-buffer-root))
      (tree-buffer-add-node node 0))
    (tree-buffer-highlight-node-data tree-buffer-highlighted-node-data)
    (goto-char p)
    (set-window-start (selected-window) ws)))

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

(defun tree-buffer-create(name node-selected-fn node-expanded-fn menus tr-lines
			       &optional type-facer expand-symbol-before)
  "Creates a new tree buffer with
NAME: Name of the buffer
NODE-SELECTED-FN: Function to call if a node has been selected
NODE-EXPANDED-FN: Function to call if a node has been expanded
MENUS: Nil or a list of one or two conses, each cons for a node-type \(0 or 1)
       Example: \(\(0 . menu-for-type-0) \(1 . menu-for-type-1)). The cdr of a
       cons must be a menu.
TR-LINES: Should lines in this tree buffer be truncated \(not nil)
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
  (set-buffer (get-buffer-create name))

  (make-local-variable 'truncate-lines)
  (make-local-variable 'tree-buffer-key-map)
  (make-local-variable 'tree-buffer-root)
  (make-local-variable 'tree-buffer-nodes)
  (make-local-variable 'tree-buffer-indent)
  (make-local-variable 'tree-node-selected-fn)
  (make-local-variable 'tree-node-expanded-fn)
  (make-local-variable 'tree-node-update-fn)
  (make-local-variable 'tree-buffer-highlighted-node-data)
  (make-local-variable 'tree-buffer-menus)
  (make-local-variable 'tree-buffer-type-facer)
  (make-local-variable 'tree-buffer-expand-symbol-before)
  (make-local-variable 'tree-buffer-highlight-overlay)
  
  (setq truncate-lines tr-lines)
  (setq tree-buffer-key-map (make-sparse-keymap))
  (setq tree-node-selected-fn node-selected-fn)
  (setq tree-node-expanded-fn node-expanded-fn)
  (setq tree-buffer-indent 2)
  (setq tree-buffer-highlighted-node-data nil)
  (setq tree-buffer-menus menus)
  (setq tree-buffer-root (tree-node-new "root" 0 "root"))
  (setq tree-buffer-type-facer type-facer)
  (setq tree-buffer-expand-symbol-before expand-symbol-before)
  (setq tree-buffer-highlight-overlay (make-overlay 1 1))
  (overlay-put tree-buffer-highlight-overlay 'face 'region)

  (define-key tree-buffer-key-map "\C-m"
    '(lambda()
       (interactive)
       (tree-buffer-select 0)))
  (define-key tree-buffer-key-map [tab]
    '(lambda()
       (interactive)
       (let ((node (tree-buffer-get-node-at-point)))
	 (when (tree-node-is-expandable node)
	   (when (not (tree-node-is-expanded node))
	     (funcall tree-node-expanded-fn node))
	   (when (tree-node-is-expandable node)
	     (tree-node-toggle-expanded node))
	   (tree-buffer-update)))))

  ;; mouse-1
  (define-key tree-buffer-key-map [down-mouse-1]
    '(lambda(e)
       (interactive "e")
       (mouse-set-point e)
       (tree-buffer-select 0)))
  
  (define-key tree-buffer-key-map [S-down-mouse-1]
    '(lambda(e)
       (interactive "e")
       (mouse-set-point e)
       (tree-buffer-select 0 t)))

  (define-key tree-buffer-key-map [drag-mouse-1] '(lambda()(interactive)))
  (define-key tree-buffer-key-map [mouse-1] '(lambda()(interactive)))
  (define-key tree-buffer-key-map [double-mouse-1] '(lambda()(interactive)))
  (define-key tree-buffer-key-map [triple-mouse-1] '(lambda()(interactive)))

  ;; mouse-2
  (define-key tree-buffer-key-map [down-mouse-2]
    '(lambda(e)
       (interactive "e")
       (mouse-set-point e)
       (tree-buffer-select 1)))

  (define-key tree-buffer-key-map [S-down-mouse-2]
    '(lambda(e)
       (interactive "e")
       (mouse-set-point e)
       (tree-buffer-select 1 t)))

  (define-key tree-buffer-key-map [mouse-2] '(lambda()(interactive)))
  (define-key tree-buffer-key-map [double-mouse-2] '(lambda()(interactive)))
  (define-key tree-buffer-key-map [triple-mouse-2] '(lambda()(interactive)))

  ;; mouse-3
  (define-key tree-buffer-key-map [down-mouse-3] 'tree-buffer-show-menu)
  (define-key tree-buffer-key-map [mouse-3] '(lambda()(interactive)))
  (define-key tree-buffer-key-map [double-mouse-3] '(lambda()(interactive)))
  (define-key tree-buffer-key-map [triple-mouse-3] '(lambda()(interactive)))
  (use-local-map tree-buffer-key-map))

; (defun tree-insert-line(line)
;   (let ((p (point)))
;     (insert line "\n")
;     (put-text-property p (+ p (length line)) 'mouse-face 'highlight)))

; (defun delete-line(linenr)
;   (goto-line (+ linenr 1))
;   (let ((p (point)))
;     (goto-line (1+ linenr))
;     (delete-region (point) p)))

; (defun tree-remove-line(linenr)
;   (setcdr (nthcdr (1- pos) tree-buffer-nodes) (nthcdr (1+ pos) tree-buffer-nodes))
;   (delete-line linenr))

; (defun tree-remove-node(node)
;   (let ((pos (find tree-buffer-nodes node)))
;     (if (> pos -1)
; 	(tree-remove-line pos))))
      
  
;   (tree-insert-line (tree-item-name item) (tree-item-type item))
;   (setq tree-items (append tree-items item)))

;;; Tree node

(defun tree-node-add-child(node child)
  (tree-node-set-children node (list-append (tree-node-get-children node) (list child)))
  (tree-node-set-parent child node))

(defun tree-node-remove-child(node child)
  (tree-node-set-parent child nil)
  (tree-node-set-children node
			  (delq child (tree-node-get-children node))))

(defun tree-node-find-child-data(node child-data)
  (catch 'exit
    (dolist (child (tree-node-get-children node))
      (when (equal (tree-node-get-data child) child-data)
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

;;; dirtest.el ends here
