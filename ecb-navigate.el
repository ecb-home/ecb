;;; ecb-navigate.el --- 

;; Copyright (C) 2001 by Free Software Foundation, Inc.

;; Author:  <Sune Mangs@MAYHEM>
;; Keywords: 

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(require 'eieio)

;;====================================================
;; 
;;====================================================

(defclass ecb-dlist-node ()
  ((previous :initform nil); :protection :private)
   (next :initform nil); :protection :private)
   (data :initarg :data :initform nil); :protection :private)
   )
  "A node in a double linked list."
  )

(defun ecb-dlist-node-new (data)
  (ecb-dlist-node "node" :data data))

(defmethod ecb-get-data ((node ecb-dlist-node))
  (oref node data))

(defmethod ecb-get-next ((node ecb-dlist-node))
  (oref node next))

(defmethod ecb-get-previous ((node ecb-dlist-node))
  (oref node previous))

(defmethod ecb-set-data ((node ecb-dlist-node) data)
  (oset node data data))

(defmethod ecb-set-next ((node ecb-dlist-node) next)
  (let ((old-next (ecb-get-next node)))
    (when old-next
      (oset old-next previous nil))
    (oset node next next)
    (when next
      (ecb-set-previous next nil)
      (oset next previous node))))

(defmethod ecb-set-previous ((node ecb-dlist-node) previous)
  (let ((old-previous (ecb-get-previous node)))
    (when old-previous
      (oset old-previous next nil))
    (oset node previous previous)
    (when previous
      (ecb-set-next previous nil)
      (oset previous next node))))


;;====================================================
;; 
;;====================================================

(defclass ecb-nav-history-item ()
  ((pos :initarg :pos :initform 0); :protection :private)
   (window-start :initarg :window-start :initform 0); :protection :private)
   )
  )
  
(defmethod ecb-nav-set-pos ((item ecb-nav-history-item) pos)
  (oset item pos pos))

(defmethod ecb-nav-set-window-start ((item ecb-nav-history-item) point)
  (oset item window-start point))

(defmethod ecb-nav-get-pos ((item ecb-nav-history-item))
  (oref item pos))

(defmethod ecb-nav-get-window-start ((item ecb-nav-history-item))
  (oref item window-start))

(defmethod ecb-nav-to-string ((item ecb-nav-history-item))
  (concat (int-to-string (ecb-nav-get-pos item)) ":"
	  (int-to-string (ecb-nav-get-window-start item))))

(defmethod ecb-nav-save ((item ecb-nav-history-item))
  )


;;====================================================
;; 
;;====================================================

(defclass ecb-nav-token-history-item (ecb-nav-history-item)
  ((token :initarg :token :initform nil); :protection :private)
   (narrow :initarg :narrow :initform nil); :protection :private)
   )
  )

(defun ecb-nav-token-history-item-new (token &optional narrow)
  (ecb-nav-token-history-item (semantic-token-name token)
			  :token token
			  :narrow narrow))

(defmethod ecb-nav-get-token ((item ecb-nav-token-history-item))
  (oref item token))

(defmethod ecb-nav-get-narrow ((item ecb-nav-token-history-item))
  (oref item narrow))

(defmethod ecb-nav-goto ((item ecb-nav-token-history-item))
  (let ((token (ecb-nav-get-token item)))
    (set-window-buffer (selected-window) (semantic-token-buffer token))
    (widen)
    (goto-char (semantic-token-start token))
    (when (ecb-nav-get-narrow item)
      (narrow-to-region (tree-buffer-line-beginning-pos)
			(semantic-token-end token)))
    (goto-char (+ (semantic-token-start token) (ecb-nav-get-pos item)))
    (set-window-start (selected-window)
		      (+ (semantic-token-start token)
			 (ecb-nav-get-window-start item)))))

(defmethod ecb-nav-save ((item ecb-nav-token-history-item))
  (let ((token (ecb-nav-get-token item)))
    (when (semantic-token-start token)
      (ecb-nav-set-pos item (- (point) (semantic-token-start token)))
      (ecb-nav-set-window-start item (- (window-start)
					(semantic-token-start token))))))

(defmethod ecb-nav-to-string ((item ecb-nav-token-history-item))
  (concat (semantic-token-name (ecb-nav-get-token item)) ":" (call-next-method)))


;;====================================================
;; 
;;====================================================

(defclass ecb-nav-file-history-item (ecb-nav-history-item)
  ((file :initarg :file :initform ""); :protection :private)
   )
  )

(defun ecb-nav-file-history-item-new ()
  (let ((item (ecb-nav-file-history-item (buffer-file-name)
	       :file (buffer-file-name))))
    (ecb-nav-set-pos item (point))
    (ecb-nav-set-window-start item
			  (window-start (get-buffer-window (current-buffer))))
    item))

(defmethod ecb-nav-get-file ((item ecb-nav-file-history-item))
  (oref item file))

(defmethod ecb-nav-set-file ((item ecb-nav-file-history-item) file)
  (oset item file file))

(defmethod ecb-nav-save ((item ecb-nav-file-history-item))
  (ecb-nav-set-pos item (point))
  (ecb-nav-set-window-start item (window-start))
  (ecb-nav-set-file item (buffer-file-name)))

(defmethod ecb-nav-goto ((item ecb-nav-file-history-item))
  (find-file (ecb-nav-get-file item))
  (widen)
  (goto-char (ecb-nav-get-pos item))
  (set-window-start (selected-window) (ecb-nav-get-window-start item)))
  
(defmethod ecb-nav-to-string ((item ecb-nav-file-history-item))
  (concat (ecb-nav-get-file item) ":" (call-next-method)))


;;====================================================
;; 
;;====================================================

(defvar ecb-nav-first-node nil)
(setq ecb-nav-first-node (ecb-dlist-node-new (ecb-nav-history-item "First item")))

(defvar ecb-nav-current-node nil)
(setq ecb-nav-current-node ecb-nav-first-node)

(defun ecb-nav-jump-to-token (file token &optional narrow)
  (ecb-nav-save-current)
  (find-file file)
  (ecb-nav-add-item (ecb-nav-token-history-item token narrow)))

(defun ecb-nav-jump-to-file (file)
  (ecb-nav-save-current)
  (find-file file)
  (ecb-nav-add-item (ecb-nav-file-history-item file)))

(defun ecb-nav-add-item (item)
  (let*	((node (ecb-dlist-node-new item)))
    (ecb-set-next node (ecb-get-next ecb-nav-current-node))
    (ecb-set-next ecb-nav-current-node node)
    (setq ecb-nav-current-node node)))

(defun ecb-nav-save-current ()
  (ecb-nav-save (ecb-get-data ecb-nav-current-node)))

(defun ecb-nav-goto-next ()
  "Go forward in the navigator history list."
  (interactive)
  (ecb-nav-goto--internal (ecb-get-next ecb-nav-current-node)))

(defun ecb-nav-goto-previous ()
  "Go back in the navigator history list."
  (interactive)
  (ecb-nav-goto--internal (ecb-get-previous ecb-nav-current-node)))

(defun ecb-nav-dump-history ()
  (interactive)
  (ecb-nav-dump-history--internal ecb-nav-first-node))

(defun ecb-nav-dump-history--internal (node)
  (when node
    (insert (ecb-nav-to-string (ecb-get-data node)) "\n")
    (ecb-nav-dump-history--internal (ecb-get-next node))))

(defun ecb-nav-goto--internal (node)
  (if (or (not node) (eq ecb-nav-first-node node))
      (message "No more history items!")
    (ecb-nav-save-current)
    (setq ecb-nav-current-node node)
    (ecb-nav-goto (ecb-get-data node))))

(provide 'ecb-navigate)

;;; ecb-navigate.el ends here
