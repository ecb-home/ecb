;;; tree-buffer.el --- functions for tree buffers

;; Copyright (C) 2000, 2001 Jesper Nordenberg

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
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
;; http://ecb.sourceforge.net

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;; $Id: tree-buffer.el,v 1.116 2003/07/11 15:53:31 berndl Exp $

;;; Code:

(eval-when-compile
  (require 'silentcomp))

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

(defconst tree-buffer-running-xemacs
  (string-match "XEmacs\\|Lucid" emacs-version))
(defconst tree-buffer-running-emacs-21
  (and (not tree-buffer-running-xemacs)
       (> emacs-major-version 20)))

;; XEmacs stuff
(silentcomp-defun button-release-event-p)
(silentcomp-defun button-press-event-p)
(silentcomp-defun event-key)
(silentcomp-defun extent-end-position)
(silentcomp-defun event-glyph-extent)
(silentcomp-defun event-over-glyph-p)
(silentcomp-defun display-message)
(silentcomp-defun clear-message)
;; Emacs
(silentcomp-defvar message-log-max)
(silentcomp-defvar message-truncate-lines)
(silentcomp-defvar track-mouse)
(silentcomp-defvar special-event-map)
(silentcomp-defun posn-window)
(silentcomp-defun event-start)
(silentcomp-defun posn-point)
(silentcomp-defun event-basic-type)
(silentcomp-defun display-graphic-p)

(defconst tree-buffer-images-can-be-used
  (and (or (fboundp 'defimage)
           (fboundp 'make-image-specifier))
       (if (fboundp 'display-graphic-p)
           (display-graphic-p)
         window-system)))


(if tree-buffer-running-xemacs
    ;; XEmacs
    (progn
      (defalias 'tree-buffer-line-beginning-pos 'point-at-bol)
      (defalias 'tree-buffer-line-end-pos 'point-at-eol)
      (defalias 'tree-buffer-window-display-height 'window-displayed-height)
      (defun tree-buffer-event-to-key (event)
        (cond ((button-release-event-p event)
               'mouse-release)
              ((button-press-event-p event)
               'mouse-press)
              (t
               ;; the ignore-errors is a little hack because i don't no all
               ;; events of XEmacs so sometimes event-key produces a
               ;; wrong-type-argument error.
               (ignore-errors (event-key event)))))
      (defalias 'tree-buffer-event-window 'event-window)
      (defalias 'tree-buffer-event-point 'event-point)
      ;; stolen from dframe.el of the speedbar-library.
      (defun tree-buffer-mouse-set-point (e)
        "Set POINT based on event E. Handles clicking on images in XEmacs."
        (if (and (fboundp 'event-over-glyph-p) (event-over-glyph-p e))
            ;; We are in XEmacs, and clicked on a picture
            (let ((ext (event-glyph-extent e)))
              ;; This position is back inside the extent where the
              ;; junk we pushed into the property list lives.
              (if (extent-end-position ext)
                  (progn
                    (mouse-set-point e)
                    (goto-char (1- (extent-end-position ext))))
                (mouse-set-point e)))
          ;; We are not in XEmacs, OR we didn't click on a picture.
          (mouse-set-point e)))
      (require 'overlay)
      )
  ;; GNU Emacs
  ;; needed to handle correct mouse avoidance
  (require 'avoid)
  (defalias 'tree-buffer-line-beginning-pos 'line-beginning-position)
  (defalias 'tree-buffer-line-end-pos 'line-end-position)
  (defun tree-buffer-window-display-height (&optional window)
    (1- (window-height window)))
  (defun tree-buffer-event-window (event)
    (posn-window (event-start event)))
  (defun tree-buffer-event-point (event)
    (posn-point (event-start event)))
  (defalias 'tree-buffer-mouse-set-point 'mouse-set-point)
  (defun tree-buffer-event-to-key (event)
    (let ((type (event-basic-type event)))
      (cond ((or (equal type 'mouse-1)
                 (equal type 'mouse-2)
                 (equal type 'mouse-3))
             'mouse-release)
            ((or (equal type 'down-mouse-1)
                 (equal type 'down-mouse-2)
                 (equal type 'down-mouse-3))
             'mouse-press)
            (t
             (event-basic-type event))))))

;; tree-buffer local variables
(defvar tree-buffer-root nil)

(defvar tree-buffer-nodes nil
  "Contains all the visible nodes in the buffer in top-to-bottom order. Each
item in this list is a cons pair of the displayed node name and the node. Note
that the displayed node name can be truncated and therefore different from the
node name.")

(defvar tree-buffer-frame nil)
(defvar tree-buffer-key-map nil)
(defvar tree-buffer-indent nil)
(defvar tree-buffer-highlighted-node-data nil)
(defvar tree-buffer-menus nil)
(defvar tree-buffer-menu-titles nil)
(defvar tree-buffer-type-facer nil)
(defvar tree-buffer-expand-symbol-before nil)
(defvar tree-buffer-is-click-valid-fn nil)
(defvar tree-node-selected-fn nil)
(defvar tree-node-expanded-fn nil)
(defvar tree-node-mouse-over-fn nil)
(defvar tree-node-data-equal-fn nil)
(defvar tree-buffer-highlight-overlay nil)
(defvar tree-buffer-general-face nil)
(defvar tree-buffer-general-overlay nil)
(defvar tree-buffer-incr-searchpattern nil)
(defvar tree-buffer-last-incr-searchpattern nil)
(defvar tree-buffer-incr-search nil)
(defvar tree-buffer-hor-scroll-step nil)
(defvar tree-buffer-expand-collapse-tokens nil)

;; tree-buffer-local data-storage with get- and set-function
(defvar tree-buffer-data-store nil)
(defun tree-buffer-set-data-store (data)
  (setq tree-buffer-data-store data))
(defun tree-buffer-get-data-store ()
  tree-buffer-data-store)

;; tree-buffer global variables
(defvar tree-buffers nil)
(defvar tree-buffer-saved-mouse-movement-fn nil)
(defvar tree-buffer-saved-track-mouse nil)
(defvar tree-buffer-track-mouse-timer nil)
(defvar tree-buffer-track-mouse-idle-delay 0.2
  "After this idle-time of Emacs `tree-buffer-do-mouse-tracking' is called if
mouse-tracking is activated by `tree-buffer-activate-mouse-tracking'")
(defvar tree-buffer-old-mouse-avoidance-mode
  (if (null mouse-avoidance-mode) 'none mouse-avoidance-mode))

(defvar tree-buffer-syntax-table nil
  "Syntax-table used in a tree-buffer.")

(if tree-buffer-syntax-table
    nil
  (setq tree-buffer-syntax-table (make-syntax-table))
  ;; turn off paren matching around here.
  (modify-syntax-entry ?\' " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\" " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\( " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\) " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\{ " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\} " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\[ " " tree-buffer-syntax-table)
  (modify-syntax-entry ?\] " " tree-buffer-syntax-table))


(defun tree-buffer-nolog-message (&rest args)
  "Works exactly like `message' but does not log the message"
  (let ((msg (cond ((or (null args)
                        (null (car args)))
                    nil)
                   ((null (cdr args))
                    (car args))
                   (t
                    (apply 'format args)))))
    ;; Now message is either nil or the formated string.
    (if tree-buffer-running-xemacs
        ;; XEmacs way of preventing log messages.
        (if msg
            (display-message 'no-log msg)
          (clear-message 'no-log))
      ;; Emacs way of preventing log messages.
      (let ((message-log-max nil)
            (message-truncate-lines nil))
        (if msg
            (message "%s" msg)
          (message nil))))
    msg))

(defun tree-buffer-get-node-name-start-column (node)
  "Returns the buffer column where the name of the node starts."
  (+ (tree-buffer-get-node-indent node)
     (if (and tree-buffer-expand-symbol-before
              (tree-node-is-expandable node))
         4 0)))

(defun tree-buffer-get-node-name-start-point (name node)
  "Returns the buffer point where the name of the node starts."
  (let ((linenr (tree-buffer-find-node node)))
    (when linenr
      (goto-line linenr)
      (beginning-of-line)
      (+ (point) (tree-buffer-get-node-name-start-column node)))))

(defun tree-buffer-get-node-name-end-point (name node)
  "Returns the buffer point where the name of the node ends."
  (+ (tree-buffer-get-node-name-start-point name node)
     (length name)))

(defun tree-buffer-at-expand-symbol (name node p)
  (if tree-buffer-expand-symbol-before
      (< p (1- (tree-buffer-get-node-name-start-point name node)))
    (> p (tree-buffer-get-node-name-end-point name node))))

(defun tree-buffer-select (mouse-button shift-pressed control-pressed)
  "If the callback-function in `tree-buffer-is-click-valid-fn' returns nil
then nothing is done. Otherwise: If the node is expandable and the node is not
expanded then the callback-function in `tree-node-expanded-fn' is called with
the node, the clicked MOUSE-BUTTON \(1 for mouse-1, 2 for mouse-2, 0 for no
mouse-button but a key like RET or TAB), SHIFT-PRESSED and CONTROL-PRESSED
informations and the name of the tree-buffer as arguments. If the node is not
expandable then the callback-function in `tree-node-selected-fn' is called
with the same arguments as `tree-node-expanded-fn'."
  (unless (not (equal (selected-frame) tree-buffer-frame))
    (when (and tree-buffer-is-click-valid-fn
               (funcall tree-buffer-is-click-valid-fn mouse-button
                        shift-pressed control-pressed (buffer-name)))
      (let* ((p (point))
	     (name-node (tree-buffer-get-name-node-at-point))
	     (name (car name-node))
	     (node (cdr name-node)))
        (when node
          (if (and (tree-node-is-expandable node)
                   (tree-buffer-at-expand-symbol name node p)
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
            (setq tree-buffer-incr-searchpattern "")
            (when tree-node-selected-fn
              (funcall tree-node-selected-fn node mouse-button
                       shift-pressed control-pressed (buffer-name))))))
      )))

(defun tree-buffer-get-node-at-point (&optional p)
  (save-excursion
    (if p (goto-char p))
    (let ((linenr (+ (count-lines 1 (point)) (if (= (current-column) 0) 0 -1))))
      (cdr (nth linenr tree-buffer-nodes)))))

(defun tree-buffer-get-name-node-at-point (&optional p)
  (save-excursion
    (if p (goto-char p))
    (let ((linenr (+ (count-lines 1 (point)) (if (= (current-column) 0) 0 -1))))
      (nth linenr tree-buffer-nodes))))

(defun tree-buffer-get-node-indent (node)
  (* tree-buffer-indent (1- (tree-node-get-depth node))))

(defun tree-buffer-node-data-equal-p (node-data-1 node-data-2)
  (and node-data-1 node-data-2
       ;; if this comparison-function runs into an error we handle this as
       ;; non-equality!
       (ignore-errors
         (funcall tree-node-data-equal-fn node-data-1 node-data-2))))

(defun tree-buffer-find-node-data (node-data)
  (catch 'exit
    (dolist (node tree-buffer-nodes)
      (when (tree-buffer-node-data-equal-p (tree-node-get-data (cdr node))
                                           node-data)
        (throw 'exit (cdr node))))))

(defun tree-buffer-find-name-node-data (node-data &optional start-node)
  (catch 'exit
    (let ((node-list (if (or (not start-node)
                             (eq start-node (tree-buffer-get-root)))
                         tree-buffer-nodes
                       ;; because tree-buffer-nodes is a list of conses with
                       ;; car is the node-name and cdr is the node itself we
                       ;; must first create such a cons for START-NODE!
                       (or (member (cons (tree-node-get-name start-node)
                                         start-node)
                                   tree-buffer-nodes)
                           tree-buffer-nodes))))
      (dolist (node node-list)
        (when (tree-buffer-node-data-equal-p (tree-node-get-data (cdr node))
                                             node-data)
          (throw 'exit node))))))

(defun tree-buffer-find-node (node)
  (catch 'exit
    (let ((linenr 1))
      (dolist (node2 tree-buffer-nodes)
        (when (eq node (cdr node2))
          (throw 'exit linenr))
        (setq linenr (1+ linenr))))))

(defun tree-buffer-get-node-facer (node)
  (let ((facer (cdr (assoc (tree-node-get-type node) tree-buffer-type-facer))))
    (if facer
        facer
      nil)))

(defun tree-buffer-pos-hor-visible-p (pos window)
  "Returns non nil if POS is horizontal visible otherwise nil."
  (save-excursion
    (goto-char pos)
    (and (>= (- (current-column) (window-hscroll window)) 0)
         (< (- (current-column) (window-hscroll window))
            (window-width window)))))

(defun tree-buffer-hscroll (amount)
  (ignore-errors
    (let ((current-prefix-arg amount))
      (call-interactively 'scroll-left))))

;; Stolen from dframe.el from the speedbar-library
;; XEmacs: this can be implemented using modeline keymaps, but there
;; is no use, as we have horizontal scrollbar (as the docstring
;; hints.)
(defun tree-buffer-mouse-hscroll (e)
  "Read a mouse event E from the mode line, and horizontally scroll.
If the mouse is being clicked on the far left, or far right of the
mode-line.  This is only useful for non-XEmacs"
  (interactive "e")
  (let* ((x-point (car (nth 2 (car (cdr e)))))
	 (pixels-per-10-col (/ (* 10 (frame-pixel-width))
			       (frame-width)))
	 (click-col (1+ (/ (* 10 x-point) pixels-per-10-col)))
	 )
    (cond ((< click-col 3)
	   (tree-buffer-hscroll (- tree-buffer-hor-scroll-step)))
	  ((> click-col (- (window-width) 4))
	   (tree-buffer-hscroll tree-buffer-hor-scroll-step))
          (t (tree-buffer-nolog-message
	      "Click on the edge of the modeline to scroll left/right")))
    ))

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
    (if (not tree-buffer-running-xemacs)
        (ignore-errors (tree-buffer-hscroll -1000)))
    ;; first make point best visible, means display node in the middle of the
    ;; window if possible (if there are enough lines before/after the node).
    (when (not (pos-visible-in-window-p node-point window))
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
          ;; if the current node is not already displayed in the first line of
          ;; the window (= condition 1) and if not all of it큦 children are
          ;; visible in the window then we can do some optimization.
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
                                    (tree-buffer-line-beginning-pos)))))))
    ))

;; Klaus: Now we use overlays to highlight current node in a tree-buffer. This
;; makes it easier to do some facing with the nodes itself and above all this
;; the faces of the node are always visible even if the node is highlighted
;; (useful e.g. if you show the sources in the ECB directory buffer, and if
;; you do some syntax highlighting in the method-buffer).
(defun tree-buffer-remove-highlight ()
  (when tree-buffer-highlighted-node-data
    (let ((node (tree-buffer-find-node-data tree-buffer-highlighted-node-data)))
      (when node
        (delete-overlay tree-buffer-highlight-overlay))))
  (setq tree-buffer-highlighted-node-data nil))

(defun tree-buffer-highlight-node-data (node-data &optional start-node
                                                  dont-make-visible)
  "Highlights in current tree-buffer the node which has as data NODE-DATA. If
START-NODE is nil or equal to the root-node then all nodes of current
tree-buffer are searched from beginning until the node with data NODE-DATA has
been found otherwise the search starts with START-NODE. If DONT-MAKE-VISIBLE
is true then no tree-buffer recentering has been done to make this node
visible.

If either NODE-DATA is nil or if the node belonging to NODE-DATA can not be
found because it is invisible \(probably because its parent-node is not
expanded) then no highlighting takes place but the existing highlighting is
removed and nil is returned. Otherwise the node is highlighted and not nil is
returned."
  (if node-data
      (let* ((name-node (tree-buffer-find-name-node-data node-data start-node))
	     (name (car name-node))
	     (node (cdr name-node))
	     (w (get-buffer-window (current-buffer))))
        (if (null node)
            (progn
              ;; node can not be found because maybe the node is a subnode and
              ;; it큦 parent is not expanded --> then there is no node for
              ;; NODE-DATA; therefore we must remove the highlighting
              (tree-buffer-remove-highlight)
              nil)
          (setq tree-buffer-highlighted-node-data node-data)
          (save-excursion
            (move-overlay tree-buffer-highlight-overlay
                          (tree-buffer-get-node-name-start-point name node)
                          (tree-buffer-get-node-name-end-point name node)))
          (when (not dont-make-visible)
            ;; make node visible if not and optimize the windows display for
            ;; the node.
            (tree-buffer-recenter node w))
          ;; we have highlighted the node wo we return not nil.
          t))
    (tree-buffer-remove-highlight)
    nil))

(defun tree-buffer-help-echo-fn (win obj pos)
  "This function is the value of the `help-echo' property of each
tree-node. This is only used with GNU Emacs 21!"
  (let* ((window win)
         (position pos)
         (buffer (window-buffer window))
         node)
    (save-excursion
      (set-buffer buffer)
      (setq node (tree-buffer-get-node-at-point position))
      (and tree-node-mouse-over-fn
           node
           (funcall tree-node-mouse-over-fn node window 'no-print)))))


(defun tree-buffer-insert-text (text &optional facer help-echo)
  "Insert TEXT at point and faces it with FACER. FACER can be a face then the
text gets this face or it can be a function-symbol which is called to face the
inserted TEXT. Such a function gets two arguments: Point where TEXT has been
inserted and the TEXT itself"
  (when (stringp text)
    (let ((p (point)))
      (insert text)
      (put-text-property p (+ p (length text)) 'mouse-face 'highlight)
      (if (and help-echo (not tree-buffer-running-xemacs))
          (put-text-property p (+ p (length text)) 'help-echo
                             'tree-buffer-help-echo-fn))
      (if facer
          (if (functionp facer)
              (funcall facer p text)
            (put-text-property p (+ p (length text)) 'face facer))))))


(defun tree-buffer-make-image-icon-maybe (symbol-str)
  "Add an image button based on SYMBOL-STR to SYMBOL-STR which is a string. If
SYMBOL-STR is unknown, do nothing. If we have an image-symbol associated with
it, use that image-symbol. Symbol-strings are associated with image-symbols
via `tree-buffer-expand-collapse-tokens'.

Always return SYMBOL-STR."
  (if symbol-str
      (tree-buffer-add-image-icon-maybe
       symbol-str
       (cdr (assoc symbol-str tree-buffer-expand-collapse-tokens)))))
      

(defun tree-buffer-add-image-icon-maybe (symbol-str image-icon)
  "Add IMAGE-ICON to SYMBOL-STR which is a string. If IMAGE-ICON-SYMBOL is not
nil and has a value \(which must be an image in the sense of \(X)Emacs) then
add this image to SYMBOL-STR otherwise do nothing. Always return SYMBOL-STR."
  (when tree-buffer-images-can-be-used
	;; Regular images (created with `insert-image' are intangible
	;; which (I suppose) make them more compatible with XEmacs 21.
	;; Unfortunatly, there is a giant pile o code dependent on the
	;; underlying text.  This means if we leave it tangible, then I
	;; don't have to change said giant piles o code.
	(if (and image-icon (symbol-value image-icon))
	    (if (featurep 'xemacs)
		(add-text-properties (length symbol-str) 0
				     (list 'end-glyph (symbol-value image-icon)
					   'rear-nonsticky (list 'display)
					   'invisible t
					   'detachable t)
                                     symbol-str)
	      (add-text-properties 0 (length symbol-str)
				   (list 'display (symbol-value image-icon)
					 'rear-nonsticky (list 'display))
                                   symbol-str))))
  symbol-str)

(defun tree-buffer-add-node (node depth)
  (let* ((ww (window-width))
	 (name (tree-node-get-name node))
	 (width (+ (* depth tree-buffer-indent)
		   (length name)
		   (if (tree-node-is-expandable node) 4 0)))
         (expand-collapse-token
          (if (tree-node-is-expanded node)
              (car (nth 1 tree-buffer-expand-collapse-tokens))
            (car (nth 0 tree-buffer-expand-collapse-tokens)))))
    ;; Truncate name if necessary
    (when (>= width ww)
      (if (eq 'beginning (tree-node-get-shorten-name node))
	  (setq name (concat "..." (substring name (+ (if tree-buffer-running-xemacs 5 4)
                                                      (- width ww)))))
	(if (and (not tree-buffer-expand-symbol-before)
		 (tree-node-is-expandable node)
		 (eq 'end (tree-node-get-shorten-name node)))
	    (setq name (concat (substring name 0 (- (+ (if tree-buffer-running-xemacs 5 4)
                                                       (- width ww))))
                               "...")))))
    (insert (make-string (* depth tree-buffer-indent) ? ))
    (when (and tree-buffer-expand-symbol-before
	       (tree-node-is-expandable node))
      (tree-buffer-insert-text 
       (tree-buffer-make-image-icon-maybe expand-collapse-token))
      (insert " "))
    (tree-buffer-insert-text name (tree-buffer-get-node-facer node) t)
    (when (and (not tree-buffer-expand-symbol-before)
	       (tree-node-is-expandable node))
      (insert " ")
      (tree-buffer-insert-text 
       (tree-buffer-make-image-icon-maybe expand-collapse-token)))
    (insert "\n")
    (setq tree-buffer-nodes (append tree-buffer-nodes (list (cons name node))))
    (if (tree-node-is-expanded node)
	(dolist (node (tree-node-get-children node))
	  (tree-buffer-add-node node (1+ depth))))))



(defun tree-node-count-subnodes-to-display (node)
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

(defun tree-buffer-build-tree-buffer-nodes ()
  "Rebuild the variable `tree-buffer-nodes' from the current children of
`tree-buffer-root'."
  (setq tree-buffer-nodes nil)
  (dolist (node (tree-node-get-children tree-buffer-root))
    (tree-buffer-add-node node 0)))

(defun tree-buffer-update (&optional node)
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
    (erase-buffer)
    (tree-buffer-build-tree-buffer-nodes)
    (when tree-buffer-general-face
      (move-overlay tree-buffer-general-overlay (point-min) (point-max)))
    (tree-buffer-highlight-node-data tree-buffer-highlighted-node-data)
    (goto-char p)
    (set-window-start w ws)
    ;; let큦 optimize the display of the expanded node NODE and it큦 children.
    (when node
      (tree-buffer-recenter node w))))

(defun tree-buffer-scroll (point window-start)
  "Scrolls current tree-buffer. The window will start at WINDOW-START and
point will stay on POINT."
  (goto-char point)
  (set-window-start (get-buffer-window (current-buffer)) window-start))


(defun tree-buffer-expand-nodes (level
                                 &optional expand-pred-fn collapse-pred-fn)
  "Set the expand level of the nodes in current tree-buffer.

LEVEL specifies precisely which level of nodes should be expanded. LEVEL
means the indentation-level of the nodes.

A LEVEL value X means that all nodes with an indentation-level <= X are
expanded and all other are collapsed. A negative LEVEL value means all visible
nodes are collapsed.

Nodes which are not indented have indentation-level 0!

This function expands all nodes with level <= LEVEL, so the subnodes of these
nodes get visible and collapses all their \(recursive) subnodes with
indentation-level > LEVEL.

If a node has to be expanded then first the `tree-node-expanded-fn' of current
tree-buffer \(see `tree-buffer-create') is called with the argument-values
\[node 0 nil nil \(buffer-name)\].

This function gets two optional function-arguments which are called to test if
a node should be excluded from expanding or collapsing; both functions are
called with two arguments, where the first one is the expandable/collapsable
node and the second one is the current level of indentation of this node:
EXPAND-PRED-FN is called if a node has to be expanded and must return nil if
this node should not be expanded even if its indentation level is <= LEVEL and
COLLAPSE-PRED-FN is called analogous for a node which has to be collapsed and
must return nil if the node should not be collapsed even if its indentation
level is > then LEVEL.

Examples:
- LEVEL = 0 expands only nodes which have no indentation itself.
- LEVEL = 2 expands nodess which are either not indented or indented once or
  twice."
  (dolist (node (tree-node-get-children tree-buffer-root))
    (tree-buffer-expand-node node 0 level
                             expand-pred-fn collapse-pred-fn))
  (tree-buffer-update))

(defun tree-buffer-expand-node (node current-level level
                                     expand-pred-fn collapse-pred-fn)
  "Expand NODE if CURRENT-LEVEL \(the indentation-level of NODE) <= LEVEL or
collapses NODE if CURRENT-LEVEL > LEVEL. Do this recursive for subnodes of
NODE with incremented CURRENT-LEVEL. For EXPAND-PRED-FN and COLLAPSE-PRED-FN
see `tree-buffer-expand-nodes'."
  (when (tree-node-is-expandable node)
    (when (and tree-node-expanded-fn
               (not (tree-node-is-expanded node)))
      (funcall tree-node-expanded-fn node 0 nil nil (buffer-name)))
    (when (or (and (not (tree-node-is-expanded node))
                   (or (not (functionp expand-pred-fn))
                       (funcall expand-pred-fn node current-level))
                   (<= current-level level))
              (and (tree-node-is-expanded node)
                   (or (not (functionp collapse-pred-fn))
                       (funcall collapse-pred-fn node current-level))
                   (> current-level level)))
      (tree-node-toggle-expanded node))
    (dolist (child (tree-node-get-children node))
      (tree-buffer-expand-node child (1+ current-level) level
                               expand-pred-fn collapse-pred-fn))))

(defun tree-buffer-set-root (root)
  (setq tree-buffer-root root)
  (tree-node-set-expanded tree-buffer-root t))

(defun tree-buffer-get-root ()
  tree-buffer-root)

;; Klaus Berndl <klaus.berndl@sdm.de>: Seems that the docstring of
;; x-popup-menu is wrong because it seems this function needs offsets related
;; to current window not to frame!
;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: For XEmacs this does not work!
(defun tree-buffer-show-menu-keyboard ()
  (interactive)
  (if tree-buffer-running-xemacs
      (tree-buffer-show-menu)
    (let ((curr-frame-ypos (* (/ (frame-pixel-height) (frame-height))
                              (count-lines (window-start) (point))))
          (curr-frame-xpos (* (/ (frame-pixel-width) (frame-width))
                              (current-column))))
      (tree-buffer-show-menu (list (list curr-frame-xpos curr-frame-ypos)
                                   (selected-window))))))

(defun tree-buffer-show-menu (&optional event)
  (unless (not (equal (selected-frame) tree-buffer-frame))
    (when tree-buffer-menus
      (let ((node (tree-buffer-get-node-at-point)))
	(when node
	  (let* ((menu (cdr (assoc (tree-node-get-type node) tree-buffer-menus)))
                 (menu-title-creator
                  (cdr (assoc (tree-node-get-type node) tree-buffer-menu-titles)))
                 (menu-title (cond ((stringp menu-title-creator)
                                    menu-title-creator)
                                   ((functionp menu-title-creator)
                                    (funcall menu-title-creator node))
                                   (t "ECB-tree-buffer-menu"))))
            (when menu
	      (if tree-buffer-running-xemacs
		  (popup-menu (cons menu-title menu))
		(let ((fn (x-popup-menu
			   event (cons 'keymap (cons menu-title menu)))))
                  (when fn
		    (funcall (car fn) node)))))))))))

;; (defun tree-buffer-show-menu (event)
;;   (interactive "e")
;;   (tree-buffer-mouse-set-point event)
;;   (unless (not (equal (selected-frame) tree-buffer-frame))
;;     (when tree-buffer-menus
;;       (let ((node (tree-buffer-get-node-at-point)))
;; 	(when node
;; 	  (let* ((menu (cdr (assoc (tree-node-get-type node) tree-buffer-menus)))
;;                  (menu-title-creator
;;                   (cdr (assoc (tree-node-get-type node) tree-buffer-menu-titles)))
;;                  (menu-title (cond ((stringp menu-title-creator)
;;                                     menu-title-creator)
;;                                    ((functionp menu-title-creator)
;;                                     (funcall menu-title-creator node))
;;                                    (t "ECB-tree-buffer-menu"))))
;;             (when menu
;; 	      (if tree-buffer-running-xemacs
;; 		  (popup-menu (cons menu-title menu))
;; 		(let ((fn (x-popup-menu
;; 			   event (cons 'keymap (cons menu-title menu)))))
;;                   (when fn
;; 		    (funcall (car fn) node)))))))))))

(defconst tree-buffer-incr-searchpattern-basic-prefix
  "^[ \t]*\\([[<][+-][]>] \\)?"
  "Prefix-pattern which ignores all not interesting basic stuff of a displayed
token at incr. search. The following contents of a displayed token are ignored
by this pattern:
- beginning spaces
- The expand/collapse-buttons: \[+], <+> resp. \[-], <->")

(defconst tree-buffer-incr-searchpattern-node-prefix "\\([^ ]+ \\|[-+#]\\)?"
  "Prefix-pattern which ignores all not interesting stuff of a node-name at
incr. search. The following contents of a node-name are ignored by this
pattern:
- types of a variable or returntypes of a method
- const specifier of variables
- protection sign of a variable/method: +, - or #")

;; idea is stolen from ido.el, written by Kim F. Storm <stormware@get2net.dk>
(defun tree-buffer-find-common-substring (lis subs &optional only-prefix)
  "Return common substring beginning with SUBS in each element of LIS. If
  ONLY-PREFIX is not nil then only common prefix is returned."
  (let ((change-word-sub (concat (if only-prefix
                                     (concat "^" tree-buffer-incr-searchpattern-node-prefix)
                                   "")
                                 "\\(" (regexp-quote subs) "\\)"))
        res alist)
    (setq res (mapcar (function (lambda (word)
                                  (if (string-match change-word-sub word)
                                      (substring word
                                                 (match-beginning (if only-prefix 2 1)))
                                    ;; else no match
                                    nil)))
                      lis))
    (setq res (delq nil res)) ;; remove any nil elements (shouldn't happen)
    (setq alist (mapcar (function (lambda (r)
                                    (cons r 1)))
                        res)) ;; could use an  OBARRAY

    ;; try-completion returns t if there is an exact match.
    (let ((completion-ignore-case t))
      (try-completion subs alist))))

(defun tree-node-get-all-visible-node-names (start-node)
  (let ((result (if (not (equal tree-buffer-root start-node))
                    (list (tree-node-get-name start-node)))))
    (when (or (equal tree-buffer-root start-node)
              (tree-node-is-expanded start-node))
      (dolist (child (tree-node-get-children start-node))
        (setq result (append result (tree-node-get-all-visible-node-names child)))))
    result))

(defun tree-buffer-incremental-node-search ()
  "Incremental search for a node in current tree-buffer. Each displayable
key \(e.g. all keys normally bound to `self-insert-command') is appended to
the current seach-pattern. The tree-buffer tries to jump to the current
search-pattern. If no match is found then nothing is done. Some special keys:
- \[backspace] and \[delete]: Delete the last character from the search-pattern.
- \[home]: Delete the complete search-pattern
- \[end]: Expand either to a complete node if current search-pattern is
         already unique or expands to the greates common prefix of the nodes.
         If there are at least two nodes with the same greatest common-prefix
         than every hit of \[end] jumps to the next node with this common
         prefix.

The current search-pattern is shown in the echo area.
After selecting a node with RET the search-pattern is cleared out.

Do NOT call this function directly. It works only if called from the binding
mentioned above!"
  (interactive)
  (unless (not (equal (selected-frame) tree-buffer-frame))
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
              (let* ((node-name-list (tree-node-get-all-visible-node-names
                                      tree-buffer-root))
                     (common-prefix (tree-buffer-find-common-substring
                                     node-name-list tree-buffer-incr-searchpattern
                                     (if (equal tree-buffer-incr-search 'prefix) t))))
                (if (stringp common-prefix)
                    (setq tree-buffer-incr-searchpattern common-prefix))))
             ((null last-comm)
              nil) ;; do nothing
             (t
              ;; add the last command to the end
              (setq tree-buffer-incr-searchpattern
                    (concat tree-buffer-incr-searchpattern
                            (char-to-string last-comm)))))
      (tree-buffer-nolog-message
       "%s node search: [%s]%s"
       (buffer-name (current-buffer))
       tree-buffer-incr-searchpattern
       (if (save-excursion
             (if (or (not (equal last-comm 'end))
                     (not (string= tree-buffer-incr-searchpattern
                                   tree-buffer-last-incr-searchpattern)))
                 (goto-char (point-min)))
             (re-search-forward
              (concat tree-buffer-incr-searchpattern-basic-prefix
                      tree-buffer-incr-searchpattern-node-prefix
                      (if (equal tree-buffer-incr-search 'substring)
                          "[^()\n]*"
                        "")
                      (regexp-quote tree-buffer-incr-searchpattern)) nil t))
           ;; we have found a matching ==> jump to it
           (progn
             (goto-char (match-end 0))
             "")
         " - no match"))
      ;; lets save the search-pattern so we can compare it with the next one.
      (setq tree-buffer-last-incr-searchpattern tree-buffer-incr-searchpattern))))

(defun tree-buffer-create-menu (menu-items)
  "Creates a popup menu from a list with menu items."
  (when menu-items
    (cons
     (if tree-buffer-running-xemacs
         (if (null (cdar menu-items))
             (caar menu-items)
           (let ((v (make-vector 3 t)))
             (aset v 0 (caar menu-items))
             (aset v 1 (list (cadar menu-items)
                             '(tree-buffer-get-node-at-point)))
             (aset v 2 t)
             v))
       (cons (cadar menu-items)
	     (cons (caar menu-items) t)))
     (tree-buffer-create-menu (cdr menu-items)))))


(defun tree-buffer-create-menus (menus)
  "Creates a popup menus from an assoc list with menus."
  (when menus
    (cons (cons (caar menus)
		(tree-buffer-create-menu (cdar menus)))
	  (tree-buffer-create-menus (cdr menus)))))

;; mouse tracking stuff

(defun tree-buffer-follow-mouse (event)
  (interactive "e")
  (let ((window (tree-buffer-event-window event))
	(current-window (get-buffer-window (current-buffer))))
    (if (and (or (not (window-minibuffer-p current-window))
		 (not (minibuffer-window-active-p current-window)))
	     (windowp window)
	     (member (window-buffer window) tree-buffers))
	(tree-buffer-mouse-movement event)))
  (if (not tree-buffer-running-xemacs)
      (if tree-buffer-saved-mouse-movement-fn
	  (funcall tree-buffer-saved-mouse-movement-fn event)
	;; Enable dragging
	(setq unread-command-events
	      (nconc unread-command-events (list event))))))

(defun tree-buffer-mouse-movement (event)
  (interactive "e")
  (set-buffer (window-buffer (tree-buffer-event-window event)))
  (let ((p (tree-buffer-event-point event)))
    (when (integer-or-marker-p p)
      ;;      (unless (not (equal (selected-frame) tree-buffer-frame))
      (let ((node (tree-buffer-get-node-at-point p)))
	(when (and tree-node-mouse-over-fn node)
	  (funcall tree-node-mouse-over-fn node
                   (get-buffer-window (current-buffer))))))))

(defvar tree-buffer-uncompleted-keyseq nil
  "Not nil only if there is at evaluation-time of this variable an uncompleted
keysequence, e.g. the \"C-h\" of the keysequence \"C-h v\".")

(defun tree-buffer-do-mouse-tracking ()
  "This function is called every time Emacs is idle for seconds defined in
`tree-buffer-track-mouse-idle-delay'. It enables mouse-tracking but only if
isearch is not active and if no uncompleted keysequence is open, means if this
function is called by the idle timer during a keysequence is inserted by the
user \(e.g. between the \"C-h\" and the \"v\" of the keysequence \"C-h v\"),
then mouse-tracking is always not enabled, because otherwise all very slighly
\(invisible) and unintended mouse-movements \(can occur for example only by
the convulsion cause of hitting keys onto the keyboard!) would break the
keysequence!"
  (setq track-mouse nil)
  (if (not (equal (tree-buffer-event-to-key last-input-event)
                  'mouse-movement))
      (setq tree-buffer-uncompleted-keyseq
            (not (equal last-input-event last-command-event))))
  (unless (or tree-buffer-uncompleted-keyseq
              ;; maybe there are even more similar modes where we should not
              ;; activate mouse-tracking?!
              isearch-mode)
    (setq track-mouse t))
  (add-hook 'post-command-hook 'tree-buffer-stop-mouse-tracking))

(defun tree-buffer-stop-mouse-tracking ()
  (remove-hook 'post-command-hook 'tree-buffer-stop-mouse-tracking)
  (setq track-mouse nil))

(defun tree-buffer-activate-mouse-tracking ()
  "Activates GNU Emacs < version 21 mouse tracking for all tree-buffers.
With GNU Emacs 21 this functionality is done with the `help-echo'-property and
the function `tree-buffer-help-echo-fn'!"
  (unless (or tree-buffer-running-xemacs tree-buffer-running-emacs-21)
    (unless tree-buffer-track-mouse-timer
      ;; disable mouse avoidance because this can be very annoying with
      ;; key-sequences: If a key is pressed during mouse is over point then
      ;; the mouse goes away and therefore the key-sequence is broken because
      ;; the mouse move generates a mouse-movement event.
      (setq tree-buffer-old-mouse-avoidance-mode
            (if (null mouse-avoidance-mode) 'none mouse-avoidance-mode))
      (mouse-avoidance-mode 'none)
      (setq tree-buffer-saved-track-mouse track-mouse)
      (setq tree-buffer-track-mouse-timer
            (run-with-idle-timer tree-buffer-track-mouse-idle-delay
                                 t 'tree-buffer-do-mouse-tracking)))))

(defun tree-buffer-deactivate-mouse-tracking ()
  "Deactivates GNU Emacs < version 21 mouse tracking for all tree-buffers.
With GNU Emacs 21 this functionality is done with the `help-echo'-property and
the function `tree-buffer-help-echo-fn'!"
  (unless (or tree-buffer-running-xemacs tree-buffer-running-emacs-21)
    (unless (not tree-buffer-track-mouse-timer)
      ;; restore the old value
      (mouse-avoidance-mode tree-buffer-old-mouse-avoidance-mode)
      (setq track-mouse tree-buffer-saved-track-mouse)
      (cancel-timer tree-buffer-track-mouse-timer)
      (setq tree-buffer-track-mouse-timer nil))))

(defun tree-buffer-activate-follow-mouse ()
  "Activates that in all tree-buffer-windows - regardless if the active window
or not - a mouse-over-node-function is called if mouse moves over a node. See
also the NODE-MOUSE-OVER-FN argument of `tree-buffer-create'.

This function does nothing for GNU Emacs 21; with this version this
functionality is done with the `help-echo'-property and the function
`tree-buffer-help-echo-fn'!"
  (tree-buffer-activate-mouse-tracking)
  (if tree-buffer-running-xemacs
      (dolist (buf tree-buffers)
        (save-excursion
          (set-buffer buf)
          (add-hook 'mode-motion-hook 'tree-buffer-follow-mouse)))
    (unless tree-buffer-running-emacs-21
      (let ((saved-fn (lookup-key special-event-map [mouse-movement])))
        (unless (equal saved-fn 'tree-buffer-follow-mouse)
          (setq tree-buffer-saved-mouse-movement-fn saved-fn)
          (define-key special-event-map [mouse-movement] 'tree-buffer-follow-mouse))))))

(defun tree-buffer-deactivate-follow-mouse ()
  (if tree-buffer-running-xemacs
      (dolist (buf tree-buffers)
        (save-excursion
          (set-buffer buf)
          (remove-hook 'mode-motion-hook 'tree-buffer-follow-mouse)))
    (unless tree-buffer-running-emacs-21
      (define-key special-event-map [mouse-movement] tree-buffer-saved-mouse-movement-fn))))

;; pressed keys

(defun tree-buffer-tab-pressed ()
  (interactive)
  (unless (not (equal (selected-frame) tree-buffer-frame))
    (let ((node (tree-buffer-get-node-at-point)))
      (when (tree-node-is-expandable node)
	(when (and tree-node-expanded-fn
		   (not (tree-node-is-expanded node)))
	  (funcall tree-node-expanded-fn node 0 nil nil (buffer-name)))
        (tree-node-toggle-expanded node)
	;; Update the tree-buffer with optimized display of NODE           
	(tree-buffer-update node)))))

(defun tree-buffer-return-pressed (&optional shift-pressed control-pressed)
  (unless (not (equal (selected-frame) tree-buffer-frame))
    ;; reinitialize the select pattern after selecting a node
    (setq tree-buffer-incr-searchpattern "")
    (tree-buffer-select 0 shift-pressed control-pressed)))

(defun tree-buffer-arrow-pressed ()
  (interactive)
  (unless (not (equal (selected-frame) tree-buffer-frame))
    (let ((node (tree-buffer-get-node-at-point))
          (arrow-key (tree-buffer-event-to-key last-command-event)))
      (cond ((equal arrow-key 'right)
             (if (and (tree-node-is-expandable node)
                      (not (tree-node-is-expanded node)))
                 (tree-buffer-tab-pressed)
               ;; jump to the first subnode
               (forward-line 1)
               (back-to-indentation)))
            ((equal arrow-key 'left)
             (if (tree-node-is-expanded node)
                 (tree-buffer-tab-pressed)
               ;; jump to next higher node
               (let* ((indent (tree-buffer-get-node-indent node))
                      (new-indent (max 0 (- indent tree-buffer-indent)))
                      (search-string
                       (concat "^"
                               (buffer-substring
                                (tree-buffer-line-beginning-pos)
                                (+ (tree-buffer-line-beginning-pos)
                                   new-indent))
                               "[^ \t]")))
                 (re-search-backward search-string nil t)
                 (back-to-indentation))))))))


;; tree-buffer creation

(defun tree-buffer-create (name frame is-click-valid-fn node-selected-fn
                                node-expanded-fn node-mouse-over-fn
                                node-data-equal-fn
                                menus menu-titles tr-lines read-only tree-indent
                                incr-search arrow-navigation hor-scroll
                                expand-collapse
                                &optional type-facer expand-symbol-before
                                highlight-node-face general-face
                                after-create-hook)
  "Creates a new tree buffer and returns the newly created buffer.
This function creates also a special data-storage for this tree-buffer which
can be accessed via `tree-buffer-set-data-store' and `tree-buffer-get-data-store'.
The user of this tree-buffer can store any arbitrary data in this storage.
Before using the accessor-functions above the tree-buffer has to be the
current buffer!

NAME: Name of the buffer FRAME: Frame in
which the tree-buffer is displayed and valid. All keybindings
       and interactive functions of the tree-buffer work only if called in
       FRAME otherwise nothing is done!
IS-CLICK-VALID-FN: `tree-buffer-create' rebinds down-mouse-1, down-mouse-2,
                   RET \(and TAB) and also in combination with shift and
                   control \(not with TAB). IS-CLICK-VALID-FN is called first
                   if a node or an expand-symbol is clicked. This function is
                   called with four arguments:
                   - mouse-button: The clicked mouse-button or RET or TAB \(0
                     = RET or TAB, 1 = mouse-1, 2 = mouse 2)
                   - shift-pressed: non nil if the SHIFT-key was pressed
                     during mouse-click or RET/TAB.
                   - control-pressed: non nil if the CONTROL-key was pressed
                     during mouse-click or RET/TAB.
                   - tree-buffer-name: The buffer-name of the tree-buffer
                     where the node has been clicked.
                   The function must return not nil iff exactly this click/hit
                   is accepted. If the function returns nil then really
                   nothing is done by the tree-buffer after this click/hit!
NODE-SELECTED-FN: Function to call if a node has been selected
                  This function is called with the following paramters:
                  - node: The selected node
                  - mouse-button \(0 = RET, 1 = mouse-1, 2 = mouse 2)
                  - shift-pressed
                  - control-pressed
                  - tree-buffer-name
                  For the last four arguments see the description above.
NODE-EXPANDED-FN: Function to call if a node is expandable, point stays onto
                  the expand-symbol and node is not already expanded. This
                  function is called with the following paramters:
                  - node: The selected node
                  - mouse-button \(0 = TAB, 1 = mouse-1, 2 = mouse 2)
                  - shift-pressed
                  - control-pressed
                  - tree-buffer-name
                  This function should add all children nodes to this node
                  \(if possible).
NODE-MOUSE-OVER-FN: Function to call when the mouse is moved over a node. This
                    function is called with three arguments: NODE, WINDOW,
                    NO-PRINT, each of them related to the current tree-buffer.
                    If NO-PRINT is nil then the function must print the text
                    itself in any manner.
                    This function must always return the text which either is
                    printed by the function itself or by the caller \(if
                    NO-PRINT is not nil).
                    The current buffer for this function is the tree-buffer.
                    With XEmacs and GNU Emacs 20.X this function is only
                    called if the tree-buffer track-mouse mechanism is
                    activated \(see `tree-buffer-activate-mouse-tracking').
                    With GNU Emacs 21 this function is called by the
                    `help-echo' property added to each node.
NODE-DATA-EQUAL-FN: Function used by the tree-buffer to test if the data of
                    two tree-nodes are equal. The data of node can be set/get
                    with `tree-node-set-data'/`tree-node-get-data'.
MENUS: Nil or a list of one to three conses, each cons for a node-type \(0, 1
       or 2) Example: \(\(0 . menu-for-type-0) \(1 . menu-for-type-1)). The
       cdr of a cons must be a menu.
MENU-TITLES: Nil or a list of one to three conses, each cons for a node-type
             \(0, 1 or 2). See MENUES. The cdr of a cons must be either a
             string or a function which will be called with current node
             under point and must return a string which is displayed as the
             menu-title.
TR-LINES: Should lines in this tree buffer be truncated \(not nil)
READ-ONLY: Should the treebuffer be read-only \(not nil)
TREE-INDENT: spaces subnodes should be indented.
INCR-SEARCH: Should the incremental search be anabled in the tree-buffer.
             Three choices: 'prefix, 'substring, nil. See
             `tree-buffer-incremental-node-search'.
ARROW-NAVIGATION: If not nil then smart navigation with horizontal arrow keys.
HOR-SCROLL: Number of columns a hor. scroll in the tree-buffer should scroll.
            If not nil then M-mouse-1 and M-mouse-2 scroll left and right and
            also M-<left-arrow> and M-<right-arrow>.
EXPAND-COLLAPSE: A 2-elem list where the first elem is a cons cell where the
                 car is the token-string for the expand-symbol \(e.g.
                 \"\[+]\") and the cdr is a symbol which has as value an image
                 in the sense of \(X)Emacs. The second elem is the same for
                 the collapse symbol. Both elems can be nil; then no expand-
                 or collapse-symbol is drawn. If only the cdr of an elem is
                 nil then no image-icon is used but always the token-string.
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
                      the node-text.
HIGHLIGHT-NODE-FACE: Face used for highlighting current node in this
                     tree-buffer.
GENERAL-FACE: General face in which the whole tree-buffer should be displayed.
AFTER-CREATE-HOOK: A function or a list of functions \(with no arguments)
                   called directly after creating the tree-buffer and defining
                   it's local keymap. For example such a function can add
                   additional keybindings for this tree-buffer local keymap."
  (let ((nop (function (lambda() (interactive))))
        (a-c-h (if (functionp after-create-hook)
                   (list after-create-hook)
                 after-create-hook)))
    (set-buffer (get-buffer-create name))

    (make-local-variable 'truncate-lines)
    (make-local-variable 'truncate-partial-width-windows)
    (make-local-variable 'tree-buffer-key-map)
    (make-local-variable 'tree-buffer-frame)
    (make-local-variable 'tree-buffer-root)
    (make-local-variable 'tree-buffer-nodes)
    (make-local-variable 'tree-buffer-indent)
    (make-local-variable 'tree-buffer-is-click-valid-fn)
    (make-local-variable 'tree-node-selected-fn)
    (make-local-variable 'tree-node-expanded-fn)
    (make-local-variable 'tree-node-update-fn)
    (make-local-variable 'tree-node-mouse-over-fn)
    (make-local-variable 'tree-node-data-equal-fn)
    (make-local-variable 'tree-buffer-highlighted-node-data)
    (make-local-variable 'tree-buffer-menus)
    (make-local-variable 'tree-buffer-menu-titles)
    (make-local-variable 'tree-buffer-type-facer)
    (make-local-variable 'tree-buffer-expand-symbol-before)
    (make-local-variable 'tree-buffer-highlight-overlay)
    (make-local-variable 'tree-buffer-general-face)
    (make-local-variable 'tree-buffer-general-overlay)
    (make-local-variable 'tree-buffer-incr-searchpattern)
    (make-local-variable 'tree-buffer-last-incr-searchpattern)
    (make-local-variable 'tree-buffer-incr-search)
    (make-local-variable 'tree-buffer-hor-scroll-step)
    (make-local-variable 'tree-buffer-expand-collapse-tokens)
    
    ;; initialize the user-data-storage for this tree-buffer.
    (set (make-local-variable 'tree-buffer-data-store) nil)

    (setq truncate-lines tr-lines)
    (setq truncate-partial-width-windows tr-lines)
    (setq buffer-read-only read-only)
    (setq tree-buffer-key-map (make-sparse-keymap))
    (setq tree-buffer-frame frame)
    (setq tree-buffer-is-click-valid-fn is-click-valid-fn)
    (setq tree-node-selected-fn node-selected-fn)
    (setq tree-node-expanded-fn node-expanded-fn)
    (setq tree-node-mouse-over-fn node-mouse-over-fn)
    (setq tree-node-data-equal-fn node-data-equal-fn)
    (setq tree-buffer-indent tree-indent)
    (setq tree-buffer-highlighted-node-data nil)
    (setq tree-buffer-menus (tree-buffer-create-menus menus))
    (setq tree-buffer-menu-titles menu-titles)
    (setq tree-buffer-root (tree-node-new "root" 0 "root"))
    (setq tree-buffer-type-facer type-facer)
    (setq tree-buffer-expand-symbol-before expand-symbol-before)
    (setq tree-buffer-highlight-overlay (make-overlay 1 1))
    (overlay-put tree-buffer-highlight-overlay 'face highlight-node-face)
    ;; setting general face and overlay for the tree-buffer
    (setq tree-buffer-general-face general-face)
    (setq tree-buffer-general-overlay (make-overlay 1 1))
    (overlay-put tree-buffer-general-overlay 'face
                 tree-buffer-general-face)
    ;; initializing the search-pattern
    (setq tree-buffer-incr-searchpattern "")
    (setq tree-buffer-last-incr-searchpattern "")
    (setq tree-buffer-incr-search incr-search)
    (setq tree-buffer-hor-scroll-step hor-scroll)
    (setq tree-buffer-expand-collapse-tokens expand-collapse)

    ;; set a special syntax table for tree-buffers
    (set-syntax-table tree-buffer-syntax-table)
    
    ;; keyboard setting
    (when incr-search
      ;; settings for the incremental search.
      ;; for all keys which are bound to `self-insert-command' in `global-map'
      ;; we change this binding to `tree-buffer-incremental-node-search'.
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
    
    (define-key tree-buffer-key-map (kbd "<RET>")
      (function (lambda ()
                  (interactive)
                  (tree-buffer-return-pressed nil nil))))
    (define-key tree-buffer-key-map (kbd "<C-return>")
      (function (lambda ()
                  (interactive)
                  (tree-buffer-return-pressed nil t))))
    (define-key tree-buffer-key-map (kbd "<S-return>")
      (function (lambda ()
                  (interactive)
                  (tree-buffer-return-pressed t nil))))
    (define-key tree-buffer-key-map (kbd "<C-S-return>")
      (function (lambda ()
                  (interactive)
                  (tree-buffer-return-pressed t t))))
    
    (define-key tree-buffer-key-map (kbd "TAB") 'tree-buffer-tab-pressed)

    (when arrow-navigation
      (define-key tree-buffer-key-map (kbd "<right>") 'tree-buffer-arrow-pressed)
      (define-key tree-buffer-key-map (kbd "<left>") 'tree-buffer-arrow-pressed))
    
    ;; mouse-1
    (define-key tree-buffer-key-map
      (if tree-buffer-running-xemacs '(button1) [down-mouse-1])
      (function (lambda(e)
		  (interactive "e")
                  (tree-buffer-mouse-set-point e)
                  (tree-buffer-select 1 nil nil))))
  
    (define-key tree-buffer-key-map
      (if tree-buffer-running-xemacs '(shift button1) [S-down-mouse-1])
      (function (lambda(e)
		  (interactive "e")
                  (tree-buffer-mouse-set-point e)
                  (tree-buffer-select 1 t nil))))

    (define-key tree-buffer-key-map
      (if tree-buffer-running-xemacs '(control button1) [C-down-mouse-1])
      (function (lambda(e)
		  (interactive "e")
                  (tree-buffer-mouse-set-point e)
                  (tree-buffer-select 1 nil t))))

    (define-key tree-buffer-key-map [drag-mouse-1] nop)
    (define-key tree-buffer-key-map [mouse-1] nop)
    (define-key tree-buffer-key-map [double-mouse-1] nop)
    (define-key tree-buffer-key-map [triple-mouse-1] nop)

    ;; mouse-2
    (define-key tree-buffer-key-map
      (if tree-buffer-running-xemacs '(button2) [down-mouse-2])
      (function (lambda(e)
		  (interactive "e")
                  (tree-buffer-mouse-set-point e)
                  (tree-buffer-select 2 nil nil))))

    (define-key tree-buffer-key-map
      (if tree-buffer-running-xemacs '(shift button2) [S-down-mouse-2])
      (function (lambda(e)
		  (interactive "e")
                  (tree-buffer-mouse-set-point e)
                  (tree-buffer-select 2 t nil))))

    (define-key tree-buffer-key-map
      (if tree-buffer-running-xemacs '(control button2) [C-down-mouse-2])
      (function (lambda(e)
		  (interactive "e")
                  (tree-buffer-mouse-set-point e)
                  (tree-buffer-select 2 nil t))))

    (define-key tree-buffer-key-map [mouse-2] nop)
    (define-key tree-buffer-key-map [double-mouse-2] nop)
    (define-key tree-buffer-key-map [triple-mouse-2] nop)

    ;; mouse-3
    (define-key tree-buffer-key-map
      (if tree-buffer-running-xemacs '(button3) [down-mouse-3])
      (function (lambda(e)
		  (interactive "e")
                  (tree-buffer-mouse-set-point e)
                  (tree-buffer-show-menu e))))
    (define-key tree-buffer-key-map (kbd "M-m")
      'tree-buffer-show-menu-keyboard)
    
    (define-key tree-buffer-key-map [mouse-3] nop)
    (define-key tree-buffer-key-map [double-mouse-3] nop)
    (define-key tree-buffer-key-map [triple-mouse-3] nop)

    ;; scrolling horiz.
    (when (and (not tree-buffer-running-xemacs)
               tree-buffer-hor-scroll-step)
      (define-key tree-buffer-key-map
        [M-down-mouse-1]
        (function (lambda(e)
                    (interactive "e")
                    (tree-buffer-mouse-set-point e)
                    (tree-buffer-hscroll (- tree-buffer-hor-scroll-step)))))

      (define-key tree-buffer-key-map
        [M-down-mouse-3]
        (function (lambda(e)
                    (interactive "e")
                    (tree-buffer-mouse-set-point e)
                    (tree-buffer-hscroll tree-buffer-hor-scroll-step))))
      
      (define-key tree-buffer-key-map
        [C-M-down-mouse-1]
        (function (lambda(e)
                    (interactive "e")
                    (tree-buffer-mouse-set-point e)
                    (tree-buffer-hscroll (- (- (window-width) 2))))))
      
      (define-key tree-buffer-key-map
        [C-M-down-mouse-3]
        (function (lambda(e)
                    (interactive "e")
                    (tree-buffer-mouse-set-point e)
                    (tree-buffer-hscroll (- (window-width) 2)))))
      
      ;; This lets the user scroll as if we had a scrollbar...
      (define-key tree-buffer-key-map
        [mode-line mouse-1] 'tree-buffer-mouse-hscroll)
      (define-key tree-buffer-key-map
        [mode-line mouse-2] 'tree-buffer-mouse-hscroll)
      
      (define-key tree-buffer-key-map [M-mouse-1] nop)
      (define-key tree-buffer-key-map [M-mouse-3] nop)
      (define-key tree-buffer-key-map [C-M-mouse-1] nop)
      (define-key tree-buffer-key-map [C-M-mouse-3] nop))
    
    (use-local-map tree-buffer-key-map)

    (setq tree-buffers (cons (current-buffer) tree-buffers))

    (prog1
        (current-buffer)
      (dolist (f a-c-h)
        (funcall f)))))

(defun tree-buffer-destroy (buffer)
  "Destroy the tree-buffer"
  (when buffer
    (setq tree-buffers (delq (get-buffer buffer) tree-buffers))
    (kill-buffer buffer)))

;;; Tree node

(defun tree-node-add-child (node child)
  (tree-node-set-children node (append (tree-node-get-children node) (list child)))
  (tree-node-set-parent child node))

(defun tree-node-add-child-first (node child)
  (tree-node-set-children node (cons child (tree-node-get-children node)))
  (tree-node-set-parent child node))

(defun tree-node-sort-children (node sortfn)
  (tree-node-set-children node (sort (tree-node-get-children node) sortfn)))

(defun tree-node-remove-child (node child)
  "Removes the child from the node."
  (tree-node-set-parent child nil)
  (tree-node-set-children node
                          (delq child (tree-node-get-children node))))

(defun tree-node-find-child-data (node child-data)
  "Finds the first child with the given child-data."
  (catch 'exit
    (dolist (child (tree-node-get-children node))
      (when (tree-buffer-node-data-equal-p (tree-node-get-data child)
                                           child-data)
        (throw 'exit child)))))

(defun tree-node-remove-child-data (node child-data)
  "Removes the first child with the given child-data. Returns the removed
child."
  (catch 'exit
    (let ((last-cell nil)
	  (cell (tree-node-get-children node)))
      (while cell
	(when (tree-buffer-node-data-equal-p (tree-node-get-data (car cell))
                                             child-data)
	  (if last-cell
	      (setcdr last-cell (cdr cell))
	    (tree-node-set-children node (cdr cell)))
	  (setcdr cell nil)
	  (tree-node-set-parent (car cell) nil)
	  (throw 'exit cell))
	(setq last-cell cell)
	(setq cell (cdr cell))))))

(defun tree-node-find-child-name (node child-name)
  (catch 'exit
    (dolist (child (tree-node-get-children node))
      (when (equal (tree-node-get-name child) child-name)
        (throw 'exit child)))))

(defun tree-node-find-data-recursively (node data)
  (if (tree-buffer-node-data-equal-p data (tree-node-get-data node))
      node
    (catch 'exit
      (dolist (child (tree-node-get-children node))
	(let ((n (tree-node-find-data-recursively child data)))
	  (when n
	    (throw 'exit n)))))))

;;; Tree node

(defconst tree-node-name 0)
(defconst tree-node-type 1)
(defconst tree-node-data 2)
(defconst tree-node-expanded 3)
(defconst tree-node-parent 4)
(defconst tree-node-children 5)
(defconst tree-node-expandable 6)
(defconst tree-node-shorten-name 7
  "Decides if the node name can be shortened when displayed in a narrow tree buffer window. The following values are valid:
- beginning: The name is truncated at the beginning so the end is always visible.
- end: The name is truncated at the end. If the node is expandable the name is truncated so that the expand symbol is visible.
- nil: The name is never truncated."
  )

(defun tree-node-new (name type data &optional not-expandable parent shorten-name)
  (let ((a (make-vector 8 nil)))
    (tree-node-set-name a name)
    (tree-node-set-type a type)
    (tree-node-set-data a data)
    (tree-node-set-expanded a nil)
    (tree-node-set-children a nil)
    (tree-node-set-parent a parent)
    (tree-node-set-expandable a (not not-expandable))
    (tree-node-set-shorten-name a shorten-name)
    (when parent
      (tree-node-add-child parent a))
    a))

(defun tree-node-get-name (node)
  (aref node tree-node-name))

(defun tree-node-set-name (node name)
  (aset node tree-node-name name))

(defun tree-node-get-type (node)
  (aref node tree-node-type))

(defun tree-node-set-type (node type)
  (aset node tree-node-type type))

(defun tree-node-get-data (node)
  (aref node tree-node-data))

(defun tree-node-set-data (node data)
  (aset node tree-node-data data))

(defun tree-node-is-expanded (node)
  (aref node tree-node-expanded))

(defun tree-node-set-expanded (node expanded)
  (aset node tree-node-expanded expanded))

(defun tree-node-is-expandable (node)
  (aref node tree-node-expandable))

(defun tree-node-set-expandable (node expandable)
  (aset node tree-node-expandable expandable))

(defun tree-node-get-parent (node)
  (aref node tree-node-parent))

(defun tree-node-set-parent (node parent)
  (aset node tree-node-parent parent))

(defun tree-node-get-children (node)
  (aref node tree-node-children))

(defun tree-node-set-children (node children)
  (aset node tree-node-children children))

(defun tree-node-toggle-expanded (node)
  (tree-node-set-expanded node (not (tree-node-is-expanded node))))

(defun tree-node-get-depth (node)
  (let ((parent (tree-node-get-parent node)))
    (if parent
        (1+ (tree-node-get-depth parent))
      '0)))

(defun tree-node-set-shorten-name (node shorten)
  (aset node tree-node-shorten-name shorten))

(defun tree-node-get-shorten-name (node)
  (aref node tree-node-shorten-name))

(silentcomp-provide 'tree-buffer)

;;; tree-buffer.el ends here
