;;; tree-buffer.el --- functions for tree buffers

;; Copyright (C) 2000 - 2003 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Jesper Nordenberg <mayhem@home.se>
;;         Klaus Berndl <klaus.berndl@sdm.de>
;;         Kevin A. Burton <burton@openprivacy.org>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
;; Keywords: browser, code, programming, tools, tree
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

;; Functions for tree buffers.
;;
;; This file is part of the ECB package which can be found at:
;; http://ecb.sourceforge.net

;;; History
;;
;; For the ChangeLog of this file see the CVS-repository. For a complete
;; history of the ECB-package see the file NEWS.

;;; Code:

(eval-when-compile
  (require 'silentcomp))

(eval-when-compile
  ;; to avoid compiler grips
  (require 'cl))

;; XEmacs stuff
(silentcomp-defun button-release-event-p)
(silentcomp-defun button-press-event-p)
(silentcomp-defun event-key)
(silentcomp-defun extent-end-position)
(silentcomp-defun event-glyph-extent)
(silentcomp-defun event-over-glyph-p)
(silentcomp-defun display-message)
(silentcomp-defun clear-message)
(silentcomp-defun locate-data-directory)
(silentcomp-defun make-image-specifier)
(silentcomp-defun make-glyph)
(silentcomp-defun popup-menu-and-execute-in-window)
(silentcomp-defun valid-image-instantiator-format-p)
;; Emacs
(silentcomp-defvar message-log-max)
(silentcomp-defvar message-truncate-lines)
(silentcomp-defvar track-mouse)
(silentcomp-defvar special-event-map)
(silentcomp-defun posn-window)
(silentcomp-defun event-start)
(silentcomp-defun posn-point)
(silentcomp-defun event-basic-type)
(silentcomp-defun display-images-p)
(silentcomp-defun image-type-available-p)
(silentcomp-defun count-screen-lines)
(silentcomp-defun tmm-prompt)
;; timer stuff for XEmacs
(silentcomp-defun delete-itimer)
(silentcomp-defun start-itimer)

(defconst tree-buffer-running-xemacs
  (string-match "XEmacs\\|Lucid" emacs-version))
(defconst tree-buffer-running-emacs-21
  (and (not tree-buffer-running-xemacs)
       (> emacs-major-version 20)))

(defconst tree-buffer-images-can-be-used
  (and (or (fboundp 'defimage)
           (fboundp 'make-image-specifier))
       (if (fboundp 'display-images-p)
           (display-images-p)
         window-system)))

(defvar tree-buffer-image-properties-emacs
  '(:ascent center :mask (heuristic t))
  "Properties of GNU Emacs images.")

(defvar tree-buffer-image-properties-xemacs
  nil
  "Properties of XEmacs images.")

;; miscellaneous differences

(if tree-buffer-running-xemacs
    ;; XEmacs
    (progn
      (defsubst tree-buffer-create-image (file type)
        "Create an image of type TYPE from FILE. Return the new image."
        (apply 'make-glyph
               `([,type :file ,file
                        ,@tree-buffer-image-properties-xemacs])))
      (defsubst tree-buffer-image-type-available-p (type)
        "Return non-nil if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
      (valid-image-instantiator-format-p type))
      (defalias 'tree-buffer-line-beginning-pos 'point-at-bol)
      (defalias 'tree-buffer-line-end-pos 'point-at-eol)
      (defalias 'tree-buffer-window-display-height 'window-displayed-height)
      (defun tree-buffer-event-to-key (event)
        (cond ((button-release-event-p event)
               'mouse-release)
              ((button-press-event-p event)
               'mouse-press)
              (t
               ;; the ignore-errors is a little hack because i don't know all
               ;; events of XEmacs so sometimes event-key produces a
               ;; wrong-type-argument error.
               (ignore-errors (event-key event)))))
      (defalias 'tree-buffer-event-window 'event-window)
      (defalias 'tree-buffer-event-point 'event-point)
      ;; stolen from dframe.el of the speedbar-library.
      (defun tree-buffer-mouse-set-point (e)
        "Set POINT based on event E. Handles clicking on images in XEmacs."
        (mouse-set-point e)
        (if (and (fboundp 'event-over-glyph-p) (event-over-glyph-p e))
            ;; We are in XEmacs, and clicked on a picture
            (let ((ext (event-glyph-extent e)))
              ;; This position is back inside the extent where the
              ;; junk we pushed into the property list lives.
              (if (extent-end-position ext)
                  (goto-char (1- (extent-end-position ext))))))))

  ;; GNU Emacs
  (defsubst tree-buffer-create-image (file type)
    (apply 'create-image
           `(,file ,type nil
                   ,@tree-buffer-image-properties-emacs)))
  (defsubst tree-buffer-image-type-available-p (type)
    "Return non-nil if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
    (image-type-available-p type))
  ;; needed to handle correct mouse avoidance
  (require 'avoid)
  (defalias 'tree-buffer-line-beginning-pos 'line-beginning-position)
  (defalias 'tree-buffer-line-end-pos 'line-end-position)
  ;; Klaus Berndl <klaus.berndl@sdm.de>: Is not really the same as
  ;; `window-displayed-height' of XEmacs, because if the buffer-end is before
  ;; the window-end (i.e. there are "empty" lines between window-end and last
  ;; char of the buffer) then these empty-lines are not counted. But in the
  ;; situations this function is used (only in tree-buffer-recenter) this
  ;; doesn't matter.
  (defun tree-buffer-window-display-height (&optional window)
    (setq window (or window (selected-window)))
    (save-selected-window
      (select-window window)
      (count-screen-lines (window-start)
                          (- (window-end window t) 1))))
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

;; overlay/extend stuff

(if (not tree-buffer-running-xemacs)
    (progn
      (defalias 'tree-buffer-make-overlay            'make-overlay)
      (defalias 'tree-buffer-overlay-put             'overlay-put)
      (defalias 'tree-buffer-overlay-move            'move-overlay)
      (defalias 'tree-buffer-overlay-delete          'delete-overlay)
      (defalias 'tree-buffer-overlay-kill            'delete-overlay))
  ;; XEmacs
  (defalias 'tree-buffer-make-overlay            'make-extent)
  (defalias 'tree-buffer-overlay-put             'set-extent-property)
  (defalias 'tree-buffer-overlay-move            'set-extent-endpoints)
  (defalias 'tree-buffer-overlay-delete          'detach-extent)
  (defalias 'tree-buffer-overlay-kill            'delete-extent))


;; timer stuff

(if (not ecb-running-xemacs)
    (progn
      (defalias 'tree-buffer-run-with-idle-timer 'run-with-idle-timer)
      (defalias 'tree-buffer-cancel-timer 'cancel-timer))
  ;; XEmacs
  (if (fboundp 'run-with-idle-timer)
      (defalias 'tree-buffer-run-with-idle-timer 'run-with-idle-timer)
    (defun tree-buffer-run-with-idle-timer (secs repeat function &rest args)
      "Perform an action the next time Emacs is idle for SECS seconds.
If REPEAT is non-nil, do this each time Emacs is idle for SECS seconds.
SECS may be an integer or a floating point number.
The action is to call FUNCTION with arguments ARGS.

This function returns a timer object which you can use in
`tree-buffer-cancel-timer'."
      (start-itimer "tree-buffer-idle-timer"
                    function secs (if repeat secs nil)
                    t (if args t nil) args)))

  (if (fboundp 'cancel-timer)
      (defalias 'tree-buffer-cancel-timer 'cancel-timer)
    (defun tree-buffer-cancel-timer (timer)
      "Remove TIMER from the list of active timers."
      (delete-itimer timer))))  

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
(defvar tree-buffer-menu-creator nil)
(defvar tree-buffer-menu-titles nil)
(defvar tree-buffer-type-facer nil)
(defvar tree-buffer-expand-symbol-before nil)
(defvar tree-buffer-is-click-valid-fn nil)
(defvar tree-node-selected-fn nil)
(defvar tree-node-expanded-fn nil)
(defvar tree-node-collapsed-fn nil)
(defvar tree-node-mouse-over-fn nil)
(defvar tree-node-data-equal-fn nil)
(defvar tree-buffer-maybe-empty-node-types nil)
(defvar tree-buffer-leaf-node-types nil)
(defvar tree-buffer-highlight-overlay nil)
(defvar tree-buffer-general-face nil)
(defvar tree-buffer-general-overlay nil)
(defvar tree-buffer-incr-searchpattern nil)
(defvar tree-buffer-last-incr-searchpattern nil)
(defvar tree-buffer-incr-search nil)
(defvar tree-buffer-incr-searchpattern-indent-prefix nil
  "Prefix-pattern which ignores all not interesting basic stuff of a displayed
tag at incr. search. The following contents of a displayed tag are ignored
by this pattern:
- beginning spaces and guide characters \(|`-)
This prefix is computed by `tree-buffer-gen-searchpattern-indent-prefix'!")

(defvar tree-buffer-incr-search-additional-pattern nil
  "Every search-pattern is prefixed at least with
`tree-buffer-incr-searchpattern-indent-prefix' and
`tree-buffer-incr-searchpattern-expand-prefix' to jump over not important
stuff. If this var is not nil then it must be a cons-cell where car is a
string which should be a regexp-pattern which is added to the basic-prefix
pattern and both of them prefix the incr-search-pattern. The cdr is the number
of subexpr. in this pattern.")

(defvar tree-buffer-hor-scroll-step nil)

;; tree-buffer-local data-storage with get- and set-function
(defvar tree-buffer-data-store nil)
(defsubst tree-buffer-set-data-store (data)
  (setq tree-buffer-data-store data))
(defsubst tree-buffer-get-data-store ()
  tree-buffer-data-store)

(defvar tree-buffer-default-images-dir nil
  "Default path where to search for image-sourcefiles. This directory should
contain an image-source-file for every image-name of
`tree-buffer-tree-image-names'.")

(defvar tree-buffer-additional-images-dir nil
  "Additional path where to search for image-sourcefiles.")
(defvar tree-buffer-local-image-dir-cache nil
  "Alist with car is one of the names in `tree-buffer-tree-image-names' and
cdr is the full path where the image-file for this image-name resides.")

(defvar tree-buffer-style nil)
(defvar tree-buffer-ascii-guide-face nil)

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

;; image support

(defvar tree-buffer-enable-xemacs-image-bug-hack
  tree-buffer-running-xemacs
  "If true then ECB tries to deal best with the XEmacs-bug to display
adjacent images not correctly. Set this to nil if your XEmacs-version has fixed
this bug.")

(defconst tree-buffer-image-formats
  '((xpm ".xpm") (png ".png") (gif ".gif") (jpeg ".jpg" ".jpeg")
    (xbm ".xbm")))

(defconst tree-buffer-expand-symbol-length 3)
(defconst tree-buffer-indent-with-images 3)
(defconst tree-buffer-indent-w/o-images-before-min 3)
(defconst tree-buffer-indent-w/o-images-after-min 2)

(defconst tree-buffer-tree-image-names
  '(("open"      . ((after . "[-]") (before . "[-]")))
    ("close"     . ((after . "[+]") (before . "[+]")))
    ("empty"     . ((after . "[x]") (before . "[x]")))
    ("leaf"      . ((after . "*")   (before . "*")))
    ("guide"     . ((after . "|")   (before . " |")))
    ("no-guide"  . ((after . " ")   (before . "  ")))
    ("end-guide" . ((after . "`")   (before . " `")))
    ("handle"    . ((after . "-")   (before . "-")))
    ("no-handle" . ((after . " ")   (before . " "))))
  "This alist contains all allowed tree-image-names and their corresponding
ascii-representation. Currently allowed names for tree-images and current
ascii-symbols are: open, close, empty, leaf, guide, noguide, end-guide,
handle, no-handle. See the value of this constant for the ascii-symbols
related to the names.")

(defsubst tree-buffer-style ()
  (if tree-buffer-images-can-be-used
      tree-buffer-style
    (if (equal tree-buffer-style 'image)
        'ascii-guides
      tree-buffer-style)))

(defsubst tree-buffer-ascii-symbol-4-image-name (name)
  "Return the ascii-symbol which displays the tree-image NAME. This is done
according to the value of `tree-buffer-expand-symbol-before'. It always
returns a copy of the registered string in `tree-buffer-tree-image-names'!"
  (let ((sym (if tree-buffer-expand-symbol-before 'before 'after)))
    ;; Klaus Berndl <klaus.berndl@sdm.de>: If there are performance issues
    ;; concerning the tree-buffer-redisplay then maybe this copy-sequence is
    ;; the reason. But must be profiled! If yes, then an alternative could be
    ;; not to return copies but references and write a function which removes
    ;; all text-properties from the strings in `tree-buffer-tree-image-names'.
    ;; This function has either to be called once or within
    ;; `tree-buffer-create'. But for the moment we use copies.
    ;; Background: Without copies or without removing the text-properties from
    ;; the strings in `tree-buffer-tree-image-names' before using tree-buffers
    ;; we also get some images if we switch from image- to ascii-display
    ;; without restarting emacs.
    (copy-sequence
     (cdr (assoc sym (cdr (assoc name tree-buffer-tree-image-names)))))))


(defun tree-buffer-add-image-icon-maybe (start len str image-icon)
  "Add IMAGE-ICON to STR between START \(incl.) and START+LEN \(excl.). If
IMAGE-ICON is not nil \(which must be an image-object in the sense of
\(X)Emacs) then add this image to STR otherwise do nothing. Always return STR.
If IMAGE-ICON is nil or `tree-buffer-style' returns not 'image then START and
LEN are ignored!"
  (when (equal 'image (tree-buffer-style))
    ;; Regular images (created with `insert-image' are intangible
    ;; which (I suppose) make them more compatible with XEmacs 21.
    ;; Unfortunately, there is a giant pile o code dependent on the
    ;; underlying text.  This means if we leave it tangible, then I
    ;; don't have to change said giant piles o code.
    (if image-icon
        (if tree-buffer-running-xemacs
            (add-text-properties (+ start len) start
                                 (list 'end-glyph image-icon
                                       'rear-nonsticky (list 'display)
                                       'invisible t
                                       'detachable t)
                                 str)
          (add-text-properties start (+ start len)
                               (list 'display image-icon
                                     'rear-nonsticky (list 'display))
                               str))))
  str)

(defvar tree-buffer-global-image-cache nil
  "An alist where car is a full path to an image-file and cdr is an
image-object created from this image-file.")

(defsubst tree-buffer-image-cache-get (tree-image-name)
  (cdr (assoc tree-image-name
              tree-buffer-local-image-dir-cache)))

(defsubst tree-buffer-image-cache-put (tree-image-name image)
  (setq tree-buffer-local-image-dir-cache
        (cons (cons tree-image-name image)
              tree-buffer-local-image-dir-cache)))

(defun tree-buffer-find-image (tree-image-name)
  "Return an image-object for the TREE-IMAGE-NAME. The needed image-file with
name \"ecb-<TREE-IMAGE-NAME>.<a supported image-file-extension>\" is first
searched in current `tree-buffer-additional-images-dir' \(if not nil) and then
- if there is no image found for this name - in
`tree-buffer-default-images-dir'. All found and created image-objectes will be
cached so every image is only created once! Returns the image-object for
TREE-IMAGE-NAME."
  (and (equal 'image (tree-buffer-style))
       ;; Klaus Berndl <klaus.berndl@sdm.de>: This comes from the XEmacs-bug
       ;; not able to display adjacent images.
       (or (not tree-buffer-enable-xemacs-image-bug-hack)
           (not (member tree-image-name
                        '("handle" "no-handle"))))
       (or (tree-buffer-image-cache-get tree-image-name)
           (let ((dirs (mapcar 'expand-file-name
                               (if tree-buffer-additional-images-dir
                                   (list tree-buffer-additional-images-dir
                                         tree-buffer-default-images-dir)
                                 (list tree-buffer-default-images-dir))))
                 (fmt-specs tree-buffer-image-formats)
                 fmt fmt-exts file file-name image loc-dirs)
             (while (and fmt-specs (not file))
               (setq fmt (car (car fmt-specs))
                     fmt-exts (cdr (car fmt-specs))
                     fmt-specs (cdr fmt-specs))
               (when (tree-buffer-image-type-available-p fmt)
                 (while (and fmt-exts (not file))
                   (setq loc-dirs dirs)
                   (while (and loc-dirs (not file))
                     (setq file-name (concat (car loc-dirs)
                                             "/ecb-"
                                             tree-image-name
                                             (car fmt-exts)))
                     (when (file-readable-p file-name)
                       (setq file file-name))
                     (setq loc-dirs (cdr loc-dirs)))
                   (setq fmt-exts (cdr fmt-exts)))))
             (when file
               (setq image (tree-buffer-create-image file fmt))
               (tree-buffer-image-cache-put tree-image-name
                                            image)
               image)))))

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
              (or (tree-node-is-expandable node)
                  (member (tree-node-get-type node)
                          tree-buffer-maybe-empty-node-types)))
         (if (or tree-buffer-enable-xemacs-image-bug-hack
                 (not (equal 'image (tree-buffer-style))))
             4 3)
       0)
     (if (and tree-buffer-expand-symbol-before
              (not (tree-node-is-expandable node))
              (member (tree-node-get-type node)
                      tree-buffer-leaf-node-types))
         (if (or tree-buffer-enable-xemacs-image-bug-hack
                 (not (equal 'image (tree-buffer-style))))
             2 1)
       0)))
     

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
          (if (and (tree-buffer-at-expand-symbol name node p)
                   ;; if the expand-symbol is displayed before and mouse-button
                   ;; = 0, means RET is pressed, we do not toggle-expand but work
                   ;; as if point would not be at expand-symbol. This is for
                   ;; convenience.
                   (not (and (= mouse-button 0)
                             tree-buffer-expand-symbol-before)))
              (progn
                ; (when (tree-node-is-expandable node)
                (when (and (not (tree-node-is-expanded node))
                           tree-node-expanded-fn)
                  (funcall tree-node-expanded-fn node mouse-button
                           shift-pressed control-pressed (buffer-name)))
                (when (tree-node-is-expandable node)
                  (when (and (tree-node-is-expanded node)
                             tree-node-collapsed-fn)
                    (funcall tree-node-collapsed-fn node mouse-button
                             shift-pressed control-pressed (buffer-name)))
                  (tree-node-toggle-expanded node))
                ;; Update the tree-buffer with optimized display of NODE
                (tree-buffer-update node)
                ; ) ;; end (when (tree-node-is-expandable node)...
                )
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

(defsubst tree-buffer-get-node-indent (node)
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
  "Find the first node in current tree-buffer which has data equal to
NODA-DATA. When START-NODE is nil then all currently visible nodes are
searched beginning with the first one otherwise START-NODE is the startpoint
for the search.

If the search has success then a cons-cell is returned with car is the name of
the node and the cdr is the data of the node which is equal to NODE-DATA."
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
                           tree-buffer-nodes)))
          (equal-fcn 'tree-buffer-node-data-equal-p))
      (dolist (node node-list)
        (when (funcall equal-fcn (tree-node-get-data (cdr node)) node-data)
          (throw 'exit node))))))

(defun tree-buffer-search-node-list (find-fcn)
  (catch 'exit
    (dolist (node tree-buffer-nodes)
      (when (funcall find-fcn (cdr node))
        (throw 'exit (cdr node))))))

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
  "Return non nil if POS is horizontal visible in WINDOW otherwise nil."
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
;; XEmacs: this can be implemented using modeline key-maps, but there
;; is no use, as we have horizontal scrollbar (as the docstring
;; hints.)
(defun tree-buffer-mouse-hscroll (e)
  "Read a mouse event E from the mode line and scroll horizontally.
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

(defvar tree-buffer-hscroll-number 0)

(defun tree-buffer-recenter (node window)
  "If NODE is not visible then first recenter the window WINDOW so NODE is
best visible, means NODE is displayed in the middle of the window if possible.
If NODE is expanded then recenter the WINDOW so as much as possible subnodes
of NODE will be visible. If NODE is not expandable then WINDOW is always
displayed without empty-lines at the end, means WINDOW is always best filled."
  (let* ((node-points (save-excursion
                        (goto-line (tree-buffer-find-node node))
                        (cons (tree-buffer-line-beginning-pos)
                              (tree-buffer-line-end-pos))))
         (node-point (car node-points))
         (point-lines-before (count-lines (point-min) node-point))
         (point-lines-after (1- (count-lines node-point (point-max)))))
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
                            (tree-buffer-line-beginning-pos)))
        ))
    ;; now optimize the window display for displaying as much possible
    ;; subnodes of node.
    (if (tree-node-is-expanded node)
        (let ((exp-node-children-count (1+ (tree-node-count-subnodes-to-display node)))
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
                                        (- (+ 1
                                              ;; Cause of a bug in GNU Emacs
                                              ;; <= 21.3 we would need here an
                                              ;; extra +1 to display all
                                              ;; subnodes (otherwise the last
                                              ;; one is not displayed). But
                                              ;; this extra +1 would only be
                                              ;; needed if the tree-buffer is
                                              ;; not completely displayed in
                                              ;; the frames default font. But
                                              ;; we accept this because with
                                              ;; GNU Emacs >= 21.4 it seems to
                                              ;; be fixed.
                                              point-window-line
                                              exp-node-children-count)
                                           (tree-buffer-window-display-height window))))
                                  (tree-buffer-line-beginning-pos)))))
      ;; maybe there are empty lines in the window after the last non-empty
      ;; line. If they are we scroll until the whole window is filled with
      ;; non-empty lines.
      (if nil; (not (tree-node-is-expandable node))
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
    (if (not tree-buffer-running-xemacs)
        (ignore-errors (tree-buffer-hscroll -1000)))
    ;; KB: testcode
;;     (if (and (not tree-buffer-running-xemacs)
;;              (not (tree-buffer-pos-hor-visible-p (cdr node-points) window)))
;;         (ignore-errors (tree-buffer-hscroll -1000)))
    ))


;; Klaus: Now we use overlays to highlight current node in a tree-buffer. This
;; makes it easier to do some facing with the nodes itself and above all this
;; the faces of the node are always visible even if the node is highlighted
;; (useful e.g. if you show the sources in the ECB directory buffer, and if
;; you do some syntax highlighting in the method-buffer).
(defun tree-buffer-remove-highlight ()
  (when tree-buffer-highlighted-node-data
    (tree-buffer-overlay-delete tree-buffer-highlight-overlay))
  (setq tree-buffer-highlighted-node-data nil))

;; (defun tree-buffer-remove-highlight ()
;;   (when tree-buffer-highlighted-node-data
;;     (let ((node (tree-buffer-find-node-data tree-buffer-highlighted-node-data)))
;;       (when node
;;         (tree-buffer-overlay-delete tree-buffer-highlight-overlay))))
;;   (setq tree-buffer-highlighted-node-data nil))

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
          (setq tree-buffer-highlighted-node-data (cons node-data node))
          (save-excursion
            (tree-buffer-overlay-move tree-buffer-highlight-overlay
                                      (tree-buffer-get-node-name-start-point name node)
                                      (tree-buffer-get-node-name-end-point name node)))
          (when (not dont-make-visible)
            ;; make node visible if not and optimize the windows display for
            ;; the node.
            (tree-buffer-recenter node w))
          ;; we have highlighted the node so we return not nil.
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


(defun tree-buffer-insert-text (text &optional facer help-echo mouse-highlight)
  "Insert TEXT at point and faces it with FACER. FACER can be a face then the
text gets this face or it can be a function-symbol which is called to face the
inserted TEXT. Such a function gets two arguments: Point where TEXT has been
inserted and the TEXT itself"
  (when (stringp text)
    (let ((p (point)))
      (insert text)
      (if mouse-highlight
          (put-text-property p (+ p (length text)) 'mouse-face 'highlight))
      (if (and help-echo (not tree-buffer-running-xemacs))
          (put-text-property p (+ p (length text)) 'help-echo
                             'tree-buffer-help-echo-fn))
      (if facer
          (if (functionp facer)
              (funcall facer p text)
            (put-text-property p (+ p (length text)) 'face facer))))))


(defun tree-buffer-insert-node-display (node node-display-name)
  (let* ((node-type (tree-node-get-type node))
         (tree-image-name (if (and (tree-node-is-expanded node)
                                   (tree-node-is-expandable node))
                              "open"
                            (if (not (tree-node-is-expandable node))
                                (if (member node-type
                                            tree-buffer-maybe-empty-node-types)
                                    "empty"
                                  (if (member node-type
                                              tree-buffer-leaf-node-types)
                                      "leaf"
                                    nil))
                              "close")))
         (ascii-symbol (tree-buffer-ascii-symbol-4-image-name tree-image-name)))
    (when (and tree-buffer-expand-symbol-before
	       ascii-symbol tree-image-name)
      (tree-buffer-insert-text 
       (tree-buffer-add-image-icon-maybe
        0 (length ascii-symbol)
        ascii-symbol (tree-buffer-find-image tree-image-name))
       nil nil t)
      (if (or tree-buffer-enable-xemacs-image-bug-hack
              (not (equal 'image (tree-buffer-style))))
          (insert " ")))
    (tree-buffer-insert-text node-display-name
                             (tree-buffer-get-node-facer node) t t)
    (when (and (not tree-buffer-expand-symbol-before)
	       ascii-symbol)
      (insert " ")
      (tree-buffer-insert-text ascii-symbol nil nil t))
    (insert "\n")))

(defsubst tree-buffer-aset (array idx newelt)
  (aset array idx newelt)
  array)

(defun tree-buffer-gen-guide-strings ()
  "Returns a list with four elements - the correct guide-strings for current
tree-buffer: \(guide-str-handle guide-str-no-handle guide-end-str no-guide-str)"
  (if (equal 'ascii-no-guides (tree-buffer-style))
      (make-list 4 (make-string tree-buffer-indent ? ))
    (let* ((indent-fill-up (make-string
                          (- tree-buffer-indent
                             (cond ((equal 'image (tree-buffer-style))
                                    tree-buffer-indent-with-images)
                                   (tree-buffer-expand-symbol-before
                                    tree-buffer-indent-w/o-images-before-min)
                                   (t
                                    tree-buffer-indent-w/o-images-after-min)))
                          ? ))
           (guide-str-handle (concat (tree-buffer-ascii-symbol-4-image-name
                                      "guide")
                                     (tree-buffer-ascii-symbol-4-image-name
                                      "handle")
                                     indent-fill-up))
           (guide-str-no-handle (concat (tree-buffer-ascii-symbol-4-image-name
                                         "guide")
                                        (tree-buffer-ascii-symbol-4-image-name
                                         "no-handle")
                                        indent-fill-up))
           (guide-end-str (concat (tree-buffer-ascii-symbol-4-image-name
                                   "end-guide")
                                  (tree-buffer-ascii-symbol-4-image-name
                                   "handle")
                                  indent-fill-up))
           (no-guide-str (concat (tree-buffer-ascii-symbol-4-image-name
                                  "no-guide")
                                 (tree-buffer-ascii-symbol-4-image-name
                                  "no-handle")
                                 indent-fill-up)))
      (list guide-str-handle guide-str-no-handle guide-end-str no-guide-str))))

(defun tree-buffer-add-node (node indent-str-first-segs indent-str-last-seg
                                  &optional last-children)
  "Insert NODE in current tree-buffer at point.
The indentation is the concatenation of INDENT-STR-FIRST-SEGS and
INDENT-STR-LAST-SEG. If LAST-CHILDREN is not nil then NODE is the last
children of its parent-node; this means it must be displayed with an
end-guide."
  (let* ((ww (window-width))
	 (name (tree-node-get-name node))
	 (width (+ (length indent-str-first-segs)
                   (length indent-str-last-seg)
		   (length name)
		   (if (tree-node-is-expandable node) 4 0))))
    ;; Truncate name if necessary
    (when (and (>= width ww)
               (> (length name)
                  (+ (if tree-buffer-running-xemacs 5 4) ;; for the "..." + space
                     (- width ww)
                     3))) ;; there should at least remain 3 visible chars of name
      (if (eq 'beginning (tree-node-get-shorten-name node))
	  (setq name
                (concat "..."
                        (substring name (+ (if tree-buffer-running-xemacs 5 4)
                                           (- width ww)))))
	(if (and (not tree-buffer-expand-symbol-before)
		 (tree-node-is-expandable node)
		 (eq 'end (tree-node-get-shorten-name node)))
	    (setq name
                  (concat (substring name 0
                                     (- (+ (if tree-buffer-running-xemacs 5 4)
                                           (- width ww))))
                          "...")))))
    ;; insert the indent-string
    (when tree-buffer-ascii-guide-face
      (put-text-property 0 (length indent-str-first-segs)
                         'face tree-buffer-ascii-guide-face
                         indent-str-first-segs)
      (put-text-property 0 (length indent-str-last-seg)
                         'face tree-buffer-ascii-guide-face
                         indent-str-last-seg))
    (insert (concat indent-str-first-segs indent-str-last-seg))
    ;; insert the node with all its symbols - either as image or ascii
    (tree-buffer-insert-node-display node name)
    ;; add the node to the `tree-buffer-nodes'
    (setq tree-buffer-nodes
          (append tree-buffer-nodes (list (cons name node))))
    ;; compute the indentation-strings for the children and run recursive for
    ;; each child
    (if (tree-node-is-expanded node)
        (let* ((number-of-childs (length (tree-node-get-children node)))
               (counter 0)
               (guide-strings (tree-buffer-gen-guide-strings))
               (guide-str (if (and (equal 'image (tree-buffer-style))
                                   tree-buffer-enable-xemacs-image-bug-hack)
                              (nth 0 guide-strings)
                            (nth 1 guide-strings)))
               (guide-end-str (nth 2 guide-strings))
               (no-guide-str (nth 3 guide-strings))
               (indent-str-last-seg-copy (copy-sequence indent-str-last-seg))
               (next-indent-str-first-segs
                (if (= 0 (length indent-str-last-seg-copy))
                    ""
                  (concat indent-str-first-segs
                          (if last-children
                              (tree-buffer-add-image-icon-maybe
                               2 1
                               (tree-buffer-add-image-icon-maybe
                                0 2 no-guide-str
                                (tree-buffer-find-image "no-guide"))
                               (tree-buffer-find-image "no-handle"))
                            (tree-buffer-add-image-icon-maybe
                             2 1
                             (tree-buffer-aset
                              indent-str-last-seg-copy
                              (1- (cond ((equal 'image (tree-buffer-style))
                                         tree-buffer-indent-with-images)
                                        (tree-buffer-expand-symbol-before
                                         tree-buffer-indent-w/o-images-before-min)
                                        (t
                                         tree-buffer-indent-w/o-images-after-min)))
                              ? )
                             (tree-buffer-find-image "no-handle"))))))
               (next-indent-str-last-seg-std
                (tree-buffer-add-image-icon-maybe
                 2 1
                 (tree-buffer-add-image-icon-maybe
                  0 2 guide-str
                  (tree-buffer-find-image "guide"))
                 (tree-buffer-find-image "handle")))
               (next-indent-str-last-seg-end
                (tree-buffer-add-image-icon-maybe
                 2 1
                 (tree-buffer-add-image-icon-maybe
                  0 2 guide-end-str
                  (tree-buffer-find-image "end-guide"))
                 (tree-buffer-find-image "handle"))))
          (dolist (node (tree-node-get-children node))
            (setq counter (1+ counter))
            (tree-buffer-add-node node
                                  next-indent-str-first-segs
                                  (if (= counter number-of-childs )
                                      next-indent-str-last-seg-end
                                    next-indent-str-last-seg-std)
                                  (= counter number-of-childs )))))))

(defun tree-buffer-clear ()
  "Clear current tree-buffer, i.e. remove all children of the root-node"
  (dolist (child (tree-node-get-children (tree-buffer-get-root)))
    (tree-buffer-remove-node child)))

(defun tree-buffer-remove-node (node &optional empty-parent-types)
  "Remove NODE from current tree-buffer. If NODE is nil or NODE eq the node
returned by `tree-buffer-get-root' then nothing will be done. If
EMPTY-PARENT-TYPES is not nil and a list of node-types \(see
`tree-buffer-create') and if the node-type of the parent of node is contained
in EMPTY-PARENT-TYPES and if NODE is the only children of its parent then its
parent is recursively removed too."
  (when (and node (not (eq (tree-buffer-get-root) node)))
    (let* ((parent (tree-node-get-parent node))
           (parent-type (tree-node-get-type parent)))
      ;; If parent is the root-node then its type is always -1 (only the
      ;; root-node has type -1) and therefore then the recursion stops here
      ;; savely.
      (if (and (member parent-type empty-parent-types)
               (= (length (tree-node-get-children parent)) 1))
          (tree-buffer-remove-node parent empty-parent-types)
        (tree-node-remove-child parent node)))))


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
    (tree-buffer-add-node node "" "")))

(defun tree-buffer-display-in-general-face ()
  "Apply the `tree-buffer-general-face' of current tree-buffer to current
tree-buffer."
  (when tree-buffer-general-face
    (tree-buffer-overlay-move tree-buffer-general-overlay
                              (point-min) (point-max))))

(defun tree-buffer-empty-p ()
  (= (point-min) (point-max)))

(defun tree-buffer-update (&optional node content)
  "Updates the current tree-buffer. The buffer will be completely rebuild with
it큦 current nodes. Window-start and point will be preserved. If NODE is not
nil and a valid and expanded node with at least one child then the display of
this node is optimized so the node itself and as much as possible of it큦
children \(and also recursive the children of a child if it큦 already
expanded, see `tree-node-count-subnodes-to-display') are visible in current
tree-buffer. If CONTENT is not nil then it must be a cons-cell where the car
is the whole string of the tree-buffer and the cdr is the value of
`tree-buffer-nodes'. Then the content of the tree-buffer will not be rebuild
by reinserting all nodes from the tree-node-structure but just by inserting
the car of CONTENT in the tree-buffer and setting `tree-buffer-nodes' to cdr
of CONTENT."
  (let* ((w (get-buffer-window (current-buffer)))
         (ws (window-start w))
         (p (point))
         (buffer-read-only nil)
         (next-line-add-newlines nil))
    (erase-buffer)
    (if (consp content)
        (progn
          (insert (car content))
          (setq tree-buffer-nodes (cdr content)))
      (tree-buffer-build-tree-buffer-nodes))
    (tree-buffer-display-in-general-face)
    (tree-buffer-highlight-node-data
     (or nil ;;(and node (tree-node-get-data node))
         (car tree-buffer-highlighted-node-data))
     (cdr tree-buffer-highlighted-node-data)
     nil)
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

(defun tree-buffer-expand-node (node level
                                     &optional expand-pred-fn collapse-pred-fn)
  "Expand the NODE up to an expand-level of LEVEL.

LEVEL specifies precisely which level of nodes should be expanded. LEVEL means
the indentation-level of the NODE itself and its \(recursive) subnodes
relative to the NODE itself.

A LEVEL value X means that all \(sub)nodes with an indentation-level <= X
relative to NODE are expanded and all other are collapsed. A negative LEVEL
value means that NODE is collapsed.

This function expands beginning from NODE the NODE itself and all subnodes of
NODE with level <= LEVEL, so the subnodes of these nodes get visible and
collapses all their \(recursive) subnodes with indentation-level > LEVEL.

If a node has to be expanded then first the `tree-node-expanded-fn' of current
tree-buffer \(see `tree-buffer-create') is called with the argument-values
\[node 0 nil nil \(buffer-name)\].

This function gets two optional function-arguments which are called to test if
a node should be excluded from expanding or collapsing; both functions are
called with two arguments, where the first one is the expandable/collapsable
node and the second one is the current level of indentation of this node
relativ to the startnode NODE: EXPAND-PRED-FN is called if a node has to be
expanded and must return nil if this node should not be expanded even if its
indentation level is <= LEVEL and COLLAPSE-PRED-FN is called analogous for a
node which has to be collapsed and must return nil if the node should not be
collapsed even if its indentation level is > then LEVEL.

Examples:
- LEVEL = -1 collapses the NODE.
- LEVEL = 0 expands only the NODE itself because it is the only node which can
  have no indentation relativ to itself.
- LEVEL = 2 expands the NODE itself, its children and its grandchildren -
  these are the nodes which are either not indented \(the NODE itself) or
  indented once \(the children) or twice \(the grandchildren)."
  (if (not (equal (tree-buffer-get-root) node))
      (tree-buffer-expand-node-internal node 0 level
                                        expand-pred-fn collapse-pred-fn)))

(defun tree-buffer-expand-node-internal (node current-level level
                                              expand-pred-fn collapse-pred-fn)
  "Expand NODE if CURRENT-LEVEL \(the indentation-level of NODE) <= LEVEL or
collapses NODE if CURRENT-LEVEL > LEVEL. Do this recursive for subnodes of
NODE with incremented CURRENT-LEVEL. For EXPAND-PRED-FN and COLLAPSE-PRED-FN
see `tree-buffer-expand-node'. This function is not for external usage; use
`tree-buffer-expand-node' instead."
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
      (tree-buffer-expand-node-internal child (1+ current-level) level
                                        expand-pred-fn collapse-pred-fn))))

(defun tree-buffer-set-root (root)
  (setq tree-buffer-root root)
  (tree-node-set-expanded tree-buffer-root t))

(defun tree-buffer-get-root ()
  tree-buffer-root)


(defconst tree-buffer-incr-searchpattern-expand-prefix
  "\\(\\[[^][]+\\] ?\\)?\\[?"
  "The prefix ignores all expand/collapse-buttons: \[+], \[x], rsp. \[-]")

(defun tree-buffer-gen-searchpattern-indent-prefix (&optional count)
  (let ((guide-strings (tree-buffer-gen-guide-strings)))
    (concat "^\\("
            (mapconcat (function (lambda (e)
                                   (format "\\(%s\\)" e)))
                       (list (nth 1 guide-strings)
                             (nth 3 guide-strings)
                             (nth 0 guide-strings)
                             (nth 2 guide-strings))
                       "\\|")
            "\\)"
            (if (integerp count)
                (format "\\{%d\\}" count)
              "*"))))


;; idea is stolen from ido.el, written by Kim F. Storm <stormware@get2net.dk>
(defun tree-buffer-find-common-substring (lis subs &optional only-prefix)
  "Return common substring beginning with SUBS in each element of LIS. If
ONLY-PREFIX is not nil then only common prefix is returned."
  (let ((change-word-sub (concat (if only-prefix
                                     (concat "^" (car tree-buffer-incr-search-additional-pattern))
                                   "")
                                 "\\(" (regexp-quote subs) "\\)"))
        res alist)
    (setq res
          (mapcar (function (lambda (word)
                              (if (string-match change-word-sub word)
                                  (substring word
                                             (match-beginning
                                              (if (and only-prefix
                                                       (cdr tree-buffer-incr-search-additional-pattern))
                                                  (1+ (cdr tree-buffer-incr-search-additional-pattern))
                                                1)))
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
  "Incremental search for a node in current tree-buffer.
Each display-able key \(e.g. all keys normally bound to `self-insert-command')
is appended to the current search-pattern. The tree-buffer tries to jump to
the current search-pattern. If no match is found then nothing is done. Some
special keys:
- \[backspace] and \[delete]: Delete the last character from the search-pattern.
- \[home]: Delete the complete search-pattern
- \[end]: Expand either to a complete node if current search-pattern is
         already unique or expands to the greatest common prefix of the nodes.
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
             (if (not (and (equal last-comm 'end)
                           (string= tree-buffer-incr-searchpattern
                                    tree-buffer-last-incr-searchpattern)))
                 (goto-char (point-min)))
             (re-search-forward
              (concat tree-buffer-incr-searchpattern-indent-prefix
                      tree-buffer-incr-searchpattern-expand-prefix
                      (car tree-buffer-incr-search-additional-pattern)
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

(defun tree-buffer-create-menu-emacs (menu-def menu-name)
  (let ((map (make-sparse-keymap menu-name))
        (counter 0)
        (menu-items (reverse menu-def)))
    (dolist (item menu-items)
      (cond ((string-equal (car item) "---")
             (define-key map
               (make-vector 1
                            (setq counter (1+ counter)))
               (list "---")))
            ((stringp (cadr item)) ;; menu-entry
             (define-key map
               (make-vector 1
                            (setq counter (1+ counter)))
               (cons (cadr item) (car item))))
            (t ;; submenu
             (define-key map
               (make-vector 1
                            (setq counter (1+ counter)))
               (cons (car item)
                     (tree-buffer-create-menu-emacs (cdr item) (car item)))))))
    map))


(defun tree-buffer-create-menu-xemacs (menu-def)
  (when menu-def
    (let ((item (car menu-def)))
      (cons (cond ((string-equal (car item) "---")
		   (car item))
		  ((stringp (cadr item)) ;; menu-entry
                   (let ((v (make-vector 3 t)))
                     (aset v 0 (cadr item))
                     (aset v 1 (list (car item)
                                     '(tree-buffer-get-node-at-point)))
                     (aset v 2 t)
                     v))
		  (t ;; submenu
                   `(,(car item)
                     ,@(tree-buffer-create-menu-xemacs (cdr item)))))
	    (tree-buffer-create-menu-xemacs (cdr menu-def))))))

(defun tree-buffer-create-menu (menu-items)
  "Creates a popup menu from a list with menu items."
  (when menu-items
    (if tree-buffer-running-xemacs
        (tree-buffer-create-menu-xemacs menu-items)
      (tree-buffer-create-menu-emacs menu-items "dummy-name"))))


(defun tree-buffer-create-menus (menus)
  "Creates a popup menus from an assoc list with menus."
  (when menus
    (cons (cons (caar menus)
		(tree-buffer-create-menu (cdar menus)))
	  (tree-buffer-create-menus (cdr menus)))))

;; Klaus Berndl <klaus.berndl@sdm.de>: Seems that the docstring of
;; x-popup-menu is wrong because it seems this function needs offsets related
;; to current window not to frame!
;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: For XEmacs this does not work!
(defun tree-buffer-show-menu-keyboard (&optional use-tmm)
  "Activate the popup-menu of current tree-buffer via keyboard."
  (interactive "P")
  (if use-tmm
      (unless (not (equal (selected-frame) tree-buffer-frame))
        (when tree-buffer-menu-creator
          (let ((node (tree-buffer-get-node-at-point)))
            (when (and (not tree-buffer-running-xemacs)
                       node
                       (locate-library "tmm"))
              (let ((menu (cdr (assoc (tree-node-get-type node)
                                      (tree-buffer-create-menus
                                       (funcall tree-buffer-menu-creator
                                                (buffer-name)))))))
                (tmm-prompt menu))))))
    (if tree-buffer-running-xemacs
        (tree-buffer-show-menu (get-buffer-window (current-buffer)
                                                  tree-buffer-frame))
      (let ((curr-frame-ypos (* (/ (frame-pixel-height) (frame-height))
                                (count-lines (window-start) (point))))
            (curr-frame-xpos (* (/ (frame-pixel-width) (frame-width))
                                (current-column))))
        (tree-buffer-show-menu (list (list curr-frame-xpos curr-frame-ypos)
                                     (selected-window)))))))



(defun tree-buffer-show-menu (&optional event)
  (unless (not (equal (selected-frame) tree-buffer-frame))
    (when tree-buffer-menu-creator
      (let ((node (tree-buffer-get-node-at-point)))
	(when node
	  (let* ((menu (cdr (assoc (tree-node-get-type node)
                                   (tree-buffer-create-menus
                                    (funcall tree-buffer-menu-creator
                                             (buffer-name))))))
                 (menu-title-creator
                  (cdr (assoc (tree-node-get-type node) tree-buffer-menu-titles)))
                 (menu-title (cond ((stringp menu-title-creator)
                                    menu-title-creator)
                                   ((functionp menu-title-creator)
                                    (funcall menu-title-creator node))
                                   (t "Tree-buffer-menu"))))
            (when menu
	      (if tree-buffer-running-xemacs
                  (if (windowp event)
                      (popup-menu-and-execute-in-window (cons menu-title menu)
                                                        event)
                    (popup-menu (cons menu-title menu)))
                ;; we must set the title for the menu-keymap
                (setcar (member (nth (1- (length menu)) menu) menu)
                        menu-title)
		(let* ((menu-selection (apply 'vector
                                              (x-popup-menu event menu)))
                       (fn (if (and menu-selection
                                    (> (length menu-selection) 0))
                               (lookup-key menu menu-selection))))
                  (when (functionp fn)
		    (funcall fn node)))))))))))


(defmacro tree-buffer-defpopup-command (name docstring &rest body)
  "Define a new popup-command for a tree-buffer.
NAME is the name of the popup-command to create. It will get one optional
argument NODE \(s.b.). DOCSTRING is a documentation string to describe the
function. BODY is the code evaluated when this command is called from a
popup-menu of a tree-buffer. BODY can refer to NODE which is bound to the node
for which this popup-command is called \(i.h. that node with the point at
call-time of this command). With the function `tree-node-get-data' the related
data of this NODE is accessible and returns for example in case of the
directories buffer the directory for which the popup-menu has been opened. The
BODY can do any arbitrary things with this node-data.

Example for the usage of this macro:

\(tree-buffer-defpopup-command ecb-my-special-dir-popup-function
   \"Prints the name of the directory of the node under point.\"
  \(let \(\(node-data=dir \(tree-node-get-data node)))
    \(message \"Dir under node: %s\" node-data=dir)))"
  `(eval-and-compile
     (defun ,name (&optional node)
       ,docstring
       (interactive)
       (let ((node (if (and (interactive-p) (null node))
                       (tree-buffer-get-node-at-point)
                     node)))
         (when node
           ,@body)))))

(put 'tree-buffer-defpopup-command 'lisp-indent-function 1)


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
key sequence, e.g. the \"C-h\" of the key sequence \"C-h v\".")

(defun tree-buffer-do-mouse-tracking ()
  "This function is called every time Emacs is idle for seconds defined in
`tree-buffer-track-mouse-idle-delay'. It enables mouse-tracking but only if
isearch is not active and if no uncompleted key sequence is open, means if this
function is called by the idle timer during a key sequence is inserted by the
user \(e.g. between the \"C-h\" and the \"v\" of the key sequence \"C-h v\"),
then mouse-tracking is always not enabled, because otherwise all very slightly
\(invisible) and unintended mouse-movements \(can occur for example only by
the convulsion cause of hitting keys onto the keyboard!) would break the
key sequence!"
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
            (tree-buffer-run-with-idle-timer tree-buffer-track-mouse-idle-delay
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
      (tree-buffer-cancel-timer tree-buffer-track-mouse-timer)
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
  "Perform the defined action after a TAB-hit."
  (interactive)
  (unless (not (equal (selected-frame) tree-buffer-frame))
    (let ((node (tree-buffer-get-node-at-point)))
      (when (tree-node-is-expandable node)
	(when (and tree-node-expanded-fn
		   (not (tree-node-is-expanded node)))
	  (funcall tree-node-expanded-fn node 0 nil nil (buffer-name)))
        (when (tree-node-is-expandable node)
          (when (and (tree-node-is-expanded node)
                     tree-node-collapsed-fn)
            (funcall tree-node-collapsed-fn node 0 nil nil (buffer-name)))
          (tree-node-toggle-expanded node))
	;; Update the tree-buffer with optimized display of NODE           
	(tree-buffer-update node)))))

(defun tree-buffer-return-pressed (&optional shift-pressed control-pressed)
  (unless (not (equal (selected-frame) tree-buffer-frame))
    ;; reinitialize the select pattern after selecting a node
    (setq tree-buffer-incr-searchpattern "")
    (tree-buffer-select 0 shift-pressed control-pressed)))

(defun tree-buffer-arrow-pressed ()
  "Perform smart arrow-key navigation/movement."
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
               (beginning-of-line)
               (re-search-forward tree-buffer-incr-searchpattern-indent-prefix nil t)))
            ((equal arrow-key 'left)
             (if (tree-node-is-expanded node)
                 (tree-buffer-tab-pressed)
               ;; jump to next higher node
               (let* ((new-indent-factor (/ (max 0 (- (tree-buffer-get-node-indent node)
                                                      tree-buffer-indent))
                                            tree-buffer-indent))
                      (search-string
                       (concat (tree-buffer-gen-searchpattern-indent-prefix new-indent-factor)
                               "[^ \t]")))
                 (re-search-backward search-string nil t)
                 (beginning-of-line)
                 (re-search-forward tree-buffer-incr-searchpattern-indent-prefix nil t))))))))


;; tree-buffer creation

(defun tree-buffer-create (name frame is-click-valid-fn node-selected-fn
                                node-expanded-fn node-collapsed-fn node-mouse-over-fn
                                node-data-equal-fn maybe-empty-node-types leaf-node-types
                                menu-creator menu-titles tr-lines read-only tree-indent
                                incr-search incr-search-add-pattern arrow-navigation hor-scroll
                                &optional
                                default-image-dir add-image-dir tree-style ascii-guide-face
                                type-facer expand-symbol-before highlight-node-face general-face
                                after-create-hook)
  "Creates a new tree buffer and returns the newly created buffer.
This function creates also a special data-storage for this tree-buffer which
can be accessed via `tree-buffer-set-data-store' and `tree-buffer-get-data-store'.
The user of this tree-buffer can store any arbitrary data in this storage.
Before using the accessor-functions above the tree-buffer has to be the
current buffer!

NAME: Name of the buffer.
FRAME: Frame in which the tree-buffer is displayed and valid. All key-bindings
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
                  This function is called with the following parameters:
                  - node: The selected node
                  - mouse-button \(0 = RET, 1 = mouse-1, 2 = mouse 2)
                  - shift-pressed
                  - control-pressed
                  - tree-buffer-name
                  For the last four arguments see the description above. This
                  function has to ensure that the expandable- and
                  expanded-state of the selected node is correct after
                  returning.
NODE-EXPANDED-FN: Function to call if a node is expandable, point stays onto
                  the expand-symbol and node is not already expanded. This
                  function is called with the following parameters:
                  - node: The selected node
                  - mouse-button \(0 = TAB, 1 = mouse-1, 2 = mouse 2)
                  - shift-pressed
                  - control-pressed
                  - tree-buffer-name
                  This function should add all children nodes to this node
                  \(if possible). This function has to ensure that the
                  expandable- and expanded state of the selected node is
                  correct after returning!
NODE-COLLAPSED-FN: Function to call if a node is expandable, point stays
                   onto the expand-symbol and node is already expanded.
                   This function is called with the following parameters:
                   - node: The selected node
                   - mouse-button \(0 = TAB, 1 = mouse-1, 2 = mouse 2)
                   - shift-pressed
                   - control-pressed
                   - tree-buffer-name
                   This function is only a callback to inform the user of
                   this tree-buffer that this node has been collapsed. This
                   function must not modify the expandable- or expanded
                   state of the selected node!
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
MAYBE-EMPTY-NODE-TYPES: Nil or a list of node-types \(a node-type is an
                        integer which must be set for `tree-node-new'). Nodes
                        with one of these types are treated as empty if they
                        are not expandable \(i.e. they have no children) and
                        will be displayed with the empty-symbol \(\[x]); for
                        other nodes see next argument.
LEAF-NODE-TYPES: Nil or a list of node-types \(see above). Nodes
                  with one of these types are treated as leafs and will be
                  displayed with the leaf-symbol \(*). Summary:
                  * Expandable nodes will always be displayed either with the
                    open- or with the close-symbol.
                  * Not-expandable nodes with a node-type contained in
                    MAYBE-EMPTY-NODE-TYPES will be displayed with the
                    empty-symbol.
                  * Not-expandable nodes with a node-type contained in
                    LEAF-NODE-TYPES will be displayed with the leaf-symbol.
                  * All other nodes will be displayed with no symbol just with
                    correct indentation.
MENU-CREATOR: A function which has to return nil or a list of one to three
              conses, each cons for a node-type \(0, 1 or 2) Example: \(\(0 .
              menu-for-type-0) \(1 . menu-for-type-1)). The cdr of a cons must
              be a menu. This function gets one argument: The name of the
              tree-buffer for which a popup-menu should be opened.
MENU-TITLES: Nil or a list of one to three conses, each cons for a node-type
             \(0, 1 or 2). See MENU-CREATOR. The cdr of a cons must be either
             a string or a function which will be called with current node
             under point and must return a string which is displayed as the
             menu-title.
TR-LINES: Should lines in this tree buffer be truncated \(not nil)
READ-ONLY: Should the treebuffer be read-only \(not nil)
TREE-INDENT: spaces subnodes should be indented.
INCR-SEARCH: Should the incremental search be enabled in the tree-buffer.
             Three choices: 'prefix, 'substring, nil. See
             `tree-buffer-incremental-node-search'.
INCR-SEARCH-ADD-PATTERN: Every search-pattern is prefixed at least with
                         `tree-buffer-incr-searchpattern-indent-prefix' and
                         `tree-buffer-incr-searchpattern-expand-prefix' to
                         jump over not important stuff. If this argument is
                         not nil then it must be a cons-cell where car is a
                         string which should be a regexp-pattern which is
                         added to the basic-prefix pattern and both of them
                         prefix the incr-search-pattern. The cdr is the number
                         of subexpr. in this pattern.
ARROW-NAVIGATION: If not nil then smart navigation with horizontal arrow keys.
HOR-SCROLL: Number of columns a hor. scroll in the tree-buffer should scroll.
            If not nil then M-mouse-1 and M-mouse-2 scroll left and right and
            also M-<left-arrow> and M-<right-arrow>.
DEFAULT-IMAGE-DIR: Full path where the default images for the tree-buffer can
                   be found. It should contain an image for every name of
                   `tree-buffer-tree-image-names'.
ADD-IMAGE-DIR: Additional image-dir which should be searched first for images
               needed for current tree-buffer. If the image can not be found
               in this directory then DEFAULT-IMAGE-DIR is searched. If the
               image can't even found here the related ascii-symbol is used.
TREE-STYLE: If 'image then images are used for the tree-buffer - if possible.
            If 'ascii-guides then the tree is drwan with pure ascii but with
            guide-lines. If 'ascii-no-guides then an ascii-tree w/o
            guide-lines is drawn
ASCII-GUIDE-FACE: If TREE-STYLE is 'ascii-guides then this defines the face
                  the guides should be displayed with.
TYPE-FACER: Nil or a list of one or more conses, each cons for a node-type \(a
            node-type is an integer which must be set for `tree-node-new').
            The cdr of a cons can be:
            - a symbol of a face
            - a symbol of a function which gets to arguments \(see
              `tree-buffer-insert-text'). This function can do anything, but
              normally it should face a tree-buffer node.
            - the symbol t. Then the tree-buffer assumes that the node-text is
              already faced and therefore it does not face the node, means it
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
                   additional key-bindings for this tree-buffer local keymap."
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
    (make-local-variable 'tree-node-collapsed-fn)
    (make-local-variable 'tree-node-update-fn)
    (make-local-variable 'tree-node-mouse-over-fn)
    (make-local-variable 'tree-node-data-equal-fn)
    (make-local-variable 'tree-buffer-maybe-empty-node-types)
    (make-local-variable 'tree-buffer-leaf-node-types)
    (make-local-variable 'tree-buffer-highlighted-node-data)
    (make-local-variable 'tree-buffer-menu-creator)
    (make-local-variable 'tree-buffer-menu-titles)
    (make-local-variable 'tree-buffer-type-facer)
    (make-local-variable 'tree-buffer-expand-symbol-before)
    (make-local-variable 'tree-buffer-highlight-overlay)
    (make-local-variable 'tree-buffer-general-face)
    (make-local-variable 'tree-buffer-general-overlay)
    (make-local-variable 'tree-buffer-incr-searchpattern)
    (make-local-variable 'tree-buffer-last-incr-searchpattern)
    (make-local-variable 'tree-buffer-incr-search)
    (make-local-variable 'tree-buffer-incr-searchpattern-indent-prefix)
    (make-local-variable 'tree-buffer-incr-search-additional-pattern)
    (make-local-variable 'tree-buffer-hor-scroll-step)
    (make-local-variable 'tree-buffer-default-images-dir)
    (make-local-variable 'tree-buffer-additional-images-dir)
    (make-local-variable 'tree-buffer-style)
    (make-local-variable 'tree-buffer-ascii-guide-face)
    (make-local-variable 'tree-buffer-hscroll-number)
    
    ;; initialize the user-data-storage for this tree-buffer.
    (set (make-local-variable 'tree-buffer-data-store) nil)
    ;; initialize the local image-cache for this tree-buffer
    (set (make-local-variable 'tree-buffer-local-image-dir-cache) nil)

    (setq truncate-lines tr-lines)
    (setq truncate-partial-width-windows tr-lines)
    (setq buffer-read-only read-only)
    (setq tree-buffer-key-map (make-sparse-keymap))
    (setq tree-buffer-frame frame)
    (setq tree-buffer-is-click-valid-fn is-click-valid-fn)
    (setq tree-node-selected-fn node-selected-fn)
    (setq tree-node-expanded-fn node-expanded-fn)
    (setq tree-node-collapsed-fn node-collapsed-fn)
    (setq tree-node-mouse-over-fn node-mouse-over-fn)
    (setq tree-node-data-equal-fn node-data-equal-fn)
    (setq tree-buffer-maybe-empty-node-types maybe-empty-node-types)
    (setq tree-buffer-leaf-node-types leaf-node-types)
    (setq tree-buffer-highlighted-node-data nil)
    (setq tree-buffer-menu-creator menu-creator)
    (setq tree-buffer-menu-titles menu-titles)
    (setq tree-buffer-root (tree-node-new-root))
    (setq tree-buffer-type-facer type-facer)
    (setq tree-buffer-highlight-overlay (tree-buffer-make-overlay 1 1))
    (tree-buffer-overlay-put tree-buffer-highlight-overlay
                             'face highlight-node-face)
    ;; setting general face and overlay for the tree-buffer
    (setq tree-buffer-general-face general-face)
    (setq tree-buffer-general-overlay (tree-buffer-make-overlay 1 1))
    (tree-buffer-overlay-put tree-buffer-general-overlay 'face
                             tree-buffer-general-face)
    ;; initializing the search-pattern
    (setq tree-buffer-incr-searchpattern "")
    (setq tree-buffer-last-incr-searchpattern "")
    (setq tree-buffer-incr-search incr-search)
    (setq tree-buffer-incr-search-additional-pattern incr-search-add-pattern)
    (setq tree-buffer-hor-scroll-step hor-scroll)
    (setq tree-buffer-default-images-dir default-image-dir)
    (setq tree-buffer-additional-images-dir add-image-dir)
    (setq tree-buffer-style tree-style)
    (setq tree-buffer-ascii-guide-face ascii-guide-face)
    (setq tree-buffer-hscroll-number 0)

    ;; special settings for tree-buffer-indent and
    ;; tree-buffer-expand-symbol-before
    (cond ((equal 'image (tree-buffer-style))
           (setq tree-buffer-indent tree-buffer-indent-with-images
                 tree-buffer-expand-symbol-before t))
          (expand-symbol-before
           (setq tree-buffer-indent
                 (if (< tree-indent
                        tree-buffer-indent-w/o-images-before-min)
                     tree-buffer-indent-w/o-images-before-min
                   tree-indent)
                 tree-buffer-expand-symbol-before expand-symbol-before))
          (t
           (setq tree-buffer-indent
                 (if (< tree-indent
                        tree-buffer-indent-w/o-images-after-min)
                     tree-buffer-indent-w/o-images-after-min
                   tree-indent)
                 tree-buffer-expand-symbol-before expand-symbol-before)))

    (setq tree-buffer-incr-searchpattern-indent-prefix
          (tree-buffer-gen-searchpattern-indent-prefix))
    
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
      
      ;; This lets the GNU Emacs user scroll as if we had a horiz.
      ;; scrollbar...
      (define-key tree-buffer-key-map
        [mode-line mouse-1] 'tree-buffer-mouse-hscroll)
      
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
    (ignore-errors (kill-buffer buffer))))

;;; Tree node

(defsubst tree-node-add-child (node child)
  (tree-node-set-children node (append (tree-node-get-children node) (list child)))
  (tree-node-set-parent child node))

(defsubst tree-node-add-child-first (node child)
  (tree-node-set-children node (cons child (tree-node-get-children node)))
  (tree-node-set-parent child node))

(defsubst tree-node-sort-children (node sortfn)
  (tree-node-set-children node (sort (tree-node-get-children node) sortfn)))

(defsubst tree-node-remove-child (node child)
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
  "Decides if the node name can be shortened when displayed in a narrow tree
buffer window. The following values are valid:
- beginning: The name is truncated at the beginning so the end is always
  visible.
- end: The name is truncated at the end. If the node is expandable the name is
  truncated so that the expand symbol is visible.
- nil: The name is never truncated." )

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

(defun tree-node-new-root ()
  "Creates a new root node. The root node has always NAME=\"root\", TYPE=-1
and DATA=nil."
  (tree-node-new "root" -1 nil))

(defsubst tree-node-get-name (node)
  (aref node tree-node-name))

(defsubst tree-node-set-name (node name)
  (aset node tree-node-name name))

(defsubst tree-node-get-type (node)
  (aref node tree-node-type))

(defsubst tree-node-set-type (node type)
  (aset node tree-node-type type))

(defsubst tree-node-get-data (node)
  (aref node tree-node-data))

(defsubst tree-node-set-data (node data)
  (aset node tree-node-data data))

(defsubst tree-node-is-expanded (node)
  (aref node tree-node-expanded))

(defsubst tree-node-set-expanded (node expanded)
  (aset node tree-node-expanded expanded))

(defsubst tree-node-is-expandable (node)
  (aref node tree-node-expandable))

(defsubst tree-node-set-expandable (node expandable)
  (aset node tree-node-expandable expandable))

(defsubst tree-node-get-parent (node)
  (aref node tree-node-parent))

(defsubst tree-node-set-parent (node parent)
  (aset node tree-node-parent parent))

(defsubst tree-node-get-children (node)
  (aref node tree-node-children))

(defsubst tree-node-set-children (node children)
  (aset node tree-node-children children))

(defsubst tree-node-toggle-expanded (node)
  (tree-node-set-expanded node (not (tree-node-is-expanded node))))

(defun tree-node-get-depth (node)
  (let ((parent (tree-node-get-parent node)))
    (if parent
        (1+ (tree-node-get-depth parent))
      '0)))

(defsubst tree-node-set-shorten-name (node shorten)
  (aset node tree-node-shorten-name shorten))

(defsubst tree-node-get-shorten-name (node)
  (aref node tree-node-shorten-name))

(silentcomp-provide 'tree-buffer)

;;; tree-buffer.el ends here
