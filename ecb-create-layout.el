;;; ecb-create-layout.el --- creating new layouts

;; Copyright (C) 2001 Jesper Nordenberg
;; Copyright (C) 2001 Free Software Foundation, Inc.
;; Copyright (C) 2001 Klaus Berndl <klaus.berndl@sdm.de>

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: java, class, browser

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Contains code for easy creating new layouts

;;; Code

(eval-when-compile
  (require 'ecb-bytecomp))

(require 'ecb-mode-line)
(require 'ecb-util)

;; XEmacs stuff
(ecb-bytecomp-defvar vertical-divider-map)
(ecb-bytecomp-defvar modeline-map)
;; Emacs 21.X stuff
(ecb-bytecomp-defvar automatic-hscrolling)
(ecb-bytecomp-defvar before-make-frame-hook)
(ecb-bytecomp-defvar after-make-frame-functions)


(defgroup ecb-create-layout nil
  "Settings for creating new ECB-layouts."
  :group 'ecb-layout
  :prefix "ecb-create-layout-")

(defcustom ecb-create-layout-file "~/.ecb-user-layouts.el"
  "*File where all layouts created by `ecb-create-new-layout' are stored."
  :group 'ecb-create-layout
  :type 'file)

(defcustom ecb-create-layout-frame-width 110
  "*Frame width of the layout creation frame."
  :group 'ecb-create-layout
  :type 'integer)

(defcustom ecb-create-layout-frame-height 42
  "*Frame height of the layout creation frame."
  :group 'ecb-create-layout
  :type 'integer)


;; internal vars and consts


(defconst ecb-create-layout-buf-prefix " *ECB-LC-")
(defconst ecb-create-layout-frame-name "Creation of a new ECB-layout")
(defconst ecb-create-layout-all-buf-types
  '("directories" "history" "methods" "sources"))

(defconst ecb-create-layout-help-text-left-right
  "
 ECB layout creation mode
 ========================

 This is the help-screen of this mode. The window displaying
 this help text is called the edit-window which is neither
 selectable nor deletable nor splittable in this mode.

 <left/right/up/down-arrow>: Moving around in current window
 C-n, C-p: Go to next/previous window (beside the edit-window)

 C-s: Split current window. You will be asked:
      - If \"vertical\" or \"horizontal\" split
      - How to split: \"at-point\", \"half\" or \"other\" (i.e.
        you can specify any fraction between 0.1 and 0.9)
      - Which type (\"directories\", \"sources\", \"methods\"
        or \"history\") the current window should be.
 C-u: Delete current window
 C-t: Give the current window a type (\"directories\", \"sources\",
      \"methods\" or \"history\")

 C-c: Cancel layout creation. This does not save the layout.
      Deletes this frame.
 C-q: Save current defined layout and quit the layout creation.
      You will be asked for a layout-number. Deletes this frame.

There are NO other commands or keys avaliable. ALL other keys
are disabled in this mode!
")

(defconst ecb-create-layout-help-text-top
  " ECB layout creation mode
 ========================

 This is the help-screen of this mode. The window displaying this help text is called
 the edit-window which is neither selectable nor deletable nor splittable in this mode.

 <left/right/up/down-arrow>: Moving around in current window
 C-n, C-p: Go to next/previous window (beside the edit-window)

 C-s: Split current window. You will be asked:
      - If \"vertical\" or \"horizontal\" split
      - How to split: \"at-point\", \"half\" or \"other\" (i.e. you can specify any
        fraction between 0.1 and 0.9)
      - Which type (\"directories\", \"sources\", \"methods\" or \"history\") the current
        window should be.
 C-u: Delete current window
 C-t: Give the current window a type (\"directories\", \"sources\", \"methods\" or \"history\")

 C-c: Cancel layout creation. This does not save the layout. Deletes this frame.
 C-q: Save current defined layout and quit the layout creation. You will be asked for a
      layout-number. Deletes this frame.

There are NO other commands or keys avaliable. ALL other keys are disabled in this mode!
")

(defconst ecb-create-layout-file-header
     "

;; Copyright (C) 2001 Jesper Nordenberg
;; Copyright (C) 2001 Free Software Foundation, Inc.
;; Copyright (C) 2001 Klaus Berndl <klaus.berndl@sdm.de>

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;; Keywords: java, class, browser

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains all user-defined ECB-layouts created by the command
;; `ecb-create-new-layout'.

;; !!! DO NOT EDIT THIS FILE MANUALLY - IT IS GENERATED BY ECB !!!

")


(defvar ecb-create-layout-frame nil)
(defvar ecb-create-layout-edit-window nil)

(defvar ecb-create-layout-old-global-map nil)
(defvar ecb-create-layout-old-minor-mode-map-alist nil)
(defvar ecb-create-layout-old-hscroll nil)
(defvar ecb-create-layout-old-debug-on-error nil)
(defvar ecb-create-layout-old-frame nil)
(defvar ecb-create-layout-old-vertical-div-map nil)
(defvar ecb-create-layout-old-modeline-map nil)
(defvar ecb-create-layout-old-after-frame-h nil)
(defvar ecb-create-layout-old-before-frame-h nil)

(defvar ecb-create-layout-generated-lisp nil)
(defvar ecb-create-layout-gen-counter 0)

(defvar ecb-create-layout-user-layout-max-nr 99)

(defvar ecb-create-layout-buf-types nil)

;; can be 'left, 'right or 'top
(defvar ecb-create-layout-type 'left)

(defun ecb-create-layout-initilize ()
  (setq ecb-create-layout-buf-types
        (copy-list ecb-create-layout-all-buf-types))
  (setq ecb-create-layout-frame nil)
  (setq ecb-create-layout-edit-window nil)
  (setq ecb-create-layout-old-global-map nil)
  (setq ecb-create-layout-old-minor-mode-map-alist nil)
  (setq ecb-create-layout-old-hscroll nil)
  (setq ecb-create-layout-old-frame nil)
  (when ecb-running-xemacs
    (setq ecb-create-layout-old-vertical-div-map nil)
    (setq ecb-create-layout-old-modeline-map nil))
  (when ecb-running-emacs-21
    (setq ecb-create-layout-old-after-frame-h nil)
    (setq ecb-create-layout-old-before-frame-h nil))
  (setq ecb-create-layout-generated-lisp nil)
  (setq ecb-create-layout-gen-counter 0))

(defadvice delete-frame (before ecb-create-layout)
  (let ((frame (or (ad-get-arg 0) (selected-frame))))
    (when (string= (ecb-frame-parameter frame 'name)
                   ecb-create-layout-frame-name)
      (ecb-create-layout-cancel))))

(defun ecb-create-layout-frame-ok ()
  "Return not nil if current frame is the `ecb-create-layout-frame'"
  (and ecb-create-layout-frame
       (frame-live-p ecb-create-layout-frame)
       (equal (selected-frame) ecb-create-layout-frame)))


(defun ecb-create-layout-cancel (&rest ignore)
  (interactive)
  (when (ecb-create-layout-frame-ok)
    (ecb-create-layout-clear-all (interactive-p))
    (message "ECB Layout Creation canceled - the layout is not saved!")))

(defun ecb-create-layout-clear-all (&optional delete-frame)
  "Resets all stuff to state before `ecb-create-new-layout' was called. If
DELETE-FRAME is not nil then the new created frame will be deleted and the
`ecb-create-layout-old-frame' will be selected."
  ;; disabling the advice
  (ad-disable-advice 'delete-frame 'before 'ecb-create-layout)
  (ad-activate 'delete-frame)
  ;; killing all white-space-filled layout-buffers
  (dolist (b (buffer-list ecb-create-layout-frame))
    (if (string-match "^ \\*ECB-LC-" (buffer-name b))
        (kill-buffer b)))
  ;; restore the global-map
  (if (keymapp ecb-create-layout-old-global-map)
      (use-global-map ecb-create-layout-old-global-map))
  ;; restore the minor-mode-maps
  (if ecb-create-layout-old-minor-mode-map-alist
      (setq minor-mode-map-alist
            ecb-create-layout-old-minor-mode-map-alist))
  ;; restore horiz. scrolling
  (if ecb-running-emacs-21
      (setq automatic-hscrolling ecb-create-layout-old-hscroll))
  ;; for XEmacs restore these maps
  (when ecb-running-xemacs
    (setq vertical-divider-map ecb-create-layout-old-vertical-div-map)
    (setq modeline-map ecb-create-layout-old-modeline-map))
  ;; before and after makeing frame stuff
  (when ecb-running-emacs-21
    (setq before-make-frame-hook ecb-create-layout-old-before-frame-h)
    (setq after-make-frame-functions ecb-create-layout-old-after-frame-h))
  ;; restore old debug-on-error
  (setq debug-on-error ecb-create-layout-old-debug-on-error)
  ;; delete the layout-frame and select the ecb-create-layout-old-frame
  (when delete-frame
    (when (and ecb-create-layout-old-frame
               (frame-live-p ecb-create-layout-old-frame))
      (raise-frame ecb-create-layout-old-frame)
      (select-frame ecb-create-layout-old-frame))
    (when (and ecb-create-layout-frame
               (frame-live-p ecb-create-layout-frame))
      (ad-with-originals 'delete-frame
        (delete-frame ecb-create-layout-frame))))
  (setq ecb-create-layout-frame nil))

(defun ecb-create-layout-save-and-quit (&rest ignore)
  "Quit the ECB Layout creation and save the defined layout."
  (interactive)
  (when (ecb-create-layout-frame-ok)
    (if (ecb-create-layout-ready-for-save-p)
        (let ((delete-frame (interactive-p)))
          ;; if an error occurs during `ecb-create-layout-save-layout' or the
          ;; user hits C-q we must clean the layout creation stuff!
          (unwind-protect
              (ecb-create-layout-save-layout)
            ;; clean the layout creation stuff
            (ecb-create-layout-clear-all delete-frame)
            (message "ECB Layout Creation finished.")))
      (error "You must give every ECB-tree-window a type (use C-t)!"))))


(defun ecb-create-layout-ready-for-save-p ()
  "Returns only nil if all windows in current layout have a type."
  (let ((save-p t))
    (save-excursion
      (dolist (win (window-list (selected-frame)))
        (unless (equal win ecb-create-layout-edit-window)
          (set-buffer (window-buffer win))
          (setq save-p (ecb-create-layout-buffer-type)))))
    save-p))

(defun ecb-create-layout-nr-usable-p (nr &optional commit)
  (if (<= nr ecb-create-layout-user-layout-max-nr)
      (progn
        (if commit
            (ecb-query-string
             (format "The number must be >= %d. Please commit!"
                     (1+ ecb-create-layout-user-layout-max-nr)) nil))
        nil)
    (if (fboundp (intern (format "ecb-layout-function-%d" nr)))
        (progn
          (if commit
              (ecb-query-string
               (format "Layout %d is already defined. Use another numberr. Please commit!"
                       nr) nil))
          nil)
      t)))


(defmacro ecb-create-layout-insert-line (line)
  "Insert LINE in current-buffer and adds a newline."
  `(progn
     (insert ,line)
     (insert "\n")))

(defun ecb-create-layout-insert-file-header ()
  (insert (format ";;; %s --- user defined ECB-layouts" ;;
                  (file-name-nondirectory ecb-create-layout-file)))
  (insert ecb-create-layout-file-header))

(defun ecb-create-layout-save-layout ()
  "Saves current layout in `ecb-create-layout-file'."
  ;; make edit-window the current selected window
  (ecb-create-layout-select-edit-window)
  ;; we need the reversed sequence of the generated code
  (setq ecb-create-layout-generated-lisp
        (nreverse ecb-create-layout-generated-lisp))
  ;; now we have the create-code in the right sequence so we can save the new
  ;; layout in the user-layout file
  (let ((layout-nr 0))
    ;; a repeat...until-loop
    (while (progn
             ;;the while body
             (setq layout-nr
                   (string-to-number
                    (read-string
                     (format "Insert layout-number (must be >= %d): "
                             (1+ ecb-create-layout-user-layout-max-nr))
                     (number-to-string (1+
                     ecb-create-layout-user-layout-max-nr)))))
             ;; the while condition
             (not (ecb-create-layout-nr-usable-p layout-nr t))))
    (setq ecb-create-layout-user-layout-max-nr layout-nr)
    (with-temp-file (expand-file-name ecb-create-layout-file)
      (erase-buffer)
      (if (file-readable-p (expand-file-name ecb-create-layout-file))
          (insert-file-contents (expand-file-name ecb-create-layout-file))
        (ecb-create-layout-insert-file-header))
      (goto-char (point-min))
      ;; delete the old user-max-nr
      (when (re-search-forward
             "^(setq ecb-create-layout-user-layout-max-nr [0-9]+)$" nil t)
        (kill-region (match-beginning 0) (match-end 0)))
      (goto-char (point-max))
      (ecb-create-layout-insert-line "")
      ;; insert header of the layout-define macro
      (ecb-create-layout-insert-line
       (format "(ecb-layout-define %d %s nil"
               layout-nr
               (symbol-name ecb-create-layout-type)))
      ;; insert all the generated layout-code of the new layout
      (dolist (line ecb-create-layout-generated-lisp)
        (ecb-create-layout-insert-line
         (format "  %s" line)))
      ;; close the new layout-function
      (ecb-create-layout-insert-line "  )")
      (ecb-create-layout-insert-line "")
      ;; insert the new user-max-nr
      (ecb-create-layout-insert-line
       (format "(setq ecb-create-layout-user-layout-max-nr %d)" layout-nr))
      (ecb-create-layout-insert-line ""))
    ;; now we load the new layout
    (load-file (expand-file-name ecb-create-layout-file))
    (message "The new layout is saved in %s, loaded and available!"
             ecb-create-layout-file)))

(defun ecb-create-layout-gen-lisp (lisp-statement)
  (setq ecb-create-layout-generated-lisp
        (cons lisp-statement ecb-create-layout-generated-lisp)))

(defun ecb-create-layout-split-ver (&optional fraction)
  (let ((factor (or fraction
                    (/ (float (count-lines (window-start) (point)))
                       (float (- (window-height) 2))))))
    (ecb-split-ver factor t)
    (ecb-create-layout-gen-lisp `(ecb-split-ver ,factor t))
    factor))

(defun ecb-create-layout-split-hor (&optional fraction)
  (let ((factor (or fraction
                    (/ (float (- (point) (ecb-line-beginning-pos)))
                       (float (- (window-width) 3))))))
    (ecb-split-hor factor t)
    (ecb-create-layout-gen-lisp `(ecb-split-hor ,factor t))
    (beginning-of-line)
    factor))

(defun ecb-create-layout-add-to-buf-types (type)
  (when (stringp type)
    (add-to-list 'ecb-create-layout-buf-types type)
    (setq ecb-create-layout-buf-types
          (sort ecb-create-layout-buf-types 'string-lessp))))

(defun ecb-create-layout-remove-from-buf-type (type)
  (when (stringp type)
    (setq ecb-create-layout-buf-types
          (sort (delete type ecb-create-layout-buf-types) 'string-lessp))))

(defun ecb-create-layout-buffer-type ()
  (get-text-property (point-min) 'ecb-create-layout-type))

(defun ecb-create-layout-buffer-factor ()
  (get-text-property (point-min) 'ecb-create-layout-factor))

(defun ecb-create-layout-set-buffer-type (type)
  (let ((buffer-read-only nil))
    (put-text-property (point-min) (1+ (point-min)) 'ecb-create-layout-type
                       type)))

(defun ecb-create-layout-set-buffer-factor (factor)
  (let ((buffer-read-only nil))
    (put-text-property (point-min) (1+ (point-min)) 'ecb-create-layout-factor
                       factor)))

(defun ecb-create-layout-set-buffer-to-type (&optional type)
  (interactive)
  (when (ecb-create-layout-frame-ok)
    ;; adding the old buffer type to the available-list
    (ecb-create-layout-add-to-buf-types (or type
                                            (ecb-create-layout-buffer-type)))
    (let ((new-type (or (and (stringp type) type)
                        (ecb-query-string "Type of current ECB-tree-buffer:"
                                          ecb-create-layout-buf-types))))
      ;; removing the new buffer type from the available-list
      (ecb-create-layout-remove-from-buf-type new-type)
      (ecb-mode-line-set (buffer-name (current-buffer))
                         (concat "ECB " new-type))
      ;; setting the new buffer type in the buffer itself
      (ecb-create-layout-set-buffer-type new-type)
      (when (interactive-p)
        (ecb-create-layout-gen-lisp `(ecb-set-buffer ,(concat "ecb-"
                                                              new-type
                                                              "-buffer-name")))
        (ecb-create-layout-next-window))
      new-type)))

(defun ecb-create-layout-select-edit-window ()
  (let ((counter 0))
    (while (not (equal (selected-window) ecb-create-layout-edit-window))
      (other-window 1)
      (setq counter (1+ counter)))
    (ecb-create-layout-gen-lisp `(dotimes (i ,counter)
                                   (other-window 1)
                                   (if (equal (selected-window)
                                              ecb-compile-window)
                                       (other-window 1))))))

(defun ecb-create-layout-split ()
  (interactive)
  (when (ecb-create-layout-frame-ok)
    ;; splitting
    (let* ((old-buf-type (ecb-create-layout-buffer-type))
           (split-choices (if (equal ecb-create-layout-type 'top)
                              '("horizontal" "vertical")
                            '("vertical" "horizontal")))
           (split-type (ecb-query-string "Split type:" split-choices))
           (split-method
            (ecb-query-string "Split method:"
                              '("at-point" "half")
                              "Insert a fraction between 0.1 and 0.9"))
           (fraction (cond ((string= split-method "at-point")
                            nil)
                           ((string= split-method "half")
                            0.5)
                           ((floatp (string-to-number split-method))
                            (string-to-number split-method))
                           (t 0.5)))
           (real-split-factor
            (if (string= split-type "horizontal")
                (ecb-create-layout-split-hor fraction)
              (ecb-create-layout-split-ver fraction))))
      ;; creating new fitting buffers
      (save-selected-window
        (ecb-create-layout-new-buffer)
        (select-window (next-window))
        (ecb-create-layout-new-buffer))
      ;; asking for the buffer type
      (ecb-create-layout-set-buffer-factor real-split-factor)
      (ecb-create-layout-gen-lisp
       `(ecb-set-buffer
         ,(concat "ecb-"
                  (ecb-create-layout-set-buffer-to-type old-buf-type)
                  "-buffer-name")))
      (ecb-create-layout-next-window))))

(defun ecb-create-layout-forward-char ()
  (interactive)
  (when (ecb-create-layout-frame-ok)
    (unless (> (- (point) (ecb-line-beginning-pos)) (- (window-width)
                                                       (if ecb-running-emacs-21
                                                           2
                                                         3)))
      (call-interactively 'forward-char))))

(defun ecb-create-layout-next-window ()
  (interactive)
  (when (ecb-create-layout-frame-ok)
    (let ((steps (if (equal (next-window) ecb-create-layout-edit-window) 2 1)))
      (other-window steps)
      (ecb-create-layout-gen-lisp `(dotimes (i ,steps)
                                     (other-window 1)
                                     (if (equal (selected-window)
                                                ecb-compile-window)
                                         (other-window 1)))))))

(defun ecb-create-layout-previous-window ()
  (interactive)
  (when (ecb-create-layout-frame-ok)
    (let ((steps (if (equal (previous-window (selected-window) 0)
                            ecb-create-layout-edit-window)
                     -2 -1)))
      (other-window steps)
      (ecb-create-layout-gen-lisp `(dotimes (i ,(abs steps))
                                     (other-window -1)
                                     (if (equal (selected-window)
                                                ecb-compile-window)
                                         (other-window -1)))))))


(defun ecb-create-layout-delete-window ()
  (interactive)
  (when (ecb-create-layout-frame-ok)
    (unless (or (equal (selected-window) ecb-create-layout-edit-window)
                (equal (next-window)
                       (previous-window (selected-window) 0)))
      (let ((go-back (if (equal ecb-create-layout-type 'right)
                         (not (= (nth 1 (window-edges)) 0))
                       (not (and (= (nth 0 (window-edges)) 0)
                                 (= (nth 1 (window-edges)) 0))))))
        ;; add the buffer type of the deleted window to the available-list
        (ecb-create-layout-add-to-buf-types (ecb-create-layout-buffer-type))
        (kill-buffer (current-buffer))
        (delete-window)
        (ecb-create-layout-gen-lisp '(delete-window))
        (if go-back (ecb-create-layout-previous-window))
        ;; add the buffer type of the new bigger window to the available-list
        (ecb-create-layout-add-to-buf-types (ecb-create-layout-buffer-type))
        (kill-buffer (current-buffer))
        (ecb-create-layout-new-buffer)))))

(defvar ecb-create-layout-mode-map nil
  "`ecb-create-layout-mode' keymap.")

(if ecb-create-layout-mode-map
    ()
  (setq ecb-create-layout-mode-map (make-sparse-keymap))
;;  (suppress-keymap ecb-create-layout-mode-map t)

  ;; for minibuffer insertion we need the following
  (dotimes (i 26)
    (define-key ecb-create-layout-mode-map
      (string (+ i 97)) 'self-insert-command))

  (dotimes (i 26)
    (define-key ecb-create-layout-mode-map
      (string (+ i 65)) 'self-insert-command))

  (dotimes (i 10)
    (define-key ecb-create-layout-mode-map
      (string (+ i 48)) 'self-insert-command))

  (if ecb-running-xemacs
      (define-key ecb-create-layout-mode-map (kbd "<BS>")
        'delete-backward-char)
    (define-key ecb-create-layout-mode-map (kbd "<DEL>")
      'backward-delete-char-untabify))

  (define-key ecb-create-layout-mode-map (kbd "C-q")
    'ecb-create-layout-save-and-quit)
  (define-key ecb-create-layout-mode-map (kbd "C-c")
    'ecb-create-layout-cancel)
  (define-key ecb-create-layout-mode-map (kbd "C-u")
    'ecb-create-layout-delete-window)
  (define-key ecb-create-layout-mode-map (kbd "C-s") 'ecb-create-layout-split)
  (define-key ecb-create-layout-mode-map (kbd "C-t")
    'ecb-create-layout-set-buffer-to-type)
  (define-key ecb-create-layout-mode-map (kbd "<left>") 'backward-char)
  (define-key ecb-create-layout-mode-map (kbd "<right>")
    'ecb-create-layout-forward-char)
  (define-key ecb-create-layout-mode-map (kbd "<up>") 'previous-line)
  (define-key ecb-create-layout-mode-map (kbd "<down>") 'next-line)
  (define-key ecb-create-layout-mode-map (kbd "C-n")
    'ecb-create-layout-next-window)
  (define-key ecb-create-layout-mode-map (kbd "C-p")
    'ecb-create-layout-previous-window)
  (define-key ecb-create-layout-mode-map (kbd "C-h v")
    'describe-variable)
  (define-key ecb-create-layout-mode-map (kbd "C-h k")
    'describe-key)
  (define-key ecb-create-layout-mode-map (kbd "C-h d")
    'ecb-create-layout-debug)
  (define-key ecb-create-layout-mode-map (kbd "M-<down>")
    'scroll-other-window)
  (set-keymap-parent ecb-create-layout-mode-map nil))


(defun ecb-create-layout-new-buffer (&optional do-not-fill)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer (generate-new-buffer ecb-create-layout-buf-prefix))
  (erase-buffer)
  (unless do-not-fill
    (dotimes (i (window-height))
      (insert-string
       (format "%s\n"
               (make-string (- (window-width)
                               (if ecb-running-emacs-21 1 3))
                            ?\ )))))
  (goto-char (point-min))
  (ecb-create-layout-mode)
  (set-window-dedicated-p (selected-window) t))



(defun ecb-create-layout-mode ()
  "Major mode for creating new ECB-layouts."
  (setq major-mode 'ecb-create-layout-mode)
  (setq mode-name "ECB Create-Layout")
  (use-local-map ecb-create-layout-mode-map)
  (make-variable-buffer-local 'buffer-read-only)
  (ecb-mode-line-set (buffer-name (current-buffer)) "")
  (setq buffer-read-only t))

(defun ecb-create-layout-init-layout (&optional new)
  (delete-other-windows)
  (ecb-create-layout-new-buffer)
  (when new
    (setq ecb-create-layout-type (intern (ecb-query-string
                                          "Location of the the ECB-tree-windows:"
                                          '("left" "right" "top")))))
  (cond ((equal ecb-create-layout-type 'left)
         (ecb-split-hor ecb-windows-width))
        ((equal ecb-create-layout-type 'right)
         (ecb-split-hor (- ecb-windows-width) t))
        ((equal ecb-create-layout-type 'top)
         (ecb-split-ver ecb-windows-height)))
  ;; we set the buffer in the big edit-window
  (ecb-create-layout-new-buffer t)
  ;; now we insert the help in the edit-window
  (let ((buffer-read-only nil))
    (insert (if (equal ecb-create-layout-type 'top)
                ecb-create-layout-help-text-top
              ecb-create-layout-help-text-left-right)))
  (setq ecb-create-layout-edit-window (selected-window))
  (ecb-mode-line-set (buffer-name (current-buffer)) "   ECB edit-window")
  ;; The edit window must not be dedicated
  (set-window-dedicated-p (selected-window) nil)
  (other-window 1)
  ;; we set the buffer for the (currently unsplitted) ECB-window
  (ecb-create-layout-new-buffer))

(defun ecb-create-layout-make-frame ()
  "Create a new frame for the layout creation process and return it."
  (if ecb-running-xemacs
      (make-frame `((name . ,ecb-create-layout-frame-name)
                    (minibuffer . t)
                    (user-position . t)
                    (width . ,ecb-create-layout-frame-width)
                    (height . ,ecb-create-layout-frame-height)
                    (default-toolbar-visible-p . nil)
                    (left-toolbar-visible-p . nil)
                    (right-toolbar-visible-p . nil)
                    (top-toolbar-visible-p . nil)
                    (bottom-toolbar-visible-p . nil)
                    (default-gutter-visible-p . nil)
                    (left-gutter-visible-p . nil)
                    (right-gutter-visible-p . nil)
                    (top-gutter-visible-p . nil)
                    (bottom-gutter-visible-p . nil)
                    (has-modeline-p . t)
                    (use-left-overflow . nil)
                    (vertical-scrollbar-visible-p . nil)
                    (horizontal-scrollbar-visible-p . nil)
                    (use-right-overflow . nil)
                    (menubar-visible-p . nil)))
    (make-frame `((name . ,ecb-create-layout-frame-name)
                  (minibuffer . t)
                  (user-position . t)
                  (width . ,ecb-create-layout-frame-width)
                  (height . ,ecb-create-layout-frame-height)
                  (vertical-scroll-bars . nil)
                  (horizontal-scroll-bars . nil)
                  (tool-bar-lines . 0)
                  (menu-bar-lines . 0)))))


(defun ecb-create-new-layout ()
  "Start process for interactively creating a new ECB-layout."
  (interactive)
  (if (not (or ecb-running-emacs-21 ecb-running-xemacs))
      (error "This command works not with Emacs 20.X; use the macro 'ecb-layout-define'!")
    (ecb-create-layout-initilize)

    ;; before- and after make frame stuff
    (when ecb-running-emacs-21
      (setq ecb-create-layout-old-after-frame-h after-make-frame-functions)
      (setq after-make-frame-functions nil)
      (setq ecb-create-layout-old-before-frame-h before-make-frame-hook)
      (setq before-make-frame-hook nil))
    
    ;; saving old frame
    (setq ecb-create-layout-old-frame (selected-frame))

    ;; creating new frame
    (setq ecb-create-layout-frame (ecb-create-layout-make-frame))
    (raise-frame ecb-create-layout-frame)
    (select-frame ecb-create-layout-frame)
    (ad-enable-advice 'delete-frame 'before 'ecb-create-layout)
    (ad-activate 'delete-frame)

    ;; global map
    (setq ecb-create-layout-old-global-map (current-global-map))
    (use-global-map ecb-create-layout-mode-map)

    ;; minor-modes map
    (setq ecb-create-layout-old-minor-mode-map-alist minor-mode-map-alist)
    (setq minor-mode-map-alist nil)

    ;; horiz. scrolling
    (when ecb-running-emacs-21
      (setq ecb-create-layout-old-hscroll automatic-hscrolling)
      (setq automatic-hscrolling nil))

    ;; for XEmacs modeline- and vertical-divider maps
    (when ecb-running-xemacs
      (setq ecb-create-layout-old-vertical-div-map vertical-divider-map)
      (setq vertical-divider-map nil)
      (setq ecb-create-layout-old-modeline-map modeline-map)
      (setq modeline-map nil))

    ;; debug on error
    (setq ecb-create-layout-old-debug-on-error debug-on-error)
    (setq debug-on-error nil)

    (ecb-create-layout-init-layout t)))

(defun ecb-create-layout-debug ()
  (interactive)
  (message "Layout-Debug: Type: %s, Factor: %s"
           (ecb-create-layout-buffer-type)
           (ecb-create-layout-buffer-factor)))

(ecb-provide 'ecb-create-layout)

;; ecb-help.el ends here
