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

(require 'ecb-mode-line)
(require 'ecb-util)

(defgroup ecb-create-layout nil
  "Settings for creating new ECB-layouts."
  :group 'ecb-layout
  :prefix "ecb-create-layout-")

(defcustom ecb-create-layout-file "~/.ecb-user-layouts.el"
  "*File where all layouts created by `ecb-create-new-layout' are stored."
  :group 'ecb-create-layout
  :type 'file)

(defvar ecb-create-layout-user-layout-max-nr 99)

(defconst ecb-create-layout-buf-prefix " *ECB-LC-")

(defconst ecb-create-layout-frame-name "Creation of a new ECB-layout")
(defconst ecb-create-layout-frame-width 100)
(defconst ecb-create-layout-frame-hight 40)

(defvar ecb-create-layout-frame nil)
(defvar ecb-create-layout-edit-window nil)

(defvar ecb-create-layout-old-global-map nil)
(defvar ecb-create-layout-old-minor-mode-map-alist nil)
(defvar ecb-create-layout-old-hscroll nil)

(defadvice delete-frame (before ecb-create-layout)
  (let ((frame (or (ad-get-arg 0) (selected-frame))))
    (when (string= (frame-parameter frame 'name)
                   ecb-create-layout-frame-name)
      (ecb-cancel-create-layout-mode))))

(defun ecb-cancel-create-layout-mode (&rest ignore)
  "Quit the ECB Layout Creation and save the defined layout."
  (interactive)
  (when (and ecb-create-layout-frame (frame-live-p ecb-create-layout-frame))
    (ad-disable-advice 'delete-frame 'before 'ecb-create-layout)
    (ad-activate 'delete-frame)
    (ecb-create-layout-save-layout)
    (dolist (b (buffer-list ecb-create-layout-frame))
      (if (string-match "^ \\*ECB-LC-" (buffer-name b))
          (kill-buffer b)))
    (if (keymapp ecb-create-layout-old-global-map)
        (use-global-map ecb-create-layout-old-global-map))
    (if ecb-create-layout-old-minor-mode-map-alist
        (setq minor-mode-map-alist
              ecb-create-layout-old-minor-mode-map-alist))
    (setq automatic-hscrolling ecb-create-layout-old-hscroll)
    (when (interactive-p)
      (raise-frame ecb-frame)
      (select-frame ecb-frame)
      (ad-with-originals 'delete-frame
        (delete-frame ecb-create-layout-frame)))
    (message "ECB Layout Creation canceled.")))

(defun ecb-create-layout-insert-line (line)
  (insert line)
  (insert "\n"))

(defun ecb-create-layout-save-layout ()
  ;; make edit-window the current selected window
  (ecb-create-layout-select-edit-window)
  ;; we need the reversed sequence of the generated code
  (setq ecb-create-layout-generated-lisp
        (nreverse ecb-create-layout-generated-lisp))
  ;; now we have the create-code in the right sequence so we can save the new
  ;; layout in the user-layout file
  (let ((layout-nr
         (string-to-number
          (read-string (format "Insert layout-number (must be >= %d): "
                               (1+ ecb-create-layout-user-layout-max-nr))))))
    (if (< layout-nr ecb-create-layout-user-layout-max-nr)
        (message "Layout not saved cause of a too small layout number!")
      (setq ecb-create-layout-user-layout-max-nr layout-nr)
      (with-temp-file ecb-create-layout-file
        (erase-buffer)
        (if (file-readable-p ecb-create-layout-file)
            (insert-file-contents ecb-create-layout-file)
          (ecb-create-layout-insert-file-header))
        (goto-char (point-min))
        ;; delete the old user-max-nr
        (when (re-search-forward
               "^(setq ecb-create-layout-user-layout-max-nr [0-9]+)$" nil t)
          (kill-region (match-beginning 0) (match-end 0)))
        (goto-char (point-max))
        (ecb-create-layout-insert-line "")
        ;; insert header of the new layout-function
        (ecb-create-layout-insert-line
         (format "(defun ecb-layout-function-%d ()" layout-nr))
        (ecb-create-layout-insert-line
         (format "  (ecb-layout-create-layout '%s"
                 (symbol-name ecb-create-layout-type)))
        ;; insert all the generated layout-code of the new layout
        (dolist (line ecb-create-layout-generated-lisp)
          (ecb-create-layout-insert-line
           (format "    %s" line)))
        ;; close the new layout-function
        (ecb-create-layout-insert-line "  ))")
        (ecb-create-layout-insert-line "")
        ;; insert the two aliases for the correct window deleting functions
        (ecb-create-layout-insert-line
         (format "(defalias 'ecb-delete-other-windows-in-editwindow-%d"
                 layout-nr))
        (ecb-create-layout-insert-line
         (format "  'ecb-delete-other-windows-ecb-windows-%s)"
                 (symbol-name ecb-create-layout-type)))
        (ecb-create-layout-insert-line
         (format "(defalias 'ecb-delete-window-in-editwindow-%d"
                 layout-nr))
        (ecb-create-layout-insert-line
         (format "  'ecb-delete-window-ecb-windows-%s)"
                 (symbol-name ecb-create-layout-type)))
        ;; end of the newly generated layout-function code
        (ecb-create-layout-insert-line "")
        ;; insert the new user-max-nr
        (ecb-create-layout-insert-line
         (format "(setq ecb-create-layout-user-layout-max-nr %d)" layout-nr))
        (ecb-create-layout-insert-line ""))
      ;; now we load the new layout
      (load-file ecb-create-layout-file))))
        
    
(defun ecb-create-layout-insert-file-header ()
  (insert (format ";;; %s --- user defined ECB-layouts" ;;
                  (file-name-nondirectory ecb-create-layout-file)))
  (insert
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

;; Contains all user-defined ECB-layouts


"))


(defvar ecb-create-layout-generated-lisp nil)
(defvar ecb-create-layout-gen-counter 0)
(defun ecb-create-layout-gen-lisp (lisp-statement)
  (message "Lisp-statement Nr. %d: %s"
           (setq ecb-create-layout-gen-counter
                 (1+ ecb-create-layout-gen-counter))
           lisp-statement)
  (setq ecb-create-layout-generated-lisp
        (cons lisp-statement ecb-create-layout-generated-lisp)))

(defun ecb-create-layout-split-ver (&optional fraction)
  (let ((factor (or fraction
                    (/ (float (count-lines (window-start) (point)))
                       (float (window-height))))))
    (ecb-split-ver factor t)
    (ecb-create-layout-gen-lisp `(ecb-split-ver ,factor t))
    factor))

(defun ecb-create-layout-split-hor (&optional fraction)
  (let ((factor (or fraction
                    (/ (float (- (point) (line-beginning-position)))
                       (float (1- (window-width)))))))
    (ecb-split-hor factor t)
    (ecb-create-layout-gen-lisp `(ecb-split-hor ,factor t))
    (beginning-of-line)
    factor))

(defconst ecb-create-layout-all-buf-types
  '("directories" "history" "methods" "sources"))

(defvar ecb-create-layout-buf-types nil)

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
  ;; adding the old buffer type to the available-list
  (ecb-create-layout-add-to-buf-types (or type
                                          (ecb-create-layout-buffer-type)))
  (let ((new-type (or (and (stringp type) type)
                      (ecb-query-string "Type of current ECB-tree-buffer:"
                                        ecb-create-layout-buf-types))))
    ;; removing the new buffer type from the available-list
    (ecb-create-layout-remove-from-buf-type new-type)
    (ecb-mode-line-set (buffer-name (current-buffer)) new-type)
    ;; setting the new buffer type in the buffer itself
    (ecb-create-layout-set-buffer-type new-type)
    (when (interactive-p)
      (ecb-create-layout-gen-lisp `(ecb-set-buffer ,(concat "ecb-"
                                                            new-type
                                                            "-buffer-name")))
      (ecb-create-layout-next-window))
    new-type))

(defun ecb-create-layout-split ()
  (interactive)
  ;; splitting
  (let* ((old-buf-type (ecb-create-layout-buffer-type))
         (split-choices (if (equal ecb-create-layout-type 'top)
                            '("Horizontal" "Vertical")
                          '("Vertical" "Horizontal")))
         (split-type (ecb-query-string "Split type:" split-choices))
         (split-method
          (ecb-query-string "Split method:"
                            '("At-point" "Half")
                            "Insert a fraction between 0.1 and 0.9"))
         (fraction (cond ((string= split-method "At-point")
                          nil)
                         ((string= split-method "Half")
                          0.5)
                         ((floatp (string-to-number split-method))
                          (string-to-number split-method))
                         (t 0.5)))
         (real-split-factor
          (if (string= split-type "Horizontal")
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
    (ecb-create-layout-next-window)))

(defun ecb-create-layout-forward-char ()
  (interactive)
  (unless (> (- (point) (line-beginning-position)) (- (window-width) 2))
    (call-interactively 'forward-char)))

(defun ecb-create-layout-next-window ()
  (interactive)
  (let ((steps (if (equal (next-window) ecb-create-layout-edit-window) 2 1)))
    (other-window steps)
    (ecb-create-layout-gen-lisp `(dotimes (i ,steps)
                                   (other-window 1)
                                   (if (equal (selected-window)
                                              ecb-compile-window)
                                       (other-window 1))))))

(defun ecb-create-layout-previous-window ()
  (interactive)
  (let ((steps (if (equal (previous-window (selected-window) 0)
                          ecb-create-layout-edit-window)
                   -2 -1)))
    (other-window steps)
    (ecb-create-layout-gen-lisp `(dotimes (i ,(abs steps))
                                   (other-window -1)
                                   (if (equal (selected-window)
                                              ecb-compile-window)
                                       (other-window -1))))))


(defun ecb-create-layout-delete-window ()
  (interactive)
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
      (ecb-create-layout-new-buffer))))

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

(defvar ecb-create-layout-mode-map nil
  "`ecb-create-layout-mode' keymap.")

(defun ecb-create-layout-debug-informations ()
  (interactive)
  (message "Debug: Buf-Type: %s, factor: %s, availables: %s"
           (ecb-create-layout-buffer-type)
           (ecb-create-layout-buffer-factor)
           ecb-create-layout-buf-types))

(if ecb-create-layout-mode-map
    ()
  (setq ecb-create-layout-mode-map (make-sparse-keymap))
;;  (suppress-keymap ecb-create-layout-mode-map t)

  ;; for minibuffer insertion we need the following
  (dotimes (i 26)
    (define-key ecb-create-layout-mode-map
      (string (+ i 97)) 'self-insert-command))

  (dotimes (i 10)
    (define-key ecb-create-layout-mode-map
      (string (+ i 48)) 'self-insert-command))

  (define-key ecb-create-layout-mode-map (kbd "<DEL>")
    'backward-delete-char-untabify)

  (define-key ecb-create-layout-mode-map (kbd "C-q")
    'ecb-cancel-create-layout-mode)
  (define-key ecb-create-layout-mode-map (kbd "C-c")
    'ecb-cancel-create-layout-mode)
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
  (define-key ecb-create-layout-mode-map (kbd "C-h k") 'describe-key)
  (define-key ecb-create-layout-mode-map (kbd "C-h m") 'describe-mode)
  (define-key ecb-create-layout-mode-map (kbd "C-h d")
    'ecb-create-layout-debug-informations)
  (define-key ecb-create-layout-mode-map (kbd "C-n") 'ecb-create-layout-next-window)
  (define-key ecb-create-layout-mode-map (kbd "C-p")
    'ecb-create-layout-previous-window)
;;   (define-key ecb-create-layout-mode-map [t] 'undefined)
  (set-keymap-parent ecb-create-layout-mode-map nil))


(defun ecb-create-layout-new-buffer ()
  (switch-to-buffer (generate-new-buffer ecb-create-layout-buf-prefix))
  (erase-buffer)
  (dotimes (i (window-height))
    (insert-string
     (format "%s\n"
             (make-string (1- (window-width)) ?\ ))))
  (goto-char (point-min))
  (ecb-create-layout-mode))


(defun ecb-create-layout-mode ()
  "Major mode for creating new ECB-layouts."
  (setq major-mode 'ecb-create-layout-mode)
  (setq mode-name "ECB Create-Layout")
  (use-local-map ecb-create-layout-mode-map)
  (make-variable-buffer-local 'buffer-read-only)
  (ecb-mode-line-set (buffer-name (current-buffer)) "")
  (setq buffer-read-only t))

;; can be 'left, 'right or 'top
(defvar ecb-create-layout-type 'left)

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
  (ecb-create-layout-new-buffer)
  (setq ecb-create-layout-edit-window (selected-window))
  (other-window 1)
  ;; we set the buffer for the (currently unsplitted) ECB-window
  (ecb-create-layout-new-buffer))  
  

(defun ecb-create-layout-initilize ()
  (setq ecb-create-layout-buf-types
        (copy-list ecb-create-layout-all-buf-types))
  (setq ecb-create-layout-frame nil)
  (setq ecb-create-layout-edit-window nil)
  (setq ecb-create-layout-old-global-map nil)
  (setq ecb-create-layout-old-minor-mode-map-alist nil)
  (setq ecb-create-layout-old-hscroll nil)

  (setq ecb-create-layout-generated-lisp nil)
  (setq ecb-create-layout-gen-counter 0))

(defun ecb-create-new-layout ()
  "Create a new ECB-layout."
  (interactive)
  (ecb-create-layout-initilize)
  (setq ecb-create-layout-frame
        (make-frame `((name . ,ecb-create-layout-frame-name)
                      (minibuffer . t)
                      (user-position . t)
                      (width . ,ecb-create-layout-frame-width)
                      (height . ,ecb-create-layout-frame-hight)
                      (vertical-scroll-bars . nil)
                      (horizontal-scroll-bars . nil)
                      (tool-bar-lines . 0)
                      (menu-bar-lines . 0))))
  (raise-frame ecb-create-layout-frame)
  (select-frame ecb-create-layout-frame)
  (ad-enable-advice 'delete-frame 'before 'ecb-create-layout)
  (ad-activate 'delete-frame)
  (setq ecb-create-layout-old-global-map (current-global-map))
  (use-global-map ecb-create-layout-mode-map)
  (setq ecb-create-layout-old-minor-mode-map-alist minor-mode-map-alist)
  (setq minor-mode-map-alist nil)
  (setq ecb-create-layout-old-hscroll automatic-hscrolling)
  (setq automatic-hscrolling nil)
  (ecb-create-layout-init-layout t))
    
(provide 'ecb-create-layout)

;; ecb-help.el ends here
